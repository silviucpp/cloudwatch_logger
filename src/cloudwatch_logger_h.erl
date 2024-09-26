-module(cloudwatch_logger_h).

-define(PRINT_MSG(Format, Args),
    io:format(Format, Args)).

-define(AWS_HEADERS(Target), [
    {<<"accept">>, <<"application/json">>},
    {<<"content-type">>, <<"application/x-amz-json-1.1">>},
    {<<"x-amz-target">>,<<"Logs_20140328.", Target/binary>>}
]).

-define(AWS_SCOPE_CLOUDWATCH, <<"logs">>).

-behaviour(gen_server).

-export([
    start_link/2,

    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    hostname,
    formatter,

    cloudwatch_group_name,
    cloudwatch_stream_name,
    upload_batch_max_size,
    upload_batch_inteval_ms,
    upload_failed_retry_count,
    upload_failed_retry_delay_ms,

    messages,
    msg_count,
    flush_timer
}).

start_link(Id, Opts) ->
    gen_server:start_link({local, Id}, ?MODULE, Opts, []).

init([Config, Formatter]) ->
    {ok, update_config(Config, Formatter, #state {
        hostname = get_hostname(),
        messages = [],
        msg_count = 0}
    )}.

handle_call({update_config, Config, Formatter}, _From, State) ->
    {reply, ok, update_config(Config, Formatter, State)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Msg}, State) ->
    {noreply, log_message(Msg, State)};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(push_messages, #state{msg_count = MsgCount} = State) ->
    case MsgCount > 0 of
        true ->
            {noreply, do_push_messages(State)};
        _ ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    case Reason of
        {stop, shutdown} ->
            flush(State),
            wait_for_childrens(self(), 1000);
        _ ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internals

log_message(Msg, #state {
    messages = MessageList,
    msg_count = MessageCount,
    upload_batch_max_size = UploadBatchMaxSize,
    upload_batch_inteval_ms = UploadBatchMaxIntervalMs,
    flush_timer = FlushTimerRef
} = State) ->

    NewMessageCount = MessageCount + 1,

    case NewMessageCount >= UploadBatchMaxSize of
        true ->
            do_push_messages(State#state{messages = [Msg | MessageList], msg_count = NewMessageCount});
        _ ->
            NewTimer = case FlushTimerRef of
                undefined ->
                    erlang:send_after(UploadBatchMaxIntervalMs, self(), push_messages);
                _ ->
                    FlushTimerRef
            end,

            State#state{flush_timer = NewTimer, messages = [Msg | MessageList], msg_count = NewMessageCount}
    end.

do_push_messages(#state {
    cloudwatch_group_name = CloudWatchGroupName,
    cloudwatch_stream_name = CloudWatchStreamName,
    upload_failed_retry_count = RetryCount,
    upload_failed_retry_delay_ms = RetryDelayMs,
    flush_timer = FlushTimerRef,
    formatter = Formatter,
    hostname = Hostname,
    messages = Messages} = State) ->

    case FlushTimerRef of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(FlushTimerRef)
    end,

    % Log events in a single PutLogEvents request must be in chronological order.
    % By default logger will provide them in chronological order but we can use also cloudwatch_logger:log to push events
    % from external sources like a kafka queue and those can have their own timestamps.
    
    SortedEvents = lists:sort(
        fun({[{<<"timestamp">>, Ts1} | _]}, {[{<<"timestamp">>, Ts2} | _]}) ->
            Ts1 =< Ts2
        end, lists:map(fun(X) -> cloudwatch_encoder:encode(X, Hostname, Formatter) end, lists:reverse(Messages))),

    spawn_link(fun() ->
        cloudwatch_send_logs(cloudwatch_utils:safe_json_encode([
            {<<"logGroupName">>, CloudWatchGroupName},
            {<<"logStreamName">>, CloudWatchStreamName},
            {<<"logEvents">>, SortedEvents}
        ]), RetryCount, RetryDelayMs)
    end),

    State#state{messages = [], msg_count = 0, flush_timer = undefined}.

flush(#state{msg_count = MsgCount} = State) ->
    case MsgCount > 0 of
        true ->
            ?PRINT_MSG("~p:flush_messages remaining messages: ~p, ~n", [?MODULE, MsgCount]),
            do_push_messages(State);
        _ ->
            ?PRINT_MSG("~p:flush_messages no remaining messages to push ~n", [?MODULE]),
            State
    end.

cloudwatch_send_logs(_PayloadJson, 0, _IntervalMs) ->
    ok;
cloudwatch_send_logs(PayloadJson, Retries, IntervalMs) ->
    case erlaws:post(<<"/">>, PayloadJson, ?AWS_SCOPE_CLOUDWATCH, ?AWS_HEADERS(<<"PutLogEvents">>)) of
        {ok, #{body := Body, status_code := HttpStatus}} ->
            case HttpStatus of
                200 ->
                    ok;
                _ ->
                    ?PRINT_MSG("~p:cloudwatch_send_logs failed status: ~p payload: ~p response: ~p retries: ~p ~n", [?MODULE, HttpStatus, PayloadJson, Body, Retries]),
                    timer:sleep(IntervalMs),
                    cloudwatch_send_logs(PayloadJson, Retries-1, IntervalMs)
            end;
        Error ->
            ?PRINT_MSG("~p:cloudwatch_send_logs payload: ~p error: ~p retries: ~p ~n", [?MODULE, PayloadJson, Error, Retries]),
            cloudwatch_send_logs(PayloadJson, Retries-1, IntervalMs)
    end.

get_hostname() ->
    {ok, Host} = inet:gethostname(),
    cloudwatch_utils:to_binary(Host).

wait_for_childrens(Pid, Timeout) ->
    {links, LinkedProcesses} = process_info(Pid, links),
    NumberChildrens = length(LinkedProcesses) - 1,

    case NumberChildrens > 0 of
        true ->
            ?PRINT_MSG("wait for childrens: ~p ~n", [NumberChildrens]),
            timer:sleep(Timeout),
            wait_for_childrens(Pid, Timeout);
        _
            -> ok
    end.

update_config(Config, Formatter, State) ->
    State#state {
        formatter = Formatter,
        cloudwatch_group_name = maps:get(cloudwatch_group, Config),
        cloudwatch_stream_name = maps:get(cloudwatch_stream, Config),
        upload_batch_max_size = erlang:min(10000, maps:get(upload_batch_max_size, Config, 50)),
        upload_batch_inteval_ms = maps:get(upload_batch_inteval_ms, Config, 5000),
        upload_failed_retry_count = maps:get(upload_failed_retry_count, Config, 3),
        upload_failed_retry_delay_ms = maps:get(upload_failed_retry_delay_ms, Config, 1000)
    }.
