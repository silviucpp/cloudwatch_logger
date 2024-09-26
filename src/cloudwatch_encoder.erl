-module(cloudwatch_encoder).

-export([
    encode/3
]).

% no_formatter and hostname inside event are related to some internal apps.

encode(#{level := Level, meta := Meta} = Event, Hostname, Formatter) ->
    {Module, _Function, _Arity} = maps:get(mfa, Meta, {null, null, null}),
    Ts = erlang:trunc(maps:get(time, Meta)/1000),
    {[
        {<<"timestamp">>, Ts},
        {<<"message">>, cloudwatch_utils:safe_json_encode([
            {<<"sv">>, severity2int(Level)},
            {<<"host">>, maps:get(hostname, Event, Hostname)},
            {<<"tags">>, cloudwatch_utils:to_binary(Module)},
            {<<"ts">>, Ts},
            {<<"msg">>, format_message(Event, Formatter)}
        ])}
    ]}.

% internals

format_message(Event, {FormatterModule, FormatterConfig}) ->
    case maps:get(no_formatter, Event, false) of
        true ->
            maps:get(msg, Event);
        _ ->
            cloudwatch_utils:to_binary(FormatterModule:format(Event, FormatterConfig))
    end.

severity2int(debug) ->
    7;
severity2int(info) ->
    6;
severity2int(notice) ->
    5;
severity2int(warning) ->
    4;
severity2int(error) ->
    3;
severity2int(critical) ->
    2;
severity2int(alert) ->
    1;
severity2int(emergency) ->
    0;
severity2int(_) ->
    7.
