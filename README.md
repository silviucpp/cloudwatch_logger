# cloudwatch_logger

[![Build Status](https://app.travis-ci.com/silviucpp/cloudwatch_logger.svg?branch=master)](https://travis-ci.com/github/silviucpp/cloudwatch_logger)
[![GitHub](https://img.shields.io/github/license/silviucpp/cloudwatch_logger)](https://github.com/silviucpp/cloudwatch_logger/blob/master/LICENSE)

OTP [logger][1] backend that sends log events to [Amazon CloudWatch][2].

## Quick start

Add to your `rebar.config`

```erlang
{deps, [cloudwatch_logger]}.
```

add the logger handler settings to your `sys.config` and also the `erlaws` configs:

```erlang
[
    {cloudwatch_logger, [
        {logger,
            [
                {handler, aws_cloudwatch_logger, cloudwatch_logger, #{
                    level => info,
                    config => #{
                        cloudwatch_group => <<"CLOUDWATCH_GROUP_NAME">>,
                        cloudwatch_stream => <<"CLOUDWATCH_STREAM_NAME">>,
                        upload_batch_max_size => 50,
                        upload_batch_inteval_ms => 5000,
                        upload_failed_retry_count => 3,
                        upload_failed_retry_delay_ms => 1000
                    },
                    formatter => {
                        logger_formatter, #{
                            single_line => true,
                            template => [pid, " ", mfa,":",line, " => ", msg],
                            time_offset => "Z"
                        }
                    },
                    filters => [
                        %{remote_group_leader, {fun logger_filters:remote_gl/2, stop}},
                        %{progress, {fun logger_filters:progress/2, stop}},
                        %{sasl, {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}}
                    ]
                }}
            ]}
    ]},

    {erlaws, [
        {access_key, <<"AWS_ACCESS_KEY_HERE">>},
        {secret_key, <<"AWS_SECRET_KEY_HERE">>},
        {default_region, <<"AWS_DEFAULT_REGION">>} % example: us-east-1
    ]}
].

```

Or you can add it via logger API from your code:

```erlang
logger:add_handler(my_handler, cloudwatch_logger, #{...}).
```

**Note:** It's not currently possible to set `cloudwatch_logger` as `default` handler via `sys.config` (or add the handler there directly under the `kernel` app),
because `sys.config` is applied at `kernel` application start time and `cloudwatch_logger` application depends on `kernel` application (cyclic dependency). 

## Config properties

| Property                     | Mandatory | Description                                                                                                                                                                                          |
|------------------------------|:---------:|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| cloudwatch_group             |     Y     | CloudWatch group name.                                                                                                                                                                               |                                                                                                                                                                                  |
| cloudwatch_stream            |     Y     | CloudWatch stream name.                                                                                                                                                                              |
| upload_batch_max_size        |           | *Default: 50*. The events are sent in batches. A batch is sent when is reaching the `upload_batch_max_size` size or a number of `upload_batch_inteval_ms` ms elapsed.                                |
| upload_batch_inteval_ms      |           | *Default: 5000*. Number of milliseconds we can wait for events to accumulate. A batch is sent when is reaching the `upload_batch_max_size` size or a number of `upload_batch_inteval_ms` ms elapsed. |
| upload_failed_retry_count    |           | *Default: 3*. In case a batch sending operation failed, how many times we retry to resubmit.                                                                                                         |
| upload_failed_retry_delay_ms |           | *Default: 1000*. The delay between resubmitting failed batches.                                                                                                                                      |

- Beside this custom settings, all other standard `logger:handler_config()` properties are accepted (`level`, `filters`, `formatter`).
- Multiple instances of `cloudwatch_logger` handler can be started. 

[1]:https://www.erlang.org/doc/apps/kernel/logger_chapter.html
[2]:https://docs.aws.amazon.com/cloudwatch/

