raven-erlang
============

raven-erlang is an Erlang client for [Sentry](http://aboutsentry.com/) that integrates with the
standard ```error_logger``` module.

Quick Start
===========

```shell
git clone https://github.com/soundrop/raven-erlang.git
cd raven-erlang
make
erl -pa ebin/ -pa deps/jiffy/ebin/ -s raven_app -raven project '"PROJECT_ID"' -raven public_key '"PUBLIC_KEY"' -raven private_key '"PRIVATE_KEY"'
```

Which should start and erlang shell prompt, where you should be able to type something like this:

```shell
Erlang R15B03 (erts-5.9.3.1) [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V5.9.3.1  (abort with ^G)
1> raven:capture("Hello Raven", []).
ok
2> q().
ok
```

Basic Usage
===========

Add raven as a dependency to your project, and include the raven application in
your release (in the reltool.config rel section):

```erlang
{rel, "my_app", "1", [
    kernel,
    stdlib,
    sasl,
    crypto,
    ssl,
    inets,
    raven,

    my_app
]}.
```

The raven application itself needs to be configured using the application's environment, this is
generally done in app.config or sys.config.

It will accept either the individual config components:

```erlang
{raven, [
    {uri, "https://app.getsentry.com"},
    {project, "1"},
    {public_key, "PUBLIC_KEY"},
    {private_key, "PRIVATE_KEY"},
    {error_logger, true},  % Set to true in order to install the standard error logger
    {ipfamily, inet}  % Set to inet6 to use IPv6. See `ipfamily` in `httpc:set_options/1` for more information.
]}.
```

or just the DSN:

```erlang
{raven, [
    {dsn, "https://PUBLIC_KEY:PRIVATE_KEY@app.getsentry.com/1"},
    {error_logger, true}  % Set to true in order to install the standard error logger
]}.
```


Now all events logged using error_logger will be sent to the [Sentry](http://aboutsentry.com/) service.

Advanced Usage
==============

You can log directly events to sentry using the ```raven:capture/2``` function, for example:

```erlang
raven:capture("Test Event", [
    {exception, {error, badarg}},
    {stacktrace, erlang:get_stacktrace()},
    {extra, [
        {pid, self()},
        {process_dictionnary, erlang:get()}
    ]}
]).
```
