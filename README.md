raven-erlang
============

raven-erlang is an Erlang client for [Sentry](http://aboutsentry.com/) that integrates with the
standard ```error_logger``` module.

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
generally done in app.config or sys.config:

```erlang
{raven, [
    {transport, {http, "https://app.getsentry.com"}},
    {project, "1"},
    {public_key, "PUBLIC_KEY"},
    {private_key, "PRIVATE_KEY"},
    {error_logger, true}  % Set to true in order to install the standard error logger
]}.
 ```

For UDP transport change transport line:

```eralng
    {transport, {udp, "app.getsentry.com", 9999}},
```

Now all events logged using error_logger will be sent to the [Sentry](http://aboutsentry.com/) service.

Advanced Usage
==============

You can log directly events to sentry using the ```raven:capture/2``` function, for example:

```erlang
raven:capture("Test Event", [
    {logger, mylogger},
    {exception, {error, badarg}},
    {stacktrace, erlang:get_stacktrace()},
    {extra, [
        {pid, self()},
        {process_dictionnary, erlang:get()}
    ]}
]).
```
