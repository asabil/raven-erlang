-module(raven_app).

-behaviour(application).
-export([
	start/2,
	stop/1
]).


start(_StartType, _StartArgs) ->
	error_logger:add_report_handler(raven_error_logger),
	raven_sup:start_link().

stop(_State) ->
	ok.
