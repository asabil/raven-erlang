-module(raven_app).

-behaviour(application).
-export([
	start/2,
	stop/1
]).


start(_StartType, _StartArgs) ->
	case application:get_env(transport) of
		{ok, _} ->
			case application:get_env(error_logger) of
				{ok, true} -> error_logger:add_report_handler(raven_error_logger);
				_ -> ok
			end,
			raven_sup:start_link();
		_ ->
			{error, missing_configuration}
	end.

stop(_State) ->
	ok.
