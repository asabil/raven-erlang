-module(raven_app).
-export([
	start/0,
	stop/0
]).

-behaviour(application).
-export([
	start/2,
	stop/1
]).

-spec start() -> ok | {error, term()}.
start() ->
	ensure_started(raven).

-spec stop() -> ok | {error, term()}.
stop() ->
	application:stop(raven).


%% @hidden
start(_StartType, _StartArgs) ->
	case application:get_env(dsn) of
		{ok, _} ->
			case application:get_env(error_logger) of
				{ok, true} -> error_logger:add_report_handler(raven_error_logger);
				_ -> ok
			end,
			raven_sup:start_link();
		_ ->
			{error, missing_configuration}
	end.

%% @hidden
stop(_State) ->
	case application:get_env(error_logger) of
		{ok, true} ->
			error_logger:delete_report_handler(raven_error_logger),
			ok;
		_ ->
			ok
	end.

%% @private
ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok;
		{error, {not_started, Other}} ->
			ensure_started(Other),
			ensure_started(App)
	end.
