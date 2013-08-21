%% @doc raven backend for lager

-module(raven_lager_backend).
-behaviour(gen_event).


-export([
	init/1,
	code_change/3,
	terminate/2,
	handle_call/2,
	handle_event/2,
	handle_info/2
]).


-record(state, {level}).

init(Level) ->
    {ok, #state{level=lager_util:config_to_mask(Level)}}.


%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
   try lager_util:config_to_mask(Level) of
        Levels ->
            {ok, ok, State#state{level=Levels}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_, State) ->
	{ok, ok, State}.

%% @private
handle_event({log, Data},
    #state{level=L} = State) ->
    case lager_util:is_loggable(Data, L, ?MODULE) of
        true ->
            {Message, Params} = parse_message(Data),
            raven:capture(Message, Params),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.


handle_info(_, State) ->
	{ok, State}.

code_change(_, State, _) ->
	{ok, State}.

terminate(_, _) ->
	ok.


%% TODO - check what other metadata can be sent to sentry
parse_message({lager_msg, [], MetaData, Level, _, _Time, Message}) ->
    Extra = parse_meta(MetaData),
    {Message, [{level, Level},
               {extra, Extra}]}.


%% @doc at the moment this is just a passthrough -- what other metadata is out there?
parse_meta(MetaData) ->
    parse_meta(MetaData, []).

parse_meta([], Acc) ->
    Acc;
parse_meta([{pid, Pid} = PidProp | Rest], Acc) when is_pid(Pid) ->
    parse_meta(Rest, [PidProp | Acc]);
parse_meta([{_, _} | Rest], Acc) ->
    parse_meta(Rest, Acc).

