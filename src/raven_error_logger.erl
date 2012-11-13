-module(raven_error_logger).

-behaviour(gen_event).
-export([
	init/1,
	code_change/3,
	terminate/2,
	handle_call/2,
	handle_event/2,
	handle_info/2
]).


init(_) ->
	{ok, []}.

handle_call(_, State) ->
	{ok, ok, State}.

handle_event({error, _, {Pid, Format, Data}}, State) ->
	capture_message(error, Pid, Format, Data),
	{ok, State};
handle_event({error_report, _, {Pid, Type, Report}}, State) ->
	capture_report(error, Pid, Type, lists:sort(Report)),
	{ok, State};

handle_event({warning_msg, _, {Pid, Format, Data}}, State) ->
	capture_message(warning, Pid, Format, Data),
	{ok, State};
handle_event({warning_report, _, {Pid, Type, Report}}, State) ->
	capture_report(warning, Pid, Type, lists:sort(Report)),
	{ok, State};

handle_event({info_msg, _, {Pid, Format, Data}}, State) ->
	capture_message(info, Pid, Format, Data),
	{ok, State};
handle_event({info_report, _, {Pid, Type, Report}}, State) ->
	capture_report(info, Pid, Type, lists:sort(Report)),
	{ok, State};

handle_event(_, State) ->
	{ok, State}.

handle_info(_, State) ->
	{ok, State}.

code_change(_, State, _) ->
	{ok, State}.

terminate(_, _) ->
	ok.


capture_message(error = Level, Pid, "** Generic server " ++ _, [Name, LastMessage, State, Reason]) ->
	%% gen_server terminate
	{Error, Trace} = parse_reason(Reason),
	raven:capture(format("gen_server ~w terminated with reason: ~s", [Name, format_reason(Reason)]), [
		{level, Level},
		{exception, {error, Error}},
		{stacktrace, Trace},
		{extra, [
			{name, Name},
			{pid, Pid},
			{last_message, LastMessage},
			{state, State}
		]}
	]);
capture_message(error = Level, Pid, "** State machine " ++ _, [Name, LastMessage, StateName, State, Reason]) ->
	%% gen_fsm terminate
	{Error, Trace} = parse_reason(Reason),
	raven:capture(format("gen_fsm ~w in state ~w terminated with reason: ~s", [Name, StateName, format_reason(Reason)]), [
		{level, Level},
		{exception, {error, Error}},
		{stacktrace, Trace},
		{extra, [
			{name, Name},
			{pid, Pid},
			{last_message, LastMessage},
			{state, State}
		]}
	]);
capture_message(error = Level, Pid, "** gen_event handler " ++ _, [ID, Name, LastMessage, State, Reason]) ->
	%% gen_event handler terminate
	{Error, Trace} = parse_reason(Reason),
	raven:capture(format("gen_event ~w installed in ~w terminated with reason: ~s", [ID, Name, format_reason(Reason)]), [
		{level, Level},
		{exception, {error, Error}},
		{stacktrace, Trace},
		{extra, [
			{name, Name},
			{pid, Pid},
			{last_message, LastMessage},
			{state, State}
		]}
	]);
capture_message(error = Level, Pid, "Error in process " ++ _, [Name, Node, Reason]) ->
	%% process terminate
	raven:capture(format("Process ~w on node ~w terminated with reason: ~s", [Name, Node, format_reason(Reason)]), [
		{level, Level},
		{extra, [
			{name, Name},
			{pid, Pid},
			{reason, Reason}
		]}
	]);
capture_message(error = Level, Pid, "** Generic process " ++ _, [Name, LastMessage, State, Reason]) ->
	%% gen_process terminate
	{Error, Trace} = parse_reason(Reason),
	raven:capture(format("gen_process ~w terminated with reason: ~s", [Name, format_reason(Reason)]), [
		{level, Level},
		{exception, {error, Error}},
		{stacktrace, Trace},
		{extra, [
			{name, Name},
			{pid, Pid},
			{last_message, LastMessage},
			{state, State}
		]}
	]);
capture_message(Level, Pid, Format, Data) ->
	raven:capture(format(Format, Data), [
		{level, Level},
		{extra, [
			{pid, Pid}
		]}
	]).

capture_report(Level, Pid, crash_report, [Report, Neighbors]) ->
	Name = case proplists:get_value(registered_name, Report, []) of
		[] -> proplists:get_value(pid, Report);
		Atom -> Atom
	end,
	{Type, Reason, Trace} = proplists:get_value(error_info, Report),
	raven:capture(format("Process ~w with ~w neighbors crashed with reason: ~s", [Name, length(Neighbors), format_reason(Reason)]), [
		{level, Level},
		{logger, supervisors},
		{exception, {Type, Reason}},
		{stacktrace, Trace},
		{extra, [
			{pid, Pid}
		]}
	]);
capture_report(info, Pid, progress, [{application, App}, {started_at, Node} | _]) ->
	raven:capture(format("Application ~w started on node ~w", [App, Node]), [
		{level, info},
		{logger, supervisors},
		{extra, [
			{pid, Pid}
		]}
	]);
capture_report(info, Pid, progress, [{started, Started}, {supervisor, Supervisor} | _]) ->
	Message = case proplists:get_value(name, Started, []) of
		[] -> format("Supervisor ~w started child", [Supervisor]);
		Name -> format("Supervisor ~w started ~w", [Supervisor, Name])
	end,
	raven:capture(Message, [
		{level, info},
		{logger, supervisors},
		{extra, [
			{supervisor, Supervisor},
			{pid, Pid},
			{child_pid, proplists:get_value(pid, Started)},
			{mfa, format_mfa(proplists:get_value(mfargs, Started))},
			{restart_type, proplists:get_value(restart_type, Started)},
			{child_type, proplists:get_value(child_type, Started)},
			{shutdown, proplists:get_value(shutdown, Started)}
		]}
	]);
capture_report(Level, Pid, Type, Report) ->
	Message = unicode:characters_to_binary(proplists:get_value(message, Report, "Report from process")),
	raven:capture(Message, [
		{level, Level},
		{extra, [
			{type, Type},
			{pid, Pid} |
			Report
		]}
	]).


%% @private
parse_reason({Reason, [MFA|_] = Trace}) when is_tuple(MFA) ->
	{Atom, DeepTrace} = parse_reason(Reason),
	{Atom, DeepTrace ++ Trace};
parse_reason({Reason, []}) ->
	parse_reason(Reason);
parse_reason({Reason, {_, _, _} = MFA}) ->
	{Atom, DeepTrace} = parse_reason(Reason),
	{Atom, DeepTrace ++ [MFA]};
parse_reason({Reason, {_, _, _, Location} = MFA}) when is_list(Location) ->
	{Atom, DeepTrace} = parse_reason(Reason),
	{Atom, DeepTrace ++ [MFA]};
parse_reason(Reason) when is_atom(Reason) ->
	{Reason, []};
parse_reason(_Reason) ->
	{unknown, []}.

format_reason({'function not exported', [{M, F, A},MFA|_]}) ->
	["call to undefined function ", format_mfa({M, F, length(A)}), " from ", format_mfa(MFA)];
format_reason({'function not exported', [{M, F, A, _Props},MFA|_]}) ->
	["call to undefined function ", format_mfa({M, F, length(A)}), " from ", format_mfa(MFA)];
format_reason({undef, [MFA|_]}) ->
	["call to undefined function ", format_mfa(MFA)];
format_reason({bad_return, {_MFA, {'EXIT', Reason}}}) ->
	format_reason(Reason);
format_reason({bad_return, {MFA, Val}}) ->
    ["bad return value ", format("~w", Val), " from ", format_mfa(MFA)];
format_reason({bad_return_value, Val}) ->
	["bad return value: ", format("~w", Val)];
format_reason({{bad_return_value, Val}, MFA}) ->
	["bad return value: ", format("~w", Val), " in ", format_mfa(MFA)];
format_reason({{badrecord, Record}, [MFA|_]}) ->
	["bad record ", format("~w", Record), " in ", format_mfa(MFA)];
format_reason({{case_clause, Val}, [MFA|_]}) ->
	["no case clause matching ", format("~w", Val), " in ", format_mfa(MFA)];
format_reason({function_clause, [MFA|_]}) ->
	["no function clause matching ", format_mfa(MFA)];
format_reason({if_clause, [MFA|_]}) ->
	["no true branch found while evaluating if expression in ", format_mfa(MFA)];
format_reason({{try_clause, Val}, [MFA|_]}) ->
	["no try clause matching ", format("~w", Val), " in ", format_mfa(MFA)]; 
format_reason({badarith, [MFA|_]}) ->
	["bad arithmetic expression in ", format_mfa(MFA)];
format_reason({{badmatch, Val}, [MFA|_]}) ->
	["no match of right hand value ", format("~w", Val), " in ", format_mfa(MFA)];
format_reason({emfile, _Trace}) ->
	"maximum number of file descriptors exhausted, check ulimit -n";
format_reason({system_limit, [{M, F, _}|_] = Trace}) ->
	Limit = case {M, F} of
		{erlang, open_port} ->
			"maximum number of ports exceeded";
		{erlang, spawn} ->
			"maximum number of processes exceeded";
		{erlang, spawn_opt} ->
			"maximum number of processes exceeded";
		{erlang, list_to_atom} ->
			"tried to create an atom larger than 255, or maximum atom count exceeded";
		{ets, new} ->
			"maximum number of ETS tables exceeded";
		_ ->
			{Str, _} = lager_trunc_io:print(Trace, 500),
			Str
	end,
	["system limit: ", Limit];
format_reason({badarg, [MFA,MFA2|_]}) ->
	case MFA of
		{_M, _F, A, _Props} when is_list(A) ->
			["bad argument in call to ", format_mfa(MFA), " in ", format_mfa(MFA2)];
		{_M, _F, A} when is_list(A) ->
			["bad argument in call to ", format_mfa(MFA), " in ", format_mfa(MFA2)];
		_ ->
			["bad argument in ", format_mfa(MFA)]
	end;
format_reason({{badarity, {Fun, Args}}, [MFA|_]}) ->
	{arity, Arity} = lists:keyfind(arity, 1, erlang:fun_info(Fun)),
	[io_lib:format("fun called with wrong arity of ~w instead of ~w in ", [length(Args), Arity]), format_mfa(MFA)];
format_reason({noproc, MFA}) ->
	["no such process or port in call to ", format_mfa(MFA)];
format_reason({{badfun, Term}, [MFA|_]}) ->
	["bad function ", format("~w", Term), " in ", format_mfa(MFA)];
format_reason({Reason, [{M, F, A}|_]}) when is_atom(M), is_atom(F), is_integer(A) ->
	[format_reason(Reason), " in ", format_mfa({M, F, A})];
format_reason({Reason, [{M, F, A, Props}|_]}) when is_atom(M), is_atom(F), is_integer(A), is_list(Props) ->
	[format_reason(Reason), " in ", format_mfa({M, F, A, Props})];
format_reason(Reason) ->
	format("~120p", [Reason]).

%% @private
format_mfa({M, F, A} = MFA) ->
	if
		is_list(A) ->
			{FmtStr, Args} = format_args(A, [], []),
			io_lib:format("~w:~w(" ++ FmtStr ++ ")", [M, F | Args]);
		is_integer(A) ->
			io_lib:format("~w:~w/~w", [M, F, A]);
		true ->
			io_lib:format("~w", [MFA])
	end;
format_mfa({M, F, A, _}) ->
	format_mfa({M, F, A});
format_mfa(Other) ->
	io_lib:format("~w", [Other]).

%% @private
format_args([], FmtAcc, ArgsAcc) ->
	{string:join(lists:reverse(FmtAcc), ", "), lists:reverse(ArgsAcc)};
format_args([H|T], FmtAcc, ArgsAcc) ->
	format_args(T, ["~s" | FmtAcc], [format("~w", H) | ArgsAcc]).

%% @private
format(Format, Data) ->
	iolist_to_binary(io_lib:format(Format, Data)).
