-module(raven).
-export([
	capture/2,
	user_agent/0
]).

-define(SENTRY_VERSION, "2.0").

-record(cfg, {
	uri :: string(),
	public_key :: string(),
	private_key :: string(),
	project :: string(),
	ipfamily :: atom()
}).

-type cfg_rec() :: #cfg{}.

-spec capture(string() | binary(), [parameter()]) -> ok.
-type parameter() ::
	{stacktrace, [stackframe()]} |
	{exception, {exit | error | throw, term()}} |
	{atom(), binary() | integer()}.
-type stackframe() ::
	{module(), atom(), non_neg_integer() | [term()]} |
	{module(), atom(), non_neg_integer() | [term()], [{atom(), term()}]}.
capture(Message, Params) when is_list(Message) ->
	capture(unicode:characters_to_binary(Message), Params);
capture(Message, Params) ->
	Cfg = get_config(),
	Document = {[
		{event_id, event_id_i()},
		{project, unicode:characters_to_binary(Cfg#cfg.project)},
		{platform, erlang},
		{server_name, node()},
		{timestamp, timestamp_i()},
		{message, term_to_json_i(Message)} |
		lists:map(fun
			({stacktrace, Value}) ->
				{'sentry.interfaces.Stacktrace', {[
					{frames,lists:reverse([frame_to_json_i(Frame) || Frame <- Value])}
				]}};
			({exception, {Type, Value}}) ->
				{'sentry.interfaces.Exception', {[
					{type, Type},
					{value, term_to_json_i(Value)}
				]}};
			({tags, Tags}) ->
				{tags, {[{Key, term_to_json_i(Value)} || {Key, Value} <- Tags]}};
			({extra, Tags}) ->
				{extra, {[{Key, term_to_json_i(Value)} || {Key, Value} <- Tags]}};
			({Key, Value}) ->
				{Key, term_to_json_i(Value)}
		end, Params)
	]},
	Timestamp = integer_to_list(unix_timestamp_i()),
	Body = base64:encode(zlib:compress(jiffy:encode(Document, [force_utf8]))),
	UA = user_agent(),
	Headers = [
		{"X-Sentry-Auth",
		["Sentry sentry_version=", ?SENTRY_VERSION,
		 ",sentry_client=", UA,
		 ",sentry_timestamp=", Timestamp,
		 ",sentry_key=", Cfg#cfg.public_key]},
		{"User-Agent", UA}
	],
	ok = httpc:set_options([{ipfamily, Cfg#cfg.ipfamily}]),
	httpc:request(post,
		{Cfg#cfg.uri ++ "/api/store/", Headers, "application/octet-stream", Body},
		[],
		[{body_format, binary}, {sync, false}]
	),
	ok.

-spec user_agent() -> iolist().
user_agent() ->
	{ok, Vsn} = application:get_key(raven, vsn),
	["raven-erlang/", Vsn].

%% @private
-spec get_config() -> cfg_rec().
get_config() ->
	get_config(raven).

-spec get_config(App :: atom()) -> cfg_rec().
get_config(App) ->
	{ok, IpFamily} = application:get_env(App, ipfamily),
	case application:get_env(App, dsn) of
		{ok, Dsn} ->
			{match, [_, Protocol, PublicKey, SecretKey, Uri, Project]} =
				re:run(Dsn, "^(https?://)(.+):(.+)@(.+)/(.+)$", [{capture, all, list}]),
			#cfg{uri = Protocol ++ Uri,
			     public_key = PublicKey,
			     private_key = SecretKey,
			     project = Project,
			     ipfamily = IpFamily};
		undefined ->
			{ok, Uri} = application:get_env(App, uri),
			{ok, PublicKey} = application:get_env(App, public_key),
			{ok, PrivateKey} = application:get_env(App, private_key),
			{ok, Project} = application:get_env(App, project),
			#cfg{uri = Uri,
			     public_key = PublicKey,
			     private_key = PrivateKey,
			     project = Project,
			     ipfamily = IpFamily}
	end.


event_id_i() ->
	U0 = crypto:rand_uniform(0, (2 bsl 32) - 1),
	U1 = crypto:rand_uniform(0, (2 bsl 16) - 1),
	U2 = crypto:rand_uniform(0, (2 bsl 12) - 1),
	U3 = crypto:rand_uniform(0, (2 bsl 32) - 1),
	U4 = crypto:rand_uniform(0, (2 bsl 30) - 1),
	<<UUID:128>> = <<U0:32, U1:16, 4:4, U2:12, 2#10:2, U3:32, U4:30>>,
	iolist_to_binary(io_lib:format("~32.16.0b", [UUID])).

timestamp_i() ->
	{{Y,Mo,D}, {H,Mn,S}} = calendar:now_to_datetime(os:timestamp()),
	FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B",
	iolist_to_binary(io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S])).

unix_timestamp_i() ->
	{Mega, Sec, Micro} = os:timestamp(),
	Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

frame_to_json_i({Module, Function, Arguments}) ->
	frame_to_json_i({Module, Function, Arguments, []});
frame_to_json_i({Module, Function, Arguments, Location}) ->
	Arity = case is_list(Arguments) of
		true -> length(Arguments);
		false -> Arguments
	end,
	Line = case lists:keyfind(line, 1, Location) of
		false -> -1;
		{line, L} -> L
	end,
	{
		case is_list(Arguments) of
			true -> [{vars, [iolist_to_binary(io_lib:format("~w", [Argument])) || Argument <- Arguments]}];
			false -> []
		end ++ [
			{module, Module},
			{function, <<(atom_to_binary(Function, utf8))/binary, "/", (list_to_binary(integer_to_list(Arity)))/binary>>},
			{lineno, Line},
			{filename, case lists:keyfind(file, 1, Location) of
				false -> <<(atom_to_binary(Module, utf8))/binary, ".erl">>;
				{file, File} -> list_to_binary(File)
			end}
		]
	}.

term_to_json_i(Term) when is_binary(Term); is_atom(Term) ->
	Term;
term_to_json_i(Term) ->
	iolist_to_binary(io_lib:format("~120p", [Term])).
