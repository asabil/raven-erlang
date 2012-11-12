-module(raven_sup).
-export([
	start_link/0
]).

-behaviour(supervisor).
-export([
	init/1
]).

-define(SUPERVISOR(I),      {I, {supervisor, start_link, [?MODULE, I]}, permanent, infinity, supervisor, [?MODULE]}).
-define(SUPERVISOR(I, N),   {I, {supervisor, start_link, [{local, N}, ?MODULE, I]}, permanent, infinity, supervisor, [?MODULE]}).
-define(WORKER(M, F, A, R), {M,  {M, F, A}, R, 5000, worker, [M]}).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @hidden
init([]) ->
	{ok, {
		{one_for_one, 5, 10}, [
		]
	}}.
