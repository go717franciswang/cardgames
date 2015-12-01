-module(cardgames_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [{tables_sup, {tables_sup, start_link, []}, permanent, 5000, supervisor, [tables_sup]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
