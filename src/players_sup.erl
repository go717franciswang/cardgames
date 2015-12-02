-module(players_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([create_player/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create_player() ->
    ChildId = erlang:unique_integer(),
    supervisor:start_child(?MODULE, {ChildId,
            {player, start_link, []},
            transient, 5000, worker, [player]}).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
