-module(players_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([create_player/1, terminate_player/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create_player(NickName) ->
    ChildId = erlang:unique_integer([positive,monotonic]),
    supervisor:start_child(?MODULE, {ChildId,
            {player, start_link, [NickName]},
            temporary, 5000, worker, [player]}).

terminate_player(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
