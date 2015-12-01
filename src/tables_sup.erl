-module(tables_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([create_table/0, list_tables/0, id_to_pid/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create_table() ->
    ChildId = erlang:unique_integer(),
    supervisor:start_child(?MODULE, {ChildId, 
            {holdem, start_link, []}, 
            transient, 5000, worker, [holdem]}).

list_tables() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({Id, _Child, _Type, _Modules}) -> Id end, Children).

id_to_pid(Id) ->
    Children = supervisor:which_children(?MODULE),
    case lists:keyfind(Id, 1, Children) of
        {Id, Child, _Type, _Modules} -> Child;
        false -> {error, game_does_not_exist}
    end.

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
