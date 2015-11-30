-module(cardgames_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([create_table/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

create_table() ->
    ChildId = make_ref(),
    supervisor:start_child(?MODULE, {ChildId, {holdem, start_link, []}, transient, 5000, worker, [holdem]}).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
