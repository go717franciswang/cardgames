-module(player).
-behaviour(gen_fsm).

%% API.
-export([start_link/0, create_table/1, join_table/2, start_game/1]).

%% gen_fsm.
-export([init/1]).
-export([lobby/3, lobby/2, in_game/2]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {game_pid, money
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

create_table(Pid) ->
    gen_fsm:sync_send_event(Pid, create_table).

join_table(Pid, TableId) ->
    gen_fsm:send_event(Pid, {join_table, TableId}).

start_game(Pid) ->
    gen_fsm:send_event(Pid, start_game).

%% gen_fsm.

init([]) ->
	{ok, lobby, #state{}}.

lobby(create_table, _From, StateData) ->
    {ok, Pid} = tables_sup:create_table(),
    holdem:join(Pid, self()),
	{reply, {ok, Pid}, in_game, StateData#state{game_pid=Pid, money=10}}.
lobby({join_table, TableId}, StateData) ->
    Pid = tables_sup:id_to_pid(TableId),
    holdem:join(Pid, self()),
	{next_state, in_game, StateData#state{game_pid=Pid, money=10}}.

in_game(start_game, StateData) ->
    Reply = holdem:start_game(StateData#state.game_pid),
    io:format("starting game: ~p~n", [Reply]),
    {next_state, in_game, StateData};
in_game({new_player, Pid}, StateData) ->
    io:format("new player: ~p", [Pid]),
    {next_state, in_game, StateData};
in_game(bet, StateData) ->
	{next_state, in_game, StateData};
in_game(check, StateData) ->
	{next_state, in_game, StateData};
in_game(raise, StateData) ->
	{next_state, in_game, StateData};
in_game(call, StateData) ->
	{next_state, in_game, StateData};
in_game(fold, StateData) ->
	{next_state, in_game, StateData}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
