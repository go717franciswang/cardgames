-module(holdem).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([join/2]).

%% gen_fsm.
-export([init/1]).
-export([waiting_for_players/2]).
-export([game_in_progess/2]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {players=[]
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

join(Pid, PlayerPid) ->
    io:format("New player has joined~n"),
    gen_fsm:send_event(Pid, {join, PlayerPid}).

%% gen_fsm.

init([]) ->
	{ok, waiting_for_players, #state{}}.

waiting_for_players({join, PlayerPid}, StateData) ->
    Players = [PlayerPid | StateData#state.players],
    lists:foreach(
        fun(Pid) -> 
                gen_fsm:send_event(Pid, {new_player, PlayerPid})
        end, Players),
    {next_state, waiting_for_players, #state{players=Players}};
waiting_for_players(start, StateData) ->
    {next_state, game_in_progess, StateData}.

game_in_progess({bet, _PlayerId}, StateData) ->
    {next_state, game_in_progess, StateData};
game_in_progess({check, _PlayerId}, StateData) ->
    {next_state, game_in_progess, StateData};
game_in_progess({raise, _PlayerId}, StateData) ->
    {next_state, game_in_progess, StateData};
game_in_progess({call, _PlayerId}, StateData) ->
    {next_state, game_in_progess, StateData};
game_in_progess({fold, _PlayerId}, StateData) ->
    {next_state, game_in_progess, StateData}.

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
