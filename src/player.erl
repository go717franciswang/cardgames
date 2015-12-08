-module(player).
-behaviour(gen_fsm).

%% API.
-export([start_link/0, create_table/1, join_table/2, start_game/1, deal_card/2,
        show_cards/1, new_player/2, signal_turn/2, take_turn/2]).

%% gen_fsm.
-export([init/1]).
-export([lobby/3, in_game/2, in_game/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {game, cards=[]
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

create_table(Pid) ->
    gen_fsm:sync_send_event(Pid, create_table).

join_table(Pid, TableId) ->
    gen_fsm:sync_send_event(Pid, {join_table, TableId}).

start_game(Pid) ->
    gen_fsm:sync_send_event(Pid, start_game).

deal_card(Pid, Card) ->
    gen_fsm:send_event(Pid, {deal_card, Card}).

show_cards(Pid) ->
    gen_fsm:sync_send_event(Pid, show_cards).

new_player(Pid, Player) ->
    gen_fsm:send_event(Pid, {new_player, Player}).

signal_turn(Pid, Options) ->
    gen_fsm:send_event(Pid, {signal_turn, Options}).

take_turn(Pid, Action) ->
    gen_fsm:sync_send_event(Pid, {take_turn, Action}).

%% gen_fsm.

init([]) ->
	{ok, lobby, #state{}}.

lobby(create_table, _From, StateData) ->
    {ok, Pid} = tables_sup:create_table(),
    holdem:join(Pid, self()),
	{reply, {ok, Pid}, in_game, StateData#state{game=Pid}};
lobby({join_table, TableId}, _From, StateData) ->
    Pid = tables_sup:id_to_pid(TableId),
    Reply = holdem:join(Pid, self()),
	{reply, Reply, in_game, StateData#state{game=Pid}}.

in_game(show_cards, _From, StateData) ->
    {reply, StateData#state.cards, in_game, StateData};
in_game(start_game, _From, StateData) ->
    Reply = holdem:start_game(StateData#state.game),
    {reply, Reply, in_game, StateData};
in_game({take_turn, Action}, _From, StateData) ->
    Reply = holdem:take_turn(StateData#state.game, Action),
    {reply, Reply, in_game, StateData}.
in_game({new_player, Player}, StateData) ->
    io:format("~p new player: ~p~n", [self(), Player]),
    {next_state, in_game, StateData};
in_game({deal_card, Card}, StateData) ->
    NewCards = [Card|StateData#state.cards],
    io:format("~p got card: ~p~n", [self(), Card]),
    {next_state, in_game, StateData#state{cards=NewCards}};
in_game({signal_turn, Options}, StateData) ->
    io:format("~p's turn with options (~p)~n", [self(), Options]),
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
