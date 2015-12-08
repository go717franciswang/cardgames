-module(holdem).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([join/2, start_game/1, get_seats/1, take_turn/2]).

%% gen_fsm.
-export([init/1]).
-export([waiting_for_players/3, game_in_progess/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {deck, seats, actor
}).
-include("records.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

join(Pid, Player) ->
    io:format("New player has joined~n"),
    gen_fsm:sync_send_event(Pid, {join, Player}).

start_game(Pid) ->
    gen_fsm:sync_send_event(Pid, start).

get_seats(Pid) ->
    gen_fsm:sync_send_event(Pid, get_seats).

take_turn(Pid, Action) ->
    gen_fsm:sync_send_event(Pid, {take_turn, Action}).

%% gen_fsm.

init([]) ->
    {ok, Seats} = seats:start_link(6),
	{ok, waiting_for_players, #state{seats=Seats}}.

waiting_for_players({join, Player}, _From, StateData) ->
    io:format("broadcast new player: ~p~n", [Player]),

    lists:foreach(
        fun(#seat{player=P}) -> 
                player:new_player(P, Player) 
        end, seats:show_active_seats(StateData#state.seats)),
    seats:join(StateData#state.seats, Player),
    {reply, ok, waiting_for_players, StateData};
waiting_for_players(start, _From, #state{seats=Seats}=StateData) ->
    {ok, Deck} = deck:start_link(),
    NewState = StateData#state{deck=Deck},
    seats:rotate_dealer_button(Seats),
    {SmallBlind, BigBlind} = seats:get_blinds(Seats),
    seats:place_bet(Seats, SmallBlind, 0.05),
    seats:place_bet(Seats, BigBlind, 0.10),
    DealTimes = length(seats:show_active_seats(Seats))*2,
    deal_cards_(NewState, SmallBlind, DealTimes),
    ActorSeat = seats:get_preflop_actor(Seats),
    player:signal_turn(ActorSeat#seat.player),
    {reply, ok, game_in_progess, NewState#state{actor=ActorSeat}}.

game_in_progess(get_seats, _From, StateData) ->
    {reply, StateData#state.seats, game_in_progess, StateData};
game_in_progess({take_turn, Action}, _From, #state{seats=Seats,actor=Actor}=StateData) ->
    io:format("received action ~p~n", [Action]),
    seats:handle_action(StateData#state.seats, Actor, Action),
    NextActor = seats:get_next_seat(Seats, Actor),
    player:signal_turn(NextActor#seat.player),
    {reply, ok, game_in_progess, StateData#state{actor=NextActor}}.

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

deal_cards_(_,_,0) -> ok;
deal_cards_(#state{seats=Seats,deck=Deck}=State,DealTo,TimesLeft) ->
    [Card] = deck:draw_cards(Deck, 1),
    seats:deal_card(Seats, DealTo, Card),
    NextSeat = seats:get_next_seat(Seats, DealTo),
    deal_cards_(State, NextSeat, TimesLeft-1).
