-module(holdem).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([join/2, start_game/1, get_seats/1, take_turn/2, show_cards/2]).

%% gen_fsm.
-export([init/1]).
-export([waiting_for_players/3, game_in_progess/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {deck, seats, actor, community_cards=[], stage
}).
-include("records.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

join(Pid, Player) -> gen_fsm:sync_send_event(Pid, {join, Player}).
start_game(Pid) -> gen_fsm:sync_send_event(Pid, start).
get_seats(Pid) -> gen_fsm:sync_send_event(Pid, get_seats).
take_turn(Pid, Action) -> gen_fsm:sync_send_event(Pid, {take_turn, Action}).
show_cards(Pid, Player) -> gen_fsm:sync_send_event(Pid, {show_cards, Player}).

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
    seats:handle_action(Seats, SmallBlind, small_blind),
    seats:handle_action(Seats, BigBlind, big_blind),
    DealTimes = length(seats:show_active_seats(Seats))*2,
    deal_cards_(NewState, SmallBlind, DealTimes),
    ActorSeat = seats:get_preflop_actor(Seats),
    player:signal_turn(ActorSeat#seat.player),
    {reply, ok, game_in_progess, NewState#state{actor=ActorSeat, stage=preflop}}.

game_in_progess(get_seats, _From, StateData) ->
    {reply, StateData#state.seats, game_in_progess, StateData};
game_in_progess({take_turn, Action}, _From, #state{seats=Seats,actor=Actor,stage=Stage}=StateData) ->
    io:format("received action ~p~n", [Action]),
    seats:handle_action(Seats, Actor, Action),

    NewState = case {seats:is_hand_over(Seats), seats:is_betting_complete(Seats), Stage} of
        {true,_,_} -> hand_over_(StateData);
        {_,true,preflop} -> draw_community_cards_(StateData, 3, flop);
        {_,true,flop} -> draw_community_cards_(StateData, 1, turn);
        {_,true,turn} -> draw_community_cards_(StateData, 1, river);
        {_,true,river} -> show_down_(StateData);
        _ -> StateData
    end,

    NewStateName = case NewState#state.stage of
        show_down -> waiting_for_players;
        hand_over -> waiting_for_players;
        _ -> game_in_progess
    end,

    NextActor = seats:get_next_seat(Seats, Actor),
    player:signal_turn(NextActor#seat.player),
    {reply, ok, NewStateName, NewState#state{actor=NextActor}};
game_in_progess({show_cards, Player}, _From, StateData) ->
    Cards = seats:show_cards_from_player(StateData#state.seats, Player),
    {reply, Cards, game_in_progess, StateData}.

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

draw_community_cards_(#state{community_cards=CC,deck=Deck,seats=Seats}=State, N, NextStage) ->
    deck:draw_cards(Deck, 1), % burn card
    NewCC = CC ++ deck:draw_cards(Deck, N),
    io:format("community cards: ~p~n", [NewCC]),
    seats:clear_last_action(Seats),
    seats:pot_bets(Seats),
    State#state{community_cards=NewCC,stage=NextStage}.

show_down_(#state{community_cards=CC,deck=Deck,seats=Seats}=State) ->
    ActiveSeats = seats:show_active_seats(Seats),
    SeatHands = lists:map(
        fun(#seat{cards=Cards}=Seat) ->
                {Hand,FiveCards} = hand:get_highest_hand(Cards++CC),
                {Seat,Hand,FiveCards}
        end, ActiveSeats),

    SeatHandsRanked = lists:sort(
        fun({_,HandA,_},{_,HandB,_}) -> 
                hand:is_higher_hand(HandA,HandB)
        end, SeatHands),
    [{_,BestHand,_}|_] = SeatHandsRanked,

    WinningSeatHands = lists:takewhile(
        fun({_,Hand,_}) -> Hand == BestHand end, SeatHandsRanked),

    io:format("winning player hands: ~p~n", [WinningSeatHands]),
    seats:pot_bets(Seats),
    seats:distribute_winning(Seats, [S || {S,_,_} <- WinningSeatHands]),
    seats:prepare_new_game(Seats),
    deck:stop(Deck),
    State#state{community_cards=[],deck=undefined,stage=show_down}.

hand_over_(#state{seats=Seats,deck=Deck}=State) ->
    ActiveSeats = seats:show_active_seats(Seats),
    [Winner] = [X || X <- ActiveSeats, X#seat.last_action /= fold],
    seats:pot_bets(Seats),
    seats:distribute_winning([Winner]),
    seats:prepare_new_game(Seats),
    deck:stop(Deck),
    State#state{community_cards=[],deck=undefined,stage=hand_over}.

