-module(holdem).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([join/2, sit/2, start_game/1, get_seats/1, take_turn/2, show_cards/2,
         leave/2, set_timeout/2, show_game_state/1]).

%% gen_fsm.
-export([init/1]).
-export([waiting_for_players/3, game_in_progess/3, game_in_progess/2]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {deck, users=[], seats, actor, actor_options=[], community_cards=[], stage,
        timeout=30000, timer
}).
-include("records.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

join(Pid, Player) -> gen_fsm:sync_send_all_state_event(Pid, {join, Player}).
leave(Pid, Player) -> gen_fsm:sync_send_all_state_event(Pid, {leave, Player}).
sit(Pid, Player) -> gen_fsm:sync_send_event(Pid, {sit, Player}).
start_game(Pid) -> gen_fsm:sync_send_event(Pid, start_game).
get_seats(Pid) -> gen_fsm:sync_send_all_state_event(Pid, get_seats).
take_turn(Pid, Action) -> gen_fsm:sync_send_event(Pid, {take_turn, Action}).
show_cards(Pid, Player) -> gen_fsm:sync_send_event(Pid, {show_cards, Player}).
set_timeout(Pid, Timeout) -> gen_fsm:sync_send_all_state_event(Pid, {set_timeout, Timeout}).
show_game_state(Pid) -> gen_fsm:sync_send_all_state_event(Pid, show_game_state).

%% gen_fsm.

init([]) ->
    {ok, Seats} = seats:start_link(6),
	{ok, waiting_for_players, #state{seats=Seats}}.

waiting_for_players({sit, Player}, _From, StateData) ->
    io:format("broadcast new player: ~p~n", [Player]),
    lists:foreach(
        fun(#seat{player=P}) -> 
                player:notify(P, {new_player, Player})
        end, seats:show_active_seats(StateData#state.seats)),
    seats:join(StateData#state.seats, Player),
    {reply, ok, waiting_for_players, StateData};
waiting_for_players(start_game, _From, #state{seats=Seats,timeout=Timeout}=StateData) ->
    {ok, Deck} = deck:start_link(),
    NewState = StateData#state{deck=Deck},
    seats:rotate_dealer_button(Seats),
    {SmallBlind, BigBlind} = seats:get_blinds(Seats),
    seats:handle_action(Seats, SmallBlind, small_blind),
    seats:handle_action(Seats, BigBlind, big_blind),
    DealTimes = length(seats:show_active_seats(Seats))*2,
    deal_cards_(NewState, SmallBlind, DealTimes),
    ActorSeat = seats:get_preflop_actor(Seats),
    Options = seats:get_available_options(Seats, ActorSeat),
    player:notify(ActorSeat#seat.player, {signal_turn, Options}),
    Timer = gen_fsm:start_timer(Timeout, get_timeout_action_(Options)),
    broadcast_(StateData, game_started),
    {reply, ok, game_in_progess, 
        NewState#state{actor=ActorSeat, stage=preflop, actor_options=Options, timer=Timer}};
waiting_for_players(_, _, StateData) ->
    {reply, ignored, waiting_for_players, StateData}.

game_in_progess({take_turn,_}, {Player,_Tag}, #state{actor=Actor}=State) when Player /= Actor#seat.player ->
    {reply, {error, not_your_turn}, game_in_progess, State};
game_in_progess({take_turn,Action}, _From, #state{actor_options=Options}=State) ->
    {Reply, NewStateName, NewState} = case lists:member(Action, Options) of
        false -> {{error, invalid_action}, game_in_progess, State};
        true -> handle_action_(State, Action)
    end,
    {reply, Reply, NewStateName, NewState};
game_in_progess({show_cards, Player}, _From, StateData) ->
    Cards = seats:show_cards_from_player(StateData#state.seats, Player),
    {reply, Cards, game_in_progess, StateData}.

game_in_progess({timeout, _Ref, Action}, State) ->
    io:format("last player did not take action in time~n"),
    Player = State#state.actor#seat.player,
    broadcast_(State, {timeout, Player}),
    {_Reply, NewStateName, NewState} = handle_action_(State, Action),
    {next_state, NewStateName, NewState}.

handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event({join, Player}, _From, StateName, StateData) ->
    Users = [Player|StateData#state.users],
    broadcast_(StateData, {join, Player}),
    {reply, ok, StateName, StateData#state{users=Users}};
handle_sync_event({leave, Player}, _From, StateName, #state{seats=Seats,users=Users}=StateData) ->
    case seats:leave(Seats, Player) of
        ok -> io:format("Player ~p dropped out~n", [Player]);
        {error, no_such_player} -> ok
    end,
    NewUsers = lists:delete(Player, Users),
    broadcast_(StateData, {leave, Player}),
    {reply, ok, StateName, StateData#state{users=NewUsers}};
handle_sync_event(get_seats, _From, StateName, StateData) ->
    {reply, StateData#state.seats, StateName, StateData};
handle_sync_event(show_game_state, {Player, _Tag}, StateName, #state{seats=Seats,community_cards=CC}=StateData) ->
    Dealer = seats:get_dealer(Seats),
    Reply = #game_state{
        seats=seats:show_seats(Seats, Player),
        pots=seats:get_pots(Seats),
        community_cards=CC,
        dealer_button_pos=Dealer#seat.position},
    {reply, Reply, StateName, StateData};
handle_sync_event({set_timeout, Timeout}, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData#state{timeout=Timeout}};
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

handle_action_(#state{seats=Seats,actor=Actor,stage=Stage,timer=Timer,timeout=Timeout}=StateData, Action) ->
    io:format("received action ~p from ~p~n", [Action, Actor#seat.player]),
    case Timer of
        undefined -> ok;
        _ -> gen_fsm:cancel_timer(Timer)
    end,
    seats:handle_action(Seats, Actor, Action),
    IsHandOver = seats:is_hand_over(Seats),
    IsBettingComplete = seats:is_betting_complete(Seats),

    NewState = case {IsHandOver, IsBettingComplete, Stage} of
        {true,_,_} -> hand_over_(StateData);
        {_,true,preflop} -> draw_community_cards_(StateData, 3, flop);
        {_,true,flop} -> draw_community_cards_(StateData, 1, turn);
        {_,true,turn} -> draw_community_cards_(StateData, 1, river);
        {_,true,river} -> show_down_(StateData);
        _ -> next_player_(StateData)
    end,

    NewStateName = case NewState#state.stage of
        show_down -> waiting_for_players;
        hand_over -> waiting_for_players;
        _ -> game_in_progess
    end,

    NewTimer = case NewState#state.actor of
        undefined -> undefined;
        NextActor -> 
            player:notify(NextActor#seat.player, {signal_turn, NewState#state.actor_options}),
            gen_fsm:start_timer(Timeout, get_timeout_action_(NewState#state.actor_options))
    end,
    broadcast_(StateData, {take_turn, Actor#seat.player, Action}),
    {ok, NewStateName, NewState#state{timer=NewTimer}}.

draw_community_cards_(#state{community_cards=CC,deck=Deck,seats=Seats}=State, N, NextStage) ->
    deck:draw_cards(Deck, 1), % burn card
    NewCC = CC ++ deck:draw_cards(Deck, N),
    broadcast_(State, {community_cards, NewCC}),
    io:format("community cards: ~p~n", [NewCC]),
    seats:clear_last_action(Seats),
    seats:pot_bets(Seats),
    NextActor = seats:get_flop_actor(Seats),
    Options = seats:get_available_options(Seats, NextActor),
    State#state{community_cards=NewCC,stage=NextStage,actor=NextActor,actor_options=Options}.

show_down_(#state{community_cards=CC,seats=Seats}=State) ->
    Competitors = seats:get_nonfolded_seats(Seats),
    lists:foreach(
        fun(#seat{player=P,cards=CS}) ->
                broadcast_(State, {reveal_cards, P, CS})
        end, Competitors),
    seats:pot_bets(Seats),
    PotWins = seats:show_down(Seats, CC),
    lists:foreach(
        fun(#pot_wins{pot=Pot,wins=Plays}) ->
                io:format("pot: ~p~n", [Pot]),
                io:format("winning plays: ~p~n~n", [Plays]),
                broadcast_(State, {pot, Pot}),
                broadcast_(State, {winning_plays, Plays})
        end, PotWins),
    game_end_routine_(State),
    State#state{community_cards=[],deck=undefined,stage=show_down,actor=undefined,actor_options=[]}.

hand_over_(#state{seats=Seats}=State) ->
    seats:pot_bets(Seats),
    PotWins = seats:hand_over(Seats),
    lists:foreach(
        fun(#pot_wins{pot=Pot,wins=Plays}) ->
                io:format("pot: ~p~n", [Pot]),
                io:format("winning plays: ~p~n~n", [Plays]),
                broadcast_(State, {pot, Pot}),
                broadcast_(State, {winning_plays, Plays})
        end, PotWins),
    game_end_routine_(State),
    State#state{community_cards=[],deck=undefined,stage=hand_over,actor=undefined,actor_options=[]}.

game_end_routine_(#state{seats=Seats,deck=Deck}) ->
    seats:drop_broke_players(Seats),
    seats:prepare_new_game(Seats),
    deck:stop(Deck).

next_player_(#state{seats=Seats,actor=Actor}=State) ->
    NewActor = seats:get_next_seat(Seats,Actor),
    Options = seats:get_available_options(Seats,NewActor),
    State#state{actor=NewActor,actor_options=Options}.

get_timeout_action_(Options) ->
    case lists:member(check, Options) of
            true -> check;
            false -> fold
    end.

broadcast_(#state{users=Users}, Event) ->
    lists:foreach(fun(User) -> player:notify(User, Event) end, Users).
