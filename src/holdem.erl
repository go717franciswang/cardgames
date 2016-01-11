-module(holdem).
-behaviour(gen_fsm).

%% API.
-export([start_link/1]).
-export([join/2, sit/2, start_game/1, get_seats/1, take_turn/2, show_cards/2,
         leave/2, set_timeout/2, show_game_state/1, register_nickname/3]).

%% gen_fsm.
-export([init/1]).
-export([waiting_for_players/3, waiting_for_players/2, game_in_progress/3, game_in_progress/2]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {deck, users=[], seats, actor, actor_options=[], community_cards=[], stage,
        timeout=30000, timer, restart_timer=5000, id, max_raises_per_street=4
}).
-include("records.hrl").

%% API.

-spec start_link(any()) -> {ok, pid()}.
start_link(TableId) ->
	gen_fsm:start_link(?MODULE, [TableId], []).

join(Pid, Player) -> gen_fsm:sync_send_all_state_event(Pid, {join, Player}).
leave(Pid, Player) -> gen_fsm:sync_send_all_state_event(Pid, {leave, Player}).
sit(Pid, Player) -> gen_fsm:sync_send_all_state_event(Pid, {sit, Player}).
start_game(Pid) -> gen_fsm:sync_send_event(Pid, start_game).
get_seats(Pid) -> gen_fsm:sync_send_all_state_event(Pid, get_seats).
take_turn(Pid, Action) -> gen_fsm:sync_send_event(Pid, {take_turn, Action}).
show_cards(Pid, Player) -> gen_fsm:sync_send_event(Pid, {show_cards, Player}).
set_timeout(Pid, Timeout) -> gen_fsm:sync_send_all_state_event(Pid, {set_timeout, Timeout}).
show_game_state(Pid) -> gen_fsm:sync_send_all_state_event(Pid, show_game_state).
register_nickname(Pid, Player, NickName) -> 
    gen_fsm:send_all_state_event(Pid, {register_nickname, Player, NickName}).

%% gen_fsm.

init([TableId]) ->
    {ok, Seats} = seats:start_link(6),
	{ok, waiting_for_players, #state{seats=Seats, id=TableId}}.

waiting_for_players(start_game, _From, StateData) ->
    case length(seats:get_nonempty_seats(StateData#state.seats)) of
        N when N < 2 -> {reply, {error, not_enough_players}, waiting_for_players, StateData};
        _ -> {reply, ok, game_in_progress, start_game_routine_(StateData)}
    end;
waiting_for_players(_, _, StateData) ->
    {reply, ignored, waiting_for_players, StateData}.

waiting_for_players({timeout, _Ref, restart}, StateData) ->
    case length(seats:get_nonempty_seats(StateData#state.seats)) of
        N when N < 2 -> {next_state, waiting_for_players, StateData};
        _ -> {next_state, game_in_progress, start_game_routine_(StateData)}
    end.

game_in_progress({take_turn,_}, {Player,_Tag}, #state{actor=Actor}=State) when Player /= Actor#seat.player ->
    {reply, {error, not_your_turn}, game_in_progress, State};
game_in_progress({take_turn,Action}, _From, #state{actor_options=Options}=State) ->
    {Reply, NewStateName, NewState} = case lists:member(Action, Options) of
        false -> {{error, invalid_action}, game_in_progress, State};
        true -> handle_action_(State, Action)
    end,
    {reply, Reply, NewStateName, NewState};
game_in_progress({show_cards, Player}, _From, StateData) ->
    Cards = seats:show_cards_from_player(StateData#state.seats, Player),
    {reply, Cards, game_in_progress, StateData};
game_in_progress(_, _, StateData) ->
    {reply, ignored, game_in_progress, StateData}.

game_in_progress({timeout, _Ref, Action}, State) ->
    io:format("last player did not take action in time~n"),
    Player = State#state.actor#seat.player,
    player:notify(Player, timeout),
    broadcast_(State, {timeout, Player}),
    {_Reply, NewStateName, NewState} = handle_action_(State, Action),
    {next_state, NewStateName, NewState}.

handle_event({register_nickname, Player, NickName}, StateName, #state{users=Users}=StateData) ->
    NewUsers = case lists:keyfind(Player, #user.player, Users) of
        false -> Users;
        User -> lists:keystore(Player, #user.player, Users, User#user{nickname=NickName})
    end,
    {next_state, StateName, StateData#state{users=NewUsers}};
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event({sit, Player}, _From, StateName, StateData) ->
    Reply = seats:join(StateData#state.seats, Player),
    case Reply of
        ok -> broadcast_(StateData, {new_player, Player});
        _ -> ignore
    end,
    {reply, Reply, StateName, StateData};
handle_sync_event({join, Player}, _From, StateName, StateData) ->
    Users = [#user{player=Player}|StateData#state.users],
    broadcast_(StateData, {join, Player}),
    {reply, ok, StateName, StateData#state{users=Users}};
handle_sync_event({leave, Player}, _From, StateName, #state{seats=Seats,users=Users}=StateData) ->
    case seats:leave(Seats, Player) of
        ok -> io:format("Player ~p dropped out~n", [Player]);
        {error, no_such_player} -> ok
    end,
    NewUsers = lists:keydelete(Player, #user.player, Users),
    NewState = StateData#state{users=NewUsers},
    broadcast_(NewState, {leave, Player}),

    case NewUsers of
        [] -> {stop, normal, ok, NewState};
        _ -> {reply, ok, StateName, NewState}
    end;
handle_sync_event(get_seats, _From, StateName, StateData) ->
    {reply, StateData#state.seats, StateName, StateData};
handle_sync_event(show_game_state, {Player, _Tag}, StateName, 
        #state{seats=Seats,community_cards=CC,users=Users}=StateData) ->
    Dealer = seats:get_dealer(Seats),
    Reply = #game_state{
        state_name=StateName,
        seats=seats:show_seats(Seats, Player),
        pots=seats:get_pots(Seats),
        community_cards=CC,
        dealer_button_pos=Dealer#seat.position,
        users=Users},
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

start_game_routine_(#state{seats=Seats,timeout=Timeout,max_raises_per_street=MR}=StateData) ->
    {ok, Deck} = deck:start_link(),
    NewState = StateData#state{deck=Deck},
    seats:mark_active_players(Seats),
    seats:set_raises_left(Seats, MR),
    seats:rotate_dealer_button(Seats),
    {SmallBlind, BigBlind} = seats:get_blinds(Seats),
    seats:handle_action(Seats, SmallBlind, small_blind),
    seats:handle_action(Seats, BigBlind, big_blind),
    DealTimes = length(seats:show_active_seats(Seats))*2,
    deal_cards_(NewState, SmallBlind, DealTimes),
    ActorSeat = seats:get_preflop_actor(Seats),
    Options = seats:get_available_options(Seats, ActorSeat),
    player:notify(ActorSeat#seat.player, {signal_turn, Options}),
    broadcast_(StateData, {timer, ActorSeat#seat.player, Timeout}),
    Timer = gen_fsm:start_timer(Timeout, get_timeout_action_(Options)),
    broadcast_(StateData, game_started),
    NewState#state{actor=ActorSeat, stage=preflop, actor_options=Options, timer=Timer}.

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
        _ -> game_in_progress
    end,

    NewTimer = case NewState#state.actor of
        undefined -> undefined;
        NextActor -> 
            player:notify(NextActor#seat.player, {signal_turn, NewState#state.actor_options}),
            broadcast_(StateData, {timer, NextActor#seat.player, Timeout}),
            gen_fsm:start_timer(Timeout, get_timeout_action_(NewState#state.actor_options))
    end,
    broadcast_(StateData, {take_turn, Actor#seat.player, Action}),
    {ok, NewStateName, NewState#state{timer=NewTimer}}.

draw_community_cards_(#state{community_cards=CC,deck=Deck,seats=Seats,max_raises_per_street=MR}=State, 
        N, NextStage) ->
    deck:draw_cards(Deck, 1), % burn card
    NewCC = CC ++ deck:draw_cards(Deck, N),
    io:format("community cards: ~p~n", [NewCC]),
    seats:clear_last_action(Seats),
    seats:pot_bets(Seats),
    case NextStage of
        turn -> seats:double_bet_amount(Seats);
        _ -> ignored
    end,
    seats:set_raises_left(Seats, MR),
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
    io:format("pot wins: ~p~n", [PotWins]),
    broadcast_(State, {community_cards, CC}),
    broadcast_(State, {pot_wins, PotWins}),
    game_end_routine_(State),
    State#state{community_cards=[],deck=undefined,stage=show_down,actor=undefined,actor_options=[]}.

hand_over_(#state{community_cards=CC,seats=Seats}=State) ->
    seats:pot_bets(Seats),
    PotWins = seats:hand_over(Seats),
    io:format("pot wins: ~p~n", [PotWins]),
    broadcast_(State, {community_cards, CC}),
    broadcast_(State, {pot_wins, PotWins}),
    game_end_routine_(State),
    State#state{community_cards=[],deck=undefined,stage=hand_over,actor=undefined,actor_options=[]}.

game_end_routine_(#state{seats=Seats,deck=Deck,restart_timer=RestartTimer}) ->
    seats:drop_broke_players(Seats),
    seats:prepare_new_game(Seats),
    deck:stop(Deck),
    gen_fsm:start_timer(RestartTimer, restart).

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
    lists:foreach(fun(#user{player=Player}) -> player:notify(Player, Event) end, Users).
