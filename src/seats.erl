-module(seats).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([join/2, show_players/1, show_active_seats/1, get_dealer/1, 
        rotate_dealer_button/1, get_blinds/1, get_preflop_actor/1, 
        get_flop_actor/1, place_bet/3, deal_card/3, get_next_seat/2,
        handle_action/3, is_betting_complete/1, clear_last_action/1,
        pot_bets/1, get_pots/1, distribute_winning/2, prepare_new_game/1,
        show_cards_from_player/2, is_hand_over/1, get_available_options/2,
        leave/2, drop_broke_players/1, set_money/3, show_down/2, 
        hand_over/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {seats=[], dealer_button_pos=1, blind_amount=0.1, pots=[]
}).
-include("records.hrl").

%% API.

-spec start_link(integer) -> {ok, pid()}.
start_link(SeatCount) ->
	gen_server:start_link(?MODULE, [SeatCount], []).

join(Pid, Player) -> gen_server:call(Pid, {join, Player}).
show_players(Pid) -> gen_server:call(Pid, show_players).
show_active_seats(Pid) -> gen_server:call(Pid, show_active_seats).
get_dealer(Pid) -> gen_server:call(Pid, get_dealer).
rotate_dealer_button(Pid) -> gen_server:call(Pid, rotate_dealer_button).
get_blinds(Pid) -> gen_server:call(Pid, get_blinds).
get_preflop_actor(Pid) -> gen_server:call(Pid, get_preflop_actor).
get_flop_actor(Pid) -> gen_server:call(Pid, get_flop_actor).
set_money(Pid, Seat, Money) -> gen_server:call(Pid, {set_money, Seat, Money}).
place_bet(Pid, Seat, BetAmount) -> gen_server:call(Pid, {place_bet, Seat, BetAmount}).
deal_card(Pid, Seat, Card) -> gen_server:call(Pid, {deal_card, Seat, Card}).
get_next_seat(Pid, Seat) -> gen_server:call(Pid, {get_next_seat, Seat}).
handle_action(Pid, Actor, Action) -> gen_server:call(Pid, {handle_action, Actor, Action}).
is_betting_complete(Pid) -> gen_server:call(Pid, is_betting_complete).
clear_last_action(Pid) -> gen_server:call(Pid, clear_last_action).
pot_bets(Pid) -> gen_server:call(Pid, pot_bets).
get_pots(Pid) -> gen_server:call(Pid, get_pots).
distribute_winning(Pid,WinningSeats) -> gen_server:call(Pid, {distribute_winning,WinningSeats}).
prepare_new_game(Pid) -> gen_server:call(Pid, prepare_new_game).
show_cards_from_player(Pid, Player) -> gen_server:call(Pid, {show_cards_from_player, Player}).
is_hand_over(Pid) -> gen_server:call(Pid, is_hand_over).
get_available_options(Pid, Seat) -> gen_server:call(Pid, {get_available_options, Seat}).
leave(Pid, Player) -> gen_server:call(Pid, {leave, Player}).
drop_broke_players(Pid) -> gen_server:call(Pid, drop_broke_players).
show_down(Pid, CC) -> gen_server:call(Pid, {show_down, CC}).
hand_over(Pid) -> gen_server:call(Pid, hand_over).

%% gen_server.

init([SeatCount]) ->
    Seats = [#seat{position=X} || X <- lists:seq(1, SeatCount)],
	{ok, #state{seats=Seats}}.

handle_call(show_players, _From, State) ->
    {reply, [X#seat.player || X <- State#state.seats], State};
handle_call(show_active_seats, _From, State) ->
    {reply, get_active_seats_(State), State};
handle_call({join, Player}, _From, State) ->
    Seats = State#state.seats,
    EmptySeat = get_empty_seat_(State),
    NewSeat = EmptySeat#seat{player=Player, money=10},
    NewSeats = lists:keystore(NewSeat#seat.position, #seat.position, Seats, NewSeat),
    {reply, ok, State#state{seats=NewSeats}};
handle_call(rotate_dealer_button, _From, State) ->
    Seats = State#state.seats,
    {Front, Back} = lists:split(State#state.dealer_button_pos, Seats),
    SearchSeats = lists:concat([Back, Front]),
    [NewDealer|_] = lists:filter(fun(Seat) -> Seat#seat.player /= undefined end, SearchSeats),
    NewDealerButtonPos = NewDealer#seat.position,
    {reply, ok, State#state{dealer_button_pos=NewDealerButtonPos}};
handle_call(get_dealer, _From, State) ->
    {reply, get_dealer_(State), State};
handle_call(get_preflop_actor, _From, State) ->
    {_,B} = get_blinds_(State),
    {reply, get_next_seat_(State, B), State};
handle_call(get_flop_actor, _From, State) ->
    Dealer = get_dealer_(State),
    {reply, get_next_seat_(State, Dealer), State};
handle_call(get_blinds, _From, State) ->
    {reply, get_blinds_(State), State};
handle_call({set_money, #seat{position=Pos}, Money}, _From, State) ->
    Seat = lists:keyfind(Pos, #seat.position, State#state.seats),
    NewSeat = Seat#seat{money=Money},
    NewSeats = lists:keystore(Pos, #seat.position, State#state.seats, NewSeat),
    {reply, ok, State#state{seats=NewSeats}};
handle_call({place_bet, Seat, BetAmount}, _From, State) ->
    NewState = place_bet_(State, Seat, BetAmount),
    {reply, ok, NewState};
handle_call({deal_card, #seat{position=Pos,player=Player}, Card}, _From, State) ->
    player:deal_card(Player, Card),
    Seat = lists:keyfind(Pos,#seat.position,State#state.seats),
    Cards = Seat#seat.cards,
    NewSeat = Seat#seat{cards=[Card|Cards]},
    NewSeats = lists:keystore(Pos, #seat.position, State#state.seats, NewSeat),
    {reply, ok, State#state{seats=NewSeats}};
handle_call({get_next_seat, Seat}, _From, State) ->
    {reply, get_next_seat_(State, Seat), State};
handle_call({handle_action, Actor, call}, _From, State) ->
    BetAmount = get_call_amount_(State),
    NewState = log_action_(place_bet_(State, Actor, BetAmount), Actor, call),
    {reply, ok, NewState};
handle_call({handle_action, Actor, raise}, _From, State) ->
    BetAmount = get_raise_amount_(State),
    NewState = log_action_(place_bet_(State, Actor, BetAmount), Actor, raise),
    {reply, ok, NewState};
handle_call({handle_action, Actor, small_blind}, _From, State) ->
    BetAmount = State#state.blind_amount/2,
    NewState = place_bet_(State, Actor, BetAmount),
    {reply, ok, NewState};
handle_call({handle_action, Actor, big_blind}, _From, State) ->
    BetAmount = State#state.blind_amount,
    NewState = place_bet_(State, Actor, BetAmount),
    {reply, ok, NewState};
handle_call({handle_action, Actor, bet}, _From, State) ->
    BetAmount = State#state.blind_amount,
    NewState = log_action_(place_bet_(State, Actor, BetAmount), Actor, bet),
    {reply, ok, NewState};
handle_call({handle_action, Actor, check}, _From, State) ->
    NewState = log_action_(State, Actor, check),
    {reply, ok, NewState};
handle_call({handle_action, Actor, fold}, _From, State) ->
    NewState = log_action_(State, Actor, fold),
    {reply, ok, NewState};
handle_call(is_betting_complete, _From, State) ->
    Seats = get_active_seats_(State),
    CallAmount = get_call_amount_(State),
    Reply = lists:all(
        fun(#seat{last_action=fold}) -> true;
           (#seat{last_action=undefined}) -> false;
           (#seat{bet=Bet}) -> CallAmount == Bet;
           (_) -> false
        end, Seats),
    {reply, Reply, State};
handle_call(clear_last_action, _From, State) ->
    NewSeats = lists:map(
        fun(#seat{last_action=fold}=Seat) -> Seat;
           (#seat{player=undefined}=Seat) -> Seat;
           (Seat) -> Seat#seat{last_action=undefined}
        end, State#state.seats),
    {reply, ok, State#state{seats=NewSeats}};
handle_call(pot_bets, _From, #state{seats=Seats,pots=Pots}=State) ->
    BetSeatId = [{min(B,M),Pos} || #seat{position=Pos,bet=B,money=M} <- Seats, B > 0],
    NewPots = pot:build_pots(BetSeatId),
    MergedPots = pot:merge_pots(Pots, NewPots),
    {SinglePots, MultiPots} = pot:split_single_player_pots(MergedPots),
    NewSeats0 = lists:map(
        fun(#seat{bet=B,money=M}=S) ->
                S#seat{bet=0,money=max(0,M-B)}
        end, Seats),
    NewSeats = lists:foldl(
        fun(#pot{money=M1,eligible_ids=[Pos]},Ss) ->
                S = lists:keyfind(Pos, #seat.position, Ss),
                M0 = S#seat.money,
                lists:keystore(Pos, #seat.position, Ss, S#seat{money=M0+M1})
        end, NewSeats0, SinglePots),
    {reply, ok, State#state{seats=NewSeats,pots=MultiPots}};
handle_call(get_pots, _From, State) ->
    {reply, State#state.pots, State};
handle_call({distribute_winning, _WinningSeats}, _From, #state{pots=Pots,seats=Seats}=State) ->
    % TODO: remove this method b/c distribute_winning is now handled internally
    NewSeats = distribute_pots_(Pots, Seats),
    % WinningPerPlayer = Pot / length(WinningSeats),
    % NewSeats = lists:foldl(
    %     fun(#seat{position=Pos}, Seats) ->
    %             Seat = lists:keyfind(Pos, #seat.position, Seats),
    %             Money = Seat#seat.money,
    %             NewSeat = Seat#seat{money=Money+WinningPerPlayer},
    %             lists:keystore(Pos, #seat.position, Seats, NewSeat)
    %     end, OSeats, WinningSeats),
    {reply, ok, State#state{seats=NewSeats,pots=[]}};
handle_call(prepare_new_game, _From, #state{seats=Seats}=State) ->
    NewSeats = [Seat#seat{last_action=undefined,cards=[]} || Seat <- Seats],
    {reply, ok, State#state{seats=NewSeats}};
handle_call({show_cards_from_player,Player}, _From, #state{seats=Seats}=State) ->
    Seat = lists:keyfind(Player, #seat.player, Seats),
    {reply, Seat#seat.cards, State};
handle_call(is_hand_over, _From, State) ->
    Reply = length(get_nonfolded_seats_(State)) == 1,
    {reply, Reply, State};
handle_call({get_available_options, Seat}, _From, State) ->
    Options = case {is_first_bet_(State),is_highest_bet_(State,Seat)} of
        {true,_} -> [check, bet];
        {false,true} -> [check, raise];
        {false,false} -> [call, raise]
    end,
    {reply, [fold|Options], State};
handle_call({leave, Player}, _From, #state{seats=Seats}=State) ->
    {Reply,NewSeats} = case lists:keyfind(Player, #seat.player, Seats) of
        false -> 
            {{error, no_such_player}, Seats};
        #seat{position=Pos,bet=Bet} -> 
            NewSeat = #seat{position=Pos,bet=Bet},
            {ok, lists:keystore(Pos, #seat.position, Seats, NewSeat)}
    end,
    {reply, Reply, State#state{seats=NewSeats}};
handle_call(drop_broke_players, _From, #state{seats=Seats}=State) ->
    NewSeats = lists:map(
        fun(#seat{money=0,position=Pos}) -> #seat{position=Pos};
           (Seat) -> Seat
        end, Seats),
    {reply, ok, State#state{seats=NewSeats}};
handle_call({show_down, CC}, _From, #state{pots=Pots,seats=Seats}=State) ->
    Reply = lists:map(
        fun(#pot{eligible_ids=Ids}=Pot) ->
                SS = [lists:keyfind(Pos, #seat.position, Seats) || Pos <- Ids],
                SH = lists:map(
                    fun(#seat{cards=Cards,player=P}) ->
                            {H,FC} = hand:get_highest_hand(Cards++CC),
                            #play{player=P,hand=H,cards=FC}
                    end, SS),
                SHR = lists:sort(fun(#play{hand=HA},#play{hand=HB}) -> hand:is_higher_hand(HA,HB) end, SH),
                [#play{hand=BH}|_] = SHR,
                WSH = lists:takewhile(fun(#play{hand=H}) -> H == BH end, SHR),
                #pot_wins{pot=Pot, wins=WSH}
        end, Pots),
    NewState = distribute_winning_(State, Reply),
    {reply, Reply, NewState};
handle_call(hand_over, _From, #state{seats=Seats,pots=Pots}=State) ->
    [#seat{position=WinId,player=Winner}] = get_nonfolded_seats_(State),
    Reply = lists:map(
        fun(#pot{eligible_ids=Ids}=Pot) ->
                case lists:member(WinId, Ids) of
                    true -> #pot_wins{pot=Pot, wins=[#play{player=Winner}]};
                    false ->
                        Plays = lists:map(
                            fun(Id) ->
                                    #seat{player=P} = lists:keyfind(Id,#seat.position,Seats),
                                    #play{player=P}
                            end, Ids),
                        #pot_wins{pot=Pot, wins=Plays}
                end
        end, Pots),
    NewState = distribute_winning_(State, Reply),
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

get_empty_seat_(#state{seats=Seats}) ->
    EmptySeats = [X || X <- Seats, X#seat.player == undefined],
    lists:nth(rand:uniform(length(EmptySeats)), EmptySeats).

get_blinds_(State) ->
    Seats = State#state.seats,
    {Front, Back} = lists:split(State#state.dealer_button_pos, Seats),
    SearchSeats = lists:concat([Back, Front]),
    ActiveSeats = lists:filter(fun(Seat) -> Seat#seat.player /= undefined end, SearchSeats),
    case length(ActiveSeats) of
        2 -> [B,S] = ActiveSeats, {S,B};
        _ -> [S,B|_] = ActiveSeats, {S,B}
    end.

get_active_seats_(#state{seats=Seats}) ->
    [X || X <- Seats, X#seat.player /= undefined].

get_nonfolded_seats_(#state{seats=Seats}) ->
    [X || X <- Seats, X#seat.player /= undefined, X#seat.last_action /= fold].

get_next_seat_(State, #seat{position=Pos}) ->
    ActiveSeats = get_active_seats_(State),
    {Front, Back} = lists:splitwith(fun(#seat{position=P}) -> P /= Pos end, ActiveSeats),
    [_,Next|_] = Back++Front,
    Next.

% BetAmount represent final bet amount, it is not incremental change
place_bet_(State, #seat{position=Pos}, BetAmount) ->
    % use position to get the most current seat data in case seat in the
    % argument is out-of-date
    Seat = lists:keyfind(Pos, #seat.position, State#state.seats),
    NewSeat = Seat#seat{bet=BetAmount},
    NewSeats = lists:keystore(Pos, #seat.position, State#state.seats, NewSeat),
    State#state{seats=NewSeats}.

get_call_amount_(State) ->
    Seats = get_active_seats_(State),
    lists:max([Seat#seat.bet || Seat <- Seats]).

get_raise_amount_(State) ->
    get_call_amount_(State) + State#state.blind_amount.

log_action_(State, #seat{position=Pos}, Action) ->
    Seat = lists:keyfind(Pos, #seat.position, State#state.seats),
    NewSeat = Seat#seat{last_action=Action},
    NewSeats = lists:keystore(Pos, #seat.position, State#state.seats, NewSeat),
    State#state{seats=NewSeats}.

get_dealer_(#state{seats=Seats, dealer_button_pos=DealerPos}) ->
    lists:keyfind(DealerPos, #seat.position, Seats).

is_first_bet_(State) -> get_call_amount_(State) == 0.
is_highest_bet_(#state{seats=Seats}=State, #seat{position=Pos}) -> 
    #seat{bet=Bet} = lists:keyfind(Pos, #seat.position, Seats),
    get_call_amount_(State) == Bet.

distribute_pots_(_Pots, Seats) ->
    % TODO: what is this?
    Seats.

distribute_winning_(#state{seats=Seats}=State, [#pot_wins{pot=#pot{money=M},wins=Plays}|PotWins]) ->
    MoneyPerWinner = M / length(Plays),
    NewSeats = lists:foldl(
        fun(#play{player=P}, SS) ->
                S = lists:keyfind(P, #seat.player, SS),
                Money = S#seat.money,
                NS = S#seat{money=Money+MoneyPerWinner},
                lists:keystore(P, #seat.player, SS, NS)
        end, Seats, Plays),
    distribute_winning_(State#state{seats=NewSeats,pots=[]}, PotWins);
distribute_winning_(State, []) -> State.


