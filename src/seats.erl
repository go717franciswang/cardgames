-module(seats).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([join/2, show_players/1, get_dealer/1, rotate_dealer_button/1, get_blinds/1,
    get_preflop_actor/1, get_flop_actor/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {seats=[], dealer_button_pos=1
}).
-include("records.hrl").

%% API.

-spec start_link(integer) -> {ok, pid()}.
start_link(SeatCount) ->
	gen_server:start_link(?MODULE, [SeatCount], []).

join(Pid, Player) -> gen_server:call(Pid, {join, Player}).
show_players(Pid) -> gen_server:call(Pid, show_players).
get_dealer(Pid) -> gen_server:call(Pid, get_dealer).
rotate_dealer_button(Pid) -> gen_server:call(Pid, rotate_dealer_button).
get_blinds(Pid) -> gen_server:call(Pid, get_blinds).
get_preflop_actor(Pid) -> gen_server:call(Pid, get_preflop_actor).
get_flop_actor(Pid) -> gen_server:call(Pid, get_flop_actor).

%% gen_server.

init([SeatCount]) ->
    Seats = [#seat{position=X} || X <- lists:seq(1, SeatCount)],
	{ok, #state{seats=Seats}}.

handle_call(show_players, _From, State) ->
    {reply, [X#seat.player || X <- State#state.seats], State};
handle_call({join, Player}, _From, State) ->
    Seats = State#state.seats,
    EmptySeat = get_empty_seat(Seats),
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
handle_call(get_dealer, _From, #state{seats=Seats, dealer_button_pos=DealerPos}=State) ->
    Dealer = lists:keyfind(DealerPos, #seat.position, Seats),
    {reply, Dealer, State};
handle_call(get_blinds, _From, State) ->
    Seats = State#state.seats,
    {Front, Back} = lists:split(State#state.dealer_button_pos, Seats),
    SearchSeats = lists:concat([Back, Front]),
    ActiveSeats = lists:filter(fun(Seat) -> Seat#seat.player /= undefined end, SearchSeats),
    Reply = case length(ActiveSeats) of
        2 -> [B,S] = ActiveSeats, {S,B};
        _ -> [S,B|_] = ActiveSeats, {S,B}
    end,
    {reply, Reply, State};
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

get_empty_seat(Seats) ->
    EmptySeats = [X || X <- Seats, X#seat.player == undefined],
    lists:nth(random:uniform(length(EmptySeats)), EmptySeats).


