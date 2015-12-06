-module(deck).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([show_deck/1, shuffle/1, draw_cards/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {cards=[]
}).
-include("records.hrl").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

show_deck(Pid) ->
    gen_server:call(Pid, show_deck).

shuffle(Pid) ->
    gen_server:call(Pid, shuffle).

draw_cards(Pid, Count) ->
    gen_server:call(Pid, {draw_cards, Count}).

%% gen_server.

init([]) ->
    Suits = [club, diamond, heart, spade],
    Ranks = [ace, two, three, four, five, six, seven, eight, nine, ten, jack, queen, king],
    Cards = [#card{suit=X, rank=Y} || X <- Suits, Y <- Ranks],
    ShuffledCards = shuffle_cards(Cards),
	{ok, #state{cards=ShuffledCards}}.

handle_call(show_deck, _From, State) ->
    {reply, State#state.cards, State};
handle_call(shuffle, _From, State) ->
    NewCards = shuffle_cards(State#state.cards),
	{reply, ok, State#state{cards=NewCards}};
handle_call({draw_cards, Count}, _From, State) ->
    {DrawnCards, NewCards} = lists:split(Count, State#state.cards),
    {reply, DrawnCards, State#state{cards=NewCards}};
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

shuffle_cards(Items) ->
    % http://stackoverflow.com/a/8820501/3678068
    [X || {_,X} <- lists:sort([{random:uniform(), Item} || Item <- Items])].

