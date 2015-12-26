-module(player_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, { player
}).
-include("records.hrl").

init(_, _, _) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    io:format("Player connected~n"),
    {ok, Player} = players_sup:create_player(),
    player:add_event_handler(Player, player_ws_event_handler, [self()]),
	NewReq = cowboy_req:compact(Req),
    {ok, NewReq, #state{player=Player}}.

websocket_handle({text, <<"create_table">>}, Req, #state{player=Player}=State) ->
    {ok, _Table} = player:create_table(Player),
    {reply, {text, "create_table|{\"status\":\"ok\"}"}, Req, State};
websocket_handle({text, <<"list_tables">>}, Req, State) ->
    Tables = tables_sup:list_tables(),
    Reply = build_reply_(list_tables, jiffy:encode(Tables)),
    {reply, {text, Reply}, Req, State};
websocket_handle({text, <<"join_table ", Id/binary>>}, Req, #state{player=Player}=State) ->
    ok = player:join_table(Player, erlang:binary_to_integer(Id)),
    Reply = build_seats_reply_(join_table, Player),
    {reply, {text, Reply}, Req, State};
websocket_handle({text, <<"sit">>}, Req, #state{player=Player}=State) ->
    ok = player:sit(Player),
    Reply = build_seats_reply_(sit, Player),
    {reply, {text, Reply}, Req, State};
websocket_handle({text, <<"start_game">>}, Req, #state{player=Player}=State) ->
    ok = player:start_game(Player),
    {reply, {text, "start_game|{\"status\":\"ok\"}"}, Req, State};
websocket_handle({text, <<"show_cards">>}, Req, #state{player=Player}=State) ->
    Cards = player:show_cards(Player),
    {reply, {text, jiffy:encode(hand:cards_to_strs(Cards))}, Req, State};
websocket_handle({text, <<"take_turn ", Action/binary>>}, Req, #state{player=Player}=State) ->
    Reply = case player:take_turn(Player, erlang:binary_to_existing_atom(Action, utf8)) of
        ok -> "ok";
        {error, E} -> jiffy:encode({[{error,E}]})
    end,
    {reply, {text, Reply}, Req, State};
websocket_handle({text, <<"leave">>}, Req, #state{player=Player}=State) ->
    ok = player:leave(Player),
    {reply, {text, "ok"}, Req, State};
websocket_handle({text, Data}, Req, State) ->
    io:format("Got message: ~p~n", [Data]),
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info({reply, update_seats}, Req, State) ->
    Reply = build_seats_reply_(update_seats, State#state.player),
    {reply, {text, Reply}, Req, State};
websocket_info({reply, {signal_turn, Options}}, Req, State) ->
    Reply = build_reply_(signal_turn, jiffy:encode(Options)),
    {reply, {text, Reply}, Req, State};
websocket_info({reply, Reply}, Req, State) ->
    {reply, {text, Reply}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.

build_reply_(Header, Content) ->
    HeaderBinary = erlang:atom_to_binary(Header, utf8),
    Sep = <<"|">>,
    <<HeaderBinary/binary, Sep/binary, Content/binary>>.

build_seats_reply_(Header, Player) ->
    Seats = player:show_seats(Player),
    Bin = jiffy:encode(#{status=>ok, seats=>seats_to_maps_(Seats)}),
    build_reply_(Header, Bin).

seat_to_map_(#seat{position=Pos, player=undefined}) ->
    #{position=>Pos};
seat_to_map_(#seat{position=Pos, player=Player, money=Money, bet=Bet}) ->
    #{position=>Pos, player=>pid_to_list(Player), money=>Money, bet=>Bet}.
seats_to_maps_(Seats) -> [seat_to_map_(S) || S <- Seats].
