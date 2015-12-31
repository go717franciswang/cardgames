-module(player_ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-record(state, { player
}).
-include("records.hrl").

init(Req, _Opts) ->
    io:format("Player connected~n"),
    #{nickname := NickName} = cowboy_req:match_qs([nickname], Req),
    {ok, Player} = players_sup:create_player(NickName),
    player:add_event_handler(Player, player_ws_event_handler, [self()]),
    {cowboy_websocket, Req, #state{player=Player}, 30000}.

websocket_handle({text, <<"create_table">>}, Req, #state{player=Player}=State) ->
    {ok, _Table} = player:create_table(Player),
    {reply, {text, "create_table|{\"status\":\"ok\"}"}, Req, State};
websocket_handle({text, <<"list_tables">>}, Req, State) ->
    Tables = tables_sup:list_tables(),
    Reply = ws_util:build_reply(list_tables, jiffy:encode(Tables)),
    {reply, {text, Reply}, Req, State};
websocket_handle({text, <<"join_table ", Id/binary>>}, Req, #state{player=Player}=State) ->
    ok = player:join_table(Player, erlang:binary_to_integer(Id)),
    Reply = ws_util:build_game_state_reply(join_table, Player),
    {reply, {text, Reply}, Req, State};
websocket_handle({text, <<"sit">>}, Req, #state{player=Player}=State) ->
    ok = player:sit(Player),
    Reply = ws_util:build_game_state_reply(sit, Player),
    {reply, {text, Reply}, Req, State};
websocket_handle({text, <<"start_game">>}, Req, #state{player=Player}=State) ->
    Res = player:start_game(Player),
    {reply, {text, ws_util:build_ok_or_error_reply(start_game, Res)}, Req, State};
websocket_handle({text, <<"show_cards">>}, Req, #state{player=Player}=State) ->
    Cards = player:show_cards(Player),
    {reply, {text, jiffy:encode(hand:cards_to_strs(Cards))}, Req, State};
websocket_handle({text, <<"take_turn ", Action/binary>>}, Req, #state{player=Player}=State) ->
    Res = player:take_turn(Player, erlang:binary_to_existing_atom(Action, utf8)),
    {reply, {text, ws_util:build_ok_or_error_reply(took_turn, Res)}, Req, State};
websocket_handle({text, <<"leave">>}, Req, #state{player=Player}=State) ->
    ok = player:leave(Player),
    {reply, {text, "ok"}, Req, State};
websocket_handle({text, <<"ping">>}, Req, State) ->
    {ok, Req, State};
websocket_handle({text, Data}, Req, State) ->
    io:format("Got message: ~p~n", [Data]),
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info({reply, update_game}, Req, State) ->
    Reply = ws_util:build_game_state_reply(update_game, State#state.player),
    {reply, {text, Reply}, Req, State};
websocket_info({reply, {signal_turn, Options}}, Req, State) ->
    Reply = ws_util:build_reply(signal_turn, jiffy:encode(Options)),
    {reply, {text, Reply}, Req, State};
websocket_info({reply, Reply}, Req, State) ->
    {reply, {text, Reply}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

terminate(_Info, _Req, #state{player=Player}) ->
    player:leave(Player),
    players_sup:terminate_player(Player),
    ok.
