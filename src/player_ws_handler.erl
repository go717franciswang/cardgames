-module(player_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, { player_pid
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    io:format("Player connected~n"),
    {ok, Pid} = players_sup:create_player(),
	Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{player_pid=Pid}}.

websocket_handle({text, <<"create_table">>}, Req, State) ->
    player:create_table(State#state.player_pid),
    {reply, {text, "Created table"}, Req, State};
websocket_handle({text, <<"list_tables">>}, Req, State) ->
    Tables = tables_sup:list_tables(),
    {reply, {text, io_lib:format("~p", [Tables])}, Req, State};
websocket_handle({text, <<"join_table ", Id/binary>>}, Req, State) ->
    player:join_table(State#state.player_pid, erlang:binary_to_integer(Id)),
    {reply, {text, "Joined table"}, Req, State};
websocket_handle({text, Data}, Req, State) ->
    io:format("Got message: ~p~n", [Data]),
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info({reply, Reply}, Req, State) ->
    {reply, Reply, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
