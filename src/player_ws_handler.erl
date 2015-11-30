-module(player_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, { 
}).

init(_, _, _) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    io:format("Player connected~n"),
	Req2 = cowboy_req:compact(Req),
    {ok, Req2, #state{}}.

websocket_handle({text, <<"create_table">>}, Req, State) ->
    {ok, Pid} = cardgames_sup:create_table(),
    holdem:join(Pid, self()),
    io:format("Created table Pid: ~p~n", [Pid]),
    {reply, {text, list_to_binary("Created table with pid"++erlang:pid_to_list(Pid))}, Req, State};
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
