-module(cardgames_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
            {'_', [
                    {"/", cowboy_static, {priv_file, cardgames, "index.html"}},
                    {"/player_ws", player_ws_handler, []},
                    {"/static/[...]", cowboy_static, {priv_dir, cardgames, "static"}}
                ]}
        ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	cardgames_sup:start_link().

stop(_State) ->
	ok.
