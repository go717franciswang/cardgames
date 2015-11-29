-module(cardgames_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	cardgames_sup:start_link().

stop(_State) ->
	ok.
