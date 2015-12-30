-module(player_ws_event_handler).
-behaviour(gen_event).

%% gen_event.
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {ws}).

init([WS]) ->
    {ok, #state{ws=WS}}.

handle_event(game_started, State) ->
    State#state.ws ! {reply, "game_started|{\"status\":\"ok\"}"},
    {ok, State};
handle_event({signal_turn, Options}, State) ->
    State#state.ws ! {reply, {signal_turn, Options}},
    {ok, State};
handle_event({join, Player}, State) ->
    State#state.ws ! {reply, update_game},
    Content = jiffy:encode(#{player => util:pid_to_serializable(Player)}),
    State#state.ws ! {reply, ws_util:build_reply(join, Content)},
    {ok, State};
handle_event({new_player, Player}, State) ->
    State#state.ws ! {reply, update_game},
    Content = jiffy:encode(#{player => util:pid_to_serializable(Player)}),
    State#state.ws ! {reply, ws_util:build_reply(new_player, Content)},
    {ok, State};
handle_event({deal_card, _Card}, State) ->
    State#state.ws ! {reply, update_game},
    {ok, State};
handle_event(Event, State) ->
    io:format("Got notification: ~p~n", [Event]),
    State#state.ws ! {reply, update_game},
    State#state.ws ! {reply, io_lib:format("~p", [Event])},
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ignored, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.
