-module(ws_util).
-export([build_reply/2, build_game_state_reply/2, build_ok_or_error_reply/2]).

build_reply(Header, Content) ->
    HeaderBinary = erlang:atom_to_binary(Header, utf8),
    Sep = <<"|">>,
    <<HeaderBinary/binary, Sep/binary, Content/binary>>.

build_game_state_reply(Header, Player) ->
    GameState = player:show_game_state(Player),
    Bin = jiffy:encode(#{status=>ok, game=>util:game_state_to_serializable(GameState)}),
    build_reply(Header, Bin).

build_ok_or_error_reply(Header, ok) ->
    build_reply(Header, jiffy:encode(#{status => ok}));
build_ok_or_error_reply(Header, {error, E}) ->
    build_reply(Header, jiffy:encode(#{error => E})).
