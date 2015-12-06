-module(seats_test).
-include_lib("eunit/include/eunit.hrl").

show_players_test() ->
    {ok, Seats} = seats:start_link(3),
    Players = seats:show_players(Seats),
    ?assertEqual([undefined, undefined, undefined], Players).

join_test() ->
    {ok, Seats} = seats:start_link(3),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    seats:join(Seats, dummy_player3),
    Players = seats:show_players(Seats),
    ?assertEqual([dummy_player1, dummy_player2, dummy_player3], lists:sort(Players)).

rotate_dealer_button_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    seats:join(Seats, dummy_player3),

    [D1,D2,D3,D4,D5,D6] = lists:map(
        fun(_) ->
                seats:rotate_dealer_button(Seats),
                Dealer1 = seats:get_dealer(Seats)
        end, lists:seq(1,6)),

    ?assertEqual(D1, D4),
    ?assertEqual(D2, D5),
    ?assertEqual(D3, D6),
    ?assertNotEqual(D1, D2).

