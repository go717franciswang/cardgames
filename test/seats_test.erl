-module(seats_test).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

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

get_blinds_heads_up_test() ->
    {ok, Seats} = seats:start_link(2),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),

    Dealer = seats:get_dealer(Seats),
    {SmallBlind,BigBlind} = seats:get_blinds(Seats),

    % in heads-up poker, by convention, the dealer is also the small blind
    % http://www.holdemreview.com/texas-holdem-heads-up-rules/
    ?assertEqual(Dealer, SmallBlind),
    ?assertNotEqual(SmallBlind, BigBlind).

get_blinds_test() ->
    {ok, Seats} = seats:start_link(3),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    seats:join(Seats, dummy_player3),
    [P1,P2,P3] = seats:show_players(Seats),
    {S,B} = seats:get_blinds(Seats),

    ?assertEqual(S#seat.player, P2),
    ?assertEqual(B#seat.player, P3).

place_bet_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),

    seats:place_bet(Seats, S1, 1),
    seats:place_bet(Seats, S2, 2),

    [NS1, NS2] = seats:show_active_seats(Seats),
    ?assertEqual(NS1#seat.bet, 1),
    ?assertEqual(NS2#seat.bet, 2),
    ?assertEqual(S1#seat.money - NS1#seat.money, 1),
    ?assertEqual(S2#seat.money - NS2#seat.money, 2),

    seats:place_bet(Seats, S1, 2),
    seats:place_bet(Seats, S2, 3),
    [NS1_, NS2_] = seats:show_active_seats(Seats),
    ?assertEqual(NS1_#seat.bet, 2),
    ?assertEqual(NS2_#seat.bet, 3),
    ?assertEqual(S1#seat.money - NS1_#seat.money, 2),
    ?assertEqual(S2#seat.money - NS2_#seat.money, 3).

get_next_seat_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),

    ?assertEqual(seats:get_next_seat(Seats, S1), S2),
    ?assertEqual(seats:get_next_seat(Seats, S2), S1).

handle_action_call_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),
    seats:place_bet(Seats, S1, 1),
    seats:handle_action(Seats, S2, call),

    [NS1, NS2] = seats:show_active_seats(Seats),
    ?assertEqual(NS1#seat.bet, 1),
    ?assertEqual(NS2#seat.bet, 1).

handle_action_raise_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),
    seats:place_bet(Seats, S1, 1),
    seats:handle_action(Seats, S2, raise),

    [NS1, NS2] = seats:show_active_seats(Seats),
    ?assertEqual(NS1#seat.bet, 1),
    ?assertEqual(NS2#seat.bet, 1.1).

is_betting_complete_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    ?assertEqual(false, seats:is_betting_complete(Seats)),

    [S1, S2] = seats:show_active_seats(Seats),
    seats:handle_action(Seats, S1, small_blind),
    seats:handle_action(Seats, S2, big_blind),
    ?assertEqual(false, seats:is_betting_complete(Seats)),

    seats:handle_action(Seats, S1, call),
    ?assertEqual(false, seats:is_betting_complete(Seats)),
    seats:handle_action(Seats, S2, check),
    ?assertEqual(true, seats:is_betting_complete(Seats)).




