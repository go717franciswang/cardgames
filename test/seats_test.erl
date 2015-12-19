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

    ?assertEqual(P2, S#seat.player),
    ?assertEqual(P3, B#seat.player).

place_bet_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),

    seats:place_bet(Seats, S1, 1),
    seats:place_bet(Seats, S2, 2),

    [NS1, NS2] = seats:show_active_seats(Seats),
    ?assertEqual(1, NS1#seat.bet),
    ?assertEqual(2, NS2#seat.bet),
    ?assertEqual(1, S1#seat.money - NS1#seat.money),
    ?assertEqual(2, S2#seat.money - NS2#seat.money),

    seats:place_bet(Seats, S1, 2),
    seats:place_bet(Seats, S2, 3),
    [NS1_, NS2_] = seats:show_active_seats(Seats),
    ?assertEqual(2, NS1_#seat.bet),
    ?assertEqual(3, NS2_#seat.bet),
    ?assertEqual(2, S1#seat.money - NS1_#seat.money),
    ?assertEqual(3, S2#seat.money - NS2_#seat.money).

get_next_seat_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),

    ?assertEqual(S2, seats:get_next_seat(Seats, S1)),
    ?assertEqual(S1, seats:get_next_seat(Seats, S2)).

handle_action_call_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),
    seats:place_bet(Seats, S1, 1),
    seats:handle_action(Seats, S2, call),

    [NS1, NS2] = seats:show_active_seats(Seats),
    ?assertEqual(1, NS1#seat.bet),
    ?assertEqual(1, NS2#seat.bet).

handle_action_raise_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),
    seats:place_bet(Seats, S1, 1),
    seats:handle_action(Seats, S2, raise),

    [NS1, NS2] = seats:show_active_seats(Seats),
    ?assertEqual(1, NS1#seat.bet),
    ?assertEqual(1.1, NS2#seat.bet).

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

pot_bets_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),
    seats:handle_action(Seats, S1, small_blind),
    seats:handle_action(Seats, S2, big_blind),
    seats:handle_action(Seats, S1, call),
    seats:handle_action(Seats, S2, check),
    seats:pot_bets(Seats),
    [NS1, NS2] = seats:show_active_seats(Seats),
    ?assertEqual(0.2, seats:get_pot(Seats)),
    ?assertEqual(0, NS1#seat.bet),
    ?assertEqual(0, NS2#seat.bet).

drop_broke_players_test() ->
    {ok, Seats} = seats:start_link(6),
    seats:join(Seats, dummy_player1),
    seats:join(Seats, dummy_player2),
    [S1, S2] = seats:show_active_seats(Seats),

    seats:place_bet(Seats, S1, S1#seat.money),
    seats:drop_broke_players(Seats),

    LeftOver = seats:show_active_seats(Seats),
    ?assertEqual([S2], LeftOver).



