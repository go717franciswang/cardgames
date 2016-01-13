-module(player_SUITE).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([testShowDown/1, testHandOver/1, testPlayerLeaveDuringGame/1, testPlayerLeaveDuringGameDuringTurn/1,
    testPlayerLeaveDuringWait/1, testTimeout/1, testTableAutoDelete/1, testPlayerJoiningDuringGame/1,
    testPotsMergeAfterPlayerFolding/1]).
-include_lib("common_test/include/ct.hrl").
-include("records.hrl").

all() -> [testShowDown, testHandOver, testPlayerLeaveDuringGame, testPlayerLeaveDuringGameDuringTurn,
    testPlayerLeaveDuringWait, testTimeout, testTableAutoDelete, testPlayerJoiningDuringGame,
    testPotsMergeAfterPlayerFolding].

init_per_testcase(testPlayerLeaveDuringWait, Config) -> Config;
init_per_testcase(_TestName, Config) ->
    {ok, GamesSup} = cardgames_sup:start_link(),

    {ok, Player1} = players_sup:create_player(p),
    {ok, Player2} = players_sup:create_player(p),
    {ok, Player3} = players_sup:create_player(p),
    {ok, Player4} = players_sup:create_player(p),
    {ok, Table} = player:create_table(Player1),
    [TableId] = tables_sup:list_tables(),
    ok = player:join_table(Player2, TableId),
    ok = player:join_table(Player3, TableId),
    ok = player:join_table(Player4, TableId),

    ok = player:sit(Player1),
    ok = player:sit(Player2),
    ok = player:sit(Player3),
    ok = player:sit(Player4),

    ok = player:start_game(Player1),
    [C1,C2] = player:show_cards(Player1),
    [C3,C4] = player:show_cards(Player2),
    [C5,C6] = player:show_cards(Player3),
    [C7,C8] = player:show_cards(Player4),

    % make sure cards are unique
    length(lists:usort([C1,C2,C3,C4,C5,C6,C7,C8])) == 8,

    Seats = holdem:get_seats(Table),
    [S1,S2,S3,S4] = seats:show_active_seats(Seats),
    {Dealer, SB, BB, FirstActor} = case S1#seat.position of
        1 -> {S2,S3,S4,S1};
        _ -> {S1,S2,S3,S4}
    end,

    % make sure correct blinds are set
    {SB,BB} = seats:get_blinds(Seats),

    [{games_sup,GamesSup},
     {table,Table},
     {seats,Seats},
     {first_actor,FirstActor},
     {dealer,Dealer},
     {sb,SB},
     {bb,BB} | Config].

end_per_testcase(_, _Config) ->
    ok.

testShowDown(Config) ->
    Seats = ?config(seats, Config),
    FirstActor = ?config(first_actor, Config),
    Dealer = ?config(dealer, Config),
    SB = ?config(sb, Config),
    BB = ?config(bb, Config),

    % preflop
    ok = player:take_turn(FirstActor#seat.player, call),
    ok = player:take_turn(Dealer#seat.player, call),
    ok = player:take_turn(SB#seat.player, call),
    ok = player:take_turn(BB#seat.player, check),
    ok = player:take_turn(FirstActor#seat.player, check),
    ok = player:take_turn(Dealer#seat.player, check),
    ok = player:take_turn(SB#seat.player, check),
    
    % flop
    ok = player:take_turn(SB#seat.player, check),
    ok = player:take_turn(BB#seat.player, check),
    ok = player:take_turn(FirstActor#seat.player, check),
    ok = player:take_turn(Dealer#seat.player, check),

    % turn
    ok = player:take_turn(SB#seat.player, check),
    ok = player:take_turn(BB#seat.player, check),
    ok = player:take_turn(FirstActor#seat.player, check),
    ok = player:take_turn(Dealer#seat.player, check),

    % river
    ok = player:take_turn(SB#seat.player, check),
    ok = player:take_turn(BB#seat.player, check),
    ok = player:take_turn(FirstActor#seat.player, check),
    ok = player:take_turn(Dealer#seat.player, check),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]).

testHandOver(Config) ->
    Seats = ?config(seats, Config),
    FirstActor = ?config(first_actor, Config),
    Dealer = ?config(dealer, Config),
    SB = ?config(sb, Config),
    BB = ?config(bb, Config),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]),
    ok = player:take_turn(FirstActor#seat.player, call),
    ok = player:take_turn(Dealer#seat.player, fold),
    ok = player:take_turn(SB#seat.player, fold),
    ok = player:take_turn(BB#seat.player, fold),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]).

testPlayerLeaveDuringGame(Config) ->
    Seats = ?config(seats, Config),
    FirstActor = ?config(first_actor, Config),
    Dealer = ?config(dealer, Config),
    SB = ?config(sb, Config),
    BB = ?config(bb, Config),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]),
    ok = player:take_turn(FirstActor#seat.player, call),
    ok = player:leave(SB#seat.player),
    ok = player:take_turn(Dealer#seat.player, fold),
    ok = player:take_turn(BB#seat.player, fold),
    3 == length(seats:show_active_seats(Seats)),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]).

testPlayerLeaveDuringGameDuringTurn(Config) ->
    Seats = ?config(seats, Config),
    FirstActor = ?config(first_actor, Config),
    Dealer = ?config(dealer, Config),
    SB = ?config(sb, Config),
    BB = ?config(bb, Config),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]),
    ok = player:take_turn(FirstActor#seat.player, call),
    ok = player:take_turn(Dealer#seat.player, fold),
    ok = player:leave(SB#seat.player),
    ok = player:take_turn(BB#seat.player, fold),
    3 == length(seats:show_active_seats(Seats)),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]).

testPlayerLeaveDuringWait(Config) ->
    {ok, GamesSup} = cardgames_sup:start_link(),

    {ok, Player1} = players_sup:create_player(p),
    {ok, Player2} = players_sup:create_player(p),
    {ok, Player3} = players_sup:create_player(p),
    {ok, Player4} = players_sup:create_player(p),
    {ok, Table} = player:create_table(Player1),
    [TableId] = tables_sup:list_tables(),
    ok = player:join_table(Player2, TableId),
    ok = player:join_table(Player3, TableId),
    ok = player:join_table(Player4, TableId),

    ok = player:sit(Player1),
    ok = player:sit(Player2),
    ok = player:sit(Player3),
    ok = player:sit(Player4),

    ok = player:leave(Player3).

testTimeout(Config) ->
    Seats = ?config(seats, Config),
    FirstActor = ?config(first_actor, Config),
    Dealer = ?config(dealer, Config),
    SB = ?config(sb, Config),
    BB = ?config(bb, Config),
    Table = ?config(table, Config),

    ok = holdem:set_timeout(Table, 200),
    ok = player:take_turn(FirstActor#seat.player, call),
    timer:sleep(200),
    {error, not_your_turn} = player:take_turn(Dealer#seat.player, raise),
    ok = player:take_turn(SB#seat.player, fold),
    ok = player:take_turn(BB#seat.player, fold),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]).

testTableAutoDelete(Config) ->
    FirstActor = ?config(first_actor, Config),
    Dealer = ?config(dealer, Config),
    SB = ?config(sb, Config),
    BB = ?config(bb, Config),
    player:leave(FirstActor#seat.player),
    player:leave(Dealer#seat.player),
    player:leave(SB#seat.player),
    player:leave(BB#seat.player),
    [] = tables_sup:list_tables().

testPlayerJoiningDuringGame(Config) ->
    Seats = ?config(seats, Config),
    FirstActor = ?config(first_actor, Config),
    Dealer = ?config(dealer, Config),
    SB = ?config(sb, Config),
    BB = ?config(bb, Config),

    ok = player:take_turn(FirstActor#seat.player, call),

    {ok, Player5} = players_sup:create_player(p),
    [TableId] = tables_sup:list_tables(),
    ok = player:join_table(Player5, TableId),
    ok = player:sit(Player5),
    
    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]),
    ok = player:take_turn(Dealer#seat.player, fold),
    ok = player:take_turn(SB#seat.player, fold),
    ok = player:take_turn(BB#seat.player, fold),

    io:format("current seats: ~p~n", [seats:show_active_seats(Seats)]).

testPotsMergeAfterPlayerFolding(Config) ->
    Seats = ?config(seats, Config),
    FirstActor = ?config(first_actor, Config),
    Dealer = ?config(dealer, Config),
    SB = ?config(sb, Config),
    BB = ?config(bb, Config),
 
    % preflop
    ok = player:take_turn(FirstActor#seat.player, call),
    ok = player:take_turn(Dealer#seat.player, call),
    ok = player:take_turn(SB#seat.player, call),
    ok = player:take_turn(BB#seat.player, check),
    ok = player:take_turn(FirstActor#seat.player, check),
    ok = player:take_turn(Dealer#seat.player, check),
    ok = player:take_turn(SB#seat.player, check),
    
    % flop
    ok = player:take_turn(SB#seat.player, bet),
    ok = player:take_turn(BB#seat.player, raise),
    ok = player:take_turn(FirstActor#seat.player, call),
    ok = player:take_turn(Dealer#seat.player, fold),
    ok = player:take_turn(SB#seat.player, fold),
    ok = player:take_turn(BB#seat.player, check),
    ok = player:take_turn(FirstActor#seat.player, check),

    % turn
    ok = player:take_turn(BB#seat.player, check),

    Pots = seats:get_pots(Seats),
    io:format("~p~n", [Pots]),
    1 = length(Pots).
