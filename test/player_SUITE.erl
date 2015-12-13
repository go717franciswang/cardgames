-module(player_SUITE).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([testGameInitializationFlow/1]).
-include("records.hrl").

all() -> [testGameInitializationFlow].

init_per_testcase(testGameInitializationFlow, Config) ->
	cardgames_sup:start_link(),
    Config.

end_per_testcase(testGameInitializationFlow, Config) ->
    ok.

testGameInitializationFlow(_Config) ->
    {ok, Player1} = players_sup:create_player(),
    {ok, Player2} = players_sup:create_player(),
    {ok, Player3} = players_sup:create_player(),
    {ok, Player4} = players_sup:create_player(),
    {ok, Table} = player:create_table(Player1),
    [TableId] = tables_sup:list_tables(),
    ok = player:join_table(Player2, TableId),
    ok = player:join_table(Player3, TableId),
    ok = player:join_table(Player4, TableId),
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

    % every one play their hand
    % preflop
    player:take_turn(FirstActor#seat.player, call),
    player:take_turn(Dealer#seat.player, call),
    player:take_turn(SB#seat.player, call),
    player:take_turn(BB#seat.player, check),
    
    % flop
    player:take_turn(FirstActor#seat.player, check),
    player:take_turn(Dealer#seat.player, check),
    player:take_turn(SB#seat.player, check),
    player:take_turn(BB#seat.player, check),

    % turn
    player:take_turn(FirstActor#seat.player, check),
    player:take_turn(Dealer#seat.player, check),
    player:take_turn(SB#seat.player, check),
    player:take_turn(BB#seat.player, check),

    % river
    player:take_turn(FirstActor#seat.player, check),
    player:take_turn(Dealer#seat.player, check),
    player:take_turn(SB#seat.player, check),
    player:take_turn(BB#seat.player, check).



