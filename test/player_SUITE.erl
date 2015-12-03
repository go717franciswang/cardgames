-module(player_SUITE).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([testGameInitializationFlow/1]).

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
    {ok, _Table} = player:create_table(Player1),
    [TableId] = tables_sup:list_tables(),
    player:join_table(Player2, TableId),
    player:join_table(Player3, TableId),
    player:join_table(Player4, TableId),
    player:start_game(Player1).





