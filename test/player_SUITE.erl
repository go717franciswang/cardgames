-module(player_SUITE).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test1/1]).

all() -> [test1].

init_per_testcase(test1, Config) ->
	cardgames_sup:start_link(),
    Config.

end_per_testcase(test1, Config) ->
    ok.

test1(_Config) ->
    {ok, _Player} = players_sup:create_player().
