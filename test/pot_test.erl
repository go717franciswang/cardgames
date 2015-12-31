-module(pot_test).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

build_pots1_test() ->
    Pots = pot:build_pots([{5.0,a}, {10.0,b}, {15.0,c}, {15.0,d}]),
    ?assertEqual([#pot{money=20.0,eligible_ids=[a,b,c,d]},
                  #pot{money=15.0,eligible_ids=[b,c,d]},
                  #pot{money=10.0,eligible_ids=[c,d]}], Pots).

build_pots2_test() ->
    Pots = pot:build_pots([{5.0,a}, {10.0,b}, {15.0,c}]),
    ?assertEqual([#pot{money=15.0,eligible_ids=[a,b,c]},
                  #pot{money=10.0,eligible_ids=[b,c]},
                  #pot{money=5.0,eligible_ids=[c]}], Pots).

merge_pots_test() ->
    Pots = pot:merge_pots([#pot{money=15.0,eligible_ids=[a,b,c]},
                           #pot{money=10.0,eligible_ids=[b,c]},
                           #pot{money=5.0,eligible_ids=[c]}],
                          [#pot{money=3.0,eligible_ids=[b,c]},
                           #pot{money=2.0,eligible_ids=[c]}]),
    ?assertEqual([#pot{money=15.0,eligible_ids=[a,b,c]},
                  #pot{money=13.0,eligible_ids=[b,c]},
                  #pot{money=7.0,eligible_ids=[c]}], Pots).

split_single_player_pots_test() ->
    {SP, MP} = pot:split_single_player_pots([
            #pot{money=15.0,eligible_ids=[a,b,c]},
            #pot{money=13.0,eligible_ids=[b,c]},
            #pot{money=7.0,eligible_ids=[c]}]),
    ?assertEqual({[#pot{money=7.0,eligible_ids=[c]}],
                  [#pot{money=15.0,eligible_ids=[a,b,c]},
                   #pot{money=13.0,eligible_ids=[b,c]}]}, {SP,MP}).
            
