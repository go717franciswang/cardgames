-module(pot_test).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

build_pots1_test() ->
    Pots = pot:build_pots([{5,a}, {10,b}, {15,c}, {15,d}]),
    ?assertEqual([#pot{money=20,eligible_ids=[a,b,c,d]},
                  #pot{money=15,eligible_ids=[b,c,d]},
                  #pot{money=10,eligible_ids=[c,d]}], Pots).

build_pots2_test() ->
    Pots = pot:build_pots([{5,a}, {10,b}, {15,c}]),
    ?assertEqual([#pot{money=15,eligible_ids=[a,b,c]},
                  #pot{money=10,eligible_ids=[b,c]},
                  #pot{money=5,eligible_ids=[c]}], Pots).

% merge_pots_test() ->
%     Pots = pot:merge_pots([#pot{money=15,eligible_ids=[a,b,c]},
%                            #pot{money=10,eligible_ids=[b,c]},
%                            #pot{money=5,eligible_ids=[c]}],
%                           [#pot{money=3,eligible_ids=[b,c]},
%                            #pot{money=2,eligible_ids=[c]}]),
%     ?assertEqual([#pot{money=15,eligible_ids=[a,b,c]},
%                   #pot{money=13,eligible_ids=[b,c]},
%                   #pot{money=7,eligible_ids=[c]}], Pots).
