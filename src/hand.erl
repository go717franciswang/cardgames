-module(hand).
-export([highest_hand/1, get_royal_flush/1, get_straight_flush/1, get_straight/1,
         get_flush/1]).

-include("records.hrl").

highest_hand(_Cards) ->
    royal_flush.

get_royal_flush(Cards) ->
    case Cards of
        [#card{rank=ten,suit=S}, #card{rank=jack,suit=S}, 
         #card{rank=queen,suit=S}, #card{rank=king,suit=S}, 
         #card{rank=ace,suit=S}] -> #hand{name=royal_flush, rank_vals=[14,13,12,11,10]};
        _ -> undefined
    end.

get_straight_flush(Cards) ->
    case {get_straight(Cards), get_flush(Cards)} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {Hand, _} -> Hand#hand{name=straight_flush}
    end.

get_straight(Cards) ->
    RankVals = get_rank_vals(Cards),
    [Min|_] = RankVals,
    StraightSeq = lists:seq(Min,Min+4),
    case RankVals of
        [2,3,4,5,14] -> #hand{name=straight, rank_vals=[5,4,3,2,1]};
        StraightSeq -> #hand{name=straight, rank_vals=lists:reverse(RankVals)};
        _ -> undefined
    end.

get_flush(Cards) ->
    case get_suits(Cards) of
        [S,S,S,S,S] -> 
            #hand{name=flush, 
                rank_vals=lists:reverse(get_rank_vals(Cards))};
        _ -> undefined
    end.

get_ranks(Cards) -> lists:map(fun(#card{rank=R}) -> R end, Cards).
get_suits(Cards) -> lists:map(fun(#card{suit=S}) -> S end, Cards).
get_rank_vals(Cards) ->
    lists:map(fun(two) -> 2;
                 (three) -> 3;
                 (four) -> 4;
                 (five) -> 5;
                 (six) -> 6;
                 (seven) -> 7;
                 (eight) -> 8;
                 (nine) -> 9;
                 (ten) -> 10;
                 (jack) -> 11;
                 (queen) -> 12;
                 (king) -> 13;
                 (ace) -> 14
              end, get_ranks(Cards)).
