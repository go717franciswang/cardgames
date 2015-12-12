-module(hand).
-export([get_hand/1, get_royal_flush/1, get_straight_flush/1, get_straight/1,
         get_flush/1, get_four_of_a_kind/1, get_full_house/1, get_three_of_a_kind/1,
         get_two_pair/1, get_one_pair/1, get_high_card/1]).

-include("records.hrl").

get_hand(Cards) ->
    HandGetters = [
        fun ?MODULE:get_royal_flush/1, 
        fun ?MODULE:get_four_of_a_kind/1, 
        fun ?MODULE:get_full_house/1, 
        fun ?MODULE:get_straight/1, 
        fun ?MODULE:get_three_of_a_kind/1, 
        fun ?MODULE:get_two_pair/1, 
        fun ?MODULE:get_one_pair/1, 
        fun ?MODULE:get_high_card/1],

    SortedCards = sort_cards(Cards),
    lists:foldl(
        fun(HandGetter, undefined) -> HandGetter(SortedCards);
           (_, Hand) when Hand /= undefined -> Hand
        end, undefined, HandGetters).

sort_cards(Cards) ->
    [X || {_,X} <- lists:sort(lists:zip(get_rank_vals(Cards), Cards))].

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

get_four_of_a_kind(Cards) ->
    RankVals = get_rank_vals(Cards),
    case RankVals of
        [N,N,N,N,M] -> #hand{name=four_of_a_kind, rank_vals=[N,N,N,N,M]};
        [M,N,N,N,N] -> #hand{name=four_of_a_kind, rank_vals=[N,N,N,N,M]};
        _ -> undefined
    end.

get_full_house(Cards) ->
    RankVals = get_rank_vals(Cards),
    case RankVals of
        [N,N,N,M,M] -> #hand{name=full_house, rank_vals=[N,N,N,M,M]};
        [M,M,N,N,N] -> #hand{name=full_house, rank_vals=[N,N,N,M,M]};
        _ -> undefined
    end.

get_three_of_a_kind(Cards) ->
    RankVals = get_rank_vals(Cards),
    case RankVals of
        [A,A,A,B,C] -> #hand{name=three_of_a_kind, rank_vals=[A,A,A,C,B]};
        [B,A,A,A,C] -> #hand{name=three_of_a_kind, rank_vals=[A,A,A,C,B]};
        [B,C,A,A,A] -> #hand{name=three_of_a_kind, rank_vals=[A,A,A,C,B]};
        _ -> undefined
    end.

get_two_pair(Cards) ->
    RankVals = get_rank_vals(Cards),
    case RankVals of
        [A,A,B,B,C] -> #hand{name=two_pair, rank_vals=[B,B,A,A,C]};
        [A,A,C,B,B] -> #hand{name=two_pair, rank_vals=[B,B,A,A,C]};
        [C,A,A,B,B] -> #hand{name=two_pair, rank_vals=[B,B,A,A,C]};
        _ -> undefined
    end.

get_one_pair(Cards) ->
    RankVals = get_rank_vals(Cards),
    case RankVals of
        [A,A,B,C,D] -> #hand{name=one_pair, rank_vals=[A,A,D,C,B]};
        [B,A,A,C,D] -> #hand{name=one_pair, rank_vals=[A,A,D,C,B]};
        [B,C,A,A,D] -> #hand{name=one_pair, rank_vals=[A,A,D,C,B]};
        [B,C,D,A,A] -> #hand{name=one_pair, rank_vals=[A,A,D,C,B]};
        _ -> undefined
    end.

get_high_card(Cards) ->
    #hand{name=high_card, rank_vals=lists:reverse(get_rank_vals(Cards))}.

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
