-module(hand).
-export([get_hand/1, get_royal_flush/1, get_straight_flush/1, get_straight/1,
         get_flush/1, get_four_of_a_kind/1, get_full_house/1, get_three_of_a_kind/1,
         get_two_pair/1, get_one_pair/1, get_high_card/1, get_highest_hand/1, 
         choose/2, is_higher_hand/2, str_to_card/1, strs_to_cards/1, cards_to_strs/1]).

-include("records.hrl").

get_highest_hand(Cards) ->
    SortedCards = sort_cards(Cards),
    FiveCardsCombos = choose(SortedCards, 5),
    lists:foldl(
        fun(FiveCards, {HiHand,HiCards}) ->
                Hand = get_hand(FiveCards),
                case is_higher_hand(Hand, HiHand) of
                    true -> {Hand, FiveCards};
                    false -> {HiHand,HiCards}
                end
        end, {undefined,undefined}, FiveCardsCombos).

choose(Items, N) when length(Items) < N -> [];
choose(Items, N) when length(Items) == N -> [Items];
choose(_, 0) -> [[]];
choose([H|T], N)-> choose(T, N) ++ [[H|R] || R <- choose(T, N-1)].

is_higher_hand(A,B) -> hand_to_vals(A) > hand_to_vals(B).

hand_to_vals(undefined) -> [0];
hand_to_vals(#hand{name=Name,rank_vals=RankVals}) -> [hand_name_to_val(Name)|RankVals].

hand_name_to_val(high_card) -> 1;
hand_name_to_val(one_pair) -> 2;
hand_name_to_val(two_pair) -> 3;
hand_name_to_val(three_of_a_kind) -> 4;
hand_name_to_val(straight) -> 5;
hand_name_to_val(flush) -> 6;
hand_name_to_val(full_house) -> 7;
hand_name_to_val(four_of_a_kind) -> 8;
hand_name_to_val(straight_flush) -> 9;
hand_name_to_val(royal_flush) -> 10.

get_hand(Cards) ->
    HandGetters = [
        fun ?MODULE:get_royal_flush/1, 
        fun ?MODULE:get_straight_flush/1, 
        fun ?MODULE:get_four_of_a_kind/1, 
        fun ?MODULE:get_full_house/1, 
        fun ?MODULE:get_flush/1, 
        fun ?MODULE:get_straight/1, 
        fun ?MODULE:get_three_of_a_kind/1, 
        fun ?MODULE:get_two_pair/1, 
        fun ?MODULE:get_one_pair/1, 
        fun ?MODULE:get_high_card/1],

    lists:foldl(
        fun(HandGetter, undefined) -> HandGetter(Cards);
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

str_to_card([RankChar, SuitChar]) -> 
    #card{rank=char_to_rank(RankChar), suit=char_to_suit(SuitChar)}.
strs_to_cards(Strs) -> [str_to_card(S) || S <- Strs].

char_to_rank($2) -> two;
char_to_rank($3) -> three;
char_to_rank($4) -> four;
char_to_rank($5) -> five;
char_to_rank($6) -> six;
char_to_rank($7) -> seven;
char_to_rank($8) -> eight;
char_to_rank($9) -> nine;
char_to_rank($T) -> ten;
char_to_rank($J) -> jack;
char_to_rank($Q) -> queen;
char_to_rank($K) -> king;
char_to_rank($A) -> ace.

char_to_suit($H) -> heart;
char_to_suit($S) -> spade;
char_to_suit($D) -> diamond;
char_to_suit($C) -> club.

card_to_str(#card{rank=Rank, suit=Suit}) ->
    [rank_to_char(Rank), suit_to_char(Suit)].
cards_to_strs(Cards) -> [card_to_str(C) || C <- Cards].

rank_to_char(two) -> $2;
rank_to_char(three) -> $3;
rank_to_char(four) -> $4;
rank_to_char(five) -> $5;
rank_to_char(six) -> $6;
rank_to_char(seven) -> $7;
rank_to_char(eight) -> $8;
rank_to_char(nine) -> $9;
rank_to_char(ten) -> $T;
rank_to_char(jack) -> $J;
rank_to_char(queen) -> $Q;
rank_to_char(king) -> $K;
rank_to_char(ace) -> $A.

suit_to_char(heart) -> $H;
suit_to_char(spade) -> $S;
suit_to_char(diamond) -> $D;
suit_to_char(club) -> $C.

