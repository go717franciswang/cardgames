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
    case get_ranks(Cards) of
        [two,three,four,five,ace] -> #hand{name=straight, rank_vals=[5,4,3,2,1]};
        [two,three,four,five,six] -> #hand{name=straight, rank_vals=[6,5,4,3,2]};
        [three,four,five,six,seven] -> #hand{name=straight, rank_vals=[7,6,5,4,3]};
        [four,five,six,seven,eight] -> #hand{name=straight, rank_vals=[8,7,6,5,4]};
        [five,six,seven,eight,nine] -> #hand{name=straight, rank_vals=[9,8,7,6,5]};
        [six,seven,eight,nine,ten] -> #hand{name=straight, rank_vals=[10,9,8,7,6]};
        [seven,eight,nine,ten,jack] -> #hand{name=straight, rank_vals=[11,10,9,8,7]};
        [eight,nine,ten,jack,queen] -> #hand{name=straight, rank_vals=[12,11,10,9,8]};
        [nine,ten,jack,queen,king] -> #hand{name=straight, rank_vals=[13,12,11,10,9]};
        [ten,jack,queen,king,ace] -> #hand{name=straight, rank_vals=[14,13,12,11,10]};
        _ -> undefined
    end.

get_flush(Cards) ->
    case get_suits(Cards) of
        [S,S,S,S,S] -> 
            #hand{name=flush, 
                rank_vals=lists:reverse(lists:sort(get_rank_vals(Cards)))};
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
