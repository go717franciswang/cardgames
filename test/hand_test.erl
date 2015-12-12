-module(hand_test).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

get_royal_flush_test() ->
    Hand = hand:get_royal_flush([
            #card{rank=ten,suit=heart},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(royal_flush, Hand#hand.name),

    NoHand = hand:get_royal_flush([
            #card{rank=ten,suit=spade},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(undefined, NoHand).

get_straight_flush_test() ->
    Hand = hand:get_straight_flush([
            #card{rank=ten,suit=heart},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(#hand{name=straight_flush, rank_vals=[14,13,12,11,10]}, Hand),

    NoHand = hand:get_straight_flush([
            #card{rank=ten,suit=spade},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(undefined, NoHand).

get_straight_test() ->
    Hand1 = hand:get_straight([
            #card{rank=ten,suit=heart},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(#hand{name=straight, rank_vals=[14,13,12,11,10]}, Hand1),

    Hand2 = hand:get_straight([
            #card{rank=two,suit=heart},
            #card{rank=three,suit=heart},
            #card{rank=four,suit=heart},
            #card{rank=five,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(#hand{name=straight, rank_vals=[5,4,3,2,1]}, Hand2),

    NoHand = hand:get_straight([
            #card{rank=jack,suit=heart},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(undefined, NoHand).

get_flush_test() ->
    Hand = hand:get_flush([
            #card{rank=ten,suit=heart},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(#hand{name=flush, rank_vals=[14,13,12,11,10]}, Hand),

    NoHand = hand:get_flush([
            #card{rank=ten,suit=spade},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(undefined, NoHand).

get_four_of_a_kind_test() ->
    Hand = hand:get_four_of_a_kind([
            #card{rank=four,suit=heart},
            #card{rank=four,suit=spade},
            #card{rank=four,suit=club},
            #card{rank=four,suit=diamond},
            #card{rank=five,suit=heart}]),
    ?assertEqual(#hand{name=four_of_a_kind, rank_vals=[4,4,4,4,5]}, Hand),

    NoHand = hand:get_four_of_a_kind([
            #card{rank=ten,suit=spade},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(undefined, NoHand).

get_full_house_test() ->
    Hand = hand:get_full_house([
            #card{rank=four,suit=heart},
            #card{rank=four,suit=spade},
            #card{rank=four,suit=club},
            #card{rank=five,suit=diamond},
            #card{rank=five,suit=heart}]),
    ?assertEqual(#hand{name=full_house, rank_vals=[4,4,4,5,5]}, Hand),

    NoHand = hand:get_full_house([
            #card{rank=ten,suit=spade},
            #card{rank=jack,suit=heart},
            #card{rank=queen,suit=heart},
            #card{rank=king,suit=heart},
            #card{rank=ace,suit=heart}]),
    ?assertEqual(undefined, NoHand).

get_three_of_a_kind_test() ->
    Hand = hand:get_three_of_a_kind([
            #card{rank=four,suit=heart},
            #card{rank=four,suit=spade},
            #card{rank=four,suit=club},
            #card{rank=five,suit=diamond},
            #card{rank=six,suit=heart}]),
    ?assertEqual(#hand{name=three_of_a_kind, rank_vals=[4,4,4,6,5]}, Hand).

get_two_pair_test() ->
    Hand = hand:get_two_pair([
            #card{rank=four,suit=heart},
            #card{rank=four,suit=spade},
            #card{rank=five,suit=club},
            #card{rank=five,suit=diamond},
            #card{rank=six,suit=heart}]),
    ?assertEqual(#hand{name=two_pair, rank_vals=[5,5,4,4,6]}, Hand).

get_one_pair_test() ->
    Hand = hand:get_one_pair([
            #card{rank=four,suit=heart},
            #card{rank=four,suit=spade},
            #card{rank=five,suit=club},
            #card{rank=six,suit=diamond},
            #card{rank=seven,suit=heart}]),
    ?assertEqual(#hand{name=one_pair, rank_vals=[4,4,7,6,5]}, Hand).
