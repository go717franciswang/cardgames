-module(deck_test).
-include_lib("eunit/include/eunit.hrl").

show_deck_test() ->
    {ok, Deck} = deck:start_link(),
    Cards = deck:show_deck(Deck),
    ?assertEqual(length(Cards), 52).

shuffle_test() ->
    {ok, Deck} = deck:start_link(),
    Cards = deck:show_deck(Deck),
    deck:shuffle(Deck),
    SuffledCards = deck:show_deck(Deck),
    ?assertNotEqual(Cards, SuffledCards).





