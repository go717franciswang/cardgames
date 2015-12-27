-module(util).
-export([card_to_serializable/1, 
        game_state_to_serializable/1, 
        seat_to_serializable/1,
        pot_to_serializable/1
    ]).
-include("records.hrl").

card_to_serializable(unknown) -> unknown;
card_to_serializable(#card{suit=S, rank=R}) ->
    #{suit=>S, rank=>R}.

game_state_to_serializable(#game_state{seats=SS, pots=PS, community_cards=CC, dealer_button_pos=D}) ->
    #{seats => [seat_to_serializable(S) || S <- SS],
        pots => [pot_to_serializable(P) || P <- PS],
        community_cards => [card_to_serializable(C) || C <- CC],
        dealer_button_pos => D}.

seat_to_serializable(#seat{position=Pos, player=undefined}) -> #{position=>Pos};
seat_to_serializable(#seat{position=Pos, player=P, money=M, bet=B, last_action=L, cards=CS}) ->
    #{position => Pos,
        player => pid_to_list(P),
        money => M,
        bet => B,
        last_action => L,
        cards => [card_to_serializable(C) || C <- CS]}.

pot_to_serializable(#pot{money=M, eligible_ids=ES}) ->
    #{money => M, eligible_ids => ES}.

