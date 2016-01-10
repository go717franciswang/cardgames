-module(util).
-export([card_to_serializable/1, 
        game_state_to_serializable/1, 
        seat_to_serializable/1,
        pot_to_serializable/1,
        pid_to_serializable/1,
        pot_wins_to_serializable/1
    ]).
-include("records.hrl").

card_to_serializable(unknown) -> unknown;
card_to_serializable(#card{suit=S, rank=R}) ->
    #{suit=>S, rank=>R}.

game_state_to_serializable(#game_state{
        state_name=SN, seats=SS, pots=PS, community_cards=CC, 
        dealer_button_pos=D, users=US}) ->
    #{seats => [seat_to_serializable(S) || S <- SS],
        state_name => SN,
        pots => [pot_to_serializable(P) || P <- PS],
        community_cards => [card_to_serializable(C) || C <- CC],
        dealer_button_pos => D,
        users => [user_to_serializable(U) || U <- US]}.

seat_to_serializable(#seat{position=Pos, player=undefined}) -> #{position=>Pos};
seat_to_serializable(#seat{position=Pos, player=P, money=M, bet=B, last_action=L, cards=CS, is_active=A}) ->
    #{position => Pos,
        player => pid_to_serializable(P),
        money => M,
        bet => B,
        last_action => L,
        is_active => bool_to_serializable(A),
        cards => [card_to_serializable(C) || C <- CS]}.

bool_to_serializable(false) -> 0;
bool_to_serializable(_) -> 1.

pid_to_serializable(P) -> list_to_binary(pid_to_list(P)).

pot_to_serializable(#pot{money=M, eligible_ids=ES}) ->
    #{money => M, eligible_ids => ES}.

user_to_serializable(#user{player=P, nickname=N}) ->
    #{player => pid_to_serializable(P), nickname => N}.

hand_to_serializable(#hand{name=N}) -> N.

play_to_serializable(#play{player=P, hand=undefined}) ->
    #{player => pid_to_serializable(P)};
play_to_serializable(#play{player=P, hand=H, cards=CS}) ->
    #{player => pid_to_serializable(P),
        hand => hand_to_serializable(H),
        cards => [card_to_serializable(C) || C <- CS]}.

pot_wins_to_serializable(#pot_wins{pot=P, wins=WS}) ->
    #{pot => pot_to_serializable(P),
        wins => [play_to_serializable(W) || W <- WS]}.

