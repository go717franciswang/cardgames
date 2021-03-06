-module(pot).
-export([build_pots/1, merge_pots/2, split_single_player_pots/1, round_money/1,
    remove_player/2]).
-include("records.hrl").

build_pots([]) -> [];
build_pots(MoneyIdTuples) ->
    Sorted = lists:sort(fun({MoneyA,_},{MoneyB,_}) -> MoneyA =< MoneyB end, MoneyIdTuples),
    [{MoneyPerPerson,_}|_] = Sorted,
    build_pots(Sorted, MoneyPerPerson).

build_pots([], _) -> [];
build_pots(Sorted, MoneyPerPerson) ->
    Ids = [Id || {_,Id} <- Sorted],
    NewSorted = [{round_money(Money-MoneyPerPerson),Id} || {Money,Id} <- Sorted, 
        round_money(Money-MoneyPerPerson) > 0],
    Pot = #pot{money=round_money(MoneyPerPerson*length(Sorted)), eligible_ids=Ids},
    case NewSorted of
        [] -> [Pot];
        [{NewMoneyPerPerson,_}|_] -> [Pot|build_pots(NewSorted, NewMoneyPerPerson)]
    end.

merge_pots(Pots1, Pots2) -> merge_pots(Pots1++Pots2).
merge_pots(Pots) ->
    lists:foldl(
        fun(#pot{money=M2,eligible_ids=Ids}=P, Ps) ->
                case lists:keyfind(Ids, #pot.eligible_ids, Ps) of
                    false -> lists:keystore(Ids, #pot.eligible_ids, Ps, P);
                    #pot{money=M1} -> 
                        lists:keystore(Ids, #pot.eligible_ids, Ps, P#pot{money=round_money(M1+M2)})
                end
        end, [], Pots).

remove_player(Pots, PlayerId) ->
    NewPots = lists:map(
        fun(#pot{eligible_ids=Ids}=P) ->
                P#pot{eligible_ids=Ids--[PlayerId]}
        end, Pots),
    merge_pots(NewPots).
    
split_single_player_pots(Pots) ->
    {[P || P <- Pots, length(P#pot.eligible_ids) == 1],
     [P || P <- Pots, length(P#pot.eligible_ids) > 1]}.

round_money(Money) -> round(Money * 1000) / 1000.
