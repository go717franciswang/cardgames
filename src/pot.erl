-module(pot).
-export([build_pots/1, merge_pots/2]).
-include("records.hrl").

build_pots(MoneyIdTuples) ->
    Sorted = lists:sort(fun({MoneyA,_},{MoneyB,_}) -> MoneyA =< MoneyB end, MoneyIdTuples),
    [{MoneyPerPerson,_}|_] = Sorted,
    build_pots(Sorted, MoneyPerPerson).

build_pots([], _) -> [];
build_pots(Sorted, MoneyPerPerson) ->
    Ids = [Id || {_,Id} <- Sorted],
    NewSorted = [{Money-MoneyPerPerson,Id} || {Money,Id} <- Sorted, Money-MoneyPerPerson > 0],
    Pot = #pot{money=MoneyPerPerson*length(Sorted), eligible_ids=Ids},
    case NewSorted of
        [] -> [Pot];
        [{NewMoneyPerPerson,_}|_] -> [Pot|build_pots(NewSorted, NewMoneyPerPerson)]
    end.

merge_pots(Pots1, Pots2) ->
    lists:foldl(
        fun(#pot{money=M2,eligible_ids=Ids}=P, Pots) ->
                case lists:keyfind(Ids, #pot.eligible_ids, Pots) of
                    false -> lists:keystore(Ids, #pot.eligible_ids, Pots, P);
                    #pot{money=M1} -> lists:keystore(Ids, #pot.eligible_ids, Pots, P#pot{money=M1+M2})
                end
        end, Pots1, Pots2).
    
