-record(card, {suit, rank}).

% position: 1|2|3|...
% player: player_pid or player_id
% money: amount of money a player have on the table
% bet: amount the player have commited before going into the pot,
%      okay if bet > money, in which case the player is going all-in
% last_action: action the player have taken in the current round,
%      reset to undefined after each round unless it's folded
%      reset to undefined after each game
% cards: the 2 face-down cards a player have
-record(seat, {position, player, money=0, bet=0, last_action, cards=[]}).

% name: royal_flush|straight_flush|...
% rank_vals: int value of each card used for comparing hands of the same name
%      so the ordering matters. for example, in three_of_a_kind, the 3 cards
%      appear first, then the rest ordered by value e.g. [3,3,3,10,9]
-record(hand, {name, rank_vals=[]}).

% id: used to identify the player (e.g. seat.position)
-record(pot, {money=0, eligible_ids=[]}).
