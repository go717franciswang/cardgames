-module(player).
-behaviour(gen_fsm).

%% API.
-export([start_link/1, create_table/1, join_table/2, start_game/1, show_cards/1, 
        take_turn/2, sit/1, leave/1, add_event_handler/3, notify/2, 
        show_game_state/1]).

%% gen_fsm.
-export([init/1]).
-export([lobby/3, in_game/3]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {game, em, nickname
}).

%% API.

-spec start_link(atom()|binary()) -> {ok, pid()}.
start_link(NickName) ->
	gen_fsm:start_link(?MODULE, [NickName], []).

create_table(Pid) -> gen_fsm:sync_send_event(Pid, create_table).
join_table(Pid, TableId) -> gen_fsm:sync_send_event(Pid, {join_table, TableId}).
sit(Pid) -> gen_fsm:sync_send_event(Pid, sit).
start_game(Pid) -> gen_fsm:sync_send_event(Pid, start_game).
show_cards(Pid) -> gen_fsm:sync_send_event(Pid, show_cards).
take_turn(Pid, Action) -> gen_fsm:sync_send_event(Pid, {take_turn, Action}).
leave(Pid) -> gen_fsm:sync_send_event(Pid, leave).
add_event_handler(Pid, Handler, Args) -> 
    gen_fsm:sync_send_all_state_event(Pid, {add_event_handler, Handler, Args}).
notify(Pid, Event) -> gen_fsm:send_all_state_event(Pid, {notify, Event}).
show_game_state(Pid) -> gen_fsm:sync_send_event(Pid, show_game_state).

%% gen_fsm.

init([NickName]) ->
    {ok, EM} = gen_event:start_link(),
	{ok, lobby, #state{em=EM, nickname=NickName}}.

lobby(create_table, _From, StateData) ->
    {ok, Pid} = tables_sup:create_table(),
    holdem:join(Pid, self()),
	{reply, {ok, Pid}, in_game, StateData#state{game=Pid}};
lobby({join_table, TableId}, _From, StateData) ->
    Pid = tables_sup:id_to_pid(TableId),
    Reply = holdem:join(Pid, self()),
	{reply, Reply, in_game, StateData#state{game=Pid}}.

in_game(sit, _From, StateData) ->
    Reply = holdem:sit(StateData#state.game, self()),
    {reply, Reply, in_game, StateData};
in_game(leave, _From, StateData) ->
    % try to fold first, in case it is this player turn right now
    holdem:take_turn(StateData#state.game, fold),
    Reply = holdem:leave(StateData#state.game, self()),
    {reply, Reply, lobby, StateData#state{game=undefined}};
in_game(show_cards, _From, StateData) ->
    Cards = holdem:show_cards(StateData#state.game, self()),
    {reply, Cards, in_game, StateData};
in_game(start_game, _From, StateData) ->
    Reply = holdem:start_game(StateData#state.game),
    {reply, Reply, in_game, StateData};
in_game({take_turn, Action}, _From, StateData) ->
    Reply = holdem:take_turn(StateData#state.game, Action),
    {reply, Reply, in_game, StateData};
in_game(show_game_state, _From, StateData) ->
    Reply = holdem:show_game_state(StateData#state.game),
    {reply, Reply, in_game, StateData}.

handle_event({notify, Event}, StateName, StateData) ->
    io:format("~p got notification: ~p~n", [self(), Event]),
    EM = StateData#state.em,
    gen_event:notify(EM, Event),
    {next_state, StateName, StateData};
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

handle_sync_event({add_event_handler, Handler, Args}, _From, StateName, StateData) ->
    EM = StateData#state.em,
    Reply = gen_event:add_sup_handler(EM, Handler, Args),
    {reply, Reply, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
	{reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.
