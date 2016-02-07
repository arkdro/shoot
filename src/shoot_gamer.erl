-module(shoot_gamer).

-behaviour(gen_server).

-export([
         move/1,
         shoot/1,
         start_link/1
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          time,
          limit
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

move(Player) ->
    gen_server:call(Player, move).

shoot(Player) ->
    gen_server:call(Player, shoot).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    Limit = get_time_limit(),
    T = os:timestamp(),
    State = #state{time = T,
                   limit = Limit},
    {ok, State}.

handle_call(shoot, _From, #state{time=T1} = State) ->
    case can_shoot(State) of
        true ->
            Reply = ok,
            State2 = update_time(State),
            {reply, Reply, State2};
        false ->
            Reply = {error, too_soon},
            {reply, Reply, State}
    end;
handle_call(move, _From, #state{time=T1} = State) ->
    case can_move(State) of
        true ->
            Reply = ok,
            State2 = update_time(State),
            {reply, Reply, State2};
        false ->
            Reply = {error, too_soon},
            {reply, Reply, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

can_shoot(State) ->
    can_move(State).

can_move(#state{time = T1, limit = Limit}) ->
    T2 = os:timestamp(),
    Delta = timer:now_diff(T2, T1),
    Delta >= Limit.

update_time(#state{time = T1} = State) ->
    T = os:timestamp(),
    State#state{time = T}.

get_time_limit() ->
    shoot_util:get_env(time_limit, 100000).

