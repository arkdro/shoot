-module(shoot_field).

-behaviour(gen_server).

-export([
         start_link/0
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          storage
         }).

-record(gamer, {
          pid :: pid(),
          x :: non_neg_integer(),
          y :: non_neg_integer(),
          status = alive :: dead | alive | z,
          kills = 0 :: non_neg_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    random:seed(now()),
    State = init_state(),
    {ok, State}.

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

init_state() ->
    Storage = prepare_and_fill_storage(Ngamers, Width, Height),
    #state{
       storage = Storage,
       width = Width,
       height = Height,
       n_gamers = Ngamers
      }.

prepare_and_fill_storage(Ngamers, Width, Height) ->
    Storage = prepare_storage(),
    prepare_gamers(Storage, Ngamers, Width, Height).

prepare_gamers(Storage, Ngamers, Width, Height) ->
    F = fun(_, Acc) ->
                {_, Acc2} = prepare_one_gamer(Acc, Width, Height),
                Acc2
        end,
    lists:foldl(F, Storage, lists:seq(1, Ngamers)).

prepare_one_gamer(Storage, Width, Height) ->
    Args = [{x, X},
            {y, Y}],
    {ok, Pid} = shoot_gamer_sup:start_child(Args),
    Storage2 = add_new_gamer(Storage, Pid, X, Y),
    {Pid, Storage2}.

add_new_gamer(Storage, Pid, X, Y) ->
    Gamer = #gamer{
               pid = Pid,
               x = X,
               y = Y
              },
    store_gamer(Storage, Gamer).


prepare_storage() ->
    ets:new(?SERVER, [named_table, {keypos, #gamer.pid}]).

store_gamer(Storage, Gamer) ->
    ets:insert(Storage, Gamer),
    Storage.

