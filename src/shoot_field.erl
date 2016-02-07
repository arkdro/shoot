-module(shoot_field).

-behaviour(gen_server).

-export([
         move/3,
         new_player/0,
         start_link/0
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          storage,
          width,
          height,
          n_alive,
          n_gamers
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

move(Player, X, Y) ->
    gen_server:call(?SERVER, {move, Player, X, Y}).

new_player() ->
    gen_server:call(?SERVER, new_player).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    random:seed(now()),
    State = init_state(),
    {ok, State}.

handle_call(new_player, _From, State) ->
    {Reply, State2} = add_player(State),
    {reply, Reply, State2};
handle_call({move, Player, X, Y}, _From, State) ->
    {Reply, State2} = handle_move(Player, X, Y, State),
    {reply, Reply, State2};
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
    Ngamers = shoot_util:get_env(n_gamers, 1),
    Width = shoot_util:get_env(width, 1),
    Height = shoot_util:get_env(height, 1),
    Storage = prepare_and_fill_storage(Ngamers, Width, Height),
    #state{
       storage = Storage,
       width = Width,
       height = Height,
       n_gamers = Ngamers,
       n_alive = Ngamers
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
    X = gen_random(Width),
    Y = gen_random(Height),
    Args = [{x, X},
            {y, Y}],
    {ok, Pid} = shoot_gamer_sup:start_child(Args),
    Storage2 = add_new_gamer(Storage, Pid, X, Y),
    {Pid, Storage2}.

gen_random(Max) ->
    random:uniform(Max).

add_new_gamer(Storage, Pid, X, Y) ->
    Gamer = #gamer{
               pid = Pid,
               x = X,
               y = Y
              },
    store_gamer(Storage, Gamer).

handle_move(Player, X, Y, State) ->
    case is_player_alive(Player, State) of
        true ->
            move(Player, X, Y, State);
        false ->
            {{error, dead}, State}
    end.

is_player_alive(Player, #state{storage = Storage}) ->
    case fetch_player_info(Storage, Player) of
        #gamer{status = alive} ->
            true;
        _ ->
            false
    end.

fetch_player_info(Storage, Player) ->
    case ets:lookup(Storage, Player) of
        [Info] ->
            Info;
        [] ->
            undefined
    end.

move(Player, X, Y, State) ->
    case shoot_gamer:move(Player) of
        ok ->
            State2 = update_coordinates(Player, X, Y, State),
            New = get_coordinates(Player, State),
            shoot_util:log_move(Player, {X, Y}, New),
            {ok, State2};
        {error, _} = Error ->
            {Error, State}
    end.

update_coordinates(Player, X, Y, #state{storage = Storage,
                                        width = Width,
                                        height = Height} = State) ->
    Gamer = fetch_player_info(Storage, Player),
    #gamer{x = X0, y = Y0} = Gamer,
    {X2, Y2} = calc_new_coordinates(X0, Y0, X, Y, Width, Height),
    Gamer2 = Gamer#gamer{
               x = X2,
               y = Y2
              },
    Storage2 = store_gamer(Storage, Gamer2),
    State#state{storage = Storage2}.

calc_new_coordinates(X0, Y0, X, Y, Width, Height) ->
    X2 = calc_one_coord(X0, X, Width),
    Y2 = calc_one_coord(Y0, Y, Height),
    {X2, Y2}.

calc_one_coord(V0, V, _) when V0 + V < 1 ->
    1;
calc_one_coord(V0, V, Max) when V0 + V > Max ->
    Max;
calc_one_coord(V0, V, _) ->
    V0 + V.

get_coordinates(Player, #state{storage = Storage}) ->
    #gamer{x = X, y = Y} = fetch_player_info(Storage, Player),
    {X, Y}.

add_player(State) ->
    {Pid, Storage2} = prepare_one_gamer(Storage, Width, Height),
    State2 = State#state{
               storage = Storage2,
               n_gamers = N + 1
              },
    Reply = {ok, Pid},
    {Reply, State2}.

prepare_storage() ->
    ets:new(?SERVER, [named_table, {keypos, #gamer.pid}]).

store_gamer(Storage, Gamer) ->
    ets:insert(Storage, Gamer),
    Storage.

