-module(shoot_field).

-behaviour(gen_server).

-export([
         move/3,
         shoot/3,
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

move(_, 0, 0) ->
    error(badarg);
move(Player, X, Y) when (X =:= -1 orelse X =:= 0 orelse X =:= 1) andalso
                        (Y =:= -1 orelse Y =:= 0 orelse Y =:= 1) ->
    gen_server:call(?SERVER, {move, Player, X, Y}).

shoot(_, 0, 0) ->
    error(badarg);
shoot(Player, X, Y) when (X =:= -1 orelse X =:= 0 orelse X =:= 1) andalso
                         (Y =:= -1 orelse Y =:= 0 orelse Y =:= 1) ->
    gen_server:call(?SERVER, {shoot, Player, X, Y}).

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
handle_call({shoot, Player, X, Y}, _From, State) ->
    {Reply, State2} = handle_shoot(Player, X, Y, State),
    case is_end(State2) of
        true ->
            print_report(State2),
            {stop, normal, Reply, State2};
        false ->
            {reply, Reply, State2}
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

calc_new_raw_coordinates(X, Y, Dx, Dy) ->
    X2 = X + Dx,
    Y2 = Y + Dy,
    {X2, Y2}.

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
    get_coordinates(Player, Storage);
get_coordinates(Player, Storage) ->
    #gamer{x = X, y = Y} = fetch_player_info(Storage, Player),
    {X, Y}.

handle_shoot(Player, X, Y, State) ->
    case is_player_alive(Player, State) of
        true ->
            shoot(Player, X, Y, State);
        false ->
            {{error, dead}, State}
    end.

shoot(Player, X, Y, State) ->
    case shoot_gamer:shoot(Player) of
        ok ->
            State2 = update_target(Player, X, Y, State),
            Current = get_coordinates(Player, State),
            shoot_util:log_shoot(Player, {X, Y}, Current),
            {ok, State2};
        {error, _} = Error ->
            {Error, State}
    end.

add_player(#state{n_gamers = Total, n_alive = Alive, width = Width,
                 height = Height, storage = Storage} = State) ->
    {Pid, Storage2} = prepare_one_gamer(Storage, Width, Height),
    State2 = State#state{
               storage = Storage2,
               n_gamers = Total + 1,
               n_alive = Alive + 1
              },
    Reply = {ok, Pid},
    {Reply, State2}.

prepare_storage() ->
    ets:new(?SERVER, [named_table, {keypos, #gamer.pid}]).

store_gamer(Storage, Gamer) ->
    ets:insert(Storage, Gamer),
    Storage.

is_end(#state{n_alive = N}) ->
    N =< 1.

print_report(#state{storage = Storage}) ->
    Winner = find_winner(Storage),
    io:format("winner:~n~p~n", [Winner]),
    Data = prepare_data(Storage, Winner),
    io:format("rest:~n~p~n", [Data]).

find_winner(Storage) ->
    Pattern = #gamer{status = alive, _ = '_'},
    [Winner] = ets:match_object(Storage, Pattern),
    Winner.

prepare_data(Storage, Winner) ->
    L = ets:tab2list(Storage),
    lists:delete(Winner, L).

update_target(Player, X, Y, #state{width = Width, height = Height,
                                   storage = Storage} = State) ->
    case check_one_point(X, Y, Width, Height, Player, Storage) of
        {miss, _} ->
            State;
        {hit, Storage2} ->
            State#state{storage = Storage2}
    end.

check_one_point(Dx, Dy, Width, Height, Player, Storage) ->
    {X, Y} = get_coordinates(Player, Storage),
    Flag = has_space(X, Y, Dx, Dy, Width, Height),
    check_one_point(Flag, X, Y, Dx, Dy, Width, Height, Player, Storage).

check_one_point(false, _, _, _, _, _, _, _, Storage) ->
    {miss, Storage};
check_one_point(true, X, Y, Dx, Dy, Width, Height, Player, Storage) ->
    {X2, Y2} = calc_new_coordinates(X, Y, Dx, Dy, Width, Height),
    case find_target_at_point(X2, Y2, Storage) of
        {ok, Targets} ->
            Storage2 = kill(Targets, Player, Storage),
            {hit, Storage2};
        error ->
            Flag = has_space(X2, Y2, Dx, Dy, Width, Height),
            check_one_point(Flag, X2, Y2, Dx, Dy, Width, Height,
                            Player, Storage)
    end.

has_space(X, Y, Dx, Dy, Width, Height) ->
    {X2, Y2} = calc_new_raw_coordinates(X, Y, Dx, Dy),
    X2 >= 1
        andalso X2 =< Width
        andalso Y2 >= 1
        andalso Y2 =< Height.

find_target_at_point(X, Y, Storage) ->
    Pattern = #gamer{x = X, y = Y},
    case ets:match_object(Storage, Pattern) of
        [] ->
            error;
        L ->
            Pids = [Pid || #gamer{pid = Pid} <- L],
            {ok, Pids}
    end.

kill(Targets, Player, Storage) ->
    Storage2 = increment_kills(Targets, Player, Storage),
    shoot_gamer_sup:stop_children(Targets),
    mark_dead(Targets, Storage2).

increment_kills(Targets, Player, Storage) ->
    Info = fetch_player_info(Storage, Player),
    #gamer{kills = N} = Info,
    Delta = length(Targets),
    Info2 = Info#gamer{kills = N + Delta},
    store_gamer(Storage, Info2).

mark_dead(Targets, Storage) ->
    lists:foldl(fun mark_one_dead/2, Storage, Targets).

mark_one_dead(Target, Storage) ->
    Info = fetch_player_info(Storage, Target),
    Info2 = Info#gamer{status = dead},
    store_gamer(Storage, Info2).

