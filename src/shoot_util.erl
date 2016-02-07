-module(shoot_util).

-export([
         get_env/2,
         log_move/3,
         log_shoot/3
        ]).

log_move(Player, Point1, Point2) ->
    log_action(move, {Player, Point1, Point2}).

log_shoot(Player, X, Y) ->
    log_action(shoot, {Player, X, Y}).

get_env(Key, Default) ->
    application:get_env(shoot, Key, Default).

%%%===================================================================
%%% Internal functions
%%%===================================================================

log_action(Action, Info) ->
    T = get_timestamp(),
    error_logger:info_report({T, Action, Info}).

get_timestamp() ->
    {_, _, Us} = T = os:timestamp(),
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_local_time(T),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B"
                                " ~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0B",
                                [Y, M, D, H, Min, S, Us])).

