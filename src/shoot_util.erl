-module(shoot_util).

-export([
        ]).

log_move(Player, X, Y) ->
    log_action(move, Player, X, Y).

log_shoot(Player, X, Y) ->
    log_action(shoot, Player, X, Y).

log_action(Action, Player, X, Y) ->
    T = get_timestamp(),
    error_logger:info_report({T, action, Action, Player, X, Y}).

get_timestamp() ->
    {_, _, Us} = T = os:timestamp(),
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_local_time(T),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B"
                                " ~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0B",
                                [Y, M, D, H, Min, S, Us])).

