-module(shoot_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(ASSERT, true).

-compile([export_all]).

suite() ->
    [
     {timetrap, {seconds, 180}}
    ].

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [], [
                {group, checks}
               ]},
     {checks, [], [
                   whole_play
                  ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(whole_play, Config) ->
    ok = application:load(shoot),
    application:set_env(shoot, time_limit, time_limit()),
    application:set_env(shoot, width, width()),
    application:set_env(shoot, height, height()),
    application:set_env(shoot, n_gamers, 2),
    ok = application:start(shoot),
    Config.

end_per_testcase(whole_play, _C) ->
    application:stop(shoot),
    application:unload(shoot),
    ok.

whole_play(_) ->
    Ref = setup_monitor(),
    L = ets:tab2list(shoot_field),
    ct:pal("tab: ~p", [L]),
    [P1, P2] = prepare_positions(),
    L2 = ets:tab2list(shoot_field),
    ct:pal("tab2: ~p", [L2]),
    shoot(P1, P2),
    ok = check_result(Ref),
    1=2,
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% microseconds
time_limit() ->
    100000.

width() ->
    3.

%% exact square
height() ->
    width().

prepare_positions() ->
    [P1, P2] = ets:tab2list(shoot_field),
    move_to_1_1(P1),
    move_to_max_max(P2),
    [P1, P2].

move_to_1_1(Gamer) ->
    move_to(-1, -1, Gamer).

move_to_max_max(Gamer) ->
    move_to(1, 1, Gamer).

move_to(Dx, Dy, Gamer) ->
    Pid = shoot_field:extract_pid(Gamer),
    Nx = width() + 1,
    Ny = height() + 1,
    Nsteps = max(Nx, Ny),
    [make_one_move(Pid, Dx, Dy) || _ <- lists:seq(1, Nsteps)].

make_one_move(Pid, Dx, Dy) ->
    Res = shoot_field:move(Pid, Dx, Dy),
    timer:sleep(2 + time_limit() div 1000),
    Res.

shoot(Gamer1, Gamer2) ->
    Pid = shoot_field:extract_pid(Gamer1),
    shoot_field:shoot(Pid, 1, 1).

setup_monitor() ->
    monitor(process, shoot_field).

check_result(Ref) ->
    receive
        {'DOWN', Ref, process, _Obj, normal} ->
            ok
    after 0 ->
            {error, timeout}
    end.

