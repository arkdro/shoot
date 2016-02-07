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
    application:set_env(shoot, width, 3),
    application:set_env(shoot, height, 3),
    application:set_env(shoot, n_gamers, 2),
    ok = application:start(shoot),
    Config.

end_per_testcase(whole_play, _C) ->
    application:stop(shoot),
    application:unload(shoot),
    ok.

whole_play(_) ->
    ok.

