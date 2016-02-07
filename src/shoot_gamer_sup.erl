-module(shoot_gamer_sup).

-behaviour(supervisor).

-export([
         stop_children/1,
         stop_child/1,
         start_child/1,
         start_link/0,
         init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?SERVER, Args).

stop_child(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

stop_children(Pids) ->
    [stop_child(Pid) || Pid <- Pids].

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Strategy = simple_one_for_one,
    Restarts = 10,
    Seconds = 60,
    Flags = {Strategy, Restarts, Seconds},
    Module = shoot_gamer,
    Fun = {Module, start_link, [[]]},
    Child = {
      id,
      Fun,
      temporary,
      100,
      worker,
      [Module]
     },
    {ok, {Flags, [Child]}}.

