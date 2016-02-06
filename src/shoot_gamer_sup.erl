-module(shoot_gamer_sup).

-behaviour(supervisor).

-export([
         start_link/0,
         init/1
        ]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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

