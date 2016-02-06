-module(shoot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Field = child(field, worker),
    Gamers = child(gamers, supervisor),
    Children = [
                Field,
                Gamers
               ],
    Strategy = rest_for_one,
    Restarts = 10,
    Seconds = 60,
    Flags = {Strategy, Restarts, Seconds},
    Spec = {Flags, Children},
    {ok, Spec}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

child(I, Type) ->
    Fun = {I, start_link, []},
    {I, Fun, permanent, 5000, Type, [I]}.

