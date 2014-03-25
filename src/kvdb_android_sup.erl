-module(kvdb_android_sup).

-author('zgbjgg@gmail.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start, Args},permanent, 2000, Type, [I]}).
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
    SupFlags = {one_for_one, 1000, 3600},
    Args = [2187, {kvdb_android_acceptor, loop}],
    {ok, { SupFlags, [?CHILD(kvdb_android_agent, worker), 
		      ?CHILD(kvdb_android_socket, worker, Args)] }}.
