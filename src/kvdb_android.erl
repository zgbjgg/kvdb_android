-module(kvdb_android).

-export([start/0]).

%%
%% Start the entire kvdb_android application from an erlang flag
%% option (-s), to avoid starting another extra process or use eval 
%% instead
%%
start() ->
    application:start(?MODULE).
