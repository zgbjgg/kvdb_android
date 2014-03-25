-module(kvdb_android_app).

-author('zgbjgg@gmail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, 
	 stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    kvdb_android_sup:start_link().

stop(_State) ->
    ok.
