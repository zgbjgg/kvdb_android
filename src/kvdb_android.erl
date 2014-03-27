-module(kvdb_android).

-export([start/0,
	 put/2,
	 get/4,
	 update/6,
	 delete/2,
	 multi_delete/1]).

-define(OK, <<"ok">>).
-define(ERROR, <<"error">>).
-define(ERROR_NOTFOUND, <<"error_notfound">>).

%%
%% Start the entire kvdb_android application from an erlang flag
%% option (-s), to avoid starting another extra process or use eval 
%% instead
%%
start() ->
    application:start(?MODULE).


%% 
%% API for 'PUT' action over mnesia backend.
%% This api retrieves a valid response as {ok, Reply} and must be
%% used on each module that is set as a target. 
%%
put(Table, Attr) ->
    case kvdb_android_mnesia:put(Table, Attr) of 
        ok ->
	    {ok, ?OK};
	_  ->
	    {ok, ?ERROR} 
    end.

%% 
%% API for 'GET' action over mnesia backend.         
%% This api retrieves a valid response as {ok, Reply} and must be
%% used on each module that is set as a target.
%%
get(Table, Field, MatchField, MatchValue) ->
    case kvdb_android_mnesia:get(Table, Field, MatchField, MatchValue) of
        []  ->
	    {ok, ?ERROR_NOTFOUND};
	Obj ->
	    {ok, Obj}
    end.

%% 
%% API for 'UPDATE' action over mnesia backend.         
%% This api retrieves a valid response as {ok, Reply} and must be
%% used on each module that is set as a target.
%%
update(Type, Table, Field, NewValue, MatchField, MatchValue) -> 
    case kvdb_android_mnesia:update({Type, Table, Field, NewValue, MatchField, MatchValue}) of
        ok ->
	    {ok, ?OK};
	_  ->
	    {ok, ?ERROR}
    end.

%%
%% API for 'DELETE' action over mnesia backend.
%% This api retrieves a valid response as {ok, Reply} and must be
%% used on each module that is set as a target.
%%
delete(Table, Key) ->
    case kvdb_android_mnesia:delete(Table, Key) of
        ok ->
	    {ok, ?OK};
	_  ->
	    {ok, ?ERROR}
    end.

%%
%% API for 'MULTI-DELETE' action over mnesia backend.
%% This api retrieves a valid response as {ok, Reply} and must be
%% used on each module that is set as a target.
%%
multi_delete(KV) ->
    case kvdb_android_mnesia:multi_delete(KV) of
        ok ->
	    {ok, ?OK};
	_  ->
	    {ok, ?ERROR}
    end.

