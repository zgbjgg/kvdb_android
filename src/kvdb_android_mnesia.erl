-module(kvdb_android_mnesia).

-include_lib("stdlib/include/qlc.hrl").

-export([put/2, 
	 get/4, 
	 update/1,
	 delete/2, 
	 multi_delete/1]).

%%
%% Put data into mnesia backend, provide the table name as string or atom,
%% and the proplists according to the table definition.
%%
%% The attributes are each key on the proplist and the value is the real 
%% value to be storing.
%%
put(Table, Keys) when is_list(Table) ->
    mnesia_dlib:put(list_to_atom(Table),Keys);
put(Table, Keys)                     ->
    case kvdb_android_backend:is_proplist(Keys) of
	true  -> 
	    Set = list_to_tuple([Table| [proplists:get_value(X, Keys) ||
			       		X <- mnesia:table_info(Table, attributes)]]),
	    Trnsc = fun() ->
			    mnesia:write(Set)
		    end,
	    kvdb_android_backend:reply(Trnsc);
	false -> 
	    nok
    end.

%%
%% Get data from mnesia backend, providing the table name as string or atom, 
%% the field to get, and the match field(s) with the respective match value(s)
%%
%% This retrieves all data from a single table, an unique field of a singe table,
%% all data matching a field(s) and value(s) or an unique field matching field(s) 
%% and value(s).
%%
%% Matching field(s) and value(s) can be a list of strings for multiple matching.
%%
get(Table, Field, MatchField, MatchValue) when is_list(Table) ->
    mnesia_dlib:get(list_to_atom(Table), Field, MatchField, MatchValue);
get(Table, all, none, none)               ->
    Trnsc = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(Table)]),
		    qlc:e(Q)
	    end,
    kvdb_android_backend:reply(Trnsc);
get(Table, Field, none, none)             ->
    Trnsc = fun() ->
		    Q = qlc:q([element(kvdb_android_backend:get_field({Table, Field}), X) ||
				 X <- mnesia:table(Table)]),
		    qlc:e(Q)
	    end,
    kvdb_android_backend:reply(Trnsc);
get(Table, all, MatchField, MatchValue) ->
    Fields = case is_atom(MatchField) of true -> lists:append([[MatchField]]); false -> MatchField end,
    Values = case is_atom(MatchValue) of true -> lists:append([[MatchValue]]); false -> MatchValue end,
    Trnsc = fun() ->		    
		   Q = qlc:q([X || X <- mnesia:table(Table),
				   ok =:= kvdb_android_backend:filter(Table, X, Fields, Values)]),
		   qlc:e(Q)
	   end,
    kvdb_android_backend:reply(Trnsc);
get(Table, Field, MatchField, MatchValue) ->
    Fields = case is_atom(MatchField) of true -> lists:append([[MatchField]]); false -> MatchField end,
    Values = case is_atom(MatchValue) of true -> lists:append([[MatchValue]]); false -> MatchValue end,    
    Trnsc = fun() ->		    
		   Q = qlc:q([element(kvdb_android_backend:get_field({Table, Field}), X) || 
				 X <- mnesia:table(Table),
				 ok =:= kvdb_android_backend:filter(Table, X, Fields, Values)]),
		   qlc:e(Q)
	   end,
    kvdb_android_backend:reply(Trnsc).
    

%%
%% Update data on mnesia backend on a single table.
%%
%% The update follows the same structure as get, but a new value for the field to 
%% update is mandatory.
%%
%% This can concatenate the values or override if a replace tag is set.
%%
update({Type, Table, Field, NewValue, MatchField, MatchValue}) when is_list(Table) ->
    case Type of
	replace -> update({replace, list_to_atom(Table),
			   Field, NewValue, MatchField, MatchValue});
	append  -> update({append, list_to_atom(Table),
			   Field, NewValue, MatchField, MatchValue})
    end;
update({replace, Table, Field, NewValue, MatchField, MatchValue})                  ->
    Trnsc = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(Table), 
				    element(kvdb_android_backend:get_field({Table, MatchField}), X) =:= MatchValue]),
		    [Occ|_]=qlc:e(Q),		    
		    UpdValue = setelement(kvdb_android_backend:get_field({Table, Field}), Occ, NewValue),
		    mnesia:write(UpdValue)
	    end,
    kvdb_android_backend:reply(Trnsc);
update({append, Table, Field, NewValue, MatchField, MatchValue})                   ->
    Trnsc = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(Table), 
				    element(kvdb_android_backend:get_field({Table, MatchField}), X) =:= MatchValue]),
		    [Occ|_]=qlc:e(Q),
		    OldValue = element(kvdb_android_backend:get_field({Table, Field}), Occ),
		    Value = kvdb_android_backend:get_concat(NewValue, OldValue),
		    UpdValue = setelement(kvdb_android_backend:get_field({Table, Field}), Occ, Value),
		    mnesia:write(UpdValue)
	    end,
    kvdb_android_backend:reply(Trnsc).

%%
%% Deletes physically the data on the mnesia backend.
%%
delete(Table, Key) when is_list(Table) ->
    delete(list_to_atom(Table), Key);
delete(Table, Key) ->
    Trnsc = fun() ->
		    mnesia:delete({Table, Key})
	    end,
    kvdb_android_backend:reply(Trnsc).


%%
%% Same as delete but for multiple set of keys.
%%  
multi_delete([])                  -> ok;
multi_delete([{Table, Key}|Rest]) ->
    ok = delete(Table, Key),
    multi_delete(Rest).
    
