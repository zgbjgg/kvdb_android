-module(module_test).

-export([catching/2]).

%%
%% This module is a 'TARGET' module and catch the incoming calls of the java impl.
%% 
%% The first parameter of the function is the type of the action and the second
%% is the data to get working. In this case the data are set in a plain text, but you
%% can usage a xml or json parser for a more specific and standard data protocol.
%%
%% The example put, get, delete and update data into a backend table set into a 
%% file definition of tables.
%%
catching(<<"POST">>, PlainText) ->

    % Plain text incoming are a binary string 
    % comma separated (split it)
    [Key, Data1, Data2, Data3] = [ binary_to_list(D) || D <- binary:split(PlainText, <<",">>, [global]) ],

    % Build a plist
    Plist = [{yourkey, Key}, {data1, Data1}, {data2, Data2}, {data3, Data3}],

    % Save
    kvdb_android:put(your_table, Plist);
catching(<<"GET">>, PlainText) ->
    
    % Plain text incoming are a binary comma separated, 
    % first is a field to return, second is a match-field and third is a match-value
    [Field, MatchField, MatchValue] =  [ binary_to_list(D) || D <- binary:split(PlainText, <<",">>, [global]) ],

    % Query
    FieldA = list_to_atom(Field),
    MatchFieldA = list_to_atom(MatchField),
    case kvdb_android:get(your_table, FieldA, [MatchFieldA], [MatchValue]) of
        {ok, [Obj]} -> {ok, list_to_binary(Obj)};
	Error	    -> Error
    end;	
catching(<<"PUT">>, PlainText) ->
    
    % Plain text incoming are a binary comma separated, 
    % first is a field and value to update, nex match-field and last is a match-value
    [Field, NewValue, MatchField, MatchValue] =  [ binary_to_list(D) || D <- binary:split(PlainText, <<",">>, [global]) ],

    FieldA = list_to_atom(Field),
    MatchFieldA = list_to_atom(MatchField),
    kvdb_android:update(replace, your_table, FieldA, NewValue, MatchFieldA, MatchValue);
catching(<<"DEL">>, PlainText) ->
    
    % Plain text incoming are a binary comma separated
    [Table, Key] =  [ binary_to_list(D) || D <- binary:split(PlainText, <<",">>, [global]) ],
    
    kvdb_android:delete(Table, Key).

