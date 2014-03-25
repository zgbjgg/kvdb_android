-module(kvdb_android_backend).

-export([dynamic_tbl/1,
	 reply/1, 
	 get_field/1, 
	 is_proplist/1,
	 get_concat/2,
	 filter/4]).

-define(DISC_COPIES, {disc_copies, []}).


%%
%% Create tables dynamically into mnesia backend, the creation
%% takes a file and parse with a file:consult/1, then the resulting proplists
%% is iterated into a list comprehension and call mnesia:crate_table/3.
%%
%% The only donwside is that cannot use records syntax to access data, 
%% however the kvdb android mnesia lib is built to fix this.
%%
%% All tables are built as DISC COPIES, since any app can use this for 
%% large time storing.
%%
dynamic_tbl(File) ->

    % Consult file for dynamic creating tables into backend
    case file:consult(File) of
        {ok, Tbls} ->
	    _C = [ mnesia:create_table(Tbl, [{attributes, Struct}, ?DISC_COPIES]) || {Tbl, Struct} <- Tbls ], 
	    accfile;
	_	      ->
	    badfile
    end.            

%%
%% Reply the execution of a transaction making on 
%% the mnesia backend.
%%
%% The transaction is only made in the nonode@nohost for 
%% default.
%%
reply(Trnsc) ->
    try
	% try executes a transaction
        {atomic, State} = mnesia:sync_transaction(Trnsc),
        
	State
    catch
        _:Reason when is_list(Reason) -> {error, emptylist};
        _:Reason                      -> {error, Reason}
    end.


%%
%% Retrieves the index of the data in a entire tuple representation of 
%% the resulting table.
%%
%% Iterate over each field and when it is matched then return the index of the 
%% data in the tuple.
%%
get_field({Table, MatchField})  ->
    Fields = mnesia:table_info(Table, attributes),
    get_field(Fields, MatchField).

get_field([], _)                        -> 2;
get_field([Field | Fields], MatchField) ->
    
    % Get the index of the value in the entire tuple	
    case Field of
        MatchField ->
		
	    % No add and break 
            0 + get_field([], MatchField);
        _          ->

	    % Add 1 if not found in this iteration
            1 + get_field(Fields, MatchField)
    end.

%%
%% Just check if a list of tuples is a valid proplists, used for mnesia to put 
%% data into backend.
%%
is_proplist([]) -> true;
is_proplist([{Key, _}|Rest]) when is_atom(Key) ->
    is_proplist(Rest);
is_proplist([{Key, _}|Rest]) when is_list(Key) ->
    is_proplist(Rest);
is_proplist(_) -> false.


%%
%% Concatenate and fix values depending on the variable type. Actually used for strings and numbers:
%%
%% 	'++' = "string" ++ "string" = "stringstring".
%%	'+'  = 1+1 		    = 2
%%
get_concat(OldValue, NewValue) when is_list(OldValue),
                                    is_list(NewValue) ->
    % ++ 
    OldValue ++ NewValue;
get_concat(OldValue, NewValue) when is_number(OldValue),
                                    is_number(NewValue) ->
    % +
    OldValue + NewValue.

%%
%% Filter a set of data and found matches. This function is like a set of AND's, since the data
%% must be match over all, but if any fails then break the iteration and returns an error code.
%%
%% For OR's implementation, this function must be changed for retrieve a list 
%% and evaluate if contains any 'ok' into it.
%%
filter(_Table, _X, [], [])                                     -> ok;
filter(Table, X, [MatchField | Fields], [MatchValue | Values]) ->

    % Pass through values while match is true
    case element(get_field({Table, MatchField}), X) of
        MatchValue ->
            filter(Table, X, Fields, Values);
        _          ->

	    % Break if false
            nok
    end.
