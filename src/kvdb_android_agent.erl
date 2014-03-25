-module(kvdb_android_agent).

-author('zgbjgg@gmail.com').

-behaviour(gen_server).

-include("kvdb_android.hrl").

%% API
-export([start_link/0, 
	 add_resource/1, 
	 exit_resource/1, 
	 request_action/1, 
	 stop/0]).

%% gen_server callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, { resources, kvdb_backend, data_directory }).

%%%===================================================================
%%% API
%%%===================================================================

add_resource(Pid) -> 
    gen_server:call(?MODULE, {adding_resource, Pid}).

exit_resource(Pid) ->
    gen_server:cast(?MODULE, {exiting_resource, Pid}).

request_action(Action) ->
    gen_server:call(?MODULE, {requesting_action, Action}).

%%--------------------------------------------------------------------
%% @doc
%% Stops the generic server
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),

    % Create data dir if no exists
    ok = case file:make_dir("kvdb_data") of
	     ok    -> ok;
	     Error -> io:format("kvdb data dir is ~p\n", [Error])
	 end,

    % Start backend data store system
    {ok, DataDir} = file:get_cwd(),
    ok = application:set_env(mnesia, dir, DataDir ++ "/kvdb_data"),

    % Start mnesia backend
    ok = application:start(mnesia),

    % Register tables at the runtime, file is placed on priv directory
    Ebin = filename:dirname(code:which('kvdb_android_agent')),
    File = filename:join(filename:dirname(Ebin), "priv") ++ "/kvdb_backend_tables",
    accfile = kvdb_android_backend:dynamic_tbl(File), 

    {ok, #state{resources = [], kvdb_backend=mnesia, data_directory=DataDir}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({adding_resource, Pid}, _From, #state{resources = Resources, kvdb_backend=mnesia, data_directory=DataDir}) ->
    {reply, {ok, erlang:pid_to_list(Pid)}, #state{resources=[Pid | Resources], 
						  kvdb_backend=mnesia, 
						  data_directory=DataDir}}; 
handle_call({requesting_action, Action}, _From, State=#state{resources = [Pid | _Resources]}) ->
    
    % Parse action with a binary composition
    <<MethodS:4/binary, ReqS:50/binary, Body/binary>> = Action,    

    Method = binary:replace(MethodS, <<" ">>, <<"">>, [global]),
    Req = binary:replace(ReqS, <<" ">>, <<"">>, [global]),

    % for now print information
    io:format("Body: ~p\n", [Body]),

    % Reply to process the desired response
    Pid ! {backend, ok},

    % Reply a response 
    {ok, Reply} = kvdb_android_behaviour:target(Method, Req, Body),

   {reply, {ok, Reply}, State};
handle_call(stop, _From, State)   ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({exiting_resource, Pid}, #state{resources = Resources, kvdb_backend=mnesia, data_directory=DataDir}) ->
    Pid ! exiting,
    {noreply, #state{resources = lists:delete(Pid, Resources), kvdb_backend=mnesia, data_directory=DataDir}}; 
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State)  ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

