-module(kvdb_android_acceptor).

-export([accept/1, 
	 accept_loop/1, 
	 loop/1]).

-include("kvdb_android.hrl").

-record(state, {port, loop, ip=any, lsocket=null}).

accept(State = #state{lsocket=LSocket, loop = Loop}) ->
    Pid = proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),

    % Register our process on the agent manager 
    {ok, _Fak} = kvdb_android_agent:add_resource(Pid),

    State.

accept_loop({Server, LSocket, {M, F}}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self()}),
    M:F(Socket).

loop(Sock) ->
    ok = inet:setopts(Sock, [{active, once}]),
    receive
    {backend, _B}                ->

	% Manager responses with a reply of the backend status

        loop(Sock);
    {tcp, Socket, Bytes}        ->

	% Print incoming bytes
	io:format("Incoming socket kvdb_android bytes: ~p \n", [Bytes]),
		
	% Request the action to the agent manager
	{ok, Reply} = kvdb_android_agent:request_action(Bytes),

	% Send reply to external application (apk)
        ok = gen_tcp:send(Socket, Reply),

        loop(Socket);
    {tcp_closed, _}             ->

	% Exit from kvdb_android manager
	ok = kvdb_android_agent:exit_resource(self()),

        io:format("Socket kvdb_android closed~n");
    {tcp_error, Socket, Reason} ->
        io:format("Error on socket kvdb_android ~p reason: ~p~n", [Socket, Reason])
    end.

