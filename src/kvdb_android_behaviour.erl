-module(kvdb_android_behaviour).

-export([target/3]).

target(Method, Req, Body) ->

    % Found file 
    Ebin = filename:dirname(code:which('kvdb_android_agent')),
    File = filename:join(filename:dirname(Ebin), "priv") ++ "/kvdb_backend_targets",	
    case file:consult(File) of
	{ok, Targets} ->

	    % Found target
	    {Mod, Fun} = find_target(Req, Targets),
	    
	    % Call Mod:Fun and wait for the response, commonly binary data.
	    Mod:Fun(Method, Body);
	_	      ->
	    could_not_found_file_targets
    end.


find_target(Req, [ {Req, Target} | _ ])  	->
    Target;
find_target(Req, [ {_Req, _Target} | Targets ]) ->
    find_target(Req, Targets);
find_target(_Req, [])				->
    could_not_found_target.
