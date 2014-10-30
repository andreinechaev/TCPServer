-module (login_security).
-behaviour (gen_server).

-export ([start_link/0, encode/2]).

%%gen_server callbacks
-export ([init/1, handle_call/3,
 		handle_cast/2, handle_info/2,
  		terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
	process_flag(trap_exit, true),
	crypto:start(),
 	io:format("~p started~n", [?MODULE]),
	{ok, 0}.

handle_call(Request, _From, N) ->
	{reply, Request, N + 1}.

handle_cast(_Msg, N) -> 
	{noreply, N}.

handle_info(_Info, N) -> 
	{noreply, N}.	

terminate(_Reason, _N) ->
	io:format("~p stoped~n", [?MODULE]),
	ok.

code_change(_OldVsn, N, _Extra)	-> {ok, N}.

encode(Key, Value) ->
	BKey = list_to_binary(Key),
	BValue = list_to_binary(Value),
	<<Mac:160/integer>> = crypto:hmac(sha, BKey, BValue),
	Token = lists:flatten(io_lib:format("~40.16.0b", [Mac])),
	io:format("Token - ~p~n", [Token]).
					
