-module (json_server).
-behaviour (gen_server).

-export ([start_link/0, check_data/1]).

%%gen_server callbacks
-export ([init/1, handle_call/3,
 		handle_cast/2, handle_info/2,
  		terminate/2, code_change/3]).
-record (user, {login = "undef", password}).
-define (PORT, 1477).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [?PORT], []).


init([Port]) ->
	process_flag(trap_exit, true),
	{ok, Listen} = gen_tcp:listen(Port, 
		[{active, false},
		 binary,
		 {reuseaddr, true}]), 
	spawn(fun() ->
	accept_parallel(Listen) end),
	io:format("~p started~n", [?MODULE]),
	{ok, 0}.

accept_parallel(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> accept_parallel(Listen)	end),
	loop(Socket).

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

loop(Socket) ->
	inet:setopts(Socket, [{active, once}]),		    
	receive
		{tcp, Socket, Bin} ->
			io:format("Sent ok~n"),
			Answer = check_data(Bin),
			gen_tcp:send(Socket, atom_to_binary(Answer, utf8)),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Socket ~w closed [~w]~n", [Socket, self()]),
			ok	
	end.	

check_data(Bin) ->
	Data = mochijson:decode(Bin),
	case Data of
				{struct, [{"login", Name}, {"password", Password}]} ->
					R = #user{login = Name, password = Password},
					io:format("We got a new user:~n  Name - ~p~n  Password - ~p~n", [Name, Password]),
				 	security:encode(Name, Password),
					spawn(fun() -> save_data(R#user.login) end),
					ok;
				Any ->
					io:format("We got ~p~n", [Any]),
					error	
			end.

save_data(User) ->
	io:format("We're trying a new user - ~p~n", [User]).					
