-module (json_server).
-behaviour (gen_server).

-export ([start_link/0, check_data/1]).

%%gen_server callbacks
-export ([init/1, handle_call/3,
 		handle_cast/2, handle_info/2,
  		terminate/2, code_change/3]).

-define (PORT, 1477).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [?PORT], []).


init([Port]) ->
	process_flag(trap_exit, true),
	{ok, Listen} = gen_tcp:listen(Port, 
		[{active, true},
		 binary,
		 {reuseaddr, true}]), 
	spawn(fun() ->
	parallel(Listen) end),
	io:format("~p started~n", [?MODULE]),
	{ok, 0}.

parallel(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> parallel(Listen)	end),
	loop(Socket).

handle_call({check_data, Data}, _From, N) ->
	{reply, check_data(Data), N + 1}.

handle_cast(_Msg, N) -> 
	{noreply, N}.

handle_info(_Info, N) -> 
	{noreply, N}.	

terminate(_Reason, _N) ->
	io:format("~p stoped~n", [?MODULE]),
	ok.

code_change(_OldVsn, N, _Extra)	-> {ok, N}.

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
		case check_data(Bin) of
			{ok, Data} ->
				gen_tcp:send(Socket, Data)
		end
	end.	

check_data(Bin) ->
	Data = binary_to_list(Bin),
	case Data of
				{user, [{name, Name}, {password, Password}]} ->
					io:format("We got a new user: ~n
						Name - ~p~n
						Password - ~p~n", [Name, Password]);
				{Pid, Any} -> 
					io:format("Undefinable data - ~p.~n", [Any]),
					Pid ! ok;
				Any ->
					io:format("We got ~p~n", [Any]),
					{ok, Any}	
			end.		
