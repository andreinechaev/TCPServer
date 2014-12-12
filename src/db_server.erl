-module(db_server).
-behaviour (gen_server).
-include ("db_info.hrl").

-export([start_link/0, show_table/1, read_object_from_table/2, read_token_from_table/2]). %TODO API functions 

%gen_server callbacks functions
-export([init/1, handle_call/3,
 		handle_cast/2, handle_info/2,
  		terminate/2, code_change/3]).

-define (SERVER, ?MODULE).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

init([]) ->
	gen_server:cast(?SERVER, init_db),
	io:format("~p started~n", [?SERVER]),
	{ok, 0}.

handle_call({look_up, User}, _From, N) ->
	Info = look_up(User),
	{reply, Info, N+1}.

handle_cast(init_db, N) ->
	init_db(),
	{noreply, N};

handle_cast({save, User}, N) -> 
	save_user(User),
	{noreply, N}.

handle_info(_Info, N) ->
	{noreply, N}.

terminate(_Reason, _N) ->
	ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

show_table(Table_name)->
    Iterator =  fun(Rec,_)->
                    io:format("~p~n",[Rec]),
                    []
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction, Exec, [{Iterator,Table_name}], mnesia_frag)
    end.

init_db() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}]),
	mnesia:create_table(token, [{attributes, record_info(fields, token)}]),
	io:format("Tables have been created").

save_user(User) ->
	io:format("Tring to save User - ~p~n", [User]),
	Fun = fun() -> mnesia:write(User) end,
	mnesia:transaction(Fun).

look_up(User) ->
	Fun = fun() ->
		mnesia:read({users, User#token.login})
	end,
	case mnesia:transaction(Fun) of
			{atomic,[{users, _UserName, _Email, Token}]}
				when Token =:= User#token.token ->
				Token;
			_Any ->
				"error"
	end.

read_object_from_table(TableName, Key) ->
	F = fun() ->
		mnesia:read({TableName, Key})
	end,
	mnesia:transaction(F).

read_token_from_table(TableName, Key) ->
	F = fun() ->
		mnesia:read({TableName, Key})
	end,
	{atomic,[{_TableName, _UserName, Token}]} = mnesia:transaction(F),
    Token.   	
