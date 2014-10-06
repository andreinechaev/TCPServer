-module (server).
-export ([start/0]).

start() ->
	application:start(json_server).