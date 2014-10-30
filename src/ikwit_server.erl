-module (ikwit_server).
-export ([start/0, stop/0]).

start() ->
	application:start(json_server).

stop() ->
	application:stop(json_server).