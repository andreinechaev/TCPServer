-module(json_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _StartArgs) ->
    json_server_sup:start_link().

stop(_State) ->
    ok.
