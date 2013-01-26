-module(ehpa_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, start_application/0, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start () ->
    start([], []).

start(_StartType, _StartArgs) ->
    inets:start(),
    ehpa_http_sup:start_link(),
    ehpa_sup:start_link().

start_application() ->
    application:start(ehpa).

stop(_State) ->
    ok.
