-module(ehpa_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    inits:start(),
    ehpa_http_sup:start_link(),
    ehpa_sup:start_link().

stop(_State) ->
    ok.
