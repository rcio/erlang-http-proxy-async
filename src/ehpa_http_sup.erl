%%%-------------------------------------------------------------------
%%% File    : jimii_http_sup.erl
%%% Author  : Wang fei <fei.wang@jimii.cn>
%%% Description : 
%%%
%%% Created : Wang fei <fei.wang@jimii.cn>
%%%-------------------------------------------------------------------
-module(ehpa_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Web = web_specs(ehpa_web, 6907),
    Processes = [Web],
    Strategy = {one_for_one, 10, 10},
    {ok,
     {Strategy, lists:flatten(Processes)}}.

%%====================================================================
%% Internal functions
%%====================================================================
web_specs(Mod, Port) ->
    WebConfig = [{ip, {127,0,0,1}},
                 {port, Port},
                 {docroot, "/tmp"}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.
