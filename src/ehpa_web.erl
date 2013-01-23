%%%-------------------------------------------------------------------
%%% File    : jimii_apns_web.erl
%%% Author  : Wang fei <fei@innlab.net>
%%% Description : 
%%%
%%% Created : Wang fei <fei@innlab.net>
%%%-------------------------------------------------------------------
-module(ehpa_web).

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
	case Path of
	    "apns" ->
		post_process(Req);
	    _ ->
            Req:respond({404, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

post_process(Req) ->
    PostData = Req:parse_post(),
    Token = proplists:get_value("token", PostData, []),
    Badge = proplists:get_value("badge", PostData, 0),
    Msg = proplists:get_value("message", PostData, []),
    case Token of
	[] ->
	    Req:respond({500, [], []});
	_ ->
	    apns:send_message(unicode:characters_to_list(list_to_binary(Msg)), list_to_integer(Badge), Token),
	    Req:respond({200, [], []})
    end.

