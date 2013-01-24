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
	    "ehpa" ->
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
    UrlList = get_url_list(PostData),

    Timeout = case string:to_integer(proplists:get_value("timeout", PostData, [])) of
		  {error, _} ->
		      5000;
		  {Res, _} ->
		      Res
	      end,

    send_request(UrlList),

    {MegaSecs, Secs , MicroSecs} = erlang:now(),
    Now = (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs / 1000),
    DeadTime = Now + Timeout,

    Response  = response_loop(length(UrlList), DeadTime),

    Req:respond({200, [], [mochijson2:encode({struct, [{"result", Response}]})]}).

get_url_list(PostData) ->
    get_url_list(PostData, []).

get_url_list([], ResList) ->
    ResList;

get_url_list([H|T], ResList) ->
    case H of
	{"url", Url} ->
	    get_url_list(T, [Url|ResList]);
	_ ->
	    get_url_list(T, ResList)
    end.


send_request([]) ->
    ok;

send_request([H|T]) ->
    ehpa_request:request(H, self()),
    send_request(T).


response_loop(Len, DeadTime) ->
    response_loop(Len, [], DeadTime).

response_loop(0, ResList, _) ->
    ResList;

response_loop(Len, ResList, DeadTime) ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    Now = (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs / 1000),

    case Now > DeadTime of
	true ->
	    ResList;
	false ->
	    receive 
		{res, Res} ->
		    response_loop(Len - 1, [list_to_binary(Res)|ResList], DeadTime);
		error ->
		    response_loop(Len - 1, ResList, DeadTime);
		_ ->
		    response_loop(Len, ResList, DeadTime)
	    after 100 ->
		    response_loop(Len, ResList, DeadTime)
	    end
    end.
	    
