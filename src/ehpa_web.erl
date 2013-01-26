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
    UrlList = [X || {"url", X} <- PostData],

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

    ResDict = response_loop(length(UrlList), DeadTime),
    Response = jsonList(ResDict, length(UrlList)),

    Req:respond({200, [], [mochijson2:encode({struct, [{"result", Response}]})]}).

send_request(UrlList) ->
    send_request(UrlList, 1).

send_request([], _) ->
    ok;

send_request([H|T], Seq) ->
    ehpa_request:request(H, Seq, self()),
    send_request(T, Seq+1).


response_loop(Len, DeadTime) ->
    ResDict = dict:new(),
    response_loop(Len, ResDict, DeadTime).

response_loop(0, ResDict, _) ->
    ResDict;

response_loop(Len, ResDict, DeadTime) ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    Now = (MegaSecs * 1000000 + Secs) * 1000 + round(MicroSecs / 1000),

    case Now > DeadTime of
	true ->
	    ResDict;
	false ->
	    receive 
		{res, Seq, Res} ->
		    response_loop(Len - 1, dict:store(Seq, Res, ResDict), DeadTime);
		error ->
		    response_loop(Len - 1, ResDict, DeadTime);
		_ ->
		    response_loop(Len, ResDict, DeadTime)
	    after 100 ->
		    response_loop(Len, ResDict, DeadTime)
	    end
    end.
	    

jsonList(JsonDict, Seq) ->
    jsonList(JsonDict, 1, Seq, []).

jsonList(_JsonDict, N, Seq, ResList) when N > Seq ->
    lists:reverse(ResList);

jsonList(JsonDict, N, Seq, ResList) ->
    Value = case dict:is_key(N, JsonDict) of
		true ->
		    dict:fetch(N, JsonDict);
		false ->
		    ""
	    end,
    jsonList(JsonDict, N+1, Seq, [list_to_binary(Value) | ResList]).	    
