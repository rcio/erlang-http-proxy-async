%%%-------------------------------------------------------------------
%%% File    : ehpa_request.erl
%%% Author  : Wang fei <fei@innlab.net>
%%% Description : 
%%%
%%% Created : Wang fei <fei@innlab.net>
%%%-------------------------------------------------------------------
-module(ehpa_request).

-export([request/2]).

request(URL, PID) ->
    spawn(fun () ->
		  spawn_request(URL, PID)
	  end).

spawn_request(URL, PID) ->
    case httpc:request(get, {URL, []}, [{timeout, 5000}], []) of
	{ok, {{_Protocal, 200, _Status}, _Headers, Body}} ->
	    io:format("~p", [Body]),
	    PID ! Body;
	_ ->
	    PID ! error
    end.
