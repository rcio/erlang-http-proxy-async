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
    spawn_link(fun () ->
		       spawn_request(URL, PID)
	       end).

spawn_request(URL, PID) ->
    case httpc:request(get, {URL, []}, [{timeout, 10000}], []) of
	{ok, {{_Protocal, 200, _Status}, _Headers, Body}} ->
	    PID ! {res, Body};
	_Err ->
	    PID ! error
    end.
