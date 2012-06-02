%% Feel free to use, reuse and abuse the code in this file.

-module(default_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).
-include("http.hrl").

init({_Any, http}, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    io:format("State ~p~n",[Req#http_req.bindings]),
	{ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello world!">>, Req),
	{ok, Req2, State}.

terminate(_Req, _State) ->
	ok.
