-module(sail_web_app).

-behaviour(application).

%% Application callbacks
-export([start/0,start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	application:start(sail_server),
    application:start(sail_web).

start(_Type, _Args) ->
	Dispatch = [
		{'_', [
			{[<<"websocket">>,<<"race">>, race], sail_websocket_handler, [{service,race}]},
			{[<<"websocket">>,<<"list">>], sail_websocket_handler, [{service,list}]},
			{[<<"race">>,battlefield], static_handler, [<<"race.html">>]},
			{[<<"static">>,'...'], static_handler, []}
		]}
	],
	cowboy:start_listener(my_http_listener, 100,
		cowboy_tcp_transport, [{port, 8080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	cowboy:start_listener(my_https_listener, 100,
		cowboy_ssl_transport, [
			{port, 8443}, {certfile, "priv/ssl/cert.pem"},
			{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
    sail_web_sup:start_link().

stop(_State) ->
    ok.
