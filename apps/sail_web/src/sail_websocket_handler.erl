%% Feel free to use, reuse and abuse the code in this file.

-module(sail_websocket_handler).
-behaviour(cowboy_http_websocket_handler).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).
-export([init/3]).
-include("boat.hrl").

-record(state,{race,player}).

init({_Any, http}, Req, _Opts) ->
	case cowboy_http_req:header('Upgrade', Req) of
		{undefined, Req2} -> {ok, Req2, undefined};
		{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
		{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
	end.

websocket_init(_Any, Req, [{service,list}]) ->
    io:format("Init LIST~n"),
    bigwig_pubsubhub:register_client(self()),
	Req2 = cowboy_http_req:compact(Req),
	{ok, Req2, #state{race=undefined,player=undefined}, hibernate};

websocket_init(_Any, Req, [{service,race}]) ->
	timer:send_interval(1000, tick),
    {RaceName,_}=cowboy_http_req:binding(race,Req),
	Req2 = cowboy_http_req:compact(Req),
	{ok, Req2, #state{race=binary_to_atom(RaceName,utf8)}, hibernate}.

websocket_handle({text, <<"LIST">>}, Req, State) ->
    Fields = sail_server_app:list(),
    Rep = jsx:term_to_json(Fields),
	{reply, {text, <<Rep/binary>>}, Req, State, hibernate};

websocket_handle({text, <<"CREATE",Name/binary>>}, Req, State) ->
    sail_server_app:create(binary_to_atom(Name,utf8)),
	{reply, {text, << "You created: ", Name/binary >>}, Req, State, hibernate};

websocket_handle({text, <<"JOIN",Name/binary>>}, Req,State=#state{race=Race}) ->
    Player = binary_to_atom(Name,utf8),
    sail_server:connect(Race, Player),
	{reply, {text, << "You joined: ", Name/binary >>}, Req, State#state{player=Player}, hibernate};

websocket_handle({text, <<"STEER",Angle/binary>>}, Req,State=#state{race=Race,player=Player}) ->
    sail_server:steer(Race,Player,list_to_integer(binary_to_list(Angle))),
	{reply, {text, << "Going left: ", Angle/binary >>}, Req, State, hibernate};


websocket_handle({text, <<"UPDATE",_Name/binary>>}, Req, State=#state{race=Race}) ->
    {_W,Fleet} = sail_server:status(Race),
    Rep = list_to_binary(to_json(Fleet,[])),
	{reply, {text, <<Rep/binary>>}, Req, State, hibernate};

websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "You said: ", Msg/binary >>}, Req, State, hibernate};

websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

to_json([],Acc)->
    "["++Acc++"]";
to_json([{Name,Boat}|[]],Acc)->
    BString=io_lib:format("{name:\"~p\",xpos:~p,ypos:~p}",[Name,Boat#boat.xpos,Boat#boat.ypos]),
   to_json([],Acc++BString); 
to_json([{Name,Boat}|Rest],Acc)->
    BString=io_lib:format("{name:\"~p\",xpos:~p,ypos:~p},",[Name,Boat#boat.xpos,Boat#boat.ypos]),
    to_json(Rest,Acc++BString).

websocket_info({bigwig,Msg}, Req, State) ->
    io:format("PUBSUBHUB: ~p~n",[Msg]),
    Fields = sail_server_app:list(),
    Rep = jsx:term_to_json(Fields),
	{reply, {text, <<Rep/binary>>}, Req, State, hibernate};
	%{ok, Req, State, hibernate};

websocket_info(tick, Req, State=#state{race=Race}) ->
    {_W,Fleet} = sail_server:status(Race),
    %Rep = list_to_binary(io_lib:format("~p",[_Fleet])),
    Rep = list_to_binary(to_json(Fleet,[])),
    % io:format("Sending from ~p~n",[self()]),
	{reply, {text, Rep}, Req, State, hibernate};
websocket_info(_Info, Req, State) ->
	{ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.