-module(sail_server).
-behaviour(gen_server).
-include("boat.hrl").

-export([steer/3,change_weather/2,start_link/1, stop/1, connect/2,disconnect/2, status/1]).
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CONFIGURATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(FPS,1).
-define(BOATMODEL,boat).
-define(WEATHERMODEL, weather).
-define(MAX_X,500).
-define(MAX_Y,500).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  PUBLIC API 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Name)->
    gen_server:start_link({local,Name},?MODULE, [Name], []).

stop(Name)->
    gen_server:call(Name, stop).

connect(Name,Player)->
    gen_server:call(Name,{connect,Player}).

disconnect(Name,Player)->
    gen_server:call(Name,{disconnect,Player}).

-spec steer(Name::atom(), Player::atom(),Angle::integer()) -> term().
steer(Name,Player,Angle)->
    gen_server:call(Name,{steer,Player,Angle}).


change_weather(Name,NewWeather)->
    gen_server:call(Name,{update_weather,NewWeather}).

status(Name)->
    gen_server:call(Name,status).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  GEN_SERVER CALLBACKS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


init([Name])->
    Weather = ?WEATHERMODEL:create_weather_field(40*40),
    Fleet   = [],
	% Run timer	
    %{ok,Timer}=timer:apply_interval(1000,gen_server,call,[Name,tick]),
    io:format("sail_server ~p was started~n",[Name]),
    {ok,{Weather,Fleet}}.


%% Main server routine - clock tick, updates everything
handle_call(tick, _From, State)->
    %io:format("Tick from ~p~n",[_From]),
    NewState1 = update_weather(State),
    NewState2 = update_fleet(NewState1),
	%io:format("~p~n",[NewState2]),
    {reply, ok, NewState2};

handle_call(status, _From, State)->
    {reply, State, State};
	
handle_call({steer,Name, Angle},_From, {_Weather, Fleet})->
    case lists:keyfind(Name,1,Fleet) of
        false -> {reply, err_notconnected,{_Weather,Fleet}};
        {Name,Boat} -> 
                 NewBoat = ?BOATMODEL:steer(Boat,Angle),
                 {reply,ok, {_Weather, lists:keyreplace(Name,1,Fleet, {Name,NewBoat})}}
    end;
            
handle_call({update_weather,NewWeather},_From, {_Weather, Fleet})->
    {reply,ok,{NewWeather, Fleet}};

handle_call({connect,Name}, _From, {_Weather,Fleet})->
    case lists:keymember(Name,1,Fleet) of
        false -> 
            NewFleet = lists:append( Fleet, [{ Name, ?BOATMODEL:new(100,100,1.0,1.0) }]),
            {reply, ok, {_Weather, NewFleet}};
        true -> {reply, err_connected, {_Weather,Fleet}}
    end;

handle_call({disconnect,Name}, _From, {_Weather,Fleet})->
    case lists:keymember(Name,1,Fleet) of
        true -> 
            NewFleet = lists:keydelete( Name,1,Fleet ),
            {reply, ok, {_Weather, NewFleet}};
        false -> {reply, err_not_connected, {_Weather,Fleet}}
    end;


handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab};

handle_call(_Msg, _From, State)->
    {reply,not_implemented,State}.

handle_info(_Msg, State)->
    {noreply, State}.

terminate(Reason, _State)->
    io:format("Terminating: ~p~n",[Reason]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  INTERNAL FUNCTIONS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% No weather evolution implemented
update_weather(State)->
    State.


update_fleet({Weather,Fleet})->
    NewFleet = lists:map(fun({Name,Boat}) -> {Name,?BOATMODEL:tick(Weather,Boat)} end,Fleet),
    NewFleet2 = wrap_pos(NewFleet,[]),
    {Weather, NewFleet2}.

%% Wrap the position of boats "out o range"
%% Our world is round too!!!
wrap_pos([],NewFleet)->
    NewFleet;
wrap_pos([{Name,#boat{xpos=XPos,ypos=YPos}=Boat} | RestOfFleet], NewFleet) ->
    wrap_pos(RestOfFleet, lists:append(NewFleet,[{Name,Boat#boat{xpos=XPos rem ?MAX_X, ypos=YPos rem ?MAX_Y}}])).


