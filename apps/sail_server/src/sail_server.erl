-module(sail_server).
-behaviour(gen_server).
-include("boat.hrl").
-export([init/1, handle_call/3, handle_info/2,steer/3,
terminate/2, code_change/3, start_link/1, stop/1, connect/2,disconnect/2, status/1]).

-define(FPS,1).
-define(BOATMODEL,boat).

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


status(Name)->
    gen_server:call(Name,status).


init([Name])->
    % Weather = weather:start(),
    Fleet   = [],
	% Run timer	
    {ok,Timer}=timer:apply_interval(1000,gen_server,call,[Name,tick]),
    io:format("sail_server ~p was started~n",[Name]),
    {ok,{weather,Fleet}}.


loop()->
    timer:sleep(1000/?FPS),
    gen_server:call(?MODULE,tick),
    loop().


%% Main server routine - clock tick, updates everything
handle_call(tick, _From, State)->
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
            
handle_call({connect,Name}, _From, {_Weather,Fleet})->
    case lists:keymember(Name,1,Fleet) of
        false -> 
            NewFleet = lists:append( Fleet, [{ Name, ?BOATMODEL:new(0,0,1.0,1.0) }]),
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

%% No weather evolution implemented
update_weather(State)->
    State.

update_fleet({Weather,Fleet})->
    NewFleet = lists:map(fun({Name,Boat}) -> {Name,?BOATMODEL:tick(Weather,Boat)} end,Fleet),
    {Weather, NewFleet}.
