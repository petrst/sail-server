-module(bot).
-export([start/2]).

start(Race,Name)->
    sail_server:connect(Race,Name),
    spawn(fun()->loop(Race,Name,0) end).

loop(Race,Name,Timeout)->
    receive
        stop -> ok
    after Timeout ->
        %NewBearing = random:uniform(90),
        NewBearing = 0,
        %NewTimeout = random:uniform(120*1000), % 2 minutes
        sail_server:steer(Race,Name,NewBearing),
        loop(Race,Name,Timeout)
    end.
