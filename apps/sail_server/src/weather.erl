-module(weather).
-compile([export_all]).


-define(VX, 0.8).
-define(VY, 0.9).

start()->
    spawn( ?MODULE, loop, [] ).

wind_at(Pid,X,Y)->
    Pid ! {self(),wind,X,Y},
    receive
        Wind -> Wind
    end.


loop(State)->
    receive
        tick -> 
                NewState = tick(State),
                loop(NewState);
        {Pid, wind,X,Y} -> 
                Wind = [iwind_at(X,Y)],
                Pid ! {X,Y,Wind},
                loop(State);
        Any ->  io:format("Received ~p~n", [Any])
    end.
                

tick(State)->
    State.

iwind_at(X,Y)->
    {?VX,?VY}.
