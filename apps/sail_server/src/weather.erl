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
                

create_weather_cell(Force,Angle)->
    <<Force:4, Angle:4>>.

create_weather_field(GridSize)->
    create_weather_field(GridSize,<<>>).
create_weather_field(0,Acc)->
    Acc;
create_weather_field(Cells,Acc)->
    Cell =  create_weather_cell(5,10),
    create_weather_field(Cells-1, << Cell/binary, Acc/binary>>).


update_cell(WeatherField, CellId ,{Force,Angle})->
    <<W1:CellId/binary,OldW:8,W2/binary>> = WeatherField,
    NewW = create_weather_cell(Force,Angle),
    <<W1/binary, NewW/binary, W2/binary>>.
    


tick(State)->
    State.

iwind_at(X,Y)->
    {?VX,?VY}.
