-module(boat).
-export([tick/2,new/4,steer/2]).
-include("boat.hrl").

-define(ALPHA,0).
-define(C,0).
-define(WX,1).
-define(WY,0).


new(X,Y,VX,VY)->
    #boat{xpos=X,ypos=Y,xvel=VX,yvel=VY}.

tick(Weather,Boat)->
    NewBoat = update_position(Weather,Boat),
    update_speed(Weather, NewBoat).


steer(Boat=#boat{xvel=VX,yvel=VY},Angle)->
    NewXvel = VX*math:cos(Angle * math:pi() / 180 ) + VY * math:sin( Angle * math:pi() / 180 ),
    NewYvel = VY*math:cos(Angle * math:pi() / 180 ) - VX * math:sin( Angle * math:pi() / 180 ),
    Boat#boat{xvel=NewXvel, yvel = NewYvel}.

update_position(_Weather, #boat{xpos=X,ypos=Y,xvel=VX,yvel=VY})->
    NewPos = #boat{xpos=trunc(X+VX) rem 500,ypos=trunc(Y+VY) rem 500,xvel=VX,yvel=VY},
    %io:format("New position = ~p~n",[NewPos]),
    NewPos.

update_speed(_Weather,Boat=#boat{xvel=VX,yvel=VY})->
    NewBoat=Boat#boat{xvel=VX+?ALPHA*?WX-?C*VX, yvel=VY+?ALPHA*?WY-?C*VY},
    %io:format("New speed = ~p~n",[NewBoat]),
    NewBoat.

 
