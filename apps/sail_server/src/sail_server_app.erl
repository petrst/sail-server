-module(sail_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([create/1,list/0, terminate/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sail_server_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
create(Name)->
    sail_server_sup:start_child(Name).

terminate(Name)->
    sail_server_sup:terminate_child(Name).

list()->
    [ Id || {Id,Pid,_,_} <- supervisor:which_children(sail_server_sup), is_pid(Pid) ].
