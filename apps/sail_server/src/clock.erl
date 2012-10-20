-module(clock).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3 ]).

-define(INTERVAL,2000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  PUBLIC API 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Name)->
    ProcName = list_to_atom( atom_to_list(clock) ++ "_" ++ atom_to_list(Name) ),
    gen_server:start_link({local,ProcName},?MODULE, [Name], []).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  GEN_SERVER CALLBACKS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Module])->
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, Module}.

handle_info(trigger, Module) ->
    %% With this cancel call we are able to manually send the 'trigger' message 
    %% to re-align the timer, and prevent accidentally setting duplicate timers
    gen_server:call(Module, tick),    
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply,Module};
handle_info(_Msg, State )->
    {noreply, State}.


handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab};
handle_call(_Msg, _From, State)->
    {reply,not_implemented,State}.


handle_cast(_Msg, State)->
    {noreply, State}.


terminate(Reason, _State)->
    io:format("Terminating: ~p~n",[Reason]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.



