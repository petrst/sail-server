-module(race_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).


start_link(Name) ->
    io:format("race_sup:start_link(~p)~n",[Name]),
    %supervisor:start_link({local,list_to_atom(atom_to_list(?MODULE)++Name)}, ?MODULE, [Name]).
    SupName = list_to_atom( atom_to_list(race_sup) ++ "_" ++ atom_to_list(Name) ),
    supervisor:start_link({local,SupName}, ?MODULE, [Name]).


init([Name]) ->
    io:format("race_sup:init(~p)~n",[Name]),
    {ok, {{one_for_one, 10, 60}, 
         [
            {Name, 
               {sail_server,start_link,[Name]},
                permanent, brutal_kill,
                worker,dynamic
            },
            {clock, 
               {clock,start_link,[Name]},
                permanent, brutal_kill,
                worker,dynamic
            }
         ]}}.

