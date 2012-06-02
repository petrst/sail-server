-module(sail_server_sup).
-behaviour(supervisor).
-export([start_link/0,start_child/1, terminate_child/1]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).


start_child(Name)->
    supervisor:start_child(?MODULE, {Name, 
                                            {sail_server,start_link,[Name]},
                                            permanent, brutal_kill,
                                            worker,dynamic}),
    bigwig_pubsubhub:notify({new_child,Name}).

terminate_child(Name)->
    supervisor:terminate_child(?MODULE,Name),
    supervisor:delete_child(?MODULE,Name),
    bigwig_pubsubhub:notify({term_child,Name}).

init(_Args) ->
    {ok, {{one_for_one, 10, 60}, []}}.

