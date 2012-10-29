%%
%% show details on the VM, releases, apps, etc.
%%
-module(login_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, []) ->
  {ok, Req, undefined_state};
init({tcp, http}, Req, [Template]) ->
  {ok, Req, Template}.

handle(Req, undefined_state = State) ->
  handle(Req, "/index.html");

handle(Req, TemplatePath = State) ->
      Dict = create_dict(Req),
      {ok, Req2} = template_helper:expand_and_send(Req, TemplatePath, Dict),
      {ok, Req2, State}.


terminate(_Req, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


create_dict(Req)->
    D = template_dict:new(),
    case cowboy_http_req:cookie(<<"player">>,Req) of
        {undefined,_} -> template_dict:add_section("NOTLOGGED",template_dict:new(), D);
        {Player,_}    -> template_dict:add_section("LOGGED", template_dict:new("PLAYER",binary_to_list(Player)), D)
    end.


