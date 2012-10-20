-module(login_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init(_Transport, Req, _Any) ->
	{ok, Req, []}.

handle(Req, State) ->
    {PostVals,Req2} = cowboy_http_req:body_qs(Req),
    case proplists:get_value(<<"player">>, PostVals) of
        undefined ->
                 {ok, Reply} = cowboy_http_req:reply(
                            401, [], <<"No login">>,
                            Req2
                        );
        Player    ->
                  {[<<"login">>,Path], Req3} = cowboy_http_req:path(Req2),
                  RaceName = binary_to_atom(Path,utf8),
                  PlayerName = binary_to_atom(Player,utf8),
                  ok=sail_server:connect(RaceName, PlayerName),
                  {ok, Reply} = cowboy_http_req:reply(
                            302, [{<<"Location">>, <<"/race/",Path/binary>>}],
                            <<"Redirecting with Header!">>,
                            set_cookie(Player,Req3)
                        )
    end,
    {ok,Reply,State}.


set_cookie(Player,Req)->
    {ok,Req2}=cowboy_http_req:set_resp_cookie(<<"player">>,Player,[{path, "/"}], Req),
    Req2.

terminate(_Req, _State) ->
  ok.
