%%
%% show details on the VM, releases, apps, etc.
%%
-module(static_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-export([html/1, css/1, js/1]).

-compile(export_all).

init({tcp, http}, Req, []) ->
  {ok, Req, undefined_state};
init({tcp, http}, Req, OnlyFile) ->
  {ok, Req, OnlyFile}.


% Resource URI is in request URI
handle(Req, undefined_state = State) ->
  {[_|Path], Req2} = cowboy_http_req:path(Req), % strip <<"static">>
  load_and_send(Req2, Path, State);

% Resource URI is in State = Options
handle(Req, OnlyFile = State) ->
  load_and_send(Req, OnlyFile, State).

load_and_send(Req, PathBins, State) ->
  Path = filename:join([ binary_to_list(P) || P <- PathBins ]),
  case  file(Path) of
    {ok, Mime, Body} ->
      {ok, Req2} = send(Req,Mime,Body),
      {ok, Req2, State};
    _ ->
      {ok, Req2} = cowboy_http_req:reply(404, [], <<"404'd">>, Req),
      {ok, Req2, State}
  end.

send(Req,Mime,Body) ->
      Headers = [{<<"Content-Type">>, Mime}],
      cowboy_http_req:reply(200, Headers, Body, Req).

terminate(_Req, _State) ->
  ok.

html(Name) ->
  type("html", Name).
css(Name) ->
  type("css", Name).
js(Name) ->
  type("js", Name).

type(Type, Name) ->
  file(filename:join([Type, Name ++ Type])).

file(Path) ->
  Priv = priv(),
  {ok,Body} = file:read_file(filename:join(Priv,Path)),
  Extension=tl(filename:extension(Path)),
  Mime = mimetypes:extension(Extension),
  {ok, Mime, Body}.


priv() ->
  {ok,App} = application:get_application(?MODULE),
  case code:priv_dir(App) of
    {error,_} -> "priv";
    Priv -> Priv
  end.
