%%
%% show details on the VM, releases, apps, etc.
%%
-module(template_helper).
-compile(export_all).

expand_and_send(Req, TemplatePath, Dict) ->
  case static_handler:file(binary_to_list(TemplatePath)) of
    {ok, Mime, Body} ->
      Body2 = template:expand(binary_to_list(Body),Dict),
      {ok, Req2} = static_handler:send(Req,Mime,Body2);
    _ ->
      {ok, Req2} = cowboy_http_req:reply(404, [], <<"404'd">>, Req)
  end.


