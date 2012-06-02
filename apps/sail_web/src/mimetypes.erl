-module(mimetypes).
-export([extension/1]).

extension("js")-> <<"application/javascript">>;
extension("html")-> <<"text/html">>;
extension("css")-> <<"text/css">>;
extension("jpg")-> <<"image/jpeg">>;
extension("png")-> <<"image/png">>.
