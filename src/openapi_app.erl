%% Copyright (c) 2021-2022 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(openapi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _Args) ->
  Options = #{type_map => openapi_jsv:type_map()},
  F =
    fun (Name, Catalog) ->
        jsv:register_catalog(Name, Catalog),
        case jsv:verify_catalog(Name, Options) of
          ok ->
            ok;
          {error, Reason} ->
            throw({error, {invalid_jsv_catalog, Reason, Catalog}})
        end
    end,
  try
    maps:foreach(F, jsv_catalogs()),
    openapi_sup:start_link()
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

stop(_State) ->
  lists:foreach(fun jsv:unregister_catalog/1, maps:keys(jsv_catalogs())),
  ok.

jsv_catalogs() ->
  #{openapi_v2 => openapi_v2_jsv:catalog(),
    openapi_v3 => openapi_v3_jsv:catalog()}.
