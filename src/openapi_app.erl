-module(openapi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _Args) ->
  jsv:register_catalog(openapi, openapi_jsv:catalog()),
  Options = #{type_map => openapi_jsv:type_map()},
  case jsv:verify_catalog(openapi, Options) of
    ok ->
      openapi_sup:start_link();
    {error, Reason} ->
      {error, {invalid_jsv_catalog, Reason, openapi}}
  end.

stop(_State) ->
  jsv:unregister_catalog(openapi),
  ok.
