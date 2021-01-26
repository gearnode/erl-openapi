-module(openapi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _Args) ->
  jsv:register_catalog(openapi, openapi_jsv:catalog()),
  openapi_sup:start_link().

stop(_State) ->
  jsv:unregister_catalog(openapi),
  ok.
