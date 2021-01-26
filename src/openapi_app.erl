-module(openapi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _Args) ->
  openapi_sup:start_link().

stop(_State) ->
  ok.
