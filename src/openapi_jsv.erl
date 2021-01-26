-module(openapi_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  #{spec => spec_definition()}.

-spec spec_definition() -> jsv:definition().
spec_definition() ->
  {object,
   #{members =>
       #{},
    required =>
       []}}.
