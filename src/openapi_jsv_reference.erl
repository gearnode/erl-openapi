-module(openapi_jsv_reference).

-behaviour(jsv_type).

-export([validate_type/2, canonicalize/3, generate/2]).

validate_type(Value, _) when is_binary(Value) ->
  case uri:parse(Value) of
    {ok, URI} ->
      Fragment = uri:fragment(URI),
      case json_pointer:parse(Fragment) of
        {ok, Pointer} ->
          case maps:is_key(host, URI) of
            true ->
              {ok, Value, {maps:without([fragment], URI), Pointer}};
            false ->
              {ok, Value, Pointer}
          end;
        {error, _} ->
          error
      end;
    {error, _} ->
      error
  end;
validate_type(_, _) ->
  error.

canonicalize(_, Ref, _) ->
  Ref.

generate({URI, Pointer}, _) when is_map(URI), is_list(Pointer) ->
  uri:serialize(URI#{fragment => json_pointer:serialize(Pointer)});
generate({URIString, Pointer}, State) when is_binary(URIString),
                                           is_list(Pointer) ->
  case uri:parse(URIString) of
    {ok, URI} ->
      generate({URI, Pointer}, State);
    {error, _} ->
      error
  end;
generate(Term, _) when is_list(Term) ->
  {ok, json_pointer:serialize(Term)};
generate(_, _) ->
  error.
