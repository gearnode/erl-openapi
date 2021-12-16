%% Copyright (c) 2021 Exograd SAS.
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
