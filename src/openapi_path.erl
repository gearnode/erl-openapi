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

-module(openapi_path).

-export([summary/1, description/1, parameters/1, servers/1]).

-export([operations/1]).

-spec summary(openapi:path()) -> binary().
summary(#{summary := Summary}) ->
  Summary;
summary(_) ->
  <<>>.

-spec description(openapi:path()) -> binary().
description(#{description := Description}) ->
  Description;
description(_) ->
  <<>>.

-spec parameters(openapi:path()) -> [openapi:parameter()].
parameters(#{parameters := Parameters}) ->
  Parameters;
parameters(_) ->
  [].

-spec servers(openapi:path()) -> [openapi:server()].
servers(#{servers := Servers}) ->
  Servers;
servers(_) ->
  [].

-spec operations(openapi:path()) -> [{Verb, openapi:operation()}]
          when Verb :: get | put | post | delete | options |
                       head | patch | trace.
operations(PathItemObject) ->
  OperationKeys = [get, put, post, delete, options, head, patch, trace],
  lists:foldl(
    fun (K, Acc) ->
        case maps:find(K, PathItemObject) of
          {ok, Operation} ->
            [{K, Operation} | Acc];
          error ->
            Acc
        end
    end, [], OperationKeys).
