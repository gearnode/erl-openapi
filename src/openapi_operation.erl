%% Copyright (c) 2022 Exograd SAS.
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

-module(openapi_operation).

-export([tags/1, summary/1, description/1, operation_id/1, parameters/1,
         request_body/1, responses/1]).

-spec tags(openapi:operation()) -> [binary()].
tags(#{tags := Tags}) ->
  Tags;
tags(_) ->
  [].

-spec summary(openapi:operation()) -> binary().
summary(#{summary := Summary}) ->
  Summary;
summary(_) ->
  <<>>.

-spec description(openapi:operation()) -> binary().
description(#{description := Description}) ->
  Description;
description(_) ->
  <<>>.

-spec operation_id(openapi:operation()) -> binary().
operation_id(#{operationId := OperationId}) ->
  OperationId.

-spec parameters(openapi:operation()) -> [openapi:parameter()].
parameters(#{parameters := Parameters}) ->
  Parameters;
parameters(_) ->
  [].

-spec request_body(openapi:operation()) -> openapi:request_body().
request_body(#{requestBody := RequestBody}) ->
  RequestBody;
request_body(_) ->
  #{}.

-spec responses(openapi:operation()) -> #{binary() := openapi:response()}.
responses(#{responses := Responses}) ->
  Responses;
responses(_) ->
  #{}.
