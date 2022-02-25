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

-module(openapi_parameter).

-export([name/1, in/1, description/1, required/1, style/1, explode/1]).

-export([queries/1, paths/1, headers/1]).

-spec name(openapi:parameter()) -> binary().
name(#{name := Name}) ->
  Name.

-spec in(openapi:parameter()) -> query | header | path | cookie.
in(#{in := In}) ->
  In.

-spec description(openapi:parameter()) -> binary().
description(#{description := Description}) ->
  Description;
description(_) ->
  <<>>.

-spec required(openapi:parameter()) -> boolean().
required(#{required := true}) ->
  true;
required(_) ->
  false.

-spec style(openapi:parameter()) ->
        matrix | label | form | simple | spaceDelimited |
        pipeDelimited | deepObject.
style(#{style := Style}) ->
  Style;
style(#{in := query}) ->
  form;
style(#{in := header}) ->
  simple;
style(#{in := path}) ->
  simple;
style(#{in := cookie}) ->
  form.

-spec explode(openapi:parameter()) -> boolean().
explode(#{explode := true}) ->
  true;
explode(_) ->
  false.

-spec queries([openapi:parameter()]) -> [openapi:parameter()].
queries(Parameters) ->
  lists:filter(fun (Parameter) -> in(Parameter) =:= query end, Parameters).

-spec paths([openapi:parameter()]) -> [openapi:parameter()].
paths(Parameters) ->
  lists:filter(fun (Parameter) -> in(Parameter) =:= path end, Parameters).

-spec headers([openapi:parameter()]) -> [openapi:parameter()].
headers(Parameters) ->
  lists:filter(fun (Parameter) -> in(Parameter) =:= header end, Parameters).
