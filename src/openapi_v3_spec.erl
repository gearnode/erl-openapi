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

-module(openapi_v3_spec).

-export([read_file/1, read/1, read_value/1]).

-spec read_file(file:name_all()) ->
        {ok, openapi_v3:specification()} | {error, openapi_v3:error_reason()}.
read_file(Path) ->
  case file:read_file(Path) of
    {ok, Data} ->
      read(Data);
    {error, Reason} ->
      {error, {file_error, Reason, Path}}
  end.

-spec read(binary()) ->
        {ok, openapi_v3:specification()} | {error, openapi_v3:error_reason()}.
read(Data) ->
  case json:parse(Data) of
    {ok, Value} ->
      read_value(Value);
    {error, Error} ->
      {error, {invalid_json_data, Error}}
  end.

-spec read_value(json:value()) ->
        {ok, openapi_v3:specification()} | {error, openapi_v3:error_reason()}.
read_value(Value0) ->
  Value = inline_references(Value0),
  Options = #{type_map => openapi_jsv:type_map(),
              unknown_member_handling => keep,
              format_value_errors => true},
  case jsv:validate(Value, {ref, openapi_v3, specification}, Options) of
    {ok, Data} ->
      {ok, Data};
    {error, Errors} ->
      {error, {invalid_specification, Errors}}
  end.

-spec inline_references(map()) -> map().
inline_references(Data) ->
  inline_references(maps:next(maps:iterator(Data)), Data, #{}).

-spec inline_references(none | {term(), term(), maps:iterator()}, map(), map()) -> map().
inline_references(none, _, Acc) ->
  Acc;
inline_references({Key, Value0, Iterator}, Spec, Acc) when is_map(Value0) ->
  Value1 = maybe_resolve_ref(Spec, Value0),
  Value = inline_references(maps:next(maps:iterator(Value1)), Spec, #{}),
  inline_references(maps:next(Iterator), Spec, Acc#{Key => Value});
inline_references({Key, Value0, Iterator}, Spec, Acc) when is_list(Value0) ->
  F = fun
        (Element) when is_map(Element) ->
          inline_references(maps:next(maps:iterator(Element)), Spec, #{});
        (Element) ->
          Element
      end,
  Value = lists:map(F, Value0),
  inline_references(maps:next(Iterator), Spec, Acc#{Key => Value});
inline_references({Key, Value, Iterator}, Spec, Acc) ->
  inline_references(maps:next(Iterator), Spec, Acc#{Key => Value}).

%% TODO: manage recursive reference.
maybe_resolve_ref(Spec, #{<<"$ref">> := <<"#", Ref/binary>>}) ->
  case json_pointer:find(Ref, Spec) of
    {ok, #{<<"$ref">> := _} = Term} ->
      maybe_resolve_ref(Spec, Term);
    {ok, Term} ->
      Term;
    {error, Reason} ->
      throw({error, {invalid_reference, Reason}})
  end;
maybe_resolve_ref(_, Term) ->
  Term.
