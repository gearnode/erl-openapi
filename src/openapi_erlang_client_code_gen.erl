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

-module(openapi_erlang_client_code_gen).

-import(prettypr,
        [text/1,
         null_text/1,
         nest/2,
         above/2,
         beside/2,
         sep/1,
         par/1,
         par/2,
         floating/3,
         floating/1,
         break/1,
         follow/2,
         follow/3,
         empty/0]).

-export_type([error_reason/0]).

-export([generate/3]).

-type error_reason() :: term().

-spec generate(openapi:specification(), file:name_all(),
               openapi:generate_options()) ->
        ok | {error, error_reason()}.
generate(Spec, OutDir, Options) ->
  Now = os:system_time(second),
  Datetime = calendar:system_time_to_rfc3339(Now, [{offset, "Z"}]),
  PackageName = maps:get(package_name, Options),

  F1 = generate_openapi_file(Datetime, PackageName, Spec, Options),
  F2 = generate_model_file(Datetime, PackageName, Spec, Options),

  file:write_file(filename:join(OutDir, <<PackageName/binary, "_openapi.erl">>), F1),
  file:write_file(filename:join(OutDir, <<PackageName/binary, "_schemas.erl">>), F2),
  ok.

generate_model_file(Datetime, PackageName, Spec, Options) ->
  Schemas = maps:get(schemas, maps:get(components, Spec, #{}), #{}),
  Data = #{datetime => Datetime,
           package_name => <<PackageName/binary, "_model">>,
           types => generate_types(Schemas, Options),
           functions => []},
  openapi_mustache:render(<<"erlang-client/model.erl">>, Data).

generate_types(Schemas, Options) ->
  Iterator = maps:iterator(Schemas),
  generate_types(maps:next(Iterator), Options, []).

generate_types(none, _, Acc) ->
  lists:reverse(Acc);
generate_types({Name, Schema, I}, Options, Acc) ->
  Type = #{comment => type_comment(Name, Schema, Options),
           name => openapi_generator:to_snake_case(Name, #{}),
           value => type_definition(Schema, 8)},
  generate_types(maps:next(I), Options, [Type | Acc]).

-spec type_comment(binary(), openapi:schema(), openapi:generate_options()) -> binary().
type_comment(Name, Schema, Options) ->
  Text =
    case maps:find(description, Schema) of
      {ok, Value} when Value =/= <<>> ->
        [Name, "\n\n", Value];
      _ ->
        Name
    end,
  unicode:characters_to_binary(
    openapi_code:comment("%%", Text, 0, Options)).

-spec indent(non_neg_integer()) -> iodata().
indent(0) ->
  "";
indent(Size) ->
  lists:map(fun (_) -> $\s end, lists:seq(1, Size)).

-spec type_definition(openapi:schema(), non_neg_integer()) -> binary().
type_definition(#{type := object, properties := Props} = Schema, Indent) ->
  Required = maps:get(required, Schema, []),
  F = fun (Name, Schema2, Acc) ->
          Operator =
            case lists:member(Name, Required) of
              true -> ":=";
              false -> "=>"
            end,
          case Schema2 of
            #{type := object} ->
              [[Name, $\s, Operator, $\n,
                indent(Indent + 8), type_definition(Schema2, Indent + 8)] | Acc];
            #{type := string, enum := [_]} ->
              [[Name, $\s, Operator, $\s, type_definition(Schema2, Indent + 8)] | Acc];
            #{type := string, enum := _} ->
              [[Name, $\s, Operator, $\n,
               indent(Indent + 8), type_definition(Schema2, Indent + 8)] | Acc];
            _ ->
              [[Name, $\s, Operator, $\s, type_definition(Schema2, Indent)] | Acc]
          end
      end,
  Definition = maps:fold(F, [], Props),
  unicode:characters_to_binary(["#{", lists:join([",\n", indent(Indent + 2)], Definition), "}"]);
type_definition(#{type := object}, _) ->
  <<"json:value()">>;
type_definition(#{type := integer}, _) ->
  <<"integer()">>;
type_definition(#{type := number}, _) ->
  <<"number()">>;
type_definition(#{type := boolean}, _) ->
  <<"boolean()">>;
type_definition(#{type := array}, _) ->
  <<"list()">>;
type_definition(#{type := string, enum := Enum}, Size) ->
  case Enum of
    [Value] ->
      unicode:characters_to_binary(Value);
    _ ->
      Prefix = ["\n", openapi_string:text_indent(Size), "| "],
      unicode:characters_to_binary(
        [openapi_string:text_indent(2),
         lists:join(Prefix, Enum)])
  end;
type_definition(#{type := string}, _) ->
  <<"binary()">>;
type_definition(#{'$ref' := Ref}, _) ->
  Name = lists:last(Ref),
  <<Name/binary, "()">>;
type_definition(_, _) ->
  <<"json:value()">>.

generate_openapi_file(Datetime, PackageName, Spec, Options) ->
  Data = #{datetime => Datetime,
           package_name => <<PackageName/binary, "_openapi">>,
           functions => [info_fun_0(Spec), servers_fun_0(Spec)]},
  openapi_mustache:render(<<"erlang-client/openapi.erl">>, Data).

-spec info_fun_0(openapi:specification()) -> binary().
info_fun_0(#{info := Info}) ->
  Data = io_lib:format("~p.", [Info]),
  Lines = string:split(Data, "\n", all),
  Documents = lists:map(fun (Line) -> text(Line) end, Lines),
  iolist_to_binary(
    prettypr:format(
      sep(
        [beside(
           break(text("-spec info() -> info().")),
           break(text("info() ->"))),
         nest(4, beside(
                   sep(Documents),
                   empty())),
         empty()]))).

-spec servers_fun_0(openapi:specification()) -> iodata().
servers_fun_0(#{servers := Servers}) ->
  Data = io_lib:format("~p.", [Servers]),
  Lines = string:split(Data, "\n", all),
  Documents = lists:map(fun (Line) -> text(Line) end, Lines),
  iolist_to_binary(
    prettypr:format(
      sep(
        [beside(
           break(text("-spec servers() -> [server()].")),
           break(text("servers() ->"))),
         nest(4, beside(
                   sep(Documents),
                   empty())),
         empty()])));
servers_fun_0(_) ->
  "".
