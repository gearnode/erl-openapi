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

-module(openapi_gen_api).

-behaviour(openapi_gen).

-export([module_name/1, generate/2]).

-spec module_name(openapi_gen:options()) -> binary().
module_name(Options) ->
  <<(maps:get(module_prefix, Options, <<>>))/binary, "api">>.

-spec generate(openapi:specification(), openapi_gen:options()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate(Spec, Options) ->
  try
    Data0 = do_generate(Spec, Options),
    Data = openapi_gen:remove_trailing_whitespaces(Data0),
    case maps:get(return_binary, Options, false) of
      true ->
        openapi_gen:unicode_iolist_to_binary(Data);
      false ->
        {ok, Data}
    end
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec do_generate(openapi:specification(), openapi_gen:options()) -> iodata().
do_generate(Spec = #{paths := Paths}, Options) ->
  Ops0 = generate_paths_operations(Paths, Spec, Options),
  Ops = lists:sort(fun ({Name1, _, _}, {Name2, _, _}) ->
                       Name1 =< Name2
                   end, Ops0),
  FunSignatures = [[Name, "/1, ", Name, "/2"] || {Name, _, _} <- Ops],
  Types = [OutputType || {_, _, OutputType} <- Ops],
  ModuleName = module_name(Options),
  [openapi_gen:header(),
   openapi_gen:module_declaration(ModuleName), $\n,
   openapi_gen:export_declaration(FunSignatures), $\n,
   openapi_gen:export_type_declaration(Types), $\n,
   lists:join($\n, [Data || {_, Data, _} <- Ops])].

-spec generate_paths_operations(#{binary() := openapi:path()},
                                openapi:specification(),
                                openapi_gen:options()) ->
        [{binary(), iodata(), openapi_gen:type()}].
generate_paths_operations(Paths, Spec, Options) ->
  maps:fold(fun (URI, Path, Acc) ->
                Acc ++ generate_path_operations(URI, Path, Spec, Options)
            end, [], Paths).

-spec generate_path_operations(binary(), openapi:path(),
                               openapi:specification(),
                               openapi_gen:options()) ->
        [{binary(), iodata(), openapi_gen:type()}].
generate_path_operations(URI, Path, Spec, Options) ->
  Methods = [get, put, post, delete, options, head, patch],
  maps:fold(fun (Method, Op, Acc) ->
                [generate_operation(Method, Op, URI, Path, Spec,
                                    Options) | Acc]
            end, [], maps:with(Methods, Path)).

-spec generate_operation(atom(), openapi:operation(),
                         binary(), openapi:path(),
                         openapi:specification(), openapi_gen:options()) ->
        {FunName :: binary(), iodata(), openapi_gen:type()}.
generate_operation(Method, Op = #{operationId := Id}, URI, _Path, _Spec,
                   Options) ->
  Name = openapi_gen:name(Id, Options),
  InputType = "any()", % TODO
  OutputType = generate_operation_output_type(Name, Op),
  OutputSignature = ["{ok, ", maps:get(name, OutputType), "()}",
                     " | {error, term()}"],
  Comment = generate_operation_comment(Op, URI, Method),
  Fun1Data =
    ["-spec ", Name, "(", InputType,") ->\n",
     "        ", OutputSignature, ".\n",
     Name, "(Input) ->\n",
     "  ", Name, "(Input, #{}).\n\n"],
  Fun2Data =
    ["-spec ", Name, "(", InputType,", mhttp:request_options()) ->\n",
     "        ", OutputSignature, ".\n",
     Name, "(_Input, _Options) ->\n",
     "  ", "{error, unimplemented}.\n"], % TODO
  Data = [Comment,
          openapi_gen:type_declaration(OutputType), $\n,
          Fun1Data,
          Fun2Data],
  {Name, Data, OutputType}.

-spec generate_operation_output_type(OpName :: binary(),
                                     openapi:operation()) ->
        openapi_gen:type().
generate_operation_output_type(OpName, #{responses := Responses}) ->
  DefaultType =
    case maps:find(<<"default">>, Responses) of
      {ok, _Response} ->
        ["{mhttp:status(), mhttp:header(), ",
         "json:value()", % TODO
         "}"];
      error ->
        ["{mhttp:status(), mhttp:header(), json:value()}"]
    end,
  Types =
    maps:fold(fun (StatusString, _Response, Acc) ->
                  Status = case StatusString of
                             <<"default">> -> "mhttp:status()";
                             _ -> StatusString
                           end,
                  Type = ["{", Status, ", mhttp:header(), ",
                          "json:value()", % TODO
                          "}"],
                  [Type | Acc]
              end, [], maps:without([<<"default">>], Responses)),
  #{name => <<OpName/binary, "_output">>,
    data => lists:join(" |\n", Types ++ [DefaultType])}.

-spec generate_operation_comment(openapi:operation(), binary(), atom()) ->
        iodata().
generate_operation_comment(Op, URI, Method) ->
  IdData =
    case maps:find(operationId, Op) of
      {ok, Id} -> ["Operation: ", Id, $\n];
      error -> []
    end,
  CallData = [string:uppercase(atom_to_binary(Method)), " ", URI, $\n],
  DescriptionData =
    case maps:find(description, Op) of
      {ok, Description} ->
        [$\n, string:titlecase(Description), $.];
      error ->
        []
    end,
  Data = [IdData, CallData, DescriptionData],
  openapi_gen:comment(Data).
