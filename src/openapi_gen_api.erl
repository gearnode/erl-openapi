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
    Data = do_generate(Spec, Options),
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
  Funs0 = generate_paths_operations(Paths, Spec, Options),
  Funs = lists:sort(fun ({Name1, _}, {Name2, _}) ->
                        Name1 =< Name2
                    end, Funs0),
  FunSignatures = [[Name, "/1, ", Name, "/2"] || {Name, _} <- Funs],
  ModuleName = module_name(Options),
  [openapi_gen:header(),
   openapi_gen:module_declaration(ModuleName), $\n,
   openapi_gen:export_declaration(FunSignatures), $\n,
   lists:join($\n, [Data || {_, Data} <- Funs])].

-spec generate_paths_operations(#{binary() := openapi:path()},
                                openapi:specification(),
                                openapi_gen:options()) ->
        [{binary(), iodata()}].
generate_paths_operations(Paths, Spec, Options) ->
  maps:fold(fun (URI, Path, Acc) ->
                Acc ++ generate_path_operations(URI, Path, Spec, Options)
            end, [], Paths).

-spec generate_path_operations(binary(), openapi:path(),
                               openapi:specification(),
                               openapi_gen:options()) ->
        [{binary(), iodata()}].
generate_path_operations(URI, Path, Spec, Options) ->
  Methods = [get, put, post, delete, options, head, patch],
  maps:fold(fun (Method, Op, Acc) ->
                [generate_operation(Method, Op, URI, Path, Spec,
                                    Options) | Acc]
            end, [], maps:with(Methods, Path)).

-spec generate_operation(atom(), openapi:operation(),
                         binary(), openapi:path(),
                         openapi:specification(), openapi_gen:options()) ->
        {FunName :: binary(), iodata()}.
generate_operation(Method, Op = #{operationId := Id}, URI, _Path, _Spec,
                   Options) ->
  Name = openapi_gen:name(Id, Options),
  InputType = "any", % TODO
  OutputType = "any", % TODO
  OutputSignature = ["{ok, ", OutputType, "()} | {error, term()}"],
  Comment = generate_operation_comment(Op, URI, Method),
  Fun1Data =
    ["-spec ", Name, "(", InputType,"()) ->\n",
     "        ", OutputSignature, ".\n",
     Name, "(Input) ->\n",
     "  ", Name, "(Input, #{}).\n\n"],
  Fun2Data =
    ["-spec ", Name, "(", InputType,"(), mhttp:request_options()) ->\n",
     "        ", OutputSignature, ".\n",
     Name, "(_Input, _Options) ->\n",
     "  ", "{error, unimplemented}.\n"], % TODO
  {Name, [Comment, Fun1Data, Fun2Data]}.

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
