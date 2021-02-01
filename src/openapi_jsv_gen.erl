-module(openapi_jsv_gen).

-export([generate/1, generate/2]).

-spec generate(openapi:specification()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate(Spec) ->
  generate(Spec, #{}).

-spec generate(openapi:specification(), openapi_gen:options()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate(Spec, Options) ->
  try
    Data = do_generate(Spec, Options),
    case maps:get(return_binary, Options, false) of
      true ->
        case unicode:characters_to_binary(Data) of
          Bin when is_binary(Bin) ->
            {ok, Bin};
          {error, _, Rest} ->
            {error, {invalid_unicode_data, Rest}};
          {incomplete, _, Rest} ->
            {error, {incomplete_unicode_data, Rest}}
        end;
      false ->
        {ok, Data}
    end
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec do_generate(openapi:specification(), openapi_gen:options()) -> iodata().
do_generate(Spec = #{definitions := Definitions}, Options) ->
  ModuleName = [maps:get(module_prefix, Options, ""), "jsv"],
  [openapi_gen:header(),
   openapi_gen:module_declaration(ModuleName), $\n].
