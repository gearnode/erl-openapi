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

-module(openapi_generator).

-export_type([error_reason/0]).

-export([generate/3,
         to_snake_case/2]).

-type error_reason() ::
        {unsupported_language, atom()}
      | {unsupported_generator, atom()}
      | openapi_erlang_client_code_gen:error_reason().

-spec supported_generators() -> map().
supported_generators() ->
  #{erlang =>
      #{client => openapi_erlang_client_code_gen}}.

-spec generate(json:value(), file:name_all(), openapi:generate_options()) ->
        ok | {error, error_reason()}.
generate(Data, OutDir,
         #{language := Language, generator := Generator} = Options) ->
  case maps:find(Language, supported_generators()) of
    {ok, Generators} ->
      case maps:find(Generator, Generators) of
        {ok, Mod} ->
          Mod:generate(Data, OutDir, Options);
        error ->
          {error, {unsupported_generator, Generator}}
      end;
    error ->
      {error, {unsupported_language, Language}}
  end.

to_snake_case(Bin, Inflection) ->
  NewBin =
    maps:fold(fun (SearchPattern, Replacement, String) ->
                  string:replace(String, SearchPattern, Replacement, all)
              end, Bin, Inflection),
  iolist_to_binary(to_snake_case_1(iolist_to_binary(NewBin), [])).

to_snake_case_1(<<>>, Acc) ->
  lists:reverse(Acc);
to_snake_case_1(<<".", Rest/binary>>, Acc) ->
  to_snake_case_1(Rest, [$_ | Acc]);
to_snake_case_1(<<"-", Rest/binary>>, Acc) ->
  to_snake_case_1(Rest, [$_ | Acc]);
to_snake_case_1(<<"_", Rest/binary>>, [$_ | _] = Acc) ->
  to_snake_case_1(Rest, Acc);
to_snake_case_1(<<C, Rest/binary>>, [$_ | _] = Acc) when C >= $A, C =< $Z ->
  to_snake_case_1(Rest, [C + 32 | Acc]);
to_snake_case_1(<<C, Rest/binary>>, Acc) when C >= $A, C =< $Z ->
  to_snake_case_1(Rest, [[$_, C + 32] | Acc]);
to_snake_case_1(<<A, Rest/binary>>, Acc) ->
  to_snake_case_1(Rest, [A | Acc]).
