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

-module(openapi_path_template).

-export([parse/1]).

-export_type([path_template/0]).

-type path_template() :: {Format :: iolist(), VariableNames :: [binary()]}.

-spec parse(binary()) -> path_template().
parse(Bin) ->
  parse(Bin, [], []).

parse(<<>>, Format, Variables) ->
  {lists:reverse(Format), lists:reverse(Variables)};
parse(<<${, Rest/binary>>, Format, Variables) ->
  F = fun
        Next(<<>>, VarName) ->
          {lists:reverse(VarName), <<>>};
        Next(<<$}, R/binary>>, VarName) ->
          {lists:reverse(VarName), R};
        Next(<<C/utf8, R/binary>>, Acc2) ->
          Next(R, [C | Acc2])
      end,
  {VariableName, Rest2} = F(Rest, []),
  parse(Rest2, ["%s" | Format], [iolist_to_binary(VariableName) | Variables]);
parse(<<C/utf8, Rest/binary>>, Format, Variables) ->
  parse(Rest, [C | Format], Variables).


%% -spec format(path_template(), [openapi:parameter()]) -> iolist().
%% format({Format, VariableNames}, Parameters) ->
%%   io_lib:format(Format, []).
