-module(openapi_gen).

-export([module_declaration/1,
         export_type_declaration/1, type_declaration/1,
         erlang_name/1, erlang_atom/1,
         comment/1]).

-export_type([type/0,
              error_reason/0]).

-type type() :: #{name := binary(),
                  args => [binary()],
                  data := iodata(),
                  comment => iodata()}.

-type error_reason() ::
        {invalid_unicode_data, unicode:chardata()}
      | {incomplete_unicode_data, unicode:chardata()}.

-spec module_declaration(Name :: iodata()) -> iodata().
module_declaration(Name) ->
  ["-module(", Name, ")", $\n].

-spec export_type_declaration([type()]) -> iodata().
export_type_declaration(Types) ->
  ["-export_type(",
   lists:join(",\n             ", [format_type(Type) || Type <- Types]),
   ").\n"].

-spec type_declaration(type()) -> iodata().
type_declaration(Type = #{name := Name, data := Data}) ->
  Args = maps:get(args, Type, []),
  Comment = case maps:find(comment, Type) of
              {ok, String} -> comment(String);
              error -> []
            end,
  [Comment,
   "-type ", Name, "(", [lists:join(", ", Args)] ,") ::\n",
   "        ", indent(Data, 10),  ".\n"].

-spec format_type(type()) -> iodata().
format_type(Type = #{name := Name}) ->
  Args = maps:get(args, Type, []),
  [Name, $/, integer_to_binary(length(Args))].

-spec erlang_name(binary()) -> binary().
erlang_name(Name) ->
  Name2 = re:replace(Name, "[^A-Za-z0-9_]+", "_",
                     [global, {return, binary}]),
  erlang_name(Name2, <<>>, undefined).

-spec erlang_name(binary(), binary(), pos_integer() | undefined) -> binary().
erlang_name(<<>>, Acc, _) ->
  string:lowercase(Acc);
erlang_name(<<C/utf8, Rest/binary>>, Acc, undefined) ->
  erlang_name(Rest, <<Acc/binary, C/utf8>>, C);
erlang_name(<<C/utf8, Rest/binary>>, Acc, LastC) when C >= $A, C =< $Z ->
  if
    LastC >= $A, LastC =< $Z ->
      case Rest of
        <<NextC/utf8, _/binary>> when NextC >= $a, NextC =< $z ->
          erlang_name(Rest, <<Acc/binary, $_, C/utf8>>, C);
        _ ->
          erlang_name(Rest, <<Acc/binary, C/utf8>>, C)
      end;
    LastC /= $_ ->
      erlang_name(Rest, <<Acc/binary, $_, C/utf8>>, C);
    true ->
      erlang_name(Rest, <<Acc/binary, C/utf8>>, C)
  end;
erlang_name(<<C/utf8, Rest/binary>>, Acc, _) ->
  erlang_name(Rest, <<Acc/binary, C/utf8>>, C).

-spec erlang_atom(binary()) -> binary().
erlang_atom(Name) ->
  case re:run(Name, "^[a-z][A-Za-z_@]*$") of
    {match, _} ->
      Name;
    nomatch ->
      Name2 = string:replace(Name, "'", "\\'", all),
      iolist_to_binary([$', Name2, $'])
  end.

-spec comment(iodata()) -> iodata().
comment(Data) ->
  Paragraphs =
    lists:map(fun (LineData) ->
                  case unicode:characters_to_list(LineData) of
                    "" ->
                      prettypr:break(prettypr:text(""));
                    Line ->
                      prettypr:text_par(Line)
                  end
              end, string:split(Data, "\n", all)),
  FilledText = prettypr:format(prettypr:sep(Paragraphs), 77),
  ["%% ", string:replace(FilledText, "\n", "\n%% ", all), $\n].

-spec indent(iodata(), pos_integer()) -> iodata().
indent(Data, N) ->
  S = [$\s || _ <- lists:seq(1, N)],
  string:replace(Data, "\n", ["\n", S], all).
