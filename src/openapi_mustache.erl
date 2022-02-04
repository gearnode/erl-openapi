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

-module(openapi_mustache).

-export([render/2, render/3]).

-export_type([data/0, key/0, value/0]).

-type data() ::
        #{key() := value()}
      | [data()].

-type key() :: atom(). % subset of mustache:context_key()

-type value() ::
        mustache:context_value()
      | {template, mustache:template_name(), data()}.

-spec template_directory() -> file:name_all().
template_directory() ->
  case code:priv_dir(openapi) of
    {error, _} ->
      error(missing_private_directory);
    Dir ->
      filename:join(Dir, "mustache")
  end.

-spec render(mustache:template_name(), data()) -> iodata().
render(Name, Data) ->
  render(Name, Data, #{}).

-spec render(mustache:template_name(), data(), mustache:options()) -> iodata().
render(Name, Data, Options) ->
  render_1(Name, normalize_data(Data), Options).

-spec render_1(mustache:template_name(), mustache:context()) -> iodata().
render_1(Name, Context) ->
  render_1(Name, Context, #{}).

-spec render_1(mustache:template_name(), mustache:context(),
               mustache:options()) ->
        iodata().
render_1(Name, Context, ExtraOptions) ->
  DefaultOptions =
    #{template_directory => template_directory(),
      disable_html_escaping => true,
      error_on_unknown_variable => true,
      error_on_unknown_partial => true,
      error_on_invalid_partial => true,
      enable_cache => true},
  Options = maps:merge(DefaultOptions, ExtraOptions),
  case mustache:render(Name, Context, Options) of
    {ok, Data} ->
      Data;
    {error, Error} ->
      error({mustache, Error, Name, Context})
  end.

-spec normalize_data(data()) -> mustache:context().
normalize_data(Data) ->
  maps:map(fun
             (_, V) when is_map(V) ->
               normalize_data(V);
             (_, {template, Name, Data2}) ->
               {data, render_1(Name, Data2)};
             (_, V) ->
               V
           end, Data).
