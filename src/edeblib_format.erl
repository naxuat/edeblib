%% {{{ Copyright Notice
%%
%% This file is part of edeblib.
%%
%% Copyright (C) 2011 by the edeblib contributors.  Please see file AUTHORS for a complete list.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% }}}

-module(edeblib_format).

-include("edeblib_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

% main exports
-export([
    format_section/2,
    format_section/3
]).

% helper exports
-export([
    headers/1,
    ignore_unknown/1,
    format_unknown/1
]).

% unit test exports
-export([
    atom_to_header/1
]).

-type section_kind() :: 'source' | 'binary' | 'release'.
-type header_kind() :: 'mandatory' | 'optional'.
-type header_format_kind() :: 'flatten' | 'wrap' | 'files'.
-type header_type() :: {atom(), binary(), header_kind(), header_format_kind()}.
-type format_result() :: binary() |
                         {'missing_header', atom()}.
-type format_unhandled_fun() :: fun(({atom(), term()}) -> {'ok', binary()} | 'ignore').

-spec format_section(Kind, Props) -> Result when
    Kind :: section_kind(),
    Props :: proplist(),
    Result :: format_result().

format_section(Kind, Props) ->
    format_section(Kind, Props, fun ignore_unknown/1).

-spec format_section(Kind, Props, UnhandledFun) -> Result when
    Kind :: section_kind(),
    Props :: proplist(),
    UnhandledFun :: format_unhandled_fun(),
    Result :: format_result().

% prototype for the future format_section
% where
%   UnhandledFun = fun ({Key, Value}) -> {ok, Data} | ignore.
format_section(Kind, Props, UnhandledFun) when is_atom(Kind) ->
    format_section(headers(Kind), Props, <<>>, UnhandledFun);
format_section(Headers, Props, UnhandledFun) when is_list(Headers)->
    format_section(Headers, Props, <<>>, UnhandledFun).

-spec format_section(Header, Props, Output, UnhandledFun) -> Result when
    Header :: header_type(),
    Props :: proplist(),
    Output :: binary(),
    UnhandledFun :: format_unhandled_fun(),
    Result :: format_result().

format_section([{Key, Header, Flag, Kind} | Rest], Props, Output, UnhandledFun) ->
    case lists:keytake(Key, 1, Props) of
        {value, {Key, Value}, NewProps} ->
            Data = format_header(Header, Value, Kind),
            format_section(Rest, NewProps, <<Output/binary, Data/binary>>, UnhandledFun);

        false ->
            case Flag of
                mandatory ->
                    {missing_header, Key};

                _ ->
                    format_section(Rest, Props, Output, UnhandledFun)
            end
    end;
format_section([]=Empty, [Prop | Rest], Output, UnhandledFun) ->
    case UnhandledFun(Prop) of
        {ok, Data} ->
            format_section(Empty, Rest, <<Output/binary, Data/binary>>, UnhandledFun);

        ignore ->
            format_section(Empty, Rest, Output, UnhandledFun)
    end;
format_section([], _, Output, _) ->
    Output.

-spec format_header(Header, Value, Kind) -> Result when
    Header :: binary(),
    Value :: term(),
    Kind :: header_format_kind(),
    Result :: binary().

format_header(Header, Value, Kind) ->
    FormattedValue = format_value(Value, Kind),
    case FormattedValue of
        <<X, _/binary>> when X == 32; X == $\n ->
            <<Header/binary, ":", FormattedValue/binary, "\n">>;

        <<>> ->
            <<Header/binary, ":\n">>;

        _ ->
            <<Header/binary, ": ", FormattedValue/binary, "\n">>
    end.

-spec format_value(Value, Kind) -> Result when
    Value :: term(),
    Kind :: header_format_kind(),
    Result :: binary().

format_value(Value, _) when is_binary(Value) ->
    Value;
format_value(Value, _) when is_integer(Value) ->
    edeblib_utils:format_to_bin("~b", [Value]);
format_value([], _) ->
    <<>>;
format_value(Bits, flatten) ->
    edeblib_utils:join(Bits, <<" ">>);
format_value(Bits, wrap) ->
    edeblib_utils:join(Bits, <<"\n ">>);
format_value(Files, files) ->
    format_value([<<"">> | Files], wrap).

% Various formatting helpers
-spec headers(Kind) -> Result when
    Kind :: section_kind(),
    Result :: [header_type()].

headers(source) ->
    source_index_headers();
headers(binary) ->
    binary_index_headers();
headers(release) ->
    release_headers().

-spec ignore_unknown({atom(), term()}) -> 'ignore'.

ignore_unknown({_, _}) ->
    ignore.

-spec format_unknown({atom(), term()}) -> {'ok', binary()}.

format_unknown({Key, Value}) ->
    {ok, format_header(atom_to_header(Key), Value, wrap)}.

-spec atom_to_header(Atom) -> Result when
    Atom :: atom(),
    Result :: binary().

atom_to_header(Atom) ->
    Value = atom_to_binary(Atom, utf8),
    edeblib_utils:join(lists:map(fun edeblib_utils:first_cap/1, edeblib_utils:split_line(Value, <<"_">>)), <<"-">>).

% {{{ Standard headers for various Debian artefacts

-spec source_index_headers() -> Result when
    Result :: [header_type()].

source_index_headers() ->
    [
        {source, <<"Package">>, mandatory, flatten},
        {binary, <<"Binary">>, mandatory, wrap},
        {version, <<"Version">>, mandatory, flatten},
        {priority, <<"Priority">>, optional, flatten},
        {section, <<"Section">>, optional, flatten},
        {maintainer, <<"Maintainer">>, mandatory, wrap},
        {original_maintainer, <<"Original-Maintainer">>, optional, wrap},
        {build_depends, <<"Build-Depends">>, optional, wrap},
        {build_depends_indep, <<"Build-Depends-Indep">>, optional, wrap},
        {build_conflicts, <<"Build-Conflicts">>, optional, wrap},
        {build_conflicts_indep, <<"Build-Conflicts-Indep">>, optional, wrap},
        {architecture, <<"Architecture">>, mandatory, wrap},
        {standards_version, <<"Standards-Version">>, mandatory, wrap},
        {format, <<"Format">>, mandatory, flatten},
        {directory, <<"Directory">>, mandatory, flatten},
        {files, <<"Files">>, mandatory, files},
        {sha1, <<"Checksums-Sha1">>, mandatory, files},
        {sha256, <<"Checksums-Sha256">>, optional, files}
    ].

-spec binary_index_headers() -> Result when
    Result :: [header_type()].

binary_index_headers() ->
    [
        {package, <<"Package">>, mandatory, flatten},
        {essential, <<"Essential">>, optional, flatten},
        {status, <<"Status">>, optional, flatten},
        {priority, <<"Priority">>, mandatory, flatten},
        {section, <<"Section">>, mandatory, flatten},
        {installed_size, <<"Installed-Size">>, mandatory, flatten},
        {maintainer, <<"Maintainer">>, mandatory, flatten},
        {original_maintainer, <<"Original-Maintainer">>, optional, flatten},
        {architecture, <<"Architecture">>, mandatory, flatten},
        {source, <<"Source">>, optional, flatten},
        {version, <<"Version">>, mandatory, flatten},
        {revision, <<"Revision">>, optional, flatten},                  % Obsolete
        {config_version, <<"Config-Version">>, optional, flatten},      % Obsolete
        {replaces, <<"Replaces">>, optional, wrap},
        {provides, <<"Provides">>, optional, wrap},
        {depends, <<"Depends">>, optional, wrap},
        {pre_depends, <<"Pre-Depends">>, optional, wrap},
        {recommends, <<"Recommends">>, optional, wrap},
        {suggests, <<"Suggests">>, optional, wrap},
        {conflicts, <<"Conflicts">>, optional, wrap},
        {breaks, <<"Breaks">>, optional, wrap},
        {conffiles, <<"Conffiles">>, optional, wrap},
        {filename, <<"Filename">>, mandatory, flatten},
        {size, <<"Size">>, mandatory, flatten},
        {md5sum, <<"MD5Sum">>, mandatory, flatten},
        {sha1, <<"SHA1">>, mandatory, flatten},
        {sha256, <<"SHA256">>, optional, flatten},
        {msdos_filename, <<"MSDOS-Filename">>, optional, flatten},      % Obsolete
        {description, <<"Description">>, mandatory, wrap}
    ].

-spec release_headers() -> Result when
    Result :: [header_type()].

release_headers() ->
    [
        {origin, <<"Origin">>, optional, flatten},
        {label, <<"Label">>, optional, flatten},
        {suite, <<"Suite">>, optional, flatten},
        {version, <<"Version">>, optional, flatten},
        {codename, <<"Codename">>, optional, flatten},
        {date, <<"Date">>, optional, flatten},
        {valid_until, <<"Valid-Until">>, optional, flatten},
        {architectures, <<"Architectures">>, optional, flatten},
        {components, <<"Components">>, optional, flatten},
        {description, <<"Description">>, optional, wrap},
        {md5sum, <<"MD5Sum">>, mandatory, files},
        {sha1, <<"SHA1">>, mandatory, files},
        {sha256, <<"SHA256">>, optional, files}
    ].

% }}}

-ifdef(TEST).
format_section_test_() ->
    {"Test for format_section", [
        ?_assertEqual(<<"Hello: World\n">>,
                      format_section([{hello, <<"Hello">>, optional, flatten}], [{hello, <<"World">>}])),
        ?_assertEqual(<<"Value: 256\n">>,
                      format_section([{value, <<"Value">>, optional, flatten}], [{value, 256}])),
        ?_assertEqual(<<"Value:\n">>,
                      format_section([{value, <<"Value">>, optional, flatten}], [{value, []}])),
        ?_assertEqual(<<"Value:\n">>,
                      format_section([{value, <<"Value">>, optional, flatten}], [{value, []}, {other, <<"Value">>}])),
        ?_assertEqual(<<"Value:\nOther: Value\n">>,
                      format_section([{value, <<"Value">>, optional, flatten}],
                                                      [{value, []}, {other, <<"Value">>}],
                                                      fun format_unknown/1)),
        ?_assertEqual({missing_header, important},
                      format_section([{important, <<"Important">>, mandatory, flatten}], [{hello, <<"World">>}])),
        ?_assertEqual(<<>>,
                      format_section([{important, <<"Important">>, optional, flatten}], [{hello, <<"World">>}])),
        ?_assertEqual(<<"Header: Hello World\n">>,
                      format_section([{header, <<"Header">>, optional, flatten}], [{header, [<<"Hello">>, <<"World">>]}])),
        ?_assertEqual(<<"Header: Hello\n World\n">>,
                      format_section([{header, <<"Header">>, optional, wrap}], [{header, [<<"Hello">>, <<"World">>]}])),
        ?_assertEqual(<<"Header:\n Hello\n World\n">>,
                      format_section([{header, <<"Header">>, optional, wrap}], [{header, [<<>>, <<"Hello">>, <<"World">>]}])),
        ?_assertEqual(<<"Header:\n Hello\n World\n">>,
                      format_section([{header, <<"Header">>, optional, files}], [{header, [<<"Hello">>, <<"World">>]}]))
    ]}.

atom_to_header_test_() ->
    {"Test of atom_to_header", [
        ?_assertEqual(<<"">>, atom_to_header('')),
        ?_assertEqual(<<"Hello">>, atom_to_header(hello)),
        ?_assertEqual(<<"Hello-World">>, atom_to_header(hello_world))
    ]}.

headers_test_() ->
    {"Simple check for headers/1 helper", [
        ?_assertEqual(source_index_headers(), headers(source)),
        ?_assertEqual(binary_index_headers(), headers(binary)),
        ?_assertEqual(release_headers(), headers(release))
    ]}.
-endif.

% vim:sw=4:ts=4:et:ai
