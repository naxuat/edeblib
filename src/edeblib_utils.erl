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

-module(edeblib_utils).

-include("edeblib_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-export([
    name_version/2,
    format_to_bin/2,
    format_hash/2,
    parse_hash/2,
    join/2,
    split_line/1,
    split_line/2,
    to_integer/1,
    compare/2,
    rstrip/1,
    first_cap/1,
    file_info/1,
    default_props/0,
    check_props/2,
    check_props/3
]).

% {{{ Global definitions
-define(MD5_FORMAT, "~32.16.0b").
-define(SHA1_FORMAT, "~40.16.0b").

-define(BLOCK_SIZE, 128*1024).

-define(DEFAULT_PROPS, [size, md5]).
% }}}

% {{{ Type definitions
-type check_error_kind() :: {'mismatch', atom(), term(), term()} |
                            {'missing_actual', atom()} |
                            {'missing_provided', atom()} |
                            {'missing_both', atom()}.
-type check_error() :: {'error', check_error_kind()}.

-type check_status() :: 'ok' | check_error().
% }}}

-spec name_version(Kind, Props) -> Result when
    Kind :: 'source' | 'binary',
    Props :: proplist(),
    Result :: {binary(), binary()}.

name_version(source, Props) ->
    {source, Name} = lists:keyfind(source, 1, Props),
    {version, Version} = lists:keyfind(version, 1, Props),
    {Name, Version};
name_version(binary, Props) ->
    {version, Version} = lists:keyfind(version, 1, Props),
    case lists:keyfind(source, 1, Props) of
        {source, Value} ->
            case split_line(Value) of
                [_] ->
                    {Value, Version};

                [SourceName, <<$(, Tempo/binary>>] ->
                    VersionSize = byte_size(Tempo) - 1,
                    <<SourceVersion:VersionSize/binary, $)>> = Tempo,
                    {SourceName, SourceVersion}
            end;

        false ->
            {package, Name} = lists:keyfind(package, 1, Props),
            {Name, Version}
    end.

-spec format_to_bin(Format, Data) -> Result when
    Format :: string(),
    Data :: term(),
    Result :: binary().

format_to_bin(Format, Data) ->
    list_to_binary(io_lib:format(Format, Data)).

format_hash(Hash, md5) ->
    <<MD5:128/big-unsigned-integer>> = Hash,
    format_to_bin(?MD5_FORMAT, [MD5]);
format_hash(Hash, sha1) ->
    <<SHA1:160/big-unsigned-integer>> = Hash,
    format_to_bin(?SHA1_FORMAT, [SHA1]).

parse_hash(Hash, md5) when is_binary(Hash), byte_size(Hash) == 32 ->
    {ok, [MD5], []} = io_lib:fread("~16u", binary_to_list(Hash)),
    <<MD5:128/big-unsigned-integer>>;
parse_hash(Hash, sha1) when is_binary(Hash), byte_size(Hash) == 40 ->
    {ok, [SHA1], []} = io_lib:fread("~16u", binary_to_list(Hash)),
    <<SHA1:160/big-unsigned-integer>>.

join([Value], _) ->
    Value;
join([_ | _]=Parts, Sep) ->
    [Last | Rest] = lists:reverse(Parts),
    lists:foldl(fun (X, Acc) -> <<X/binary, Sep/binary, Acc/binary>> end, Last, Rest).

split_line(Line) ->
    split_line(Line, <<" ">>).

split_line(Line, Sep) ->
    binary:split(Line, Sep, [global]).

to_integer(Binary) when is_binary(Binary) ->
    to_integer(binary_to_list(Binary));
to_integer(List) when is_list(List) ->
    case string:to_integer(List) of
        {error, _}=Result ->
            Result;

        {Value, _} ->
            Value
    end.

-spec compare(X, Y) -> Result when
    X :: term(),
    Y :: term(),
    Result :: -1 | 0 | 1.

compare(X, X) ->
    0;
compare(X, Y) when X < Y ->
    -1;
compare(_, _) ->
    1.

rstrip(String) when is_binary(String) ->
    rstrip(String, byte_size(String) - 1);
rstrip(String) when is_list(String) ->
    binary_to_list(rstrip(list_to_binary(String))).

rstrip(_, Prefix) when Prefix < 0 ->
    <<>>;
rstrip(String, Pos) ->
    case String of
        <<Prefix:Pos/binary, X, _/binary>> when X == $ ; X == 9; X == 10; X == 13 ->
            rstrip(Prefix, Pos - 1);

        _ ->
            Len = Pos + 1,
            <<Result:Len/binary, _/binary>> = String,
            Result
    end.

first_cap(<<X, Rest/binary>>) when X >= $a, X =< $z ->
    <<(X-32), Rest/binary>>;
first_cap(Value) ->
    Value.

file_info(FileName) ->
    {ok, Device} = file:open(FileName, [read, raw, binary]),
    Result = file_info(Device, ?BLOCK_SIZE, edeblib_sums:new()),
    file:close(Device),
    Result.

file_info(Device, BlockSize, Context) ->
    case file:read(Device, BlockSize) of
        {ok, Data} ->
            file_info(Device, BlockSize, edeblib_sums:update(Context, Data));

        eof ->
            edeblib_sums:final(Context)
    end.

-spec default_props() -> Result when
    Result :: [atom()].

default_props() ->
    ?DEFAULT_PROPS.

-spec check_props(ActualProps, ProvidedProps) -> Result when
    ActualProps :: proplist(),
    ProvidedProps :: proplist(),
    Result :: check_status().

check_props(ActualProps, ProvidedProps) ->
    check_props(ActualProps, ProvidedProps, default_props()).

-spec check_props(ActualProps, ProvidedProps, Keys) -> Result when
    ActualProps :: proplist(),
    ProvidedProps :: proplist(),
    Keys :: [atom()],
    Result :: check_status().

check_props(ActualProps, ProvidedProps, [Key | Rest]) ->
    case {lists:keyfind(Key, 1, ActualProps), lists:keyfind(Key, 1, ProvidedProps)} of
        {{_, Value}, {_, Value}} ->
            check_props(ActualProps, ProvidedProps, Rest);

        {{_, Actual}, {_, Provided}} ->
            {error, mismatch, {Key, Actual, Provided}};

        {false, {_, _}} ->
            {error, missing_actual, Key};

        {{_, _}, false} ->
            {error, missing_provided, Key};

        {false, false} ->
            {error, missing_both, Key}
    end;
check_props(_, _, []) ->
    ok.

-ifdef(TEST).
% {{{ Helpers needed for performance comparison
% These will be exported by export_all directive.
measure(Fun, Args) ->
    measure(Fun, Args, 1000000).

measure(Fun, Args, N) ->
    Start = erlang:now(),
    many_runs(Fun, Args, N),
    timer:now_diff(erlang:now(), Start).

many_runs(Fun, Args, N) when N >= 1 ->
    apply(Fun, Args),
    many_runs(Fun, Args, N-1);
many_runs(_, _, 0) ->
    ok.
% }}}

name_version_test_() ->
    {"Tests for name_version", [
        {"Tests for source stanza", [
            ?_assertEqual({<<"hello">>, <<"1.0">>},
                          name_version(source, [{source, <<"hello">>}, {version, <<"1.0">>}]))
            % TODO: add tests for badly formatted source stanza
        ]},
        {"Tests for binary stanza", [
            ?_assertEqual({<<"hello">>, <<"1.0">>},
                          name_version(binary, [{package, <<"hello">>}, {version, <<"1.0">>}])),
            ?_assertEqual({<<"ehllo">>, <<"1.0">>},
                          name_version(binary, [{package, <<"hello">>}, {source, <<"ehllo">>}, {version, <<"1.0">>}])),
            ?_assertEqual({<<"ehllo">>, <<"2.0">>},
                          name_version(binary, [{package, <<"hello">>}, {source, <<"ehllo (2.0)">>}, {version, <<"1.0">>}])),
            ?_assertException(error, {case_clause, _},
                          name_version(binary, [{package, <<"hello">>}, {source, <<"ehllo 2.0">>}, {version, <<"1.0">>}]))
            % TODO: add tests for badly formatted binary stanza
        ]}
    ]}.

to_integer_test_() ->
    {"Test conversion of a string/binary to integer", [
        ?_assertEqual(1, to_integer("1")),
        ?_assertEqual(1, to_integer(<<"1">>)),
        ?_assertMatch({error, _}, to_integer("A"))
    ]}.

compare_test_() ->
    {"compare tests", [
        ?_assertEqual(0, compare(0, 0)),
        ?_assertEqual(0, compare(something, something)),
        ?_assertEqual(-1, compare(-1, 0)),
        ?_assertEqual(1, compare(1, 0))
    ]}.

rstrip_test_() ->
    {"edeblib_utils:rstrip tests", [
        ?_assertEqual(<<"blah">>, rstrip(<<"blah">>)),
        ?_assertEqual(<<"blah">>, rstrip(<<"blah ">>)),
        ?_assertEqual(<<"blah">>, rstrip(<<"blah", 9>>)),
        ?_assertEqual(<<"blah">>, rstrip(<<"blah", 10>>)),
        ?_assertEqual(<<"blah">>, rstrip(<<"blah", 13>>)),
        ?_assertEqual(<<"blah">>, rstrip(<<"blah", 13, 10>>)),
        ?_assertEqual(<<"blah">>, rstrip(<<"blah        ">>)),
        ?_assertEqual("blah", rstrip("blah        ")),
        ?_assertEqual("", rstrip(""))
    ]}.

check_props_test() ->
    FileInfo = [
        {md5,<<197,109,186,109,175,244,61,2,70,52,250,214,23,72,239,156>>},
        {sha1,<<180,33,100,219,103,33,14,21,200,207,152,52,86,32,11,199,32,223,63,120>>},
        {size,491}
    ],
    ?assertEqual(ok, check_props(FileInfo, FileInfo, default_props())),
    ?assertEqual(ok, check_props(FileInfo, FileInfo, [md5, size])),
    ?assertEqual(ok, check_props(FileInfo, FileInfo, [sha1, size])),
    {_, _, NoMD5} = lists:keytake(md5, 1, FileInfo),
    ?assertEqual(check_props(NoMD5, NoMD5), check_props(NoMD5, NoMD5, default_props())),
    ?assertEqual({error,missing_both,md5}, check_props(NoMD5, NoMD5, default_props())),
    ?assertEqual({error,missing_provided,md5}, check_props(FileInfo, NoMD5, default_props())),
    ?assertEqual({error,missing_actual,md5}, check_props(NoMD5, FileInfo, default_props())),
    ?assertEqual(ok, check_props(NoMD5, FileInfo, [sha1, size])),
    BadMD5 = lists:keyreplace(md5, 1, FileInfo, {md5, <<"bad-bad-bad">>}),
    ?assertMatch({error,mismatch,{md5, _, _}}, check_props(FileInfo, BadMD5, default_props())).

format_hash_test_() ->
    MD5 = <<197,109,186,109,175,244,61,2,70,52,250,214,23,72,239,156>>,
    SHA1 = <<180,33,100,219,103,33,14,21,200,207,152,52,86,32,11,199,32,223,63,120>>,
    {"format_hash simple tests", [
        ?_assertEqual(MD5, parse_hash(format_hash(MD5, md5), md5)),
        ?_assertEqual(SHA1, parse_hash(format_hash(SHA1, sha1), sha1))
    ]}.
-endif.

% vim:sw=4:ts=4:et:ai
