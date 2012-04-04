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

-module(edeblib_load).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-export([
    load_changes/1,
    load_source/1,
    load_source/2,
    load_binary/1,
    load_binary/2,
    type/1,
    load/1,
    check/2,
    norm_stanza/1,
    handle_files/2
]).

load_changes(FileName) when is_binary(FileName);
                            is_list(FileName) ->
    case edeblib_parser:parse_file(FileName) of
        [Tempo] ->
            Stanza = norm_stanza(Tempo),
            case lists:keyfind(files, 1, Stanza) of
                {_, FileList} ->
                    {ok, handle_files(Stanza, FileList)};

                false ->
                    {error, badinput}
            end;

        {error, _}=Error ->
            Error
    end.

load_source(FileName) when is_binary(FileName) ->
    load_source(FileName, {filename:basename(FileName), esums_file:calc(FileName)});
load_source(FileName) when is_list(FileName) ->
    load_source(list_to_binary(FileName)).

load_source(FileName, FileEntry) ->
    case edeblib_parser:parse_file(FileName) of
        [Tempo] ->
            SourceStanza = norm_stanza(Tempo),
            case lists:keyfind(files, 1, SourceStanza) of
                {_, FileList} ->
                    {value, {_, Files}, Stanza} = lists:keytake(files, 1, handle_files(SourceStanza, FileList)),
                    {ok, [{files, [FileEntry | Files]} | Stanza]};

                false ->
                    {error, badinput}
            end;

        {error, _}=Error ->
            Error
    end.

load_binary(FileName) when is_binary(FileName) ->
    load_binary(FileName, {filename:basename(FileName), esums_file:calc(FileName)});
load_binary(FileName) when is_list(FileName) ->
    load_binary(list_to_binary(FileName)).

load_binary(FileName, FileEntry) ->
    case extract_binary_stanza(FileName, FileEntry) of
        {ok, _}=Result ->
            Result;

        Other ->
            Other
    end.

type(FileName) when is_binary(FileName) ->
    case filename:extension(FileName) of
        <<".changes">> ->
            changes;

        <<".dsc">> ->
            source;

        <<".deb">> ->
            binary;

        _ ->
            unknown
    end.

load(FileName) when is_binary(FileName) ->
    case type(FileName) of
        changes ->
            load_changes(FileName);

        source ->
            load_source(FileName);

        binary ->
            load_binary(FileName);

        _ ->
            {error, unknown_type}
    end;
load(FileName) when is_list(FileName) ->
    load(list_to_binary(FileName)).

check(DirName, {FileName, Props}) ->
    FullName = filename:join(DirName, FileName),
    ActualProps = esums_file:calc(FullName),
    case edeblib_utils:check_props(ActualProps, Props) of
        ok ->
            {ok, type(FileName), ActualProps};

        Other ->
            Other
    end.

norm_stanza(Stanza) ->
    lists:map(fun norm_prop/1, Stanza).

handle_files(Stanza, FileList) ->
    {Files, NewStanza} = update_files(files_to_props(FileList), [{checksums_sha256, none}, {checksums_sha1, sha1}], Stanza),
    lists:keyreplace(files, 1, NewStanza, {files, Files}).

%% Helpers

norm_prop({Tag, _}=Prop) when Tag == description;
                              Tag == files ->
    Prop;
norm_prop({installed_size, [Value]}) ->
    {installed_size, edeblib_utils:to_integer(Value)};
norm_prop({Tag, [Value]}) ->
    {Tag, Value};
norm_prop(Prop) ->
    Prop.

files_to_props(FileList) ->
    files_to_props(FileList, []).

files_to_props([<<>> | Rest], Files) ->
    files_to_props(Rest, Files);
files_to_props([Line | Rest], Files) ->
    [MD5, Size, Name] = case edeblib_utils:split_line(Line) of
        % format of .changes file
        [X1, X2, _, _, X3] ->
            [X1, X2, X3];

        % format of .dsc file
        [_, _, _]=Info ->
            Info
    end,
    files_to_props(Rest, [{Name, [{size, edeblib_utils:to_integer(Size)}, {md5, esums:parse(md5, MD5)}]} | Files]);
files_to_props([], Files) ->
    Files.

update_files(Files, [{Header, none} | Rest], Stanza) ->
    case lists:keytake(Header, 1, Stanza) of
        {value, _, NewStanza} ->
            update_files(Files, Rest, NewStanza);

        false ->
            update_files(Files, Rest, Stanza)
    end;
update_files(Files, [{Header, Key} | Rest], Stanza) ->
    case lists:keytake(Header, 1, Stanza) of
        {value, {_, Value}, NewStanza} ->
            update_files(sums_to_props(Value, Files, Key), Rest, NewStanza);

        false ->
            update_files(Files, Rest, Stanza)
    end;
update_files(Files, [], Stanza) ->
    {Files, Stanza}.

sums_to_props([<<>> | Rest], Files, Key) ->
    sums_to_props(Rest, Files, Key);
sums_to_props([Line | Rest], Files, Key) ->
    [Sum, _, Name] = edeblib_utils:split_line(Line),
    sums_to_props(Rest, case lists:keytake(Name, 1, Files) of
        {value, {_, Props}, OtherFiles} ->
            [{Name, [{Key, esums:parse(Key, Sum)} | Props]} | OtherFiles];

        false ->
            Files
    end, Key);
sums_to_props([], Files, _) ->
    Files.

extract_binary_stanza(FileName, FileEntry) ->
    case file:read_file(FileName) of
        {ok, Body} ->
            case edeblib_ar:parse(Body, [<<"control.tar.gz">>]) of
                [{_, _, _, _, _, _, ControlTGz}] ->
                    case erl_tar:extract({binary, ControlTGz}, [compressed, memory, {files, ["./control"]}]) of
                        {ok, [{_, Control}]} ->
                            [Stanza] = edeblib_parser:parse_blob(Control),
                            {ok, [{files, [FileEntry]} | norm_stanza(Stanza)]};

                        _ ->
                            {error, badinput}
                    end;

                _ ->
                    {error, badinput}
            end;

        Other ->
            Other
    end.

-ifdef(TEST).
-define(EDEBLIB_TEST_CHANGES, "../data/dummy-dummy_0.0-1_i386.changes").
-define(EDEBLIB_TEST_DSC, "../data/dummy-dummy_0.0-1.dsc").
-define(EDEBLIB_TEST_DEB, "../data/dummy-dummy1_0.0-1_i386.deb").

load_changes_test_() -> {
    "Basic .changes file loading test",
    ?_assertMatch({ok, _}, load_changes(?EDEBLIB_TEST_CHANGES))
}.

load_changes_all_test_() ->
    ChangesName = ?EDEBLIB_TEST_CHANGES,
    {ok, Changes} = load_changes(ChangesName),
    Tests = case lists:keyfind(files, 1, Changes) of
        {files, FileList} ->
            DirName = filename:dirname(ChangesName),
            lists:map(fun(Info) -> ?_test(check(DirName, Info)) end, FileList);

        _ ->
            []
    end,
    {"Not so basic .changes file loading test", Tests}.

load_source_test_() -> {
    "Basic .dsc file loading test",
    ?_assertMatch({ok, _}, load_source(?EDEBLIB_TEST_DSC))
}.

load_binary_test_() -> {
    "Basic .deb file loading test",
    ?_assertMatch({ok, _}, load_binary(?EDEBLIB_TEST_DEB))
}.

load_compare(Fun1, Fun2, FileName) ->
    R1 = Fun1(FileName),
    R2 = Fun2(FileName),
    % ?debugFmt("~p(~s) ->\n ~p\n", [Fun1, FileName, R1]),
    % ?debugFmt("~p(~s) ->\n ~p\n", [Fun2, FileName, R2]),
    Result = R1 =:= R2,
    % ?debugFmt("Result: ~p\n", [Result]),
    Result.

load_test_() -> {
    "load/1 test", [
        ?_assert(load_compare(fun load_changes/1, fun load/1, ?EDEBLIB_TEST_CHANGES)),
        ?_assert(load_compare(fun load_source/1, fun load/1, ?EDEBLIB_TEST_DSC)),
        ?_assert(load_compare(fun load_binary/1, fun load/1, ?EDEBLIB_TEST_DEB))
    ]
}.
-endif.

% vim:sw=4:ts=4:et:ai
