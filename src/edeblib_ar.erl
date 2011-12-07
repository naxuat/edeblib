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

-module(edeblib_ar).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-export([
    parse/1,
    parse/2
]).

%% NOTE: this file parses basic ar files only.
%% Extended names are not supported.
%% TODO: either support extended names or bail out if they are used.

parse(Body) ->
    do_parse(Body, fun(_) -> true end).

parse(Body, List) when is_list(List) ->
    do_parse(Body, fun(X) -> lists:member(X, List) end).

do_parse(<<"!<arch>", 10, Body/binary>>, Fun) ->
    do_parse(Body, Fun, []).

do_parse(<<SName:16/binary, MTime:12/binary, UID:6/binary, GID:6/binary, Mode:8/binary, SSize:10/binary, 16#60, 16#0a, Other/binary>>, Fun, Acc) ->
    Size = edeblib_utils:to_integer(SSize),
    Skip = Size rem 2,
    <<Body:Size/binary, _:Skip/binary, Rest/binary>> = Other,
    Name = edeblib_utils:rstrip(SName),
    case Fun(Name) of
        true ->
            do_parse(Rest, Fun, [{Name, MTime, UID, GID, Mode, Size, Body} | Acc]);

        _ ->
            do_parse(Rest, Fun, Acc)
    end;
do_parse(_, _, Acc) ->
    Acc.

-ifdef(TEST).
ar_test_() ->
    {ok, Blob} = file:read_file("../data/dummy-dummy1_0.0-1_i386.deb"),
    {"Basic ar tests", [
        ?_assertEqual(3, length(parse(Blob))),
        ?_assertEqual(1, length(parse(Blob, [<<"control.tar.gz">>])))
    ]}.
-endif.

% vim:sw=4:ts=4:et:ai
