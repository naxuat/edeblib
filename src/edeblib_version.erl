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

-module(edeblib_version).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-export([
    new/3,
    parse/1,
    format/1,
    compare/2
]).

-record(debian_version, {
    epoch = 0,
    upstream = <<>>,
    revision = <<>>
}).

-opaque version() :: #debian_version{}.

-export_type([
    version/0
]).

new(Epoch, Upstream, Revision) when is_integer(Epoch), is_binary(Upstream), is_binary(Revision) ->
    #debian_version{epoch=Epoch, upstream=Upstream, revision=Revision}.

split_revision(<<$-, Rest/binary>>, Data, 0, Revision) ->
    split_revision(Rest, Data, Revision, 0);
split_revision(<<$-, Rest/binary>>, Data, Upstream, Revision) ->
    split_revision(Rest, Data, Upstream + Revision + 1, 0);
split_revision(<<_, Rest/binary>>, Data, Upstream, Revision) ->
    split_revision(Rest, Data, Upstream, Revision + 1);
split_revision(<<>>, Data, 0, _) ->
    {Data, <<>>};
split_revision(<<>>, Data, Upstream, _) ->
    <<P1:Upstream/binary, _, P2/binary>> = Data,
    {P1, P2}.

parse(X) ->
    {Epoch, Rest} = case binary:split(X, <<":">>) of
        [First, Undefined] ->
            {edeblib_utils:to_integer(First), Undefined};

        [NoColon] ->
            {0, NoColon}
    end,
    {Upstream, Revision} = split_revision(Rest, Rest, 0, 0),
    new(Epoch, Upstream, Revision).

format(#debian_version{epoch=Epoch, upstream=Upstream, revision=Revision}) ->
    Main = if
        Revision == <<>> ->
            Upstream;

        true ->
            <<Upstream/binary, $-, Revision/binary>>
    end,
    if
        Epoch == 0 ->
            Main;

        true ->
            <<(list_to_binary(io_lib:format("~b", [Epoch])))/binary, $:, Main/binary>>
    end.

compare(#debian_version{epoch=Epoch}=A, #debian_version{epoch=Epoch}=B) ->
    case compare_non_digits(A#debian_version.upstream, B#debian_version.upstream) of
        0 ->
            compare_non_digits(A#debian_version.revision, B#debian_version.revision);

        Result ->
            Result
    end;
compare(#debian_version{epoch=EpochA}, #debian_version{epoch=EpochB}) ->
    edeblib_utils:compare(EpochA, EpochB);
compare(A, B) when is_binary(A) ->
    compare(parse(A), B);
compare(A, B) when is_binary(B) ->
    compare(A, parse(B)).

order(X) when X >= $A, X =< $Z; X >= $a, X =< $z ->
    X;
order(X) ->
    X + 256.

compare_non_digits(<<X, LeftRest/binary>>, <<X, RightRest/binary>>) when X < $0; X > $9 ->
    compare_non_digits(LeftRest, RightRest);
compare_non_digits(<<$~, _/binary>>, _) ->
    -1;
compare_non_digits(_, <<$~, _/binary>>) ->
    1;
compare_non_digits(<<X, _/binary>>, <<Y, _/binary>>) when X < $0 orelse X > $9, Y < $0 orelse Y > $9 ->
    edeblib_utils:compare(order(X), order(Y));
compare_non_digits(<<X, _/binary>>=Left, <<Y, _/binary>>=Right) when X >= $0, X =< $9, Y >= $0, Y =< $9 ->
    compare_digits(Left, Right, 0, 0);
compare_non_digits(<<X, _/binary>>=Left, <<>>=Right) when X >= $0, X =< $9 ->
    compare_digits(Left, Right, 0, 0);
compare_non_digits(<<>>=Left, <<Y, _/binary>>=Right) when Y >= $0, Y =< $9 ->
    compare_digits(Left, Right, 0, 0);
compare_non_digits(<<>>, <<>>) ->
    0;
compare_non_digits(_, <<>>) ->
    1;
compare_non_digits(<<>>, _) ->
    -1.

compare_digits(<<X, LeftRest/binary>>, <<Y, RightRest/binary>>, Left, Right) when X >= $0, X =< $9, Y >= $0, Y =< $9 ->
    compare_digits(LeftRest, RightRest, Left * 10 + (X - $0), Right * 10 + (Y - $0));
compare_digits(<<X, LeftRest/binary>>, Y, Left, Right) when X >= $0, X =< $9 ->
    compare_digits(LeftRest, Y, Left * 10 + (X - $0), Right);
compare_digits(X, <<Y, RightRest/binary>>, Left, Right) when Y >= $0, Y =< $9 ->
    compare_digits(X, RightRest, Left, Right * 10 + (Y - $0));
compare_digits(_, _, Left, Right) when Left /= Right ->
    edeblib_utils:compare(Left, Right);
compare_digits(X, Y, _, _) ->
    compare_non_digits(X, Y).

-ifdef(TEST).
comparison_test_() ->
    {"Debian version comparison function tests", [
        ?_assertEqual(-1, compare(<<"1.0">>, <<"1.1">>)),
        ?_assertEqual(0, compare(<<"1.0">>, <<"1.0">>)),
        ?_assertEqual(1, compare(<<"1.1">>, <<"1.0">>)),
        ?_assertEqual(-1, compare(<<"1.0-1">>, <<"1.0-2">>)),
        ?_assertEqual(0, compare(<<"1.0-1">>, <<"1.0-1">>)),
        ?_assertEqual(1, compare(<<"1.0-2">>, <<"1.0-1">>)),
        ?_assertEqual(1, compare(<<"1:1.0">>, <<"1.0">>)), % Test epoch comparison

        % Debian Firefox versions
        ?_assertEqual(-1, compare(<<"1.4.2-0.1">>, <<"1.4.2-0.2">>)),
        ?_assertEqual(1, compare(<<"1.4.2-0.1">>, <<"1.3.10-2">>)),
        ?_assertEqual(1, compare(<<"1.3.10-2">>, <<"1.3.10-1">>)),
        ?_assertEqual(-1, compare(<<"1.3.6-4">>, <<"1.3.6-5">>)),

        % Webkit PPA {Testing ~ functionality}
        ?_assertEqual(1, compare(<<"7.0.13ubuntu1">>, <<"7.0.13ubuntu1~hhwkt1">>)),

        % Packaging Policy Examples
        ?_assertEqual(-1, compare(<<"~~">>, <<"~~a">>)),
        ?_assertEqual(-1, compare(<<"1.0~~">>, <<"1.0~~a">>)),
        ?_assertEqual(-1, compare(<<"~~a">>, <<"~">>)),
        ?_assertEqual(-1, compare(<<"1.0~~a">>, <<"1.0~">>)),
        ?_assertEqual(1, compare(<<"a">>, <<"a~">>)),
        ?_assertEqual(-1, compare(<<"~">>, <<"">>)),
        ?_assertEqual(-1, compare(<<"">>, <<"a">>)),

        ?_assertEqual(-1, compare(<<"1.0~beta1~svn1245">>, <<"1.0~beta1">>)),
        ?_assertEqual(-1, compare(<<"1.0~beta1">>, <<"1.0">>)),

        ?_assertEqual(1, compare(<<"2.8.10.1-1">>, <<"2.8.10.1-0">>)),
        ?_assertEqual(1, compare(<<"2.8.10.1-0">>, <<"2.8.9.1-0ubuntu6">>)),

        % A bit more complicated versions
        ?_assertEqual(0, compare(<<"blah-one-0">>, <<"blah-one-0">>)),
        ?_assertEqual(-1, compare(<<"blahf">>, <<"blah+">>)),
        ?_assertEqual(1, compare(<<"blah+">>, <<"blahf">>)),
        ?_assertEqual(1, compare(<<"blah">>, <<"bla">>)),
        ?_assertEqual(0, compare(<<"blah0">>, <<"blah">>)),
        ?_assertEqual(-1, compare(<<"blah">>, <<"blah1">>)),
        ?_assertEqual(0, compare(<<"1.00">>, <<"1.000">>))
    ]}.

parsing_formatting_test_() ->
    {"Debian version parsing and formatting tests", [
        {"Parsing tests", [
            ?_assertEqual(new(0, <<"0">>, <<"0">>), parse(<<"0-0">>)),
            ?_assertEqual(new(0, <<"0">>, <<"0">>), parse(<<"0:0-0">>)),
            ?_assertEqual(new(1, <<"0">>, <<"0">>), parse(<<"1:0-0">>)),
            ?_assertEqual(new(0, <<"0-0">>, <<"0">>), parse(<<"0-0-0">>))
        ]},
        {"Formatting tests", [
            ?_assertEqual(<<"0-0">>, format(new(0, <<"0">>, <<"0">>))),
            ?_assertEqual(<<"1:0-0">>, format(new(1, <<"0">>, <<"0">>))),
            ?_assertEqual(<<"1:0">>, format(new(1, <<"0">>, <<>>)))
        ]}
    ]}.
-endif.

% vim:sw=4:ts=4:et:ai
