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

-module(edeblib_parser).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

% API exports
-export([
    parse_file/1,
    parse_blob/1,
    new/0,
    feed/2
]).

-define(START_STATE, {[], []}).
-define(BLOCK_SIZE, (128*1024)).

-record(parser, {
    state,
    mp,
    tbp
}).

-opaque parser() :: #parser{}.

-export_type([
    parser/0
]).

parse_file(FName) ->
    case file:open(FName, [read, binary]) of
        {ok, Device} ->
            Parser = new(),
            Result = parse_file(Device, Parser),
            file:close(Device),
            Result;

        Other ->
            Other
    end.

parse_file(Device, Parser) ->
    case file:read(Device, ?BLOCK_SIZE) of
        {ok, Data} ->
            parse_file(Device, feed(Data, Parser));

        eof ->
            feed(eof, Parser)
    end.

parse_blob(Blob) when is_binary(Blob) ->
    parse_blob(Blob, get_mp(), ?START_STATE).

new() ->
    #parser{state=?START_STATE, mp=get_mp(), tbp= <<>>}.

feed(Data, #parser{state=State, mp=MP, tbp= <<>>}=Parser) when byte_size(Data) > 0 ->
    case erlang:decode_packet(line, Data, []) of
        {ok, Line, Rest} ->
                feed(Rest, Parser#parser{state=handle_line(Line, MP, State)});

        {more, _} ->
            Parser#parser{tbp=Data}
    end;
feed(Data, #parser{state=State, mp=MP, tbp=TBP}=Parser) when byte_size(Data) > 0 ->
    case erlang:decode_packet(line, Data, []) of
        {ok, Line, Rest} ->
            feed(Rest, Parser#parser{tbp= <<>>, state=handle_line(<<TBP/binary, Line/binary>>, MP, State)});

        {more, _} ->
            Parser#parser{tbp= <<TBP/binary, Data/binary>>}
    end;
feed(<<>>, Parser) ->
    Parser;
feed(eof, #parser{state=State, mp=MP, tbp= <<>>}) ->
    handle_line(eof, MP, State);
feed(eof, #parser{state=State, mp=MP, tbp=TBP}) ->
    handle_line(eof, MP, handle_line(TBP, MP, State)).

% Utilities
parse_blob(<<>>, MP, State) ->
    handle_line(eof, MP, State);
parse_blob(Data, MP, State) ->
    case erlang:decode_packet(line, Data, []) of
        {ok, Line, Rest} ->
            parse_blob(Rest, MP, handle_line(Line, MP, State));

        {more, _} ->
            parse_blob(<<>>, MP, handle_line(Data, MP, State))
    end.

get_mp() ->
    case erlang:get(edeblib_mp) of
        undefined ->
            {ok, Compiled} = re:compile("^(\\s(?P<C>.*))|((?P<H>[^:]*):\\s*(?P<V>.*))$", [{newline, anycrlf}]),
            erlang:put(edeblib_mp, Compiled),
            Compiled;

        Other ->
            Other
    end.

split(Line, MP) ->
    {match, Result} = re:run(Line, MP, [{capture, ['C', 'H', 'V'], binary}]),
    case Result of
        [<<>>, <<>>, <<>>] ->
            eos;

        [<<>>, Header, Value] ->
            {header, {Header, Value}};

        [Cont, _, _] ->
            {cont, Cont}
    end.

parse_stanza([Line | Rest], Acc) ->
    parse_stanza(Rest, [process_line(lists:reverse(Line)) | Acc]);
parse_stanza([], Acc) ->
    Acc.

process_line([{Header, Value} | Rest]) ->
    {binary_to_atom(fix_header(Header, <<>>), utf8), [Value | Rest]}.

handle_line(eof, _, {[], Result}) ->
    Result;
handle_line(eof, _, {Acc, Result}) ->
    [parse_stanza(Acc, []) | Result];
handle_line(<<>>, _, {[], _}=State) ->
    State;
handle_line(<<>>, _, {Acc, Result}) ->
    {[], [parse_stanza(Acc, []) | Result]};
handle_line(Line, MP, {Acc, Result}=State) ->
    case split(Line, MP) of
        {header, Data} ->
            {[[Data] | Acc], Result};

        {cont, Cont} ->
            [First | Other] = Acc,
            {[[Cont | First] | Other], Result};

        eos ->
            case State of
                {[], _} ->
                    State;

                _ ->
                    {[], [parse_stanza(Acc, []) | Result]}
            end
    end.

fix_header(<<N, Rest/binary>>, Acc) ->
    NN = case N of
        $- -> $_;
        N when (N >= $A) and (N =< $Z) -> N + 32;
        _ -> N
    end,
    fix_header(Rest, <<Acc/binary, NN>>);
fix_header(<<>>, Acc) ->
    Acc.

-ifdef(TEST).
-define(PACKAGES, "../data/Packages").
-define(SOURCES, "../data/Sources").

parser_test(Blob) ->
    Parser = new(),
    parser_test(Parser, Blob).

parser_test(Parser, Data) when byte_size(Data) > ?BLOCK_SIZE ->
    <<Block:?BLOCK_SIZE/binary, Rest/binary>> = Data,
    parser_test(feed(Block, Parser), Rest);
parser_test(Parser, Data) when byte_size(Data) > 0 ->
    parser_test(feed(Data, Parser), eof);
parser_test(Parser, _) ->
    feed(eof, Parser).

fix_header_test_() ->
    {"Test for header \"normalization\" routine", [
        ?_assertEqual(<<"hello">>, fix_header(<<"hello">>, <<>>)),
        ?_assertEqual(<<"hello">>, fix_header(<<"Hello">>, <<>>)),
        ?_assertEqual(<<"hello_world">>, fix_header(<<"hello-world">>, <<>>))
    ]}.

parse_sources_test_() ->
    {ok, Sources} = file:read_file(?SOURCES),
    {"Simple test on parsing Sources file", [
        {timeout, 1000000, ?_test(?debugTime("Sources (blob)", parse_blob(Sources)))},
        {timeout, 1000000, ?_test(?debugTime("Sources (file)", parse_file(?SOURCES)))},
        {timeout, 1000000, ?_assertEqual(parse_blob(Sources), parser_test(Sources))}
    ]}.

parse_packages_test_() ->
    {ok, Packages} = file:read_file(?PACKAGES),
    {"Simple test on parsing Packages file", [
        {timeout, 1000000, ?_test(?debugTime("Packages (blob)", parse_blob(Packages)))},
        {timeout, 1000000, ?_test(?debugTime("Packages (file)", parse_file(?PACKAGES)))},
        {timeout, 1000000, ?_assertEqual(parse_blob(Packages), parser_test(Packages))}
    ]}.

-endif.

% vim:sw=4:ts=4:et:ai
