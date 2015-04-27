%% rfc2234.erl
%% This module provides parsers for the grammer defined in RFC2234,
%% "Augmented BNF for Syntax Specifications: ABNF",
%% <http://www.faqs.org/rfcs/rfc2234.html>". This code is largely a port
%% from the haskell library hsemail
%% <https://hackage.haskell.org/package/hsemail-1.7.7/docs/
%% Text-ParserCombinators-Parsec-Rfc2234.html>.
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(rfc2234).
%-compile(export_all).
-export([% primitive parsers
         alpha/1, bit/1, character/1, digit/1, cr/1, lf/1, crlf/1, ctl/1,
         dquote/1, hexdig/1, htab/1, lwsp/1, octet/1, sp/1, vchar/1, wsp/1,

         % helper parsers
         qcont/1, qtext/1,

         % useful additions
         quoted_pair/1, quoted_string/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitive Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% match any character of the alphabet
-spec alpha(<<_:8,_:_*8>>) -> {65..90 | 97..122, binary()}.
alpha(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= $A andalso H =< $Z orelse
       H >= $a andalso H =< $z -> {H, T};
       true -> throw({parse_error, expected, "alphabetic character"})
    end;
alpha(X) -> error({badarg, X}).

%% match either "1" or "0"
-spec bit(<<_:8,_:_*8>>) -> {48..49, binary()}.
bit(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == $0 orelse H == $1  -> {H, T};
       true -> throw({parse_error, expected, "bit ('0' or '1')"})
    end;
bit(X) -> error({badarg, X}).

%% match any 7-bit US-ASCII character except for NUL (ASCII value 0, that is)
-spec character(<<_:8,_:_*8>>) -> {1..127, binary()}.
character(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 1 andalso H =< 127 -> {H, T};
       true -> throw({parse_error, expected, "7-bit character excluding NUL"})
    end;
character(X) -> error({badarg, X}).

%% match any digit
-spec digit(<<_:8,_:_*8>>) -> {48..57, binary()}.
digit(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= $0 andalso H =< $9 -> {H, T};
       true -> throw({parse_error, expected, "digit"})
    end;
digit(X) -> error({badarg, X}).

%% match the carriage return character "\r"
-spec cr(<<_:8,_:_*8>>) -> {13, binary()}.
cr(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == $\r -> {H, T};
       true    -> throw({parse_error, expected, "carriage return"})
    end;
cr(X) -> error({badarg, X}).

%% match the linefeed character "\n"
-spec lf(<<_:8,_:_*8>>) -> {10, binary()}.
lf(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == $\n -> {H, T};
       true -> throw({parse_error, expected, "linefeed"})
    end;
lf(X) -> error({badarg, X}).

%% match the internet newline
-spec crlf(<<_:8,_:_*8>>) -> {<<_:16>>, binary()}.
crlf(X) when is_binary(X) ->
    <<H, I, T/binary>> = X,
    try
        cr(<<H>>),
        lf(<<I>>),
        {<<H,I>>, T}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected,
                                             "carriage return followed by "
                                             "linefeed"})
    end;
crlf(X) -> error({badarg, X}).

%% match any US-ASCII control character. That is any character with a
%% decimal value in the range of [0..31, 127]
-spec ctl(<<_:8,_:_*8>>) -> {0..31 | 127, binary()}.
ctl(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 0 andalso H =< 31 orelse H == 127 -> {H, T};
       true -> throw({parse_error, expected, "control character"})
    end;
ctl(X) -> error({badarg, X}).

%% match the double quote character """
-spec dquote(<<_:8,_:_*8>>) -> {34, binary()}.
dquote(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == $" -> {H, T};
       true -> throw({parse_error, expected, "double quotes"})
    end;
dquote(X) -> error({badarg, X}).

%% match any character that is a valid hexidecimal number;
%% ['0'..'9'] and ['A'..'F','a'..'f'].
-spec hexdig(<<_:8,_:_*8>>) -> {48..57 | 65..70 | 97..102, binary()}.
hexdig(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= $0 andalso H =< $9 orelse
       H >= $A andalso H =< $F orelse
       H >= $a andalso H =< $f -> {H, T};
       true -> throw({parse_error, expected, "hexidecimal digit"})
    end;
hexdig(X) -> error({badarg, X}).

%% match the tab "\t" character
-spec htab(<<_:8,_:_*8>>) -> {9, binary()}.
htab(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == $\t -> {H, T};
       true -> throw({parse_error, expected, "horizontal tab"})
    end;
htab(X) -> error({badarg, X}).


%% match "linear white-space". That is any number of consecutive 'wsp',
%% optionally followed by a 'crlf' and (at least) one more 'wsp'.
-spec lwsp(binary()) -> {binary(), binary()}.
lwsp(X) when is_binary(X) ->
    {H1, T1} = parserlang:option([],
                                 fun (Y) -> parserlang:many(fun wsp/1, Y) end,
                                 X),
    {{H2, H3}, T2} = try
                         parserlang:both(fun crlf/1,
                                         fun(Y) ->
                                                 parserlang:many1(fun wsp/1, Y)
                                         end,
                                         T1, "crlf followed by whitespace")
                     catch
                         {parse_error, expected, _} -> {{<<>>,[]}, T1}
                     end,
    {parserlang:bin_concat(H1 ++ [H2] ++ H3), T2};
lwsp(X) -> error({badarg, X}).

%% match any character
-spec octet(<<_:8,_:_*8>>) -> {byte(), binary()}.
octet(X) when is_binary(X) -> <<H, T/binary>> = X, {H, T};
octet(X) -> error({badarg, X}).

%% match the space.
-spec sp(<<_:8,_:_*8>>) -> {32, binary()}.
sp(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == $  -> {H, T};
       true -> throw({parse_error, expected, "space"})
    end;
sp(X) -> error({badarg, X}).

%% match any printable ASCII character. (The "v" stands for "visible".)
%% That is any character in the decimal range of [33..126]
-spec vchar(<<_:8,_:_*8>>) -> {33..126, binary()}.
vchar(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 33 andalso H =< 126 -> {H, T};
       true -> throw({parse_error, expected, "printable character"})
    end;
vchar(X) -> error({badarg, X}).

%% match either 'sp' or 'htab'
-spec wsp(<<_:8,_:_*8>>) -> {9 | 32, binary()}.
wsp(X) when is_binary(X) ->
    parserlang:either(fun sp/1, fun htab/1, X, "white-space");
wsp(X) -> error({badarg, X}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Useful Additions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% match a "quoted pair". Any characters (excluding CR and LF) may be quoted.
-spec quoted_pair(<<_:16,_:_*8>>) -> {<<_:16>>, binary()}.
quoted_pair(X)  when is_binary(X) ->
    try
        <<H, I, T/binary>> = X,
        if H /= 92 orelse I == 10 orelse I == 13 ->
               throw({parse_error, expected, "quoted pair"});
           true -> {<<H,I>>, T}
        end
    catch
        error:{badmatch, _} -> throw({parse_error, expected, "quoted pair"})
    end;
quoted_pair(X) -> error({badarg, X}).

%% matches a quoted string. The specials "\" and """ must be escaped inside
%% a quoted string; CR and LF are not allowed at all
-spec quoted_string(<<_:16,_:_*8>>) -> {<<_:16,_:_*8>>, binary()}.
quoted_string(X) when is_binary(X) ->
    {OQ, T1} = dquote(X),
    {C, T2} = parserlang:many(fun qcont/1, T1),
    {CQ, T3} = dquote(T2),
    {parserlang:bin_concat([OQ|C] ++ [CQ]), T3};
quoted_string(X) -> error({badarg, X}).

%% match the contents of a quoted string
-spec qcont(<<_:16,_:_*8>>) -> {byte() | <<_:16>>, binary()}.
qcont(X) ->
    parserlang:either(fun quoted_pair/1, fun qtext/1, X,
                      "quoted text or quoted pair").

%% a binary which is valid in a quoted string
-spec qtext(<<_:8,_:_*8>>) -> {byte(), binary()}.
qtext(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == 10 orelse H == 13 orelse H == 34 orelse H == 92 ->
           throw({parse_error, expected, "quoted text"});
       true -> {H, T}
    end;
qtext(X) -> error({badarg, X}).
