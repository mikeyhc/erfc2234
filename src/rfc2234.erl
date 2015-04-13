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
-export([% parser combinators
         many/3, many1/3, option/4, either/6, both/6,

         % generic parsers
         case_char/2, case_string/2,

         % primitive parsers
         alpha/1, bit/1, character/1, cr/1, lf/1, crlf/1, ctl/1, dquote/1,
         hexdig/1, htab/1, lwsp/1, octet/1, sp/1, vchar/1, wsp/1,

         % helper parsers
         qcont/1, qtext/1, lwsp_/1,

         % useful additions
         quoted_pair/1, quoted_string/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% match 0 or more times using M:F(A)
many(M, F, A) ->
    try
        {X, Y} = apply_many(M, F, A),
        {Acc, T} = many(M, F, Y),
        {<<X, Acc/binary>>, T}
    catch
        {parse_error, expected, _} -> {<<>>, A};
        error:{badmatch, <<>>}     -> {<<>>, A}
    end.

%% match 1 or more times using M:F(A)
many1(M, F, A) ->
    {X, Y} = apply_many(M, F, A),
    {Acc, T} = many(M, F, Y),
    {<<X, Acc/binary>>, T}.

apply_many(M, F, A) when is_list(A) -> apply(M, F, A);
apply_many(M, F, A) -> M:F(A).

%% tries to match M:F(A), but if that fails will return {Def, A}
option(Def, M, F, A) ->
    try
        apply_many(M, F, A)
    catch
        {parse_error, expected, _} -> {Def, A}
    end.

%% tries to match one or the other, if both fail the error is thrown
either(M1, F1, M2, F2, A, Err) ->
    try
        M1:F1(A)
    catch
        {parse_error, expected, _} -> either_(M2, F2, A, Err);
        error:{badmatch, _} -> either_(M2, F2, A, Err)
    end.

either_(M, F, A, Err) ->
    try
        M:F(A)
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, Err});
        error:{badmatch, _} -> throw({parse_error, expected, Err})
    end.

% tries to match both, if either fail it throws a parse error with the given
% message
both(M1, F1, M2, F2, A, Err) ->
    try
        {H1, T1} = apply_many(M1, F1, A),
        {H2, T2} = apply_many(M2, F2, T1),
        {bin_join(H1, H2), T2}
    catch
        {parse_error, expected, _} -> throw({parse_error, expected, Err});
        error:{badmatch, _} -> throw({parse_error, expected, Err})
    end.

% joins characters and binarys
bin_join(X, Y) when is_binary(X) andalso is_binary(Y) ->
    <<X/binary, Y/binary>>;
bin_join(X, Y) when is_integer(X) andalso is_binary(Y) ->
    <<X, Y/binary>>;
bin_join(X, Y) when is_binary(X) andalso is_integer(y) ->
    <<X/binary, Y>>;
bin_join(X, Y) -> <<X, Y>>.


%% converts an uppercase character to its lowercase version, if
%% it is not an uppercase character no change is performed
to_lower(C) when C >= 65 andalso C =< 90 -> C + 32;
to_lower(C) -> C.

%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%

%% case insensitive character match
case_char(C, S) when is_binary(S) andalso is_integer(C) ->
    <<H, T/binary>> = S,
    case to_lower(H) == to_lower(C) of
        true -> {H,T};
        false -> throw({parse_error, expected, C})
    end;
case_char(C, _) when not is_integer(C) -> error({badarg, C});
case_char(_, S) -> error({badarg, S}).

%% case insensitive string match
case_string(<<>>, L) when is_binary(L) -> {<<>>, L};
case_string(S, <<>>) when is_binary(S) ->
    throw({parse_error, expected, S});
case_string(S, L) when is_binary(L) andalso is_binary(S) ->
    <<SH, ST/binary>> = S,
    try
        {LH, LT} = case_char(SH, L),
        {RH, RT} = case_string(ST, LT),
        {<<LH,RH/binary>>, RT}
    catch
        {parse_error, expected, SH} ->
            throw({parse_error, expected, S});
        {parse_error, expected, NT} ->
            throw({parse_error, expected, NT})
    end;
case_string(S, _) when not is_binary(S) -> error({badarg, S});
case_string(_, L) -> error({badarg, L}).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitive Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% match any character of the alphabet
alpha(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 65 andalso H =< 90 orelse
       H >= 97 andalso H =< 122 -> {H, T};
       true -> throw({parse_error, expected, "alphabetic character"})
    end;
alpha(X) -> error({badarg, X}).

%% match either "1" or "0"
bit(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == 48 orelse H == 49  -> {H, T};
       true -> throw({parse_error, expected, "bit ('0' or '1')"})
    end;
bit(X) -> error({badarg, X}).

%% match any 7-bit US-ASCII character except for NUL (ASCII value 0, that is)
character(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 1 andalso H =< 127 -> {H, T};
       true -> throw({parse_error, expected, "7-bit character excluding NUL"})
    end;
character(X) -> error({badarg, X}).

%% match the carriage return character "\r"
cr(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == 13 -> {H, T};
       true    -> throw({parse_error, expected, "carriage return"})
    end;
cr(X) -> error({badarg, X}).

%% match the linefeed character "\n"
lf(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == 10 -> {H, T};
       true -> throw({parse_error, expected, "linefeed"})
    end;
lf(X) -> error({badarg, X}).

%% match the internet newline
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
ctl(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 0 andalso H =< 31 orelse H == 127 -> {H, T};
       true -> throw({parse_error, expected, "control character"})
    end;
ctl(X) -> error({badarg, X}).

%% match the double quote character """
dquote(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == 34 -> {H, T};
       true -> throw({parse_error, expected, "double quotes"})
    end;
dquote(X) -> error({badarg, X}).

%% match any character that is a valid hexidecimal number;
%% ['0'..'9'] and ['A'..'F','a'..'f'].
hexdig(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 48 andalso H =< 57 orelse
       H >= 65 andalso H =< 70 orelse
       H >= 97 andalso H =< 102 -> {H, T};
       true -> throw({parse_error, expected, "hexidecimal digit"})
    end;
hexdig(X) -> error({badarg, X}).

%% match the tab "\t" character
htab(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == 9 -> {H, T};
       true -> throw({parse_error, expected, "horizontal tab"})
    end;
htab(X) -> error({badarg, X}).


%% match "linear white-space". That is any number of consecutive 'wsp',
%% optionally followed by a 'crlf' and (at least) one more 'wsp'.
lwsp(X) when is_binary(X) ->
    {H1, T1} = option(<<>>, rfc2234, many, [rfc2234, wsp, X]),
    {H2, T2} = try
                   both(rfc2234, crlf, rfc2234, lwsp_, T1,
                        "crlf followed by whitespace")
               catch
                   {parse_error, expected, _} -> {<<>>, T1}
               end,
    {<<H1/binary, H2/binary>>, T2};
lwsp(X) -> error({badarg, X}).

lwsp_(X) -> many1(rfc2234, wsp, X).

%% match any character
octet(X) when is_binary(X) -> <<H, T/binary>> = X, {H, T};
octet(X) -> error({badarg, X}).

%% match the space.
sp(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == 32 -> {H, T};
       true -> throw({parse_error, expected, "space"})
    end;
sp(X) -> error({badarg, X}).

%% match any printable ASCII character. (The "v" stands for "visible".)
%% That is any character in the decimal range of [33..126]
vchar(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H >= 33 andalso H =< 126 -> {H, T};
       true -> throw({parse_error, expected, "printable character"})
    end;
vchar(X) -> error({badarg, X}).

%% match either 'sp' or 'htab'
wsp(X) when is_binary(X) ->
    either(rfc2234, sp, rfc2234, htab, X, "white-space");
wsp(X) -> error({badarg, X}).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Useful Additions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% match a "quoted pair". Any characters (excluding CR and LF) may be quoted.
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
quoted_string(X) when is_binary(X) ->
    {OQ, T1} = dquote(X),
    {C, T2} = many(rfc2234, qcont, T1),
    {CQ, T3} = dquote(T2),
    {<<OQ,C/binary,CQ/binary>>, T3}.

%% match the contents of a quoted string
qcont(X) ->
    either(rfc2234, quoted_pair, rfc2234, qtext, X,
           "quoted text or quoted pair").

%% a binary which is valid in a quoted string
qtext(X) when is_binary(X) ->
    <<H, T/binary>> = X,
    if H == 10 orelse H == 13 orelse H == 34 orelse H == 92 ->
           throw({parse_error, expected, "quoted text"});
       true -> {<<H>>, T}
    end;
qtext(X) -> error({badarg, X}).
