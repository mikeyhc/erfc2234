%% rfc2234_tests.erl
%% Unit tests for RFC2234 parser provided by rfc2234.erl
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(rfc2234_tests).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Helper Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% a helper test suite for the single character primitive functions
test_helper_(M, F, L1, L2, L3) ->
    [ lists:map(fun({X, Y, Z}) -> ?_assertEqual({Y, Z}, M:F(X)) end, L1),
      lists:map(fun(X) -> ?_assertThrow({parse_error, expected, _},
                                        M:F(X)) end, L2),
      lists:map(fun(X) -> ?_assertError({badarg, _}, M:F(X)) end, L3)].

%% the list of the alphabet
alphaset() -> lists:append(lists:seq(65, 90),   % A-Z
                           lists:seq(97, 122)). % a-z

%% all 8 bit chars
list_1() -> lists:map(fun(X) -> <<X>> end, lists:seq(0, 255)).

%% all 8 bit strings of length 2
list_2() -> lists:concat(lists:map(fun(X) ->
                lists:map(fun(Y) -> <<X,Y>> end, lists:seq(0, 255))
                end, lists:seq(0,255))).

%% the list of all common types which are not binary
nonbinary_typeset() -> [ [], a, "", 0.0, fun() -> io:format("oh no~n") end,
                         {a, tuple}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% Generic Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%

% test case insensitive charcter matches
case_char_test_() ->
    [ lists:map(
        fun(X) ->
            [ ?_assertEqual({X, <<>>}, rfc2234:case_char(X, <<X>>)),
              ?_assertEqual({X + 32, <<>>},
                            rfc2234:case_char(X, <<(X + 32)>>)) ]
        end,
        lists:seq(65, 90)),
      lists:map(fun(X) ->
        lists:map(fun(Y) ->
            ?_assertThrow({parse_error, expected, X},
                          rfc2234:case_char(X, <<Y>>))
                  end,
                  lists:subtract(alphaset(), [X, X+32]))
                end,
                lists:seq(65, 90))
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitive Parsers %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% the functions in this section check the following contraints
%%  - that the correct charcter is at the start of the given binary,
%%    in the case that it is it is returned in a tuple with the remaining
%%    binary
%%  - that if it is a binary but the first character isn't expected that
%%    a parse error is thrown of type expected is thrown
%%  - that if a non-binary is passed in a badarg error is thrown

%% test alpha/1, accepts characters from the alphabet [65..90,97..122]
alpha_test_() ->
    Passes = lists:map(fun(X) -> {<<X,"bc">>, X, <<"bc">>} end, alphaset()),
    Fails = lists:map(fun(X) -> <<X, "12">> end,
                      lists:subtract(lists:seq(0, 255), alphaset())),
    test_helper_(rfc2234, alpha, Passes, Fails, nonbinary_typeset()).

%% test bit/1, accepts '0' and '1' [48, 49]
bit_test_() ->
    Passes = lists:map(fun(X) -> {<<X,"01">>, X, <<"01">>} end, [48, 49]),
    Fails = lists:map(fun(X) -> <<X, "bc">> end,
                       lists:subtract(lists:seq(0, 255), [48, 49])),
    test_helper_(rfc2234, bit, Passes, Fails, nonbinary_typeset()).

%% test character/1, accepts a 7-bit character excluding NUL [1..127]
character_test_() ->
    Passes = lists:map(fun(X) -> {<<X,"bc">>, X, <<"bc">>} end,
                       lists:seq(1, 127)),
    Fails = lists:map(fun(X) -> <<X>> end, [0|lists:seq(128, 256)]),
    test_helper_(rfc2234, character, Passes, Fails, nonbinary_typeset()).

%% test cr/1, accepts \r [13]
cr_test_() ->
    Fails = lists:map(fun(X) -> <<X>> end,
                      lists:delete(13, lists:seq(0, 256))),
    test_helper_(rfc2234, cr, [{<<"\rab">>, 13, <<"ab">>}], Fails,
                 nonbinary_typeset()).

%% test lf/1, accepts \n [10]
lf_test_() ->
    Fails = lists:map(fun(X) -> <<X>> end,
                      lists:delete(10, lists:seq(0, 256))),
    test_helper_(rfc2234, lf, [{<<"\nab">>, 10, <<"ab">>}], Fails,
                 nonbinary_typeset()).

%% test crlf/1, accepts "\r\n" [<<13,10>>]
crlf_test_() ->
    Fails = lists:delete(<<"\r\n">>, list_2()),
    test_helper_(rfc2234, crlf, [{<<"\r\n-">>, <<"\r\n">>, <<"-">>}],
                 Fails, nonbinary_typeset()).

%% test crl/1, accepts US-ASCII control charcters [0..31,127]
crl_test_() ->
    Codes = [127|lists:seq(0, 31)],
    Passes = lists:map(fun(X) -> {<<X>>, X, <<>>} end, Codes),
    Fails = lists:subtract(list_1(), lists:map(fun(X) -> <<X>> end, Codes)),
    test_helper_(rfc2234, ctl, Passes, Fails, nonbinary_typeset()).

%% test dquote/1, accepts " [34]
dquote_test_() ->
    Passes = [{<<"\"">>, 34, <<>>}],
    Fails = lists:delete(<<"\"">>, list_1()),
    test_helper_(rfc2234, dquote, Passes, Fails, nonbinary_typeset()).

%% test hexdig/1, accepts a hexidecimal digit [48..57,65..70,97..102]
hexdig_test_() ->
    Codes = lists:concat([ lists:seq(48, 57),
                           lists:seq(65, 70),
                           lists:seq(97, 102)]),
    Passes = lists:map(fun(X) -> {<<X>>, X, <<>>} end, Codes),
    Fails  = lists:subtract(list_1(), lists:map(fun(X) -> <<X>> end, Codes)),
    test_helper_(rfc2234, hexdig, Passes, Fails, nonbinary_typeset()).

%% test htab/1, accepts \t [9]
htab_test_() ->
    Passes = [{<<"\t">>, 9, <<>>}],
    Fails = lists:delete(<<"\t">>, list_1()),
    test_helper_(rfc2234, htab, Passes, Fails, nonbinary_typeset()).

%% test lwsp/1, this should accept any valid linear whitespace
%lwsp_test_() ->
%    Passes = lists:map(fun(X) -> {X, X, <<>>} end,
%                       [<<"  \r\n ">>, <<" \r\n\t">>, <<"\t\r\n">>,
%                        <<"\r\n ">>, <<" ">>, <<"\t">>]),
%    Fails = [<<"a">>, <<"\r\n">>],
%    test_helper_(rfc2234, lwsp, Passes, Fails, nonbinary_typeset()).

%% test octet/1, accepts any octet [0..255]
%% NOTE: type constraints mean there are no fail tests for this
octet_test_() ->
    Passes = lists:map(fun(X) -> {<<X>>, X, <<>>} end, lists:seq(0, 255)),
    test_helper_(rfc2234, octet, Passes, [], nonbinary_typeset()).

%% test sp/1, accepts ' ' [32]
sp_test_() ->
    Passes = [{<<" ">>, 32, <<>>}],
    Fails = lists:delete(<<" ">>, list_1()),
    test_helper_(rfc2234, sp, Passes, Fails, nonbinary_typeset()).

%% test vchar/1, accepts visible characters [33..126]
vchar_test_() ->
    Codes = lists:seq(33, 126),
    Passes = lists:map(fun(X) -> {<<X>>, X, <<>>} end, Codes),
    Fails = lists:subtract(list_1(), lists:map(fun(X) -> <<X>> end, Codes)),
    test_helper_(rfc2234, vchar, Passes, Fails, nonbinary_typeset()).

%% test wsp/1, accepts \t or ' ' [9, 32]
wsp_test_() ->
    Codes = [9, 32],
    Passes = lists:map(fun(X) -> {<<X>>, X, <<>>} end, Codes),
    Fails = lists:subtract(list_1(), lists:map(fun(X) -> <<X>> end, Codes)),
    test_helper_(rfc2234, wsp, Passes, Fails, nonbinary_typeset()).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Useful Additions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% test quoted_pair/1, accepts \X where X is a character that is not CR or LF
quoted_pair_test_() ->
    Passes = lists:map(fun(<<Y>>) -> {<<"\\",Y>>, <<"\\",Y>>, <<>>} end,
                       lists:filter(fun(<<X>>) -> X /= 10 andalso X /= 13 end,
                                    list_1())),
    Fails = lists:subtract(list_2(),
                           lists:map(fun({X, _, _}) -> X end, Passes)),
    test_helper_(rfc2234, quoted_pair, Passes, Fails, nonbinary_typeset()).

%% test qtex/1, accepts binary strings not containing \r or \n, returns on  "
