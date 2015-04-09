%% rfc2234_tests.erl
%% Unit tests for RFC2234 parser provided by rfc2234.erl
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(rfc2234_tests).
-include_lib("eunit/include/eunit.hrl").

%% a helper test suite for the single character primitive functions
test_helper_(M, F, L1, L2, L3) ->
    [ lists:map(fun({X, Y, Z}) -> ?_assertEqual({Y, Z}, M:F(X)) end, L1),
      lists:map(fun(X) -> ?_assertThrow({parse_error, expected, _},
                                        M:F(X)) end, L2),
      lists:map(fun(X) -> ?_assertError({badarg, _}, M:F(X)) end, L3)].

%% the list of the alphabet
alphaset() -> lists:append(lists:seq(65, 90),   % A-Z
                           lists:seq(97, 122)). % a-z

nonbinary_typeset() -> [ [], a, "", 0.0, fun() -> io:format("oh no~n") end,
                         {a, tuple}].

%% check the correct functioning of the alpha/1 function,
%% this includes:
%%  - make sure that if the first element in the binary is an alphabetic
%%    character that it is returned along with the remaining binary
%%  - that if it is not alphabetic that a parse_error of type expected
%%    is thrown
%%  - that if a non-binary is passed in that a badarg error is thrown
alpha_test_() ->
    Passes = lists:map(fun(X) -> {<<X,"bc">>, X, <<"bc">>} end, alphaset()),
    Fails = lists:map(fun(X) -> <<X, "12">> end,
                      lists:subtract(lists:seq(0, 255), alphaset())),
    test_helper_(rfc2234, alpha, Passes, Fails, nonbinary_typeset()).

%% check the correct functioning of the bit/1 function,
%% this includes:
%%  - make sure that if the first element in the binary is 0 or 1 that it
%%    is returned along with the remaining binary
%%  - that if it is not a bit that a parse_error of type expected is thrown
%%  - that if a non-binary is passed in that a badarg error is thrown
bit_test_() ->
    Passes = lists:map(fun(X) -> {<<X,"01">>, X, <<"01">>} end, [48, 49]),
    Fails = lists:map(fun(X) -> <<X, "bc">> end,
                       lists:subtract(lists:seq(0, 255), [48, 49])),
    test_helper_(rfc2234, bit, Passes, Fails, nonbinary_typeset()).

%% check the correct functioning of the character/1 function,
%% this includes:
%%  - make sure that if the first element in the binary is 1-127 that it
%%    is returned along with the remaining binary
%%  - that if it is not a 7-bit character that a parse error of the type
%%    expected is thrown
%%  - that if a non-binary is passed in that a badarg error is thrown
character_test_() ->
    Passes = lists:map(fun(X) -> {<<X,"bc">>, X, <<"bc">>} end,
                       lists:seq(1, 127)),
    Fails = lists:map(fun(X) -> <<X>> end, [0|lists:seq(128, 256)]),
    test_helper_(rfc2234, character, Passes, Fails, nonbinary_typeset()).

%% check the correct functioning of the cr/1 function,
%% this includes:
%%  - make sure that if the first element in the binary is \r that it is
%%    returned along with the remaining binary
%%  - that if it is not \r that a parse error of the type expected is thrown
%%  - that if a non-binary is passed in that a badarg error is thrown
cr_test_() ->
    Fails = lists:map(fun(X) -> <<X>> end,
                      lists:delete(13, lists:seq(0, 256))),
    test_helper_(rfc2234, cr, [{<<"\rab">>, 13, <<"ab">>}], Fails,
                 nonbinary_typeset()).
