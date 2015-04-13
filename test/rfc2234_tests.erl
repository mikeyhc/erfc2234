%% rfc2234_tests.erl
%% Unit tests for RFC2234 parser provided by rfc2234.erl
%%
%% author: mikeyhc <mikeyhc@atmosia.net>

-module(rfc2234_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Helper Macros %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% a helper test suite for the single character primitive functions
-define(test_helper_(M, F, L1, L2, L3),
    [ lists:map(fun({X, Y, Z}) -> ?_assertEqual({Y, Z}, M:F(X)) end, L1),
      lists:map(fun(X) -> ?_assertThrow({parse_error, expected, _},
                                        M:F(X)) end, L2),
      lists:map(fun(X) -> ?_assertError({badarg, _}, M:F(X)) end, L3)]).

%% generate a simple parser test
-define(parser_test_(Func, Passes, Fails, Invalids),
    ?test_helper_(rfc2234, Func, Passes, Fails, Invalids)).

%% the most common parser test
-define(single_binary_parser_test_(Func, Set),
    Passes = int_list_to_pass_set(Set),
    Fails = gen_rand_n(10, 1, lists:map(fun({X,_,_})-> X end, Passes)),
    ?parser_test_(Func, Passes, Fails, nonbinary_typeset())).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Test Helper Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% genertes N random bytestrings of length M, filtered against L
gen_rand_n(N, M, L) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C),
    gen_rand_n_(N, M, L).

gen_rand_n_(0, _, _) -> [];
gen_rand_n_(N, M, L) ->
    H = gen_rand_m(M),
    case lists:member(H,L) of
        true  -> gen_rand_n_(N, M, L);
        false -> [H|gen_rand_n_(N-1, M, L)]
    end.

gen_rand_m(0) -> <<>>;
gen_rand_m(M) -> <<(random:uniform(256) - 1),
                   (gen_rand_m(M - 1))/binary>>.

%% the list of the alphabet
alphaset() -> lists:append(lists:seq(65, 90),   % A-Z
                           lists:seq(97, 122)). % a-z

%% all 8 bit chars
list_1() -> lists:map(fun(X) -> <<X>> end, lists:seq(0, 255)).

%% all 8 bit strings of length 2
list_2() -> lists:concat(lists:map(fun(X) ->
                lists:map(fun(Y) -> <<X,Y>> end, lists:seq(0, 255))
                end, lists:seq(0,255))).

%% the list of all common types which are not integers
noninteger_typeset() -> [ [], <<>>,  a, "", 0.0, fun() -> {} end, {a, typle} ].

%% the list of all common types which are not binary
nonbinary_typeset() -> [ [], 0, a, "", 0.0, fun() -> {} end, {a, tuple}].

% turns a set of integers into a pass set
int_list_to_pass_set(L) -> lists:map(fun(X) -> {<<X>>, X, <<>>} end, L).

% turns a set of binaries into a pass set
bin_list_to_pass_set(L) -> lists:map(fun(X) -> {X, X, <<>>} end, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser Combinators %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

many_test_() -> [ ?_assertEqual({<<>>, <<>>},
                                rfc2234:many(rfc2234, alpha, <<>>)) ].

option_test_() -> [ ?_assertEqual(
                       {<<>>, <<"0">>},
                       rfc2234:option(<<>>, rfc2234, alpha, <<"0">>)) ].

both_test_() -> [ ?_assertThrow({parse_error, expected, _},
                                rfc2234:both(rfc2234, alpha,
                                             rfc2234, alpha,
                                             <<"a0">>, "two charcters")) ].

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type Construction %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

bin_join_test_() ->
    [ ?_assertEqual(<<"abc">>, rfc2234:bin_join(97, <<"bc">>)),
      ?_assertEqual(<<"abc">>, rfc2234:bin_join(<<"ab">>, 99)),
      ?_assertEqual(<<"aa">>, rfc2234:bin_join(97, 97))
    ].

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
                lists:seq(65, 90)),
      lists:map(
        fun(X) -> ?_assertError({badarg, _}, rfc2234:case_char(X, <<>>)) end,
        noninteger_typeset()),
      lists:map(
        fun(X) -> ?_assertError({badarg, _}, rfc2234:case_char(97, X)) end,
        nonbinary_typeset())
    ].

% test case insensitive string matches
case_string_test_() ->
    [ lists:map(fun({Match, In}) ->
                       ?_assertEqual({In, <<>>},
                       rfc2234:case_string(Match, In))
               end,
               [ { <<"abc">>, <<"abc">> },
                 { <<"XyZ">>, <<"xyz">> },
                 { <<"ijk">>, <<"Ijk">> } ]),
      lists:map(fun({Match, In}) ->
                        ?_assertThrow({parse_error, expected, _},
                                      rfc2234:case_string(Match, In))
                end,
                [ { <<"abc">>, <<"ab">> },
                  { <<"ihj">>, <<"abc">> },
                  { <<"abd">>, <<"abc">> } ]),
      lists:map(
        fun(X) -> ?_assertError({badarg, _}, rfc2234:case_string(X, <<>>)) end,
        nonbinary_typeset()),
      lists:map(
        fun(X) -> ?_assertError({badarg, _}, rfc2234:case_string(<<>>, X)) end,
        nonbinary_typeset())
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
alpha_test_() -> ?single_binary_parser_test_(alpha, alphaset()).

%% test bit/1, accepts '0' and '1' [48, 49]
bit_test_() -> ?single_binary_parser_test_(bit, [48, 49]).

%% test character/1, accepts a 7-bit character excluding NUL [1..127]
character_test_() -> ?single_binary_parser_test_(character, lists:seq(1, 127)).

%% test cr/1, accepts \r [13]
cr_test_() -> ?single_binary_parser_test_(cr, [13]).

%% test lf/1, accepts \n [10]
lf_test_() -> ?single_binary_parser_test_(lf, [10]).

%% test crlf/1, accepts "\r\n" [<<13,10>>]
crlf_test_() ->
    Codes = [<<"\r\n">>],
    Passes = bin_list_to_pass_set(Codes),
    ?parser_test_(crlf, Passes, gen_rand_n(10, 2, Codes), nonbinary_typeset()).

%% test crl/1, accepts US-ASCII control charcters [0..31,127]
crl_test_() -> ?single_binary_parser_test_(ctl, lists:seq(0, 31)).

%% test dquote/1, accepts " [34]
dquote_test_() -> ?single_binary_parser_test_(dquote, [34]).

%% test hexdig/1, accepts a hexidecimal digit [48..57,65..70,97..102]
hexdig_test_() -> ?single_binary_parser_test_(
                    hexdig, lists:concat([ lists:seq(48, 57),
                                           lists:seq(65, 70),
                                           lists:seq(97, 102)])).

%% test htab/1, accepts \t [9]
htab_test_() -> ?single_binary_parser_test_(htab, [9]).

%% test lwsp/1, this should accept any valid linear whitespace
%% TODO: add some sensible fail tests
lwsp_test_() ->
    Passes = lists:map(fun(X) -> {X, X, <<>>} end,
                       [<<"  \r\n ">>, <<" \r\n\t">>, <<"\t\r\n ">>,
                        <<"\r\n ">>, <<" ">>, <<"\t">>]),
    ?test_helper_(rfc2234, lwsp, Passes, [], nonbinary_typeset()).

%% test octet/1, accepts any octet [0..255]
%% NOTE: type constraints mean there are no fail tests for this
octet_test_() ->
    ?parser_test_(octet, int_list_to_pass_set(lists:seq(0, 255)), [],
                 nonbinary_typeset()).

%% test sp/1, accepts ' ' [32]
sp_test_() -> ?single_binary_parser_test_(sp, [32]).

%% test vchar/1, accepts visible characters [33..126]
vchar_test_() ->
    Codes = lists:seq(33, 126),
    Passes = lists:map(fun(X) -> {<<X>>, X, <<>>} end, Codes),
    Fails = gen_rand_n(10, 1, lists:map(fun(X) -> <<X>> end, Codes)),
    ?test_helper_(rfc2234, vchar, Passes, Fails, nonbinary_typeset()).

%% test wsp/1, accepts \t or ' ' [9, 32]
wsp_test_() ->
    Codes = [9, 32],
    Passes = lists:map(fun(X) -> {<<X>>, X, <<>>} end, Codes),
    Fails = [ <<"a">>, <<"\n">>, <<"\r">>, <<"z">> ],
    ?test_helper_(rfc2234, wsp, Passes, Fails, nonbinary_typeset()).

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Useful Additions %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%% test quoted_pair/1, accepts \X where X is a character that is not CR or LF
quoted_pair_test_() ->
    Passes = lists:map(fun(<<Y>>) -> {<<"\\",Y>>, <<"\\",Y>>, <<>>} end,
                       lists:filter(fun(<<X>>) -> X /= 10 andalso X /= 13 end,
                                    list_1())),
    Fails = lists:concat([gen_rand_n(8, 2, Passes),
                          [<<"\\\r">>, <<"\\\n">>]]),
    ?test_helper_(rfc2234, quoted_pair, Passes, Fails, nonbinary_typeset()).

%% test quoted_string/1, accepts a quoted string
quoted_string_test_() ->
    Passes = lists:map(fun(X) -> {X, X, <<>>} end,
                       [ <<"\"abc\"">>, <<"\"ab\b\"">>,
                         <<"\"abDD  \"">>, <<"\"a\b\l\\\"\"">> ]),
    ?test_helper_(rfc2234, quoted_string, Passes, [], nonbinary_typeset()).

%% test qtext/1, accepts any character that is not \r \n or "
qtext_test_() ->
    Fails = [<<10>>, <<13>>, <<34>>],
    Passes = bin_list_to_pass_set(gen_rand_n(10, 1, Fails)),
    ?parser_test_(qtext, Passes, Fails, nonbinary_typeset()).

%% test qcont/1, it accepts either a 'qtext' or a 'quoted_pair'
qcont_test_() ->
    SingleFails = [<<"\r">>, <<"\n">>, <<"\"">> ],
    DoubleFails = [<<"\\\n">>, <<"\\\r">>],
    Fails = lists:concat([SingleFails, DoubleFails]),
    Passes = bin_list_to_pass_set(
               lists:concat([ gen_rand_n(5, 1, SingleFails),
                              lists:map(fun(X) -> <<"\\", X/binary>> end,
                                        gen_rand_n(5, 1, [<<"\n">>, <<"\r">>]))
                            ])),
    ?test_helper_(rfc2234, qcont, Passes, Fails, nonbinary_typeset()).
