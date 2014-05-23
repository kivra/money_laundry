%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2012-2014 Kivra
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @doc Functions to handle various conversions to and from rational
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(rational).

%%%_* Includes =========================================================
-include("money_laundry.hrl").

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([new/1]).
-export([new/2]).
-export([inv/1]).
-export([neg/1]).
-export([add/2]).
-export([sub/2]).
-export([mult/2]).
-export([divide/2]).
-export([numerator/1]).
-export([denominator/1]).
-export([is_rational/1]).
-export([from_string/1]).
-export([to_decimal_fraction/1]).

-export_type([rational/0]).
-export_type([decimal/0]).

%%%_ * Types -----------------------------------------------------------
-type rational() :: #rational{}.
-type decimal()  :: #decimal{}.

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
%% @doc Create a new rational number
-spec new(integer() | rational())          -> rational().
-spec new(integer(), integer())            -> rational().
new(Num) when is_integer(Num) -> new(Num, 1);
new(#rational{}=Rational)     -> Rational.
new(Num, Denom) ->
    GCD = gcd(Num, Denom),
    #rational{numerator=Num div GCD, denom=Denom div GCD, decfact=GCD}.

%% @doc Inverse a rational
-spec inv(integer() | rational()) -> rational().
inv(N) -> A = new(N), new(A#rational.denom, A#rational.numerator).

%% @doc Negate a rational
-spec neg(integer() | rational()) -> rational().
neg(N) -> A = new(N), new(-A#rational.numerator, A#rational.denom).

%% @doc multiply one rational with another
-spec mult(integer() | rational(), integer() | rational()) -> rational().
mult(X, Y) ->
    A     = new(X),
    B     = new(Y),
    Num   = A#rational.numerator * B#rational.numerator,
    Denom = A#rational.denom * B#rational.denom,
    Gcd   = gcd(Num, Denom),
    new(Num div Gcd, Denom div Gcd).

%% @doc divide one rational with another
-spec divide(integer() | rational(),integer() | rational()) -> rational().
divide(X, Y) -> mult(X, inv(Y)).

%% @doc add one rational to another
-spec add(integer() | rational(),integer() | rational()) -> rational().
add(X, Y) ->
    A   = new(X),
    B   = new(Y),
    Lcm = lcm(A#rational.denom, B#rational.denom),
    Num = A#rational.numerator * (Lcm div A#rational.denom) +
          B#rational.numerator * (Lcm div B#rational.denom),
    new(Num, Lcm).

%% @doc subtract one rational from another
-spec sub(integer() | rational(),integer() | rational()) -> rational().
sub(X, Y) -> add(X, neg(Y)).

%% @doc Given a rational number in this module's normalized form,
%%      a decimal fraction is returned.
-spec to_decimal_fraction(rational()) -> decimal().
to_decimal_fraction(#rational{numerator=Num, denom=Denom, decfact=DecFact}) ->
    #decimal{numerator=Num*DecFact, denom=Denom*DecFact}.

-spec numerator(rational() | decimal())   -> integer().
numerator(#decimal{numerator=Numerator})  -> Numerator;
numerator(#rational{numerator=Numerator}) -> Numerator.

-spec denominator(rational() | decimal()) -> integer().
denominator(#decimal{denom=Denominator})  -> Denominator;
denominator(#rational{denom=Denominator}) -> Denominator.

%% @doc check if the input is a rational()
-spec is_rational(any()) -> boolean().
is_rational(#rational{numerator=Num, denom=Denom, decfact=DecFact}) when
        is_integer(Num),
        is_integer(Denom),
        Denom > 0,
        is_integer(DecFact),
        DecFact > 0 ->
    true;
is_rational(_) -> false.

%% @doc Given a decimal as a string, the term representing the rational
%%      number is returned in this module's normalized form.
-spec from_string(binary() | string()) -> rational().
from_string(Amount) when is_binary(Amount)           ->
    from_string(binary_to_list(Amount));
from_string(StringAmount) when is_list(StringAmount) ->
    Amount = trim_whitespace(StringAmount),
    %% Based on http://www.regular-expressions.info/floatingpoint.html
    %% - Required whole part
    %% -- May start with - or +
    %% -- required integer part
    %% - optional comma or period separator
    %% - optional fraction part
    %% -- final digit of fraction part must be non-zero
    %% - any number of trailing zeros are ignored
    RE = "^([-+]?[0-9]+)[,\.]?([0-9]*[1-9])?0*$",
    {Numerator, Denominator} =
        case re:run(Amount, RE, [{capture, all_but_first, list}]) of
            %% Integer starts with minus sign
            {match, [[$-|_] = Integer, Fraction]} ->
                %% Denominator is a power of 10 here,
                %%  but not necessarily elsewhere.
                Denominator1 = round(math:pow(10, length(Fraction))),
                Numerator1 =
                    list_to_integer(Integer) * Denominator1 -
                    list_to_integer(Fraction),
                {Numerator1, Denominator1};
            %% Positive integer with fraction part
            {match, [Integer, Fraction]} ->
                Denominator1 = round(math:pow(10, length(Fraction))),
                Numerator1 =
                    list_to_integer(Integer) * Denominator1 +
                    list_to_integer(Fraction),
                {Numerator1, Denominator1};
            %% Positive or negative integer
            %% (can also match ,123 as 123
            %%  but current assumption is well-formed input)
            {match, [Integer]} ->
                Denominator1 = 1,
                Numerator1 =
                    list_to_integer(Integer)*Denominator1,
                {Numerator1, Denominator1}
        end,
    new(Numerator, Denominator).

%%%_* Private functions ================================================
trim_whitespace(Input) -> re:replace(Input, "\\s+", "", [global]).

%% @doc Compute the greatest common divisor
%%      https://en.wikipedia.org/wiki/Euclidean_algorithm
-spec gcd(integer(), integer()) -> integer().
gcd(A, 0) when A < 0 -> -A;
gcd(A, 0)            -> A;
gcd(A, B) -> gcd(B, A rem B).

lcm(A, B) -> (A*B) div gcd(A, B).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_string_test_() ->
    FromStringCases =
        [ {"0",                    0,                   1,          1}
        , {"0,0",                  0,                   1,          1}
        , {"1",                    1,                   1,          1}
        , {"1234567890,987654321", 1234567890987654321, 1000000000, 1}
        , {"1,00",                 1,                   1,          1}
        , {"-1",                   -1,                  1,          1}
        , {"-1,1",                 -11,                 10,         1}
        , {"0,5",                  1,                   2,          5}
        , {"3,142",                1571,                500,        2}
        , {"144,144",              18018,               125,        8}
        , {"0.5",                  1,                   2,          5}
        , {"3.142",                1571,                500,        2}
        , {"144.144",              18018,               125,        8}
        , {"1 9 0",                190,                 1,          1}
        , {"190",                  190,                 1,          1}
        ],
    [from_string_test_fun(String, Numer, Denom, DecFac)
     || {String, Numer, Denom, DecFac} <- FromStringCases].

from_string_test_fun(String, Numer, Denom, DecFac) ->
    fun() ->
            ?assertEqual({rational, Numer, Denom, DecFac},
                         rational:from_string(String))
    end.

to_decimal_fraction_test_() ->
    ToDecFracCases =
        [ {"1,00",    1,      1}
        , {"-1",      -1,     1}
        , {"-1,1",    -11,    10}
        , {"0,5",     5,      10}
        , {"3,142",   3142,   1000}
        , {"-12,345", -12345, 1000}
        , {"144,144", 144144, 1000}
        , {"0,25",    25,     100}
        , {"-0,25",   -25,    100}
        ],
    [to_dec_frac_test_fun(String, Numer, Denom)
     || {String, Numer, Denom} <- ToDecFracCases].

to_dec_frac_test_fun(String, Numer, Denom) ->
    fun() ->
            ?assertEqual({decimal, Numer, Denom},
                         to_decimal_fraction(from_string(String)))
    end.

is_rational_test_() ->
    [?_assert(is_rational({rational, 10, 20, 10}))
    ,?_assert(is_rational({rational, -10, 20, 10}))
    ,?_assertNot(is_rational({rational, 20, 10}))
    ,?_assertNot(is_rational({rational, 20, 10, a}))
    ,?_assertNot(is_rational({rational, "20", 10, 5}))
    ,?_assertNot(is_rational({rational, 20, 2.2, 2}))
    ,?_assertNot(is_rational({rational, 20, -10, 1}))
    ,?_assertNot(is_rational({rational, 20, 10, -1}))
    ,?_assertNot(is_rational(10))
    ,?_assertNot(is_rational("10"))
    ].

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
