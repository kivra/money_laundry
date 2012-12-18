-module(rational).

-export([
         from_string/1
         ,to_decimal_fraction/1
         ,numerator/1
         ,denominator/1
        ]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([
              rational/0
              ,decimal/0
             ]).

-type rational() :: {rational, integer(), pos_integer(), pos_integer()}.
-type decimal() :: {decimal, integer(), pos_integer()}.

%% @doc Given a decimal as a string, the term representing the rational
%% number is returned in this module's normalized form.
-spec from_string(Amount) -> rational() when
      Amount :: binary() | string().
from_string(Amount) when is_binary(Amount) ->
    from_string(binary_to_list(Amount));
from_string(Amount) when is_list(Amount) ->
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
                %% Denominator is a power of 10 here, but not necessarily elsewhere.
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
        normalize({rational, Numerator, Denominator, 1}).


%% @doc Given a rational number in this module's normalized form,
%% a decimal fraction is returned.
-spec to_decimal_fraction(rational()) -> decimal().
to_decimal_fraction({rational, Numerator, Denominator, DecimalFactor}) ->
    {decimal, Numerator*DecimalFactor, Denominator*DecimalFactor}.

-spec numerator(Amount) -> integer() when
      Amount :: rational() | decimal().
numerator({decimal, Numerator, _}) ->
    Numerator;
numerator({rational, Numerator, _, _}) ->
    Numerator.

-spec denominator(Amount) -> integer() when
      Amount :: rational() | decimal().
denominator({decimal, _, Denominator}) ->
    Denominator;
denominator({rational, _, Denominator, _}) ->
    Denominator.

%%==============================================================================
%% internal

%% Assumes a decimal fraction as imput
normalize({rational, Numerator, Denominator, 1}) ->
    %% List fold over normalize functions might be nicer
    GCD = case gcd(Numerator, Denominator) of
              Integer when Integer < 0 -> -Integer;
              Integer -> Integer
          end,
    _Norm1 = {rational, Numerator div GCD, Denominator div GCD, GCD}.


%% Source: http://en.literateprograms.org/Euclidean_algorithm_(Erlang)
-spec gcd(integer(), integer()) -> integer().
gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

%%==============================================================================

-ifdef(TEST).

from_string_test_() ->
    FromStringCases =
        [{"0", 0, 1, 1},
         {"0,0", 0, 1, 1},
         {"1", 1, 1, 1},
         {"1234567890,987654321", 1234567890987654321, 1000000000, 1},
         {"1,00", 1, 1, 1},
         {"-1", -1, 1, 1},
         {"-1,1", -11, 10, 1},
         {"0,5", 1, 2, 5},
         {"3,142", 1571, 500, 2},
         {"144,144", 18018, 125, 8},
         {"0.5", 1, 2, 5},
         {"3.142", 1571, 500, 2},
         {"144.144", 18018, 125, 8},
         {"190", 190, 1, 1}
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
        [
         {"1,00", 1, 1},
         {"-1", -1, 1},
         {"-1,1", -11, 10},
         {"0,5", 5, 10},
         {"3,142", 3142, 1000},
         {"-12,345", -12345, 1000},
         {"144,144", 144144, 1000},
         {"0,25", 25, 100},
         {"-0,25", -25, 100}
        ],
    [to_dec_frac_test_fun(String, Numer, Denom)
     || {String, Numer, Denom} <- ToDecFracCases].

to_dec_frac_test_fun(String, Numer, Denom) ->
    fun() ->
            ?assertEqual({decimal, Numer, Denom},
                         to_decimal_fraction(from_string(String)))
    end.

-endif.
