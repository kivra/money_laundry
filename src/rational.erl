-module(rational).

-export([
         from_string/1
         ,to_decimal_fraction/1
        ]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

from_string(Amount) when is_binary(Amount) ->
    from_string(binary_to_list(Amount));
from_string(Amount) when is_list(Amount) ->
    {Numerator, Denominator} =
        case string:tokens(Amount, ",") of
            %% Integer starts with minus sign
            [[$-|_] = Integer, Fraction] ->
                %% Denominator is a power of 10 here, but not necessarily elsewhere.
                Denominator1 = round(math:pow(10, length(Fraction))),
                Numerator1 =
                    list_to_integer(Integer) * Denominator1 -
                    list_to_integer(Fraction),
                {Numerator1, Denominator1};
            %% Positive integer with fraction part
            [Integer, Fraction] ->
                Denominator1 = round(math:pow(10, length(Fraction))),
                Numerator1 =
                    list_to_integer(Integer) * Denominator1 +
                    list_to_integer(Fraction),
                {Numerator1, Denominator1};
            %% Positive or negative integer
            %% (can also match ,123 as 123
            %%  but current assumption is well-formed input)
            [Integer] ->
                Denominator1 = 1,
                Numerator1 =
                    list_to_integer(Integer)*Denominator1,
                {Numerator1, Denominator1}
        end,
        normalize({rational, Numerator, Denominator}).


to_decimal_fraction({rational, Numerator, Denominator}) ->
    Power = ceiling(math:log10(Denominator)),
    DecDenom = trunc(math:pow(10, Power)),
    DecDenomFactor = trunc(DecDenom/Denominator),
    {decimal, Numerator*DecDenomFactor, Denominator*DecDenomFactor}.

normalize({rational, Numerator, Denominator}) ->
    %% List fold over normalize functions might be nicer
    GCD = case gcd(Numerator, Denominator) of
              Integer when Integer < 0 -> -Integer;
              Integer -> Integer
          end,
    _Norm1 = {rational, Numerator div GCD, Denominator div GCD}.


%% Source: http://en.literateprograms.org/Euclidean_algorithm_(Erlang)
-spec gcd(integer(), integer()) -> integer().
gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

%%% Source: http://schemecookbook.org/Erlang/NumberRounding
-spec ceiling(float()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 ->
            T;
        Pos when Pos > 0 ->
            T + 1;
        _ ->
            T
    end.


-ifdef(TEST).

from_string_test_() ->
    FromStringCases =
        [{"0", 0, 1},
         {"0,0", 0, 1},
         {"1", 1, 1},
         {"1234567890,987654321", 1234567890987654321, 1000000000},
         {"1,00", 1, 1},
         {"-1", -1, 1},
         {"-1,1", -11, 10},
         {"0,5", 1, 2},
         {"3,142", 1571, 500},
         {"144,144", 18018, 125}],
    [from_string_test_fun(String, Numer, Denom)
     || {String, Numer, Denom} <- FromStringCases].

from_string_test_fun(String, Numer, Denom) ->
    fun() ->
            ?assertEqual({rational, Numer, Denom}, rational:from_string(String))
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
         {"144,144", 144144, 1000}
        ],
    [to_dec_frac_test_fun(String, Numer, Denom)
     || {String, Numer, Denom} <- ToDecFracCases].

to_dec_frac_test_fun(String, Numer, Denom) ->
    fun() ->
            ?assertEqual({decimal, Numer, Denom},
                         to_decimal_fraction(from_string(String)))
    end.

-endif.
