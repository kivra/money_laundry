-module(money_laundry).

-export([
         new/2
         ,format/2
         ,currency_code/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type currency_atom() :: sek.
-type currency_code() :: binary().
-type laundry_money() :: {money_laundry, currency_atom(), rational:rational()}.
-type format() :: oere
                | decimal.

%% @doc Assumes Amount uses comma for decimal separator, has no whitespace,
%% no thousands separator. That means the only valid characters are
%% 0123456789,-
%%
%% It also assumes the amount is well-formed, ie ,13 is not 0,13, but money_laundry
%% will not necessarily do anything good with it.
-spec new(Amount, Currency) -> laundry_money() when
      Amount :: binary(), Currency :: currency_code().
new(Amount, Currency) ->
    {money_laundry, currency_to_internal(Currency), rational:from_string(Amount)}.

%% @doc Returns a string representation of the amount, defined by the given format.
%% Oere returns an integer for SEK amounts. decimal returns a decimal form of any
%% amount.
%%
%% proper:check_specs/1 quickly finds examples that don't work, e.g.
%% [oere,{money_laundry,sek,{rational,1,3}}] which is infinitely repeating.
-spec format(format(), laundry_money()) -> binary().
format(Format, {money_laundry, Currency, Amount={rational,_,_,_}}) ->
    DecimalFrac = rational:to_decimal_fraction(Amount),
    format(Format, {money_laundry, Currency, DecimalFrac});

%% Keep it undefined for fractions with denominators > 100 because that
%% means losing precision in an öre integer.
format(oere, {money_laundry, sek, {decimal, Numerator, 100}}) ->
    list_to_binary(integer_to_list(Numerator));
format(oere, {money_laundry, sek, {decimal, Numerator, 10}}) ->
    list_to_binary(integer_to_list(Numerator*10));
format(oere, {money_laundry, sek, {decimal, Numerator, 1}}) ->
    list_to_binary(integer_to_list(Numerator*100));

format(decimal, {money_laundry, sek, {decimal, Numerator, Denominator}}) ->
    Fraction = Numerator rem Denominator,
    Integer = round((Numerator - Fraction)/Denominator),
    iolist_to_binary(io_lib:format("~B.~B", [Integer, Fraction])).

%% @doc Returns the currency code for a given currency amount.
-spec currency_code(laundry_money()) -> currency_code().
currency_code({money_laundry, CurrencyTerm, _}) ->
    internal_to_currency(CurrencyTerm).


%%==============================================================================

currency_to_internal(<<"SEK">>) ->
    sek.

internal_to_currency(sek) ->
    <<"SEK">>.


-ifdef(TEST).


%% Basically just tests that it doesn't crash and the the parts we care about
%% are as expected.
new_test() ->
    {money_laundry, sek, _Opaque} = money_laundry:new(<<"1234,56">>, <<"SEK">>).

format_oere_test_() ->
    Cases =
        [{<<"1234,56">>, <<"123456">>}
         ,{<<"10,1">>, <<"1010">>}
         ,{<<"10,10">>, <<"1010">>}
         ,{<<"1234">>, <<"123400">>}

         %% Found by proper:check_specs/1: oere,{money_laundry,sek,{rational,-1,4}}
         ,{<<"-0,25">>, <<"-25">>}
        ],
    [format_oere_test_fun(String, Expected) || {String, Expected} <- Cases].

format_oere_test_fun(String, Expected) ->
    fun() ->
            Rational = rational:from_string(String),
            ?assertEqual(Expected,
                         money_laundry:format(oere, {money_laundry, sek, Rational}))
    end.

%% Decimal separator for from_string is allowed to be , or .
%% Decimal separator for decimal format to format/1 is .
format_decimal_test() ->
    Rational = rational:from_string(<<"1234,56">>),
    ?assertEqual(<<"1234.56">>,
                  money_laundry:format(decimal, {money_laundry, sek, Rational})).

-endif.
