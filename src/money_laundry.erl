-module(money_laundry).

-export([
         new/2
         ,format/2
         ,currency_code/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Assumes Amount uses comma for decimal separator, has no whitespace,
%% no thousands separator. That means the only valid characters are
%% 0123456789,-
%%
%% It also assumes the amount is well-formed, ie ,13 is not 0,13, but money_laundry
%% will not necessarily do anything good with it.
new(Amount, Currency) ->
    {money_laundry, currency_to_internal(Currency), rational:from_string(Amount)}.

format(Format, {money_laundry, Currency, {rational, Numerator, Denominator}}) ->
    DecimalFrac = rational:to_decimal_fraction({rational, Numerator, Denominator}),
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
    iolist_to_binary(io_lib:format("~B,~B", [Integer, Fraction])).


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
         ,{<<"1234">>, <<"123400">>}
        ],
    [format_oere_test_fun(String, Expected) || {String, Expected} <- Cases].

format_oere_test_fun(String, Expected) ->
    fun() ->
            Rational = rational:from_string(String),
            ?assertEqual(Expected,
                         money_laundry:format(oere, {money_laundry, sek, Rational}))
    end.

format_decimal_test() ->
    Rational = rational:from_string(<<"1234,56">>),
    ?assertEqual(<<"1234,56">>,
                  money_laundry:format(decimal, {money_laundry, sek, Rational})).

-endif.
