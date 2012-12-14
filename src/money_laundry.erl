-module(money_laundry).

-export([
         new/2
         ,format/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

new(Amount, Currency) ->
    {money_laundry, currency_to_internal(Currency), rational:from_string(Amount)}.


format(oere, {money_laundry, sek, {rational, Numerator, 100}}) ->
    list_to_binary(integer_to_list(Numerator));

format(decimal, {money_laundry, sek, {rational, Numerator, Denominator}}) ->
    Fraction = Numerator rem Denominator,
    Integer = round((Numerator - Fraction)/Denominator),
    iolist_to_binary(io_lib:format("~B,~B", [Integer, Fraction])).

currency_to_internal(<<"SEK">>) ->
    sek.


-ifdef(TEST).

%% TODO: 0 0.0 1 - 1234567890.1234567890 000 1.000 etc
new_test() ->
    {money_laundry, sek, _Opaque} = money_laundry:new(<<"1234,56">>, <<"SEK">>).

format_oere_test() ->
    Rational = rational:from_string(<<"1234,56">>),
    ?assertEqual(<<"123456">>,
                  money_laundry:format(oere, {money_laundry, sek, Rational})).

format_decimal_test() ->
    Rational = rational:from_string(<<"1234,56">>),
    ?assertEqual(<<"1234,56">>,
                  money_laundry:format(decimal, {money_laundry, sek, Rational})).

-endif.
