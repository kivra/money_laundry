-module(money_laundry).

-export([
         new/2
         ,format/2
         ,currency_code/1
         ,checkspecs_test/1
         ,is_money_laundry/1
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
    FractionWidth = trunc(math:log10(Denominator)),
    Integer = round((Numerator - Fraction)/Denominator),
    case FractionWidth of
        0 ->
            iolist_to_binary(io_lib:format("~B", [Integer]));
        _ ->
            Fmt = "~B.~"++integer_to_list(FractionWidth)++"..0B",
            iolist_to_binary(io_lib:format(Fmt, [Integer, Fraction]))
    end.

%% @doc Checks if the input seems to be a valid money_laundry term
-spec is_money_laundry(any()) -> boolean().
is_money_laundry({money_laundry, sek, Amount}) ->
    rational:is_rational(Amount);
is_money_laundry(_) ->
    false.

%% @doc Returns the currency code for a given currency amount.
-spec currency_code(laundry_money()) -> currency_code().
currency_code({money_laundry, CurrencyTerm, _}) ->
    internal_to_currency(CurrencyTerm).


%% @doc This is just a quick way to let quickcheck try and break money_laundry
%% without writing properties for it yet.
-spec checkspecs_test(float()) -> float().
checkspecs_test(Float) ->
    BinIn = iolist_to_binary(io_lib:format("~p", [Float])),
    BinOut = money_laundry:format(decimal, money_laundry:new(BinIn, <<"SEK">>)),
    FloatOut = list_to_float(binary_to_list(BinOut)),
    case abs(FloatOut - Float) < 0.000001 of
        true ->
            FloatOut;
        false ->
            io:format("Not close enough:~n~p~n~p~n", [Float, FloatOut]),
            Float = FloatOut
    end.

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
         ,{<<"20.01">>, <<"2001">>}
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
format_decimal_test_() ->
    Cases =
        [{<<"1234.56">>, <<"1234,56">>}
         ,<<"1234.56">>
         ,<<"20.01">>
         ,<<"20.001">>
         ,<<"21.111">>
        ],
    [format_decimal_test_fun(String) || String <- Cases].

format_decimal_test_fun({Expected, String}) ->
    fun() ->
            Rational = rational:from_string(String),
            ?assertEqual(Expected,
                         money_laundry:format(decimal,
                                              {money_laundry, sek, Rational}))
    end;
format_decimal_test_fun(String) ->
    format_decimal_test_fun({String, String}).

is_money_laundry_test_() ->
    {foreach,
        fun mock_rational/0,
        fun unmock_rational/1,
        [fun (_) ->
            [?_assertEqual(true,
                is_money_laundry({money_laundry, sek, i_am_rational}))
            ,?_assertEqual(false,
                is_money_laundry({money_laundry, sek, not_rational}))
            ,?_assertEqual(false, is_money_laundry({money_laundry, "sek", 10}))
            ,?_assertEqual(false, is_money_laundry({money_laundry, 20, 10}))
            ,?_assertEqual(false, is_money_laundry(10))
            ,?_assertEqual(false, is_money_laundry("10"))
            ]
         end
        ]
    }.

mock_rational() ->
    ok = meck:new(rational),
    IsRational =
        fun(i_am_rational) -> true;
           (_)             -> false
        end,
    ok = meck:expect(rational, is_rational, IsRational).

unmock_rational(_) ->
    ok = meck:unload(rational).

-endif.

