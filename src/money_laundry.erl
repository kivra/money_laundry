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
%%% @doc Money Laundry. Monetary parsing and formatting
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(money_laundry).

%%%_* Includes =========================================================
-include("money_laundry.hrl").

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([new/2]).
-export([format/2]).
-export([currency_code/1]).
-export([checkspecs_test/1]).
-export([is_money_laundry/1]).

-export_type([laundry_money/0]).
-export_type([currency_atom/0]).

%%%_ * Types -----------------------------------------------------------
-type currency_atom() :: sek.
-type currency_code() :: binary().
-opaque laundry_money() :: #money_laundry{}.

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
%% @doc Assumes Amount uses comma for decimal separator, has no whitespace,
%%      no thousands separator. That means the only valid characters are
%%      0123456789,-
%%
%%      It also assumes the amount is well-formed, ie ,13 is not 0,13,
%%      but money_laundry will not necessarily do anything good with it.
-spec new(binary(), currency_code()) -> laundry_money().
new(Amount, Currency) ->
    #money_laundry{ currency=currency_to_internal(Currency)
                  , rational=rational:from_string(Amount) }.

%% @doc Returns a string representation of the amount, defined by the given
%%      format. Oere returns an integer for SEK amounts. decimal returns a
%%      decimal form of any amount.
%%
%%      proper:check_specs/1 quickly finds examples that don't work, e.g.
%%      [oere,{money_laundry,sek,{rational,1,3}}] which is infinitely repeating.
-spec format(money_format:format(), laundry_money()) -> binary().
format(Format, #money_laundry{rational={rational,_,_,_}=Amount}=ML) ->
    format( Format
          , ML#money_laundry{rational=rational:to_decimal_fraction(Amount)} );

%% @doc Format a money_laundry representation using the given format for
%%      the current currency
format(Format, #money_laundry{currency=sek}=ML) ->
    format_sek:format(Format, ML).

%% @doc Checks if the input seems to be a valid money_laundry term
-spec is_money_laundry(any()) -> boolean().
is_money_laundry(#money_laundry{rational=Amnt}) -> rational:is_rational(Amnt);
is_money_laundry(_)                             -> false.

%% @doc Returns the currency code for a given currency amount.
-spec currency_code(laundry_money()) -> currency_code().
currency_code(#money_laundry{currency=CurrencyTerm}) ->
    internal_to_currency(CurrencyTerm).

%% @doc This is just a quick way to let quickcheck try and break money_laundry
%%      without writing properties for it yet.
-spec checkspecs_test(float()) -> float().
checkspecs_test(Float) ->
    BinIn = iolist_to_binary(io_lib:format("~p", [Float])),
    BinOut = money_laundry:format(decimal, money_laundry:new(BinIn, <<"SEK">>)),
    FloatOut = list_to_float(binary_to_list(BinOut)),
    case abs(FloatOut - Float) < 0.000001 of
        true  -> FloatOut;
        false ->
            io:format("Not close enough:~n~p~n~p~n", [Float, FloatOut]),
            Float = FloatOut
    end.

%%%_* Private functions ================================================
currency_to_internal(<<"SEK">>) -> sek.
internal_to_currency(sek)       -> <<"SEK">>.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Basically just tests that it doesn't crash and the the parts we care about
%% are as expected.
new_test() ->
    {money_laundry, sek, _} = money_laundry:new(<<"1234,56">>, <<"SEK">>).

format_oere_test_() ->
    Cases =
        [{<<"1234,56">>, <<"123456">>}
         ,{<<"10,1">>,   <<"1010">>}
         ,{<<"10,10">>,  <<"1010">>}
         ,{<<"1234">>,   <<"123400">>}
         ,{<<"-0,25">>,  <<"-25">>}    %% Found by proper:check_specs/1:
         ,{<<"20.01">>,  <<"2001">>}   %% oere,{money_laundry,sek,{rational,-1,4}}
        ],
    [format_oere_test_fun(String, Expected) || {String, Expected} <- Cases].

format_oere_test_fun(String, Expected) ->
    fun() ->
        ?assertEqual( Expected
          , money_laundry:format(oere, { money_laundry
                                       , sek
                                       , rational:from_string(String) }) )
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

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
