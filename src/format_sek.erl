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
%%% @doc Money format for SEK, Swedish krona
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(format_sek).
-behaviour(money_format).

%%%_* Includes =========================================================
-include("money_laundry.hrl").

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([format/2]).

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
%% @doc Keep it undefined for fractions with denominators > 100 because that
%%      means losing precision in an öre integer.
format(oere, #money_laundry{rational={decimal, Num, 100}}) ->
    integer_to_binary(Num);
format(oere, #money_laundry{rational={decimal, Num, 10}}) ->
    integer_to_binary(Num*10);
format(oere, #money_laundry{rational={decimal, Num, 1}}) ->
    integer_to_binary(Num*100);
format(decimal, #money_laundry{rational={decimal, Num, Den}}) ->
    Fract      = Num rem Den,
    FractWidth = trunc(math:log10(Den)),
    Integer    = round((Num - Fract)/Den),
    case FractWidth of
        0 -> iolist_to_binary(io_lib:format("~B", [Integer]));
        _ -> iolist_to_binary(
                 io_lib:format( "~B.~"++integer_to_list(FractWidth)++"..0B"
                              , [Integer, Fract]) )
    end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
