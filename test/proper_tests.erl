%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (c) 2012-2016 Kivra
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
-module(proper_tests).

%%%_* Includes =========================================================
-include("money_laundry.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ============================================================
new_test() ->
    ?assertEqual(true, proper:quickcheck( ?MODULE:prop_dec()
                                        , [{to_file, user}, 10000]) ).

prop_dec() ->
    ?FORALL(Val, decimal_val(),
      begin
        [Int, Dec] = binary:split(Val, <<".">>),
        TestVal    =
          case { list_to_integer(binary_to_list(Int)) == 0
               , list_to_integer(binary_to_list(Dec)) == 0} of
              {true, true}   -> <<"0">>;
              {true, false}  -> strip_padded_zeroes(Val);
              {false, true}  -> Int;
              {false, false} -> strip_padded_zeroes(Val)
          end,
        TestVal =:= money_laundry:format( decimal
                                        , money_laundry:new(TestVal, <<"SEK">>))
      end).

strip_padded_zeroes(Val) ->
    list_to_binary(string:strip(binary_to_list(Val), right, $0)).

decimal_val() ->
    ?LET( {Precision, X}
        , {pos_integer(), float()}
        , iolist_to_binary(io_lib:format("~."++integer_to_list(Precision)++"f", [X])) ).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 4
%%% End:
