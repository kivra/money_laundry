-module(rational).

-export([
         from_string/1
        ]).

from_string(Amount) when is_binary(Amount) ->
    from_string(binary_to_list(Amount));
from_string(Amount) when is_list(Amount) ->
    [Integer, Fraction] = string:tokens(Amount, ","),
    Denominator = round(math:pow(10, length(Fraction))),
    Numerator = list_to_integer(Integer)*Denominator + list_to_integer(Fraction),
    {rational, Numerator, Denominator}.
