money_laundry
=============

Erlang Currency and money laundering functions

``` erlang
1> money_laundry:new(<<"1234,56">>, <<"SEK">>).
2> {money_laundry, sek, {rational, 123456, 100}}

3> money_laundry:format(oere, {money_laundry, sek, {rational, 123456, 100}}).
4> <<"123456">>
```
