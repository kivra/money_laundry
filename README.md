money_laundry
=============

Erlang Currency and money laundering functions. The rationale behind a custom
numbers library is to be able to store and process numeric values while only
losing precision when necessary.

``` erlang
1> Money = money_laundry:new(<<"1234,56">>, <<"SEK">>).
{money_laundry,sek,{rational,30864,25,4}}
2> money_laundry:format(oere, Money).
<<"123456">>
3> money_laundry:format(decimal, Money).
<<"1234.56">>
```

## Testing

Test with

```
$ make eunit
```
