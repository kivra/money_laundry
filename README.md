Money Laundry (1.1.0)
=============

Erlang Currency and money laundering functions. The rationale behind a custom
numbers library is to be able to store and process numeric values while only
losing precision when necessary.

money_laundry is released under the terms of the [MIT](http://en.wikipedia.org/wiki/MIT_License) license

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
