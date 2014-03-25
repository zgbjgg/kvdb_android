-module(module_test).

-export([catching/2]).

catching(<<"GET">>, BinKey) ->
    Key = binary_to_list(BinKey),
    [Obj] = kvdb_android_mnesia:get(your_table, data1, yourkey, [Key]),
    {ok, list_to_binary(Obj)};
catching(<<"POST">>, _Body) ->
    ok = kvdb_android_mnesia:put(your_table, [{yourkey, "Key"}, {data1, "Hello"}, {data2, "From"}, {data3, "KVDB"}]),
    {ok, <<"ok">>}.
