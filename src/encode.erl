-module(encode).
-export([encode/0]).
-define(TAG_END, 0).
-define(TAG_BYTE, 1).
-define(TAG_SHORT, 2).
-define(TAG_INT, 3).
-define(TAG_LONG, 4).
-define(TAG_FLOAT, 5).
-define(TAG_DOUBLE, 6).
-define(TAG_BYTE_ARRAY, 7).
-define(TAG_STRING, 8).
-define(TAG_BIG_LIST, 9).
-define(TAG_COMPOUND, 10).
-define(TAG_INT_ARRAY, 11).
-define(TAG_LONG_ARRAY, 12).


encode() ->
    {ok, Data} = file:read_file("test/bigtest.nbt"),
    
    io:format("~p~n", [zlib:gunzip(Data)]).

