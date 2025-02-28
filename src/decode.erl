-module(decode).
-export([decode_nbt/1, test/0]).
-define(TAG_END, 0).
-define(TAG_BYTE, 1).
-define(TAG_SHORT, 2).
-define(TAG_INT, 3).
-define(TAG_LONG, 4).
-define(TAG_FLOAT, 5).
-define(TAG_DOUBLE, 6).
-define(TAG_BYTE_ARRAY, 7).
-define(TAG_STRING, 8).
-define(TAG_LIST, 9).
-define(TAG_COMPOUND, 10).
-define(TAG_INT_ARRAY, 11).
-define(TAG_LONG_ARRAY, 12).

decode_nbt(Data) ->
    decode_nbt(Data, []).
decode_nbt(<<>>, Acc) ->
    Acc;

decode_nbt(<<Tag:8, Data/binary>>, Acc) ->
    case Tag of
        ?TAG_END ->
            {Data, lists:reverse(Acc)};
        ?TAG_BYTE -> 
            decode_byte(Data, Acc);
        ?TAG_SHORT -> 
            decode_short(Data, Acc);
        ?TAG_INT -> 
            decode_int(Data, Acc);
        ?TAG_LONG -> 
            decode_long(Data, Acc);
        ?TAG_FLOAT -> 
            decode_float(Data, Acc);
        ?TAG_DOUBLE -> 
            decode_double(Data, Acc);
        ?TAG_BYTE_ARRAY -> 
            decode_byte_array(Data, Acc);
        ?TAG_STRING ->
            decode_string(Data, Acc);
        ?TAG_LIST -> 
            decode_list(Data, Acc);
        ?TAG_COMPOUND ->
            decode_compound(Data, Acc);
        ?TAG_INT_ARRAY -> 
            decode_int_array(Data, Acc);
        ?TAG_LONG_ARRAY -> 
            decode_long_array(Data, Acc)
    end.


decode_byte(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Byte:8, Data3/binary>> = Data2,
    decode_nbt(Data3, [{tag_byte, Name, Byte}|Acc]).
decode_short(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Short:16, Data3/binary>> = Data2,
    decode_nbt(Data3, [{tag_short, Name, Short}|Acc]).
decode_int(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Int:32, Data3/binary>> = Data2,
    decode_nbt(Data3, [{tag_int, Name, Int}|Acc]).
decode_long(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Long:64, Data3/binary>> = Data2,
    decode_nbt(Data3, [{tag_long, Name, Long}|Acc]).
decode_float(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Float:32/float, Data3/binary>> = Data2,
    decode_nbt(Data3, [{tag_float, Name, Float}|Acc]).
decode_double(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Double:64/float, Data3/binary>> = Data2,
    decode_nbt(Data3, [{tag_double, Name, Double}|Acc]).

decode_byte_array(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Length:32, Data3/binary>> = Data2,
    <<ByteArray:Length/binary, Data4/binary>> = Data3,
    decode_nbt(Data4, [{tag_byte_array, Name, ByteArray}|Acc]).
decode_string(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Length_of_string:16, Data3/binary>> = Data2,
    <<String:Length_of_string/binary, Data4/binary>> = Data3,
    decode_nbt(Data4, [{tag_string, Name, binary_to_list(String)}|Acc]).



decode_list(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Type:8, Length:32, Data3/binary>> = Data2,
    {Data4, Result} = list(Type, Data3, [], Length),
    decode_nbt(Data4, [{tag_list, Name, Result}|Acc]).


decode_compound(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    {Rest, Result} = decode_nbt(Data2, []),
    decode_nbt(Rest, [{tag_compound, Name, Result}|Acc]).

decode_int_array(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Length:32, Data3/binary>> = Data2,
    {Data4, Int_array} = int_array(Data3, [], Length),
    decode_nbt(Data4, [{tag_int_array, Name, Int_array}|Acc]).
int_array(Data, Acc, 0) ->
    {Data, lists:reverse(Acc)};

int_array(Data, Acc, I) ->
    <<Int:32, Data2/binary>> = Data,
    int_array(Data2, [Int|Acc], I-1).

decode_long_array(Data, Acc) ->
    {Name, Data2} = get_nbt_name(Data),
    <<Length:32, Data3/binary>> = Data2,
    {Data4, Long_array} = long_array(Data3, [], Length),
    decode_nbt(Data4, [{tag_long_array, Name, Long_array}|Acc]).
long_array(Data, Acc, 0) ->
    {Data, lists:reverse(Acc)};
long_array(Data, Acc, I) ->
    <<Int:64, Data2/binary>> = Data,
    long_array(Data2, [Int|Acc], I-1).



list(_Type, Data, Acc, 0) ->
    {Data, lists:reverse(Acc)};

list(Type, Data, Acc, I) ->
    
    case Type of
        ?TAG_END ->
            Acc;
        ?TAG_BYTE -> 
            decode_byte_list(Type, Data, Acc, I);
        ?TAG_SHORT -> 
            decode_short_list(Type, Data, Acc, I);
        ?TAG_INT -> 
            decode_int_list(Type, Data, Acc, I);
        ?TAG_LONG -> 
            decode_long_list(Type, Data, Acc, I);
        ?TAG_FLOAT -> 
            decode_float_list(Type, Data, Acc, I);
        ?TAG_DOUBLE -> 
            decode_double_list(Type, Data, Acc, I);
        ?TAG_BYTE_ARRAY -> 
            decode_byte_array_list(Type, Data, Acc, I);
        ?TAG_STRING ->
            decode_string_list(Type, Data, Acc, I);
        ?TAG_LIST -> 
            decode_list_list(Type, Data, Acc, I);
        ?TAG_COMPOUND ->
            decode_compound_list(Type, Data, Acc, I);
        ?TAG_INT_ARRAY -> 
            decode_int_array_list(Type, Data, Acc, I);
        ?TAG_LONG_ARRAY -> 
           decode_long_array_list(Type, Data, Acc, I)
    end.


decode_byte_list(Type, Data2, Acc, I) ->
    <<Byte:8, Data3/binary>> = Data2,
    list(Type, Data3, [{tag_byte, Byte}|Acc], I-1).
decode_short_list(Type, Data2, Acc, I) ->
    <<Short:16, Data3/binary>> = Data2,
    list(Type, Data3, [{tag_short, Short}|Acc], I-1).
decode_int_list(Type, Data2, Acc, I) ->
    <<Int:32, Data3/binary>> = Data2,
    list(Type, Data3, [{tag_int, Int}|Acc], I-1).
decode_long_list(Type, Data2, Acc, I) ->
    <<Long:64, Data3/binary>> = Data2,
    list(Type, Data3, [{tag_long, Long}|Acc], I-1).
decode_float_list(Type, Data2, Acc, I) ->
    <<Float:32/float, Data3/binary>> = Data2,
    list(Type, Data3, [{tag_long, Float}|Acc], I-1).
decode_double_list(Type, Data2, Acc, I) ->
    <<Double:64/float, Data3/binary>> = Data2,
    list(Type, Data3, [{tag_double, Double}|Acc], I-1).

decode_byte_array_list(Type, Data2, Acc, I) ->
    <<Length:32, Data3/binary>> = Data2,
    <<Byte_array:Length/binary, Data4/binary>> = Data3,
    list(Type, Data4, [{tag_byte_array, Byte_array}|Acc], I-1).
decode_string_list(Type, Data2, Acc, I) ->
    <<Length_of_string:16, Data3/binary>> = Data2,
    <<String:Length_of_string/binary, Data4/binary>> = Data3,
    list(Type, Data4, [{tag_string, String}|Acc], I-1).
decode_list_list(Type, Data2, Acc, _I) ->
    <<Type:8, Length:32, Data3/binary>> = Data2,
    {Result, Data4} = list(Type, Data3, Acc, Length),
    decode_nbt(Data4, [{tag_list, Result}|Acc]).
decode_compound_list(Type, Data2, Acc, I) ->
    {Rest, Result} = decode_nbt(Data2, []),
    list(Type, Rest, [{tag_compound, Result}|Acc], I-1).


decode_int_array_list(Type, Data2, Acc, I) ->
    <<Length:32, Data3/binary>> = Data2,
    {Data4, Int_array} = int_array_list(Data3, [], Length),
    list(Data4, Type, [{tag_int_array, Int_array}|Acc], I-1).
int_array_list(Data, Acc, 0) ->
    {Data, lists:reverse(Acc)};

int_array_list(Data, Acc, I) ->
    <<Int:32, Data2/binary>> = Data,
    int_array_list(Data2, [Int|Acc], I-1).

decode_long_array_list(Type, Data2, Acc, I) ->
    <<Length:32, Data3/binary>> = Data2,
    {Data4, Long_array} = long_array_list(Data3, [], Length),
    list(Data4, Type, [{tag_int_array, Long_array}|Acc], I-1).

long_array_list(Data, Acc, 0) ->
    {Data, lists:reverse(Acc)};
long_array_list(Data, Acc, I) ->
    <<Int:64, Data2/binary>> = Data,
    long_array_list(Data2, [Int|Acc], I-1).




get_nbt_name(Data) ->
    <<Length:16, Data2/binary>> = Data,
    <<Name:Length/binary, Data3/binary>> = Data2,
    {binary_to_list(Name), Data3}.


test() ->
    {ok, Data} = file:read_file("test/bigtest.nbt"),
    Data2 = zlib:gunzip(Data),
    io:format("~p~n", [Data2]),
    io:format("~p~n", [decode_nbt(Data2, [])]).
