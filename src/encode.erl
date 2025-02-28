-module(encode).
-export([encode_nbt/1]).


encode_nbt(Data) ->
    encode_nbt(Data, <<>>).

encode_nbt([], Acc) ->
    Acc;
encode_nbt([{Tag, Name, Value}|Data], Acc) ->

    case Tag of
        tag_byte ->
            encode_byte([{Tag, Name, Value}|Data], Acc);
        tag_short ->
            encode_short([{Tag, Name, Value}|Data], Acc);
        tag_int ->
            encode_int([{Tag, Name, Value}|Data], Acc);
        tag_long ->
            encode_long([{Tag, Name, Value}|Data], Acc);
        tag_float ->
            encode_float([{Tag, Name, Value}|Data], Acc);
        tag_double ->
            encode_double([{Tag, Name, Value}|Data], Acc);
        tag_byte_array ->
            encode_byte_array([{Tag, Name, Value}|Data], Acc);
        tag_string ->
            encode_string([{Tag, Name, Value}|Data], Acc);
        tag_list ->
            encode_list([{Tag, Name, Value}|Data], Acc);
        tag_compound ->
            encode_compound([{Tag, Name, Value}|Data], Acc);
        tag_int_array ->
            encode_int_array([{Tag, Name, Value}|Data], Acc);
        tag_long_array ->
            encode_long_array([{Tag, Name, Value}|Data], Acc)
    end.

encode_byte([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Value:8>>).
encode_short([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Value:16>>).
encode_int([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Value:32>>).
encode_long([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Value:64>>).
encode_float([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Value:32/float>>).
encode_double([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Value:64/float>>).
encode_byte_array([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    Length = byte_size(Value),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Length:32, Value/binary>>).
encode_string([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    Length = length(Value),
    Value2 = list_to_binary(Value),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Length:16, Value2/binary>>).
encode_list([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    Length = length(Value),
    Result = list(Value, <<>>),
    [H|_T] = Value,
    {Tag3, _Value3} = H,
    Tag4 = encode_tag(Tag3),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Tag4/binary, Length:32, Result/binary>>).


encode_compound([{Tag, Name, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Name2 = encode_name(Name),
    Result = encode_nbt(Value),
    encode_nbt(Data, <<Acc/binary, Tag2/binary, Name2/binary, Result/binary, 0>>).

encode_int_array([{Tag, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Length = length(Value),
    Result = int_array(Value, <<>>),
    list(Data, <<Acc/binary, Tag2/binary, Length:32, Result/binary>>).
int_array([], Acc) ->
    Acc;
int_array([Value|Data], Acc) ->
    int_array(Data, <<Acc/binary, Value:32>>).

encode_long_array([{Tag, Value}|Data], Acc) ->
    Tag2 = encode_tag(Tag),
    Length = length(Value),
    Result = long_array(Value, <<>>),
    list(Data, <<Acc/binary, Tag2/binary, Length:32, Result/binary>>).
long_array([], Acc) ->
    Acc;
long_array([Value|Data], Acc) ->
    long_array(Data, <<Acc/binary, Value:32>>).


list([], Acc) ->
    Acc;
list([{Tag, Value}|Data], Acc) ->
    case Tag of
        
        tag_byte ->
            encode_byte_list([{Tag, Value}|Data], Acc);
        tag_short ->
            encode_short_list([{Tag, Value}|Data], Acc);
        tag_int ->
            encode_int_list([{Tag, Value}|Data], Acc);
        tag_long ->
            encode_long_list([{Tag, Value}|Data], Acc);
        tag_float ->
            encode_float_list([{Tag, Value}|Data], Acc);
        tag_double ->
            encode_double_list([{Tag, Value}|Data], Acc);
        tag_byte_array ->
            encode_byte_array_list([{Tag, Value}|Data], Acc);
        tag_string ->
            encode_string_list([{Tag, Value}|Data], Acc);
        tag_list ->
            encode_list_list([{Tag, Value}|Data], Acc);
        tag_compound ->
            encode_compound_list([{Tag, Value}|Data], Acc);
        tag_int_array ->
            encode_int_array_list([{Tag, Value}|Data], Acc);
        tag_long_array ->
            encode_long_array_list([{Tag, Value}|Data], Acc)
    end.
encode_byte_list([{_Tag, Value}|Data], Acc) ->
    list(Data, <<Acc/binary, Value:8>>).
encode_short_list([{_Tag, Value}|Data], Acc) ->
    list(Data, <<Acc/binary, Value:16>>).
encode_int_list([{_Tag, Value}|Data], Acc) ->
    list(Data, <<Acc/binary, Value:32>>).
encode_long_list([{_Tag, Value}|Data], Acc) ->
    list(Data, <<Acc/binary, Value:64>>).
encode_float_list([{_Tag, Value}|Data], Acc) ->
    list(Data, <<Acc/binary, Value:32/float>>).
encode_double_list([{_Tag, Value}|Data], Acc) ->
    list(Data, <<Acc/binary, Value:64/float>>).
encode_byte_array_list([{_Tag, Value}|Data], Acc) ->
    Length = byte_size(Value),
    list(Data, <<Acc/binary, Length:32, Value/binary>>).
encode_string_list([{_Tag, Value}|Data], Acc) ->
    Length = length(Value),
    Value2 = list_to_binary(Value),
    list(Data, <<Acc/binary, Length/binary, Value2/binary>>).
encode_list_list([{_Tag, Value}|Data], Acc) ->
    Length = length(Value),
    Result = list(Value, <<>>),
    list(Data, <<Acc/binary, Length/binary, Result/binary>>).
encode_compound_list([{_Tag, Value}|Data], Acc) ->
    Result = encode_nbt(Value),
    list(Data, <<Acc/binary, Result/binary, 0>>).

encode_int_array_list([{_Tag, Value}|Data], Acc) ->
    Length = length(Value),
    Result = int_array_list(Value, <<>>),
    list(Data, <<Acc/binary, Length:32, Result/binary>>).
int_array_list([], Acc) ->
    Acc;
int_array_list([Value|Data], Acc) ->
    int_array_list(Data, <<Acc/binary, Value:32>>).

encode_long_array_list([{_Tag, Value}|Data], Acc) ->
    Length = length(Value),
    Result = long_array_list(Value, <<>>),
    list(Data, <<Acc/binary, Length:32, Result/binary>>).
long_array_list([], Acc) ->
    Acc;
long_array_list([Value|Data], Acc) ->
    long_array_list(Data, <<Acc/binary, Value:32>>).







encode_name(Data) ->
    Length = length(Data),
    Data2 = list_to_binary(Data),
    <<Length:16, Data2/binary>>.

encode_tag(Tag) ->
    case Tag of
        tag_end -> <<0>>;
        tag_byte -> <<1>>;
        tag_short -> <<2>>;
        tag_int -> <<3>>;
        tag_long -> <<4>>;
        tag_float -> <<5>>;
        tag_double -> <<6>>;
        tag_byte_array -> <<7>>;
        tag_string -> <<8>>;
        tag_list -> <<9>>;
        tag_compound -> <<10>>;
        tag_int_array -> <<11>>;
        tag_long_array -> <<12>>
    end.