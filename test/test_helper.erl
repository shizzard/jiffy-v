-module(test_helper).

-export([get/2]).



get(undefined, _Keys) ->
    {error, undefined};
get(Value, Key) when not is_list(Key) ->
    get(Value, [Key]);
get(Value, []) ->
    {ok, Value};
get({Proplist}, [Key | Keys]) ->
    Struct = proplists:get_value(Key, Proplist),
    get(Struct, Keys);
get(_Value, Keys) ->
    get(undefined, Keys).