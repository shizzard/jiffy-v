-module(scalars_float_tests).

-include_lib("eunit/include/eunit.hrl").



%% Scalars - float



can_get_invalid_float_test() ->
    Map = jiffy_v:float(),
    Data = true,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_FLOAT">>, <<>>, []}], Errors).



can_get_valid_float_test() ->
    Map = jiffy_v:float(),
    Data = 10.0,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_valid_integer_as_float_test() ->
    %% df: I dont really remember why this is emplemented
    %% but anyway, backward compatibility
    Map = jiffy_v:float(),
    Data = 8,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_float_error_code_test() ->
    Fun = fun
        (validate, [], Value) when Value < 5 ->
            {error, <<"CUSTOM_ERROR_CODE">>}
    end,
    Map = jiffy_v:float(Fun),
    Data = 3.0,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_float_test() ->
    Fun = fun
        (fix, [], Value) when is_list(Value) ->
            {ok, list_to_float(Value)}
    end,
    Map = jiffy_v:float(Fun),
    Data = "2.19",
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(2.19, Result).
