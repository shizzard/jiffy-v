-module(scalars_string_tests).

-include_lib("eunit/include/eunit.hrl").



%% Scalars - string



can_get_invalid_string_test() ->
    Map = jiffy_v:string(),
    Data = 45,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_STRING">>, <<>>, []}], Errors).



can_get_valid_string_test() ->
    Map = jiffy_v:string(),
    Data = <<"string">>,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_string_error_code_test() ->
    Fun = fun
        (validate, [], <<"string">>) ->
            {error, <<"CUSTOM_ERROR_CODE">>}
    end,
    Map = jiffy_v:string(Fun),
    Data = <<"string">>,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_string_test() ->
    Fun = fun
        (fix, [], Value) when is_list(Value) ->
            {ok, list_to_binary(Value)}
    end,
    Map = jiffy_v:string(Fun),
    Data = "string",
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(<<"string">>, Result).
