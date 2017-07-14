-module(composites_enum_tests).

-include_lib("eunit/include/eunit.hrl").



%% Composites - enum



can_get_invalid_enum_test() ->
    Map = jiffy_v:enum([1,2,3]),
    Data = 7,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_ENUM">>, <<"">>, []}], Errors).



can_get_valid_enum_test() ->
    Map = jiffy_v:enum([1,2,3]),
    Data = 3,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_enum_error_code_test() ->
    Fun = fun
        (validate, [], Value) when Value < 4 ->
            {error, <<"CUSTOM_ERROR_CODE">>}
    end,
    Map = jiffy_v:enum([1,2,3], Fun),
    Data = 3,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_enum_test() ->
    Fun = fun
        (fix, [], _Value) ->
            {ok, <<"unknown">>}
    end,
    Map = jiffy_v:enum([<<"male">>, <<"female">>], Fun),
    Data = 1,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(<<"unknown">>, Result).
