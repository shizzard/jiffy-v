-module(composites_enum_m_tests).

-include_lib("eunit/include/eunit.hrl").



%% Composites - enum



can_get_invalid_enum_test() ->
    Map = jiffy_vm:enum([1,2,3]),
    Data = 7,
    {Errors, _Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_ENUM">>, <<"">>, []}], Errors).



can_get_valid_enum_test() ->
    Map = jiffy_vm:enum([1,2,3]),
    Data = 3,
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_enum_error_code_test() ->
    Map = jiffy_vm:enum([1,2,3]),
    Data = 3,
    Fun = fun
        (validate, [], Value) when Value < 4 ->
            {error, <<"CUSTOM_ERROR_CODE">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, _Result} = jiffy_vm:validate(Map, Data, Fun),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_enum_test() ->
    Map = jiffy_vm:enum([<<"male">>, <<"female">>]),
    Data = 1,
    Fun = fun
        (fix, [], _Value) ->
            {ok, <<"unknown">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_vm:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(<<"unknown">>, Result).
