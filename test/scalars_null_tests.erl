-module(scalars_null_tests).

-include_lib("eunit/include/eunit.hrl").



%% Scalars - null



can_get_invalid_null_test() ->
    Map = jiffy_v:null(),
    Data = <<"not_null">>,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_NULL">>, <<>>, []}], Errors).



can_get_valid_null_test() ->
    Map = jiffy_v:null(),
    Data = null,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_null_error_code_test() ->
    Map = jiffy_v:null(),
    Data = null,
    Fun = fun
        (validate, [], null) ->
            %% Why invalidating null when you're expecting null here?
            {error, <<"CUSTOM_ERROR_CODE">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, _Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_null_error_code_test() ->
    Map = jiffy_v:null(),
    Data = 0,
    Fun = fun
        (fix, [], 0) ->
            {ok, null};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(null, Result).
