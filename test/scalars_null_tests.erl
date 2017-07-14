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
    Fun = fun
        (validate, [], null) ->
            %% Why invalidating null when you're expecting null here?
            {error, <<"CUSTOM_ERROR_CODE">>}
    end,
    Map = jiffy_v:null(Fun),
    Data = null,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_null_error_code_test() ->
    Fun = fun
        (fix, [], 0) ->
            {ok, null}
    end,
    Map = jiffy_v:null(Fun),
    Data = 0,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(null, Result).
