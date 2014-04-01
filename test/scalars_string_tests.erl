-module(scalars_string_tests).

-include_lib("eunit/include/eunit.hrl").



%% Scalars - string



can_get_invalid_string_test() ->
    Map = {string},
    Data = 45,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_STRING">>, <<>>, []}], Errors).



can_get_valid_string_test() ->
    Map = {string},
    Data = <<"string">>,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_string_error_code_test() ->
    Map = {string},
    Data = <<"string">>,
    Fun = fun
        (validate, [], <<"string">>) ->
            {error, <<"CUSTOM_ERROR_CODE">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, _Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_string_test() ->
    Map = {string},
    Data = "string",
    Fun = fun
        (fix, [], Value) when is_list(Value) ->
            {ok, list_to_binary(Value)};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(<<"string">>, Result).