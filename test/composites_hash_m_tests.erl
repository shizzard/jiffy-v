-module(composites_hash_m_tests).

-include_lib("eunit/include/eunit.hrl").



%% Composites - hash



can_get_invalid_hash_error_test() ->
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer())
    ]),
    Data = not_hash,
    {Errors, _Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertEqual([{<<"INVALID_HASH">>, <<"">>, []}], Errors).



can_get_mapped_hash_value_test() ->
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer())
    ]),
    Data = #{<<"foo">> => 1},
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({ok, 1}, maps:find(<<"foo">>, Result)).



cannot_get_unmapped_hash_field_test() ->
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer())
    ]),
    Data = #{<<"foo">> => 1, <<"bar">> => 2},
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(error, maps:find(<<"bar">>, Result)).



can_get_result_with_undefined_hash_optional_field_test() ->
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer()),
        jiffy_vm:hashfield(<<"bar">>, optional, jiffy_vm:integer())
    ]),
    Data = #{<<"foo">> => 1},
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(error, maps:find(<<"bar">>, Result)),
    ?assertMatch({ok, 1}, maps:find(<<"foo">>, Result)).



can_get_undefined_field_error_test() ->
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer())
    ]),
    Data = #{<<"bar">> => 1},
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch(error, maps:find(<<"foo">>, Result)),
    ?assertMatch([{<<"UNDEFINED_FIELD">>, <<"foo">>, [<<"foo">>]}], Errors).



can_pass_custom_hash_error_code_test() ->
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer())
    ]),
    Data = #{<<"foo">> => -1},
    Fun = fun
        (validate, [<<"foo">>], Value) when Value < 0 ->
            {error, <<"CUSTOM_ERROR_CODE">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_vm:validate(Map, Data, Fun),
    ?assertEqual(1, length(Errors)),
    ?assertMatch(error, maps:find(<<"foo">>, Result)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<"foo">>, [<<"foo">>]}], Errors).



can_fix_undefined_hash_field_test() ->
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer())
    ]),
    Data = #{<<"bar">> => 3.14},
    Fun = fun
        (fix, [<<"foo">>], undefined) ->
            {ok, 15};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_vm:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({ok, 15}, maps:find(<<"foo">>, Result)).



can_fix_invalid_hash_field_test() ->
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer())
    ]),
    Data = #{<<"foo">> => false},
    Fun = fun
        (fix, [<<"foo">>], _Value) ->
            {ok, 0};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_vm:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({ok, 0}, maps:find(<<"foo">>, Result)).
