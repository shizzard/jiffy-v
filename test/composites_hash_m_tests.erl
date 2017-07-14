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
    ?assertMatch([{_, <<"foo">>, _}], Errors).




can_get_validate_structure_test() ->
    Fun = fun
        (validate, [], Value) ->
            case {maps:is_key(<<"bar">>, Value), maps:is_key(<<"baz">>, Value)} of
                {Same, Same} ->
                    {ok, valid};
                {_, _} ->
                    {error, <<"Both optional fields should be defined">>}
            end
    end,
    Map = jiffy_vm:hash([
        jiffy_vm:hashfield(<<"foo">>, required, jiffy_vm:integer()),
        jiffy_vm:hashfield(<<"bar">>, optional, jiffy_vm:integer()),
        jiffy_vm:hashfield(<<"baz">>, optional, jiffy_vm:integer())
    ], Fun),
    Data = #{<<"foo">> => 1, <<"bar">> => 1},
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{_, <<>>, _}], Errors).
