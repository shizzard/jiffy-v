-module(jiffy_v).

-define(UNDEFINED_FIELD,    <<"UNDEFINED_FIELD">>).
-define(INVALID_HASH,       <<"INVALID_HASH">>).
-define(INVALID_STRING,     <<"INVALID_STRING">>).
-define(INVALID_ENUM,       <<"INVALID_ENUM">>).
-define(INVALID_FLOAT,      <<"INVALID_FLOAT">>).
-define(INVALID_INTEGER,    <<"INVALID_INTEGER">>).
-define(INVALID_BOOLEAN,    <<"INVALID_BOOLEAN">>).
-define(INVALID_NULL,       <<"INVALID_NULL">>).
-define(INVALID_LIST,       <<"INVALID_LIST">>).
-define(INVALID_VARIANT,    <<"INVALID_VARIANT">>).

-export([validate/2, validate/3]).
-export_type([
    jv_type_integer/0, jv_type_float/0, jv_type_string/0, jv_type_boolean/0, jv_type_null/0, jv_type_scalar/0,
    jv_type_hash/0, jv_type_list/0, jv_type_enum/0, jv_type_composite/0, jv_type/0
]).
-export_type([jv_data/0]).
-export_type([
    jv_ret_code/0, jv_ret_stack/0, jv_ret_error/0, jv_ret_errorlist/0, jv_ret_result/0, jv_ret/0
]).
-export_type([jv_fun_val/0, jv_fun_fix/0, jv_fun/0]).

-type jv_type_integer() :: {integer}.
-type jv_type_float() :: {float}.
-type jv_type_string() :: {string}.
-type jv_type_boolean() :: {boolean}.
-type jv_type_null() :: {null}.
-type jv_type_scalar() :: jv_type_integer() | jv_type_float() | jv_type_string() | jv_type_boolean() | jv_type_null().
-type jv_type_hashfield() :: {FieldName :: binary(), Obligatoriness :: required | optional, Type :: jv_type}.
-type jv_type_hash() :: {hash, Fields :: list(Field :: jv_type_hashfield())}.
-type jv_type_list() :: {list, Variants :: list(Variant :: jv_type())}.
-type jv_type_enum() :: {enum, Variants :: list(Variant :: term())}.
-type jv_type_variant() :: {variant, Variants :: list(Variant :: jv_type())}.
-type jv_type_composite() :: jv_type_hash() | jv_type_list() | jv_type_enum() | jv_type_variant().
-type jv_type() :: jv_type_scalar() | jv_type_composite().

-type jv_data() :: term().

-type jv_ret_code() :: term().
-type jv_ret_stack() :: list(binary()).
-type jv_ret_path() :: binary().
-type jv_ret_error() :: {Code :: jv_ret_code(), Path :: jv_ret_path(), Stack :: jv_ret_stack()}.
-type jv_ret_errorlist() :: list(jv_ret_error()).
-type jv_ret_result() :: jv_data().
-type jv_ret() :: {Errors :: jv_ret_errorlist(), Result :: jv_ret_result()}.

-type jv_fun_val() :: fun((validate, Stack :: jv_ret_stack(), Value :: jv_data()) -> {ok, valid} | {ok, NewValue :: jv_data()} | {error, Code :: jv_ret_code()}).
-type jv_fun_fix() :: fun((fix, Stack :: list(binary()), Value :: jv_data()) -> {ok, NewValue :: jv_data()} | {error, invalid} | {error, Code :: jv_ret_code()}).
-type jv_fun() :: jv_fun_val() | jv_fun_fix().



%% Interface



-spec validate(Map :: jv_type(), Data :: jv_data()) ->
    Ret :: jv_ret().
validate(Map, Data) ->
    Fun = fun
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    validate(Map, Data, Fun).



-spec validate(Map :: jv_type(), Data :: jv_data(), Fun :: jv_fun()) ->
    Ret :: jv_ret().
validate(Map, Data, Fun) when is_function(Fun, 3)  ->
    case handle(Map, Data, [], Fun, []) of
        {ok, Errors, Result} ->
            {Errors, Result};
        {error, Errors, Result} ->
            {Errors, Result}
    end.



%% Internals



-spec handle(Type :: jv_type(), Data0 :: jv_data(), Errors0 :: jv_ret_errorlist(), Validator :: jv_fun(), Stack0 :: jv_ret_stack()) ->
    {ok, jv_ret_errorlist(), jv_ret_result()} | {error, jv_ret_errorlist(), jv_ret_result()}| {custom_error, jv_ret_errorlist(), jv_ret_result()}.
handle({hash, Fields}, {Data0}, Errors0, Validator, Stack0) when is_list(Data0) ->
    {_Data1, Result, Errors1, Validator, _Stack1} = lists:foldl(
        fun iterate_hash/2, {Data0, {[]}, Errors0, Validator, Stack0}, Fields),
    val(Validator, Result, Errors1, Stack0);

handle({hash, _Fields}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_HASH, Errors0, Stack0);

handle({list, [Variant | Variants]}, Data0, Errors0, Validator, Stack0) when is_list(Data0) ->
    Result = lists:foldl(fun
        (Elem, {I0, Acc, E0, V, S0}) ->
            case handle(Variant, Elem, E0, V, [i_to_b(I0) | S0]) of
                {ok, E1, R1} ->
                    {I0 + 1, [R1 | Acc], E1, V, S0};
                {custom_error, E1, _R1} ->
                    {I0 + 1, Acc, E1, V, S0};
                {error, _E1, _R1} ->
                    failure
            end;
        (_Elem, failure) ->
            failure
    end, {0, [], Errors0, Validator, Stack0}, Data0),
    case Result of
        failure ->
            handle({list, Variants}, Data0, Errors0, Validator, Stack0);
        {_Index, Result1, Errors1, Validator, _Stack1} ->
            val(Validator, lists:reverse(Result1), Errors1, Stack0)
    end;

handle({list, []}, Data0, Errors0, Validator, Stack0) when is_list(Data0) ->
    fix(Validator, Data0, ?INVALID_LIST, Errors0, Stack0);

handle({list, _Variants}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_LIST, Errors0, Stack0);

handle({enum, Variants}, Data0, Errors0, Validator, Stack0) ->
    case lists:member(Data0, Variants) of
        true ->
            val(Validator, Data0, Errors0, Stack0);
        false ->
            fix(Validator, Data0, ?INVALID_ENUM, Errors0, Stack0)
    end;

handle({variant, [Variant | Variants]}, Data0, Errors0, Validator, Stack0) ->
    case handle(Variant, Data0, Errors0, Validator, Stack0) of
        {error, _E1, _R1} ->
            handle({variant, Variants}, Data0, Errors0, Validator, Stack0);
        Res -> Res
    end;

handle({variant, _Variants}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_VARIANT, Errors0, Stack0);

handle({string}, Data0, Errors0, Validator, Stack0) when is_binary(Data0) ->
    val(Validator, Data0, Errors0, Stack0);
handle({string}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_STRING, Errors0, Stack0);

handle({integer}, Data0, Errors0, Validator, Stack0) when is_integer(Data0) ->
    val(Validator, Data0, Errors0, Stack0);
handle({integer}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_INTEGER, Errors0, Stack0);

handle({float}, Data0, Errors0, Validator, Stack0) when is_float(Data0); is_integer(Data0) ->
    val(Validator, Data0, Errors0, Stack0);

handle({float}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_FLOAT, Errors0, Stack0);

handle({any}, Data0, Errors0, Validator, Stack0) ->
    val(Validator, Data0, Errors0, Stack0);

handle({boolean}, Data0, Errors0, Validator, Stack0) when is_boolean(Data0) ->
    val(Validator, Data0, Errors0, Stack0);

handle({boolean}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_BOOLEAN, Errors0, Stack0);

handle({null}, null, Errors0, Validator, Stack0) ->
    val(Validator, null, Errors0, Stack0);

handle({null}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_NULL, Errors0, Stack0);

handle(Type, Data, Errors, _Validator, _Stack) ->
    erlang:error({invalid_type, Type}),
    {ok, Errors, Data}.



-spec iterate_hash(
    Spec :: jv_type_hashfield(),
    {D0 :: jv_data(), {R0 :: jv_data()}, E0 :: jv_ret_errorlist(), V :: jv_fun(), S0 :: jv_ret_stack()}
) ->
    {D0 :: jv_data(), {R1 :: jv_data()}, E1 :: jv_ret_errorlist(), V :: jv_fun(), S0 :: jv_ret_stack()}.
iterate_hash({FName, Obligatoriness, Type}, {D0, {R0}, E0, V, S0}) ->
    case proplists:get_value(FName, D0) of
        %% required field is unset, we're trying to fix it
        undefined when required == Obligatoriness ->
            case fix(V, undefined, ?UNDEFINED_FIELD, E0, [FName | S0]) of
                {ok, E1, R1} ->
                    {D0, {R0++[{FName, R1}]}, E1, V, S0};
                {error, E1, _R1} ->
                    {D0, {R0}, E1, V, S0};
                {custom_error, E1, _R1} ->
                    {D0, {R0}, E1, V, S0}
            end;
        undefined ->
            {D0, {R0}, E0, V, S0};
        Value ->
            case handle(Type, Value, E0, V, [FName | S0]) of
                {ok, E1, R1} ->
                    {D0, {R0++[{FName, R1}]}, E1, V, S0};
                {error, E1, _R1} ->
                    {D0, {R0}, E1, V, S0};
                {custom_error, E1, _R1} ->
                    {D0, {R0}, E1, V, S0}
            end
    end;

iterate_hash(Any, {D0, R0, E0, V, S0}) ->
    erlang:error({invalid_map, Any}),
    {D0, R0, E0, V, S0}.



-spec path_by_stack(Stack :: jv_ret_stack()) -> jv_ret_path().
path_by_stack(Stack) ->
    lists:foldl(fun
        (Elem, <<>>) ->
            Elem;
        (Elem, Acc) ->
            <<Acc/binary, <<".">>/binary, Elem/binary>>
    end, <<>>, lists:reverse(Stack)).



-spec i_to_b(Int :: integer()) -> binary().
i_to_b(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int)).



-spec val(Validator :: jv_fun_val(), Data :: jv_data(), Errors :: jv_ret_errorlist(), Stack :: jv_ret_stack()) ->
    {ok, Errors :: jv_ret_errorlist(), Result :: jv_ret_result()} |
    {error, Errors :: jv_ret_errorlist(), Result :: jv_ret_result()} |
    {custom_error, Errors :: jv_ret_errorlist(), Result :: jv_ret_result()}.
val(Validator, Data, Errors, Stack) ->
    case Validator(validate, lists:reverse(Stack), Data) of
        {ok, valid} ->
            {ok, Errors, Data};
        {ok, NewVal} ->
            {ok, Errors, NewVal};
        {error, Code} ->
            {custom_error, [{Code, path_by_stack(Stack), lists:reverse(Stack)} | Errors], Data}
    end.



-spec fix(Validator :: jv_fun_val(), Data :: jv_data(), Code :: jv_ret_code(), Errors :: jv_ret_errorlist(), Stack :: jv_ret_stack()) ->
    {ok, Errors :: jv_ret_errorlist(), Result :: jv_ret_result()} |
    {error, Errors :: jv_ret_errorlist(), Result :: jv_ret_result()} |
    {custom_error, Errors :: jv_ret_errorlist(), Result :: jv_ret_result()}.
fix(Validator, Data, Code, Errors, Stack) ->
    case Validator(fix, lists:reverse(Stack), Data) of
        {ok, NewVal} ->
            {ok, Errors, NewVal};
        {error, invalid} ->
            {error, [{Code, path_by_stack(Stack), lists:reverse(Stack)} | Errors], Data};
        {error, NewCode} ->
            {custom_error, [{NewCode, path_by_stack(Stack), lists:reverse(Stack)} | Errors], Data}
    end.
