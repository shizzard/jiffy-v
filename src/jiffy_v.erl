-module(jiffy_v).
-compile({no_auto_import, [float/1]}).

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

-export([validate/2]).
-export([
    integer/0, float/0, string/0, boolean/0, null/0,
    hashfield/3, hash/1, list/1, enum/1, variant/1, any/0,
    integer/1, float/1, string/1, boolean/1, null/1,
    hashfield/4, hash/2, list/2, enum/2, variant/2, any/1
]).
-export_type([
    jv_type_integer/0, jv_type_float/0, jv_type_string/0, jv_type_boolean/0, jv_type_null/0, jv_type_scalar/0,
    jv_type_hash/0, jv_type_list/0, jv_type_enum/0, jv_type_composite/0, jv_type/0
]).
-export_type([
    jv_data_object/0, jv_data_list/0, jv_data_scalar/0, jv_data/0
]).
-export_type([
    jv_ret_code/0, jv_ret_stack/0, jv_ret_error/0, jv_ret_errorlist/0, jv_ret_result/0, jv_ret/0
]).
-export_type([jv_fun_val/0, jv_fun_fix/0, jv_fun/0]).

-type jv_type_integer() :: {integer, Validator :: jv_fun()}.
-type jv_type_float() :: {float, Validator :: jv_fun()}.
-type jv_type_string() :: {string, Validator :: jv_fun()}.
-type jv_type_boolean() :: {boolean, Validator :: jv_fun()}.
-type jv_type_null() :: {null, Validator :: jv_fun()}.
-type jv_type_scalar() :: jv_type_integer() | jv_type_float() | jv_type_string() | jv_type_boolean() | jv_type_null().
-type jv_type_hashfield() :: {FieldName :: binary(), Obligatoriness :: required | optional, Type :: jv_type(), Validator :: jv_fun()}.
-type jv_type_hash() :: {hash, Fields :: list(Field :: jv_type_hashfield()), Validator :: jv_fun()}.
-type jv_type_list() :: {list, Variants :: list(Variant :: jv_type()), Validator :: jv_fun()}.
-type jv_type_enum() :: {enum, Variants :: list(Variant :: term()), Validator :: jv_fun()}.
-type jv_type_variant() :: {variant, Variants :: list(Variant :: jv_type()), Validator :: jv_fun()}.
-type jv_type_composite() :: jv_type_hash() | jv_type_list() | jv_type_enum() | jv_type_variant().
-type jv_type_any() :: {any, Validator :: jv_fun()}.
-type jv_type() :: jv_type_scalar() | jv_type_composite() | jv_type_any().

-type jv_data_object() :: {list({Key :: binary(), Value :: jv_data()})}.
-type jv_data_list() :: list(Item :: jv_data()).
-type jv_data_scalar() :: binary() | integer() | float() | boolean().
-type jv_data() :: jv_data_object() | jv_data_list() | jv_data_scalar().

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
    case handle(Map, Data, [], []) of
        {ok, Errors, Result} ->
            {Errors, Result};
        {custom_error, Errors, Result} ->
            {Errors, Result};
        {error, Errors, Result} ->
            {Errors, Result}
    end.



-spec integer() ->
    Ret :: jv_type_integer().
integer() ->
    integer(fun default_validator/3).

-spec integer(Validator :: jv_fun()) ->
    Ret :: jv_type_integer().
integer(Validator) ->
    {integer, Validator}.



-spec float() ->
    Ret :: jv_type_float().
float() ->
    float(fun default_validator/3).

-spec float(Validator :: jv_fun()) ->
    Ret :: jv_type_float().
float(Validator) ->
    {float, Validator}.



-spec string() ->
    Ret :: jv_type_string().
string() ->
    string(fun default_validator/3).

-spec string(Validator :: jv_fun()) ->
    Ret :: jv_type_string().
string(Validator) ->
    {string, Validator}.



-spec boolean() ->
    Ret :: jv_type_boolean().
boolean() ->
    boolean(fun default_validator/3).

-spec boolean(Validator :: jv_fun()) ->
    Ret :: jv_type_boolean().
boolean(Validator) ->
    {boolean, Validator}.



-spec null() ->
    Ret :: jv_type_null().
null() ->
    null(fun default_validator/3).

-spec null(Validator :: jv_fun()) ->
    Ret :: jv_type_null().
null(Validator) ->
    {null, Validator}.



-spec hashfield(
    FieldName :: binary(),
    IsRequired :: required | optional,
    Type :: jv_type()
) ->
    Ret :: jv_type_hashfield().
hashfield(FieldName, IsRequired, Type) ->
    hashfield(FieldName, IsRequired, Type, fun default_validator/3).

-spec hashfield(
    FieldName :: binary(),
    IsRequired :: required | optional,
    Type :: jv_type(),
    Validator :: jv_fun()
) ->
    Ret :: jv_type_hashfield().
hashfield(FieldName, IsRequired, Type, Validator) ->
    {FieldName, IsRequired, Type, Validator}.



-spec hash(Fields :: list(Field :: jv_type_hashfield())) ->
    Ret :: jv_type_hash().
hash(Fields) ->
    hash(Fields, fun default_validator/3).

-spec hash(
    Fields :: list(Field :: jv_type_hashfield()),
    Validator :: jv_fun()
) ->
    Ret :: jv_type_hash().
hash(Fields, Validator) ->
    {hash, Fields, Validator}.



-spec list(Variants :: list(Variant :: jv_type())) ->
    Ret :: jv_type_list().
list(Variants) ->
    list(Variants, fun default_validator/3).

-spec list(
    Variants :: list(Variant :: jv_type()),
    Validator :: jv_fun()
) ->
    Ret :: jv_type_list().
list(Variants, Validator) ->
    {list, Variants, Validator}.



-spec enum(Variants :: list(Variant :: term())) ->
    Ret :: jv_type_enum().
enum(Variants) ->
    enum(Variants, fun default_validator/3).

-spec enum(
    Variants :: list(Variant :: term()),
    Validator :: jv_fun()
) ->
    Ret :: jv_type_enum().
enum(Variants, Validator) ->
    {enum, Variants, Validator}.



-spec variant(Variants :: list(jv_type())) ->
    Ret :: jv_type_variant().
variant(Variants) ->
    variant(Variants, fun default_validator/3).

-spec variant(
    Variants :: list(jv_type()),
    Validator :: jv_fun()
) ->
    Ret :: jv_type_variant().
variant(Variants, Validator) ->
    {variant, Variants, Validator}.



-spec any() ->
    Ret :: jv_type_any().
any() ->
    any(fun default_validator/3).

-spec any(Validator :: jv_fun()) ->
    Ret :: jv_type_any().
any(Validator) ->
    {any, Validator}.



%% Internals



-spec handle(Type :: jv_type(), Data0 :: jv_data(), Errors0 :: jv_ret_errorlist(), Stack0 :: jv_ret_stack()) ->
    {ok, jv_ret_errorlist(), jv_ret_result()} |
    {error, jv_ret_errorlist(), jv_ret_result()} |
    {custom_error, jv_ret_errorlist(), jv_ret_result()}.
handle({hash, Fields, Validator}, {Data0}, Errors0, Stack0) when is_list(Data0) ->
    {_Data1, Result, Errors1, _Stack1} = lists:foldl(
        fun iterate_hash/2, {Data0, {[]}, Errors0, Stack0}, Fields),
    val(Validator, Result, Errors1, Stack0);

handle({hash, _Fields, Validator}, Data0, Errors0, Stack0) ->
    fix(Validator, Data0, ?INVALID_HASH, Errors0, Stack0);

handle({list, [Variant | Variants], Validator}, Data0, Errors0, Stack0) when is_list(Data0) ->
    Result = lists:foldl(fun
        (Elem, {I0, Acc, E0, S0}) ->
            case handle(Variant, Elem, E0, [i_to_b(I0) | S0]) of
                {ok, E1, R1} ->
                    {I0 + 1, [R1 | Acc], E1, S0};
                {custom_error, E1, _R1} ->
                    {I0 + 1, Acc, E1, S0};
                {error, _E1, _R1} ->
                    failure
            end;
        (_Elem, failure) ->
            failure
    end, {0, [], Errors0, Stack0}, Data0),
    case Result of
        failure ->
            handle({list, Variants, Validator}, Data0, Errors0, Stack0);
        {_Index, Result1, Errors1, _Stack1} ->
            val(Validator, lists:reverse(Result1), Errors1, Stack0)
    end;

handle({list, [], Validator}, Data0, Errors0, Stack0) when is_list(Data0) ->
    fix(Validator, Data0, ?INVALID_LIST, Errors0, Stack0);

handle({list, _Variants, Validator}, Data0, Errors0, Stack0) ->
    fix(Validator, Data0, ?INVALID_LIST, Errors0, Stack0);

handle({enum, Variants, Validator}, Data0, Errors0, Stack0) ->
    case lists:member(Data0, Variants) of
        true ->
            val(Validator, Data0, Errors0, Stack0);
        false ->
            fix(Validator, Data0, ?INVALID_ENUM, Errors0, Stack0)
    end;

handle({variant, [Variant | Variants], Validator}, Data0, Errors0, Stack0) ->
    case handle(Variant, Data0, Errors0, Stack0) of
        {error, _E1, _R1} ->
            handle({variant, Variants, Validator}, Data0, Errors0, Stack0);
        Res -> Res
    end;

handle({variant, [], Validator}, Data0, Errors0, Stack0) ->
    fix(Validator, Data0, ?INVALID_VARIANT, Errors0, Stack0);

handle({string, Validator}, Data0, Errors0, Stack0) when is_binary(Data0) ->
    val(Validator, Data0, Errors0, Stack0);
handle({string, Validator}, Data0, Errors0, Stack0) ->
    fix(Validator, Data0, ?INVALID_STRING, Errors0, Stack0);

handle({integer, Validator}, Data0, Errors0, Stack0) when is_integer(Data0) ->
    val(Validator, Data0, Errors0, Stack0);
handle({integer, Validator}, Data0, Errors0, Stack0) ->
    fix(Validator, Data0, ?INVALID_INTEGER, Errors0, Stack0);

handle({float, Validator}, Data0, Errors0, Stack0) when is_float(Data0); is_integer(Data0) ->
    val(Validator, Data0, Errors0, Stack0);

handle({float, Validator}, Data0, Errors0, Stack0) ->
    fix(Validator, Data0, ?INVALID_FLOAT, Errors0, Stack0);

handle({any, Validator}, Data0, Errors0, Stack0) ->
    val(Validator, Data0, Errors0, Stack0);

handle({boolean, Validator}, Data0, Errors0, Stack0) when is_boolean(Data0) ->
    val(Validator, Data0, Errors0, Stack0);

handle({boolean, Validator}, Data0, Errors0, Stack0) ->
    fix(Validator, Data0, ?INVALID_BOOLEAN, Errors0, Stack0);

handle({null, Validator}, null, Errors0, Stack0) ->
    val(Validator, null, Errors0, Stack0);

handle({null, Validator}, Data0, Errors0, Stack0) ->
    fix(Validator, Data0, ?INVALID_NULL, Errors0, Stack0);

handle(Type, Data, Errors, _Stack) ->
    erlang:error({invalid_type, Type}),
    {ok, Errors, Data}.



-spec iterate_hash(
    Spec :: jv_type_hashfield(),
    {D0 :: jv_data(), {R0 :: jv_data()}, E0 :: jv_ret_errorlist(), S0 :: jv_ret_stack()}
) ->
    {D0 :: jv_data(), {R1 :: jv_data()}, E1 :: jv_ret_errorlist(), S0 :: jv_ret_stack()}.
iterate_hash({FName, Obligatoriness, Type, V}, {D0, {R0}, E0, S0}) ->
    case proplists:get_value(FName, D0) of
        %% required field is unset, we're trying to fix it
        undefined when required == Obligatoriness ->
            case fix(V, undefined, ?UNDEFINED_FIELD, E0, [FName | S0]) of
                {ok, E1, R1} ->
                    {D0, {R0++[{FName, R1}]}, E1, S0};
                {custom_error, E1, _R1} ->
                    {D0, {R0}, E1, S0};
                {error, E1, _R1} ->
                    {D0, {R0}, E1, S0}
            end;
        undefined ->
            {D0, {R0}, E0, S0};
        Value ->
            case handle(Type, Value, E0, [FName | S0]) of
                {ok, E1, R1} ->
                    {D0, {R0++[{FName, R1}]}, E1, S0};
                {custom_error, E1, _R1} ->
                    {D0, {R0}, E1, S0};
                {error, E1, _R1} ->
                    {D0, {R0}, E1, S0}
            end
    end;

iterate_hash(Any, {D0, R0, E0, S0}) ->
    erlang:error({invalid_map, Any}),
    {D0, R0, E0, S0}.



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



-spec fix(Validator :: jv_fun_val(), Data :: jv_data() | undefined, Code :: jv_ret_code(), Errors :: jv_ret_errorlist(), Stack :: jv_ret_stack()) ->
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


default_validator(validate, _, _) ->
    {ok, valid};

default_validator(fix, _, _) ->
    {error, invalid}.
