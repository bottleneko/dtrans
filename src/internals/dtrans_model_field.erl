-module(dtrans_model_field).

-record(dtrans_model_field, {
  name          :: dtrans:model_field_name(),
  required      :: boolean(),
  validator     :: fun((FieldType) -> ok | {error, Reason :: term()}),
  default_value :: FieldType,
  constructor   ::
    fun((any()) -> FieldType)
    | {depends_on, [dtrans_model:field_name()], fun((...) -> FieldType)}
}).

-type t() :: #dtrans_model_field{}.

-export_type([t/0]).

%% API
-export([new/2]).

-export([extract/3]).

%%====================================================================
%% API functions
%%====================================================================

%% TODO: add heuristic for accurate determine "required" flag value
-spec new(dtrans:model_field_name(), dtrans:model_field()) -> t().
new(FieldName, ModelField) ->
  #dtrans_model_field{
    name          = FieldName,
    required      = maps:get(required,      ModelField,  false),
    validator     = maps:get(validator,     ModelField,  fun(_Value) -> ok end),
    default_value = maps:get(default_value, ModelField),
    constructor   = maps:get(constructor,   ModelField,  fun(Value) -> {ok, Value} end)
  }.

-spec extract(Data :: dtrans:data(), Base :: dtrans:data(), t()) ->
  {ok, any()} | {error, Error}
  when FieldErrorKind :: validation_error         | construction_error
                       | validator_invalid_output | constructor_invalid_output,
       Error :: {no_data,         dtrans:model_field_name()}
              | {FieldErrorKind, dtrans:model_field_name(), Reason :: term()}.
extract(Data, Base, #dtrans_model_field{name = Field, required = true} = FieldModel) ->
  case Data of
    #{Field := Value} ->
      do_extract(Value, Base, FieldModel);
    Data ->
      {error, {no_data, Field}}
  end;
extract(Data, Base, #dtrans_model_field{name = Field, default_value = Default} = FieldModel) ->
  case Data of
    #{Field := Value} ->
      do_extract(Value, Base, FieldModel);
    Data ->
      do_extract(Default, Base, FieldModel)
  end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec do_extract(Data :: dtrans:data(), Base :: dtrans:data(), t()) ->
  {ok, any()} | {error, Error}
  when Error          :: {FieldErrorKind, dtrans:model_field_name(), Reason :: term()},
       FieldErrorKind :: validation_error         | construction_error
                       | validator_invalid_output | constructor_invalid_output.
do_extract(Value, Base, FieldModel) ->
  case validate(Value, FieldModel) of
    ok ->
      construct(Value, Base, FieldModel);
    {error, _Reason} = Error ->
      Error
  end.

-spec validate(Value :: any(), t()) ->
  ok | {error, Error}
  when Error          :: {FieldErrorKind, dtrans:model_field_name(), Reason :: term()},
       FieldErrorKind :: validation_error | validator_invalid_output.
validate(Value, #dtrans_model_field{name = Field, validator = Validator}) ->
  try
    case Validator(Value) of
      ok -> ok;
      {error, Error} ->
        {error, {validation_error, Field, Error}}
    end
  catch
    _:{case_clause, Output} ->
      {error, {validator_invalid_output, Field, Output}};
    _:Reason ->
      {error, {validation_error, Field, Reason}}
  end.

-spec construct(Value :: any(), dtrans:data(), t()) ->
  {ok, ConstructedValue :: any()} | {error, Error}
  when Error          :: {FieldErrorKind, dtrans:model_field_name(), Reason :: term()},
       FieldErrorKind :: construction_error | constructor_invalid_output.
construct(Value, Base, #dtrans_model_field{
  constructor = {depends_on, DependencyFields, Constructor}
} = FieldModel) ->
  Fun =
    fun(ConstructorValue) ->
      erlang:apply(Constructor, [ConstructorValue | lists:map(fun(Elem) -> maps:get(Elem, Base) end, DependencyFields)])
    end,
  construct(Value, Base, FieldModel#dtrans_model_field{
    constructor = Fun
  });
construct(Value, _Base, #dtrans_model_field{name = Field, constructor = Constructor}) ->
  try
    case Constructor(Value) of
      {ok, _Value} = Success ->
        Success;
      {error, Error} ->
        {error, {construction_error, Field, Error}}
    end
  catch
    _:{case_clause, Output} ->
      {error, {constructor_invalid_output, Field, Output}};
    _:Reason ->
      {error, {construction_error, Field, Reason}}
  end.
