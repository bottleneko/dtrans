-module(dtrans_model_field).

-record(dtrans_model_field, {
  name          :: dtrans:model_field_name(),
  required      :: boolean(),
  internal      :: boolean(),
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

-export([to_map/1]).

-export([extract/3]).

-define(DTRANS_VALUE_NOT_PRESENT, '$NOT_PRESENT').

%%====================================================================
%% API functions
%%====================================================================

%% TODO: add heuristic for accurate determine "required" flag value
-spec new(dtrans:model_field_name(), dtrans:model_field()) -> t().
new(FieldName, ModelField) ->
  #dtrans_model_field{
    name          = FieldName,
    required      = maps:get(required,      ModelField,  false),
    internal      = maps:get(internal,      ModelField,  false),
    validator     = maps:get(validator,     ModelField,  fun(_Value) -> ok end),
    default_value = maps:get(default_value, ModelField, ?DTRANS_VALUE_NOT_PRESENT),
    constructor   = maps:get(constructor,   ModelField,  fun(Value) -> {ok, Value} end)
  }.

-spec to_map(t()) -> dtrans:model().
to_map(FieldModel) ->
  Keys = record_info(fields, dtrans_model_field),
  [dtrans_model_field | Value] = tuple_to_list(FieldModel),
  Proplist = lists:zip(Keys, Value),
  maps:from_list(Proplist).

-spec extract(Data :: dtrans:data(), Base :: dtrans:data(), t()) ->
  ok | {ok, any()} | {error, Error}
  when FieldErrorKind :: validation_error         | construction_error
                       | validator_invalid_output | constructor_invalid_output,
       Error :: {no_data,         dtrans:model_field_name()}
              | {FieldErrorKind, dtrans:model_field_name(), Reason :: term()}.
extract(_Data, Base, #dtrans_model_field{internal = true} = FieldModel) ->
  construct(Base, FieldModel);
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
      case Default of
        ?DTRANS_VALUE_NOT_PRESENT ->
          ok;
        Default ->
          do_extract(Default, Base, FieldModel)
      end
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

-spec construct(dtrans:data(), t()) ->
  {ok, ConstructedValue :: any()} | {error, Error}
  when Error          :: {FieldErrorKind, dtrans:model_field_name(), Reason :: term()},
       FieldErrorKind :: construction_error | constructor_invalid_output.
construct(Base, #dtrans_model_field{internal = true} = ModelField) ->
  construct(?DTRANS_VALUE_NOT_PRESENT, Base, ModelField).

-spec construct(Value :: any(), dtrans:data(), t()) ->
  {ok, ConstructedValue :: any()} | {error, Error}
  when Error          :: {FieldErrorKind, dtrans:model_field_name(), Reason :: term()},
       FieldErrorKind :: construction_error | constructor_invalid_output.
construct(Value, Base, #dtrans_model_field{name = Field} = ModelField) ->
  try
    case (construct_fun(Base, ModelField))(Value) of
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

-spec construct_fun(Base :: dtrans:data(), t()) ->
  fun(() -> Return) | fun((any()) -> Return)
  when Return :: {ok, _Value}
               | {error, Reason :: term()}.
construct_fun(Base, #dtrans_model_field{
  internal    = true,
  constructor = {depends_on, DependencyFields, Constructor}} = _ModelField) ->
  fun(_Stub) ->
    erlang:apply(Constructor, lists:map(fun(Elem) -> maps:get(Elem, Base) end, DependencyFields))
  end;
construct_fun(_Base, #dtrans_model_field{
  internal    = true,
  constructor = Constructor} = _ModelField) ->
  fun(_Stub) ->
    erlang:apply(Constructor, [])
  end;
construct_fun(Base, #dtrans_model_field{
  internal    = false,
  constructor = {depends_on, DependencyFields, Constructor}} =  _ModelField) ->
  fun(ConstructorValue) ->
    erlang:apply(Constructor, [ConstructorValue | lists:map(fun(Elem) -> maps:get(Elem, Base) end, DependencyFields)])
  end;
construct_fun(_Base, #dtrans_model_field{
  internal    = false,
  constructor = Constructor} = _ModelField) ->
  Constructor.
