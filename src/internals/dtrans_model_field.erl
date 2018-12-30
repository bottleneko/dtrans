-module(dtrans_model_field).

-define(DTRANS_VALUE_NOT_PRESENT, '$NOT_PRESENT').

-record(dtrans_model_field, {
  field_key     :: dtrans:model_field_key(),
  required      :: boolean(),
  internal      :: boolean(),
  validator     :: fun((FieldType) -> ok | {error, Reason :: term()}),
  default_value :: FieldType | ?DTRANS_VALUE_NOT_PRESENT,
  constructor   ::
    fun((any()) -> FieldType)
    | {depends_on, [dtrans_model:field_name()], fun((...) -> FieldType)},
  model         :: dtrans_model:t() | ?DTRANS_VALUE_NOT_PRESENT,
  count         :: one | many
}).

-type t() :: #dtrans_model_field{}.

-export_type([t/0]).

-type dtrans_model_field_extracting_error_no_data() :: {no_data, dtrans:model_field_name()}.

-type dtrans_model_field_extracting_error_kind() ::
     validation_error         | construction_error
   | validator_invalid_output | constructor_invalid_output
   | error_in_inner_model.

-type dtrans_model_field_extracting_error_invalid_data() ::
  {dtrans_model_field_extracting_error_kind(), dtrans:model_field_name(), Reason :: term()}.

-type dtrans_model_field_extracting_error() ::
    dtrans_model_field_extracting_error_no_data()
  | dtrans_model_field_extracting_error_invalid_data().

-export_type([
  dtrans_model_field_extracting_error/0,
  dtrans_model_field_extracting_error_no_data/0,
  dtrans_model_field_extracting_error_invalid_data/0,
  dtrans_model_field_extracting_error_kind/0
]).

%% API
-export([new/2]).

-export([to_map/1]).

-export([extract_dynamic/3, extract/3]).

%%====================================================================
%% API functions
%%====================================================================

%% TODO: add heuristic for accurate determine "required" flag value
-spec new(dtrans:model_field_key(), dtrans:model_field()) -> t().
new(FieldKey, ModelField) ->
  #dtrans_model_field{
    field_key     = FieldKey,
    required      = maps:get(required,      ModelField, false),
    internal      = maps:get(internal,      ModelField, false),
    validator     = maps:get(validator,     ModelField, fun(_Value) -> ok end),
    default_value = maps:get(default_value, ModelField, ?DTRANS_VALUE_NOT_PRESENT),
    constructor   = maps:get(constructor,   ModelField, fun(Value) -> {ok, Value} end),
    model         = maps:get(model,         ModelField, ?DTRANS_VALUE_NOT_PRESENT),
    count         = maps:get(count,         ModelField, one)
  }.

-spec to_map(t()) -> dtrans:model().
to_map(FieldModel) ->
  Keys = record_info(fields, dtrans_model_field),
  [dtrans_model_field | Value] = tuple_to_list(FieldModel),
  Proplist = lists:zip(Keys, Value),
  maps:from_list(Proplist).

-spec extract_dynamic(Data :: dtrans:data(), Field :: dtrans:model_field_name(), t()) ->
  ok | {ok, any()} | {error, dtrans_model_field_extracting_error()}.
extract_dynamic(Data, Field, ModelField) ->
  extract(Data, #{}, ModelField#dtrans_model_field{field_key = Field}).

-spec extract(Data :: dtrans:data(), Base :: dtrans:data(), t()) ->
  ok | {ok, any()} | {error, dtrans_model_field_extracting_error()}.
extract(_Data, Base, #dtrans_model_field{internal = true} = FieldModel) ->
  construct(Base, FieldModel);
extract(Data, Base, #dtrans_model_field{
  field_key     = FieldKey,
  default_value = Default,
  count         = Count,
  required      = Required} = FieldModel) ->
  case Data of
    #{FieldKey := Value} when Count =:= one ->
      do_extract(Value, Base, FieldModel);
    #{FieldKey := Values} when Count =:= many, is_list(Values) ->
      do_extract_many(Values, Base, FieldModel);
    Data when Required =:= true ->
      {error, {no_data, FieldKey}};
    Data when Required =:= false ->
      case Default of
        ?DTRANS_VALUE_NOT_PRESENT ->
          ok;
        Default when Count =:= one ->
          do_extract(Default, Base, FieldModel);
        Defaults when Count =:= many ->
          do_extract_many(Defaults, Base, FieldModel)
      end
  end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec do_extract_many([dtrans:data()], Base :: dtrans:data(), t()) ->
    {ok, [any()]}
  | {error, {
      {list_processing_error, dtrans:data()},
      dtrans:model_field_key(),
      Reason :: dtrans_model_field_extracting_error_invalid_data()
    }}.
do_extract_many(Values, Base, #dtrans_model_field{field_key = FieldKey} = FieldModel) ->
  Fun =
    fun
      (_Elem, {error, _Reason} = Error) ->
        Error;
      (Elem, {ok, Acc}) ->
        case do_extract(Elem, Base, FieldModel) of
          {ok, Value}     ->
            {ok, [Value | Acc]};
          {error, Reason} ->
            {error, {{list_processing_error, Elem}, FieldKey, Reason}}
        end
    end,
  lists:foldr(Fun, {ok, []}, Values).

-spec do_extract(Data :: dtrans:data(), Base :: dtrans:data() | dtrans:model_field_name(), t()) ->
  {ok, any()} | {error, dtrans_model_field_extracting_error_invalid_data()}.
do_extract(Value, Base, #dtrans_model_field{model = Model} = FieldModel)
  when Model =:= ?DTRANS_VALUE_NOT_PRESENT ->
  case validate(Value, FieldModel) of
    ok ->
      construct(Value, Base, FieldModel);
    {error, _Reason} = Error ->
      Error
  end;
do_extract(Value, _Base, #dtrans_model_field{field_key = FieldKey, model = Model}) ->
  case dtrans:extract(Value, Model) of
    {ok, _Value} = Success ->
      Success;
    {error, Reason} ->
      {error, {error_in_inner_model, FieldKey, Reason}}
  end.

-spec validate(Value :: any(), t()) ->
  ok | {error, Error}
  when Error          :: {FieldErrorKind, dtrans:model_field_name(), Reason :: term()},
       FieldErrorKind :: validation_error | validator_invalid_output.
validate(Value, #dtrans_model_field{field_key = FieldKey, validator = Validator}) ->
  try
    case Validator(Value) of
      ok -> ok;
      {error, Error} ->
        {error, {validation_error, FieldKey, Error}}
    end
  catch
    _:{case_clause, Output} ->
      {error, {validator_invalid_output, FieldKey, Output}};
    _:Reason ->
      {error, {validation_error, FieldKey, Reason}}
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
construct(Value, Base, #dtrans_model_field{field_key = FieldKey} = ModelField) ->
  try
    case (construct_fun(Base, ModelField))(Value) of
      {ok, _Value} = Success ->
        Success;
      {error, Error} ->
        {error, {construction_error, FieldKey, Error}}
    end
  catch
    _:{case_clause, Output} ->
      {error, {constructor_invalid_output, FieldKey, Output}};
    _:Reason ->
      {error, {construction_error, FieldKey, Reason}}
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
