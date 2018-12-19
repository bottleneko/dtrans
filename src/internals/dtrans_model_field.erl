-module(dtrans_model_field).

-record(dtrans_model_field, {
  required      :: boolean(),
  validator     :: fun((FieldType) -> ok | {error, Reason :: term()}),
  default_value :: FieldType,
  constructor   ::
  fun(() -> FieldType)
  | {depends_on, [dtrans_model:field_name()], fun((...) -> FieldType)}
}).

-type t() :: #dtrans_model_field{}.

-export_type([t/0]).

%% API
-export([new/1]).

-export([validate/2]).

%%====================================================================
%% API functions
%%====================================================================

%% TODO: add heuristic for accurate determine "required" flag value
new(ModelField) ->
  #dtrans_model_field{
    required      = maps:get(required,      ModelField,  false),
    validator     = maps:get(validator,     ModelField,  fun(_Value) -> ok end),
    default_value = maps:get(default_value, ModelField),
    constructor   = maps:get(constructor,   ModelField,  fun(Value) -> Value end)
  }.



-spec validate(Value :: any(), t()) ->
  ok | {error, Reason :: term()}.
validate(Value, #dtrans_model_field{validator = Validator}) ->
  try
    %% FIXME: check type before return, now possible invalid returns values without errors
    Validator(Value)
  catch
    _:Reason ->
      {error, Reason}
  end.

%%====================================================================
%% Internal functions
%%====================================================================
