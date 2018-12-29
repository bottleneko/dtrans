-module(dtrans).

-type model_field_name()    :: any().
-type model_dynamic_field() ::
fun((model_field_name()) -> {ok, iolist()} | {error, wrong_dynamic_field_name}).

-type model_field_key() :: model_field_name() | model_dynamic_field().

-export_type([model_field_key/0, model_field_name/0, model_dynamic_field/0]).

-type model_field(FieldType) :: #{
  required      => boolean(),
  internal      => boolean(),
  validator     => fun((FieldType) -> ok | {error, Reason :: term()}),
  default_value => FieldType,
  constructor   =>
    fun((any()) -> FieldType)
    | {depends_on, [dtrans_model:field_name()], fun((...) -> FieldType)},
  model         => dtrans_model:t()
}.

-export_type([model_field/0]).

-type model() :: #{model_field_key() => model_field(any())}.

-export_type([model/0]).

-type data() :: #{model_field_key() => any()}.

-export_type([data/0]).

%% API exports
-export([new/1]).
-export([extract/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec new(model()) ->
 {ok, dtrans_model:t()} | {error, Reason}
  when Reason :: dependency_tree_model_cannot_be_resolved
               | {cyclic_dependency, Path :: [model_field_name()]}.
new(Model) ->
  dtrans_model:new(Model).

-spec extract(data(), dtrans_model:t()) ->
  {ok, data()} | {error, Error}
  when Error          :: {no_data,        dtrans:model_field_name()}
                       | {FieldErrorKind, dtrans:model_field_name(), Reason :: term()},
       FieldErrorKind :: validation_error         | construction_error
                       | validator_invalid_output | constructor_invalid_output
                       | error_in_inner_model.
extract(Data, Model) ->
  dtrans_model:extract(Data, Model).

%%====================================================================
%% Internal functions
%%====================================================================
