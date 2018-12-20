-module(dtrans).

-type model_field_name()  :: any().

-type model_field(FieldType) :: #{
  required      => boolean(),
  validator     => fun((FieldType) -> ok | {error, Reason :: term()}),
  default_value := FieldType,
  constructor   =>
    fun((any()) -> FieldType)
    | {depends_on, [dtrans_model:field_name()], fun((...) -> FieldType)}
}.

-type model() :: #{model_field_name() => model_field(any())}.

-export_type([model/0, model_field_name/0, model_field/1]).

%% API exports
-export([new/1]).
-export([extract/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec new(model()) -> dtrans_model:t().
new(Model) ->
  dtrans_model:new(Model).

-spec extract(Data :: map(), model()) -> map().
extract(Data, Model) ->
  dtrans_model:extract(Data, Model).

%%====================================================================
%% Internal functions
%%====================================================================
