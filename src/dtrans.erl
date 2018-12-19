-module(dtrans).

-type model_field_name()  :: any().

-type model_field(FieldType) :: #{
  required      => boolean(),
  validator     => fun((FieldType) -> ok | {error, Reason :: term()}),
  default_value => FieldType,
  constructor   =>
  fun(() -> FieldType)
  | {depends_on, [dtrans_model:field_name()], fun((...) -> FieldType)}
}.

-type model() :: #{model_field_name() => model_field(any())}.

-export_type([model/0, model_field_name/0, model_field/1]).

%% API exports
-export([extract/2]).

%%====================================================================
%% API functions
%%====================================================================

extract(_Data, _Model) ->
  1.

%%====================================================================
%% Internal functions
%%====================================================================
