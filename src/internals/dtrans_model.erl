-module(dtrans_model).

-record(dtrans_model_record, {
  static_fields  :: #{dtrans:model_field_name() => dtrans_field:t()},
  layers         :: [[dtrans:model_field_name()]],
  dynamic_fields :: [dtrans:model_dynamic_field()]
}).

-opaque t() :: #dtrans_model_record{}.

-export_type([t/0]).

%% API
-export([new/1]).

-export([extract/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec new(dtrans:model()) ->
 {ok, t()} | {error, Reason :: term()}.
new(RawModel) ->
  PreparedModel = prepare_model(RawModel),
  case build_graph(PreparedModel) of
    {ok, Digraph} ->
      case check_required_dependencies(PreparedModel, Digraph) of
        true ->
          Layers = build_dependency_layers(Digraph),
          digraph:delete(Digraph),
          {ok, #dtrans_model_record{
            static_fields  = fetch_fields(fun(FieldKey) -> not is_function(FieldKey, 1) end, PreparedModel),
            layers         = Layers,
            dynamic_fields = fetch_fields(fun(FieldKey) -> is_function(FieldKey, 1) end, PreparedModel)
          }};
        false ->
          digraph:delete(Digraph),
          {error, dependency_tree_model_cannot_be_resolved}
      end;
    {error, _Reason} = Error ->
      Error
  end.

-spec extract(Data :: dtrans:data(), Model :: t()) ->
  {ok, dtrans:data()} | {error, Reason :: term()}.
extract(Data, #dtrans_model_record{layers = Layers} = Model) ->
  case extract_static_fields(Data, Model) of
    {ok, StaticFields} ->
      DynamicData = maps:without(lists:flatten(Layers), Data),
      DynamicFields = extract_dynamic_fields(DynamicData, Model),
      {ok, maps:merge(DynamicFields, StaticFields)};
    {error, _Reason} = Error ->
      Error
  end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec build_graph(RawModel :: dtrans:model()) ->
  {ok, digraph:graph()} | {error, {cyclic_dependency, Path :: [dtrans:model_field_name()]}}.
build_graph(RawModel) ->
  Digraph = digraph:new([acyclic]),

  Fields = maps:keys(RawModel),
  add_vertexes(Fields, Digraph),

  Links = get_links(RawModel),
  case add_edges(Links, Digraph) of
    {error, {bad_edge, Path}} ->
      digraph:delete(Digraph),
      {error, {cyclic_dependency, Path}};
    _ ->
      {ok, Digraph}
  end.

-spec check_required_dependencies(dtrans:model(), digraph:graph()) -> boolean().
check_required_dependencies(RawModel, Digraph) ->
  Sources = digraph:source_vertices(Digraph),
  FunAll =
    fun(Elem) ->
      #{required := Required} = maps:get(Elem, RawModel),
      check_guarantee_of_existence_dependencies(Elem, Required, RawModel, Digraph)
    end,
  lists:all(FunAll, Sources).

-spec fetch_fields(Predicate, dtrans:model()) -> [dtrans:model()]
  when Predicate :: fun((dtrans:model_field_key()) -> boolean()).
fetch_fields(Predicate, PreparedModel) ->
  Fun =
    fun(K, _V) ->
      Predicate(K)
    end,
  Fields = maps:filter(Fun, PreparedModel),
  upgrade_model(Fields).

upgrade_model(PreparedModel) ->
  Fun =
    fun(Key, Value) ->
      dtrans_model_field:new(Key, Value)
    end,
  maps:map(Fun, PreparedModel).

-spec build_dependency_layers(digraph:graph()) ->
  [[dtrans:model_field_name()]].
build_dependency_layers(Digraph) ->
  build_dependency_layers([], Digraph).

extract_static_fields(Data, #dtrans_model_record{static_fields = Model, layers = Layers} = _Model) ->
  Fun =
    fun
      (Elem, {ok, Acc}) ->
        case dtrans_model_field:extract(Data, Acc, maps:get(Elem, Model)) of
          ok ->
            {ok, Acc};
          {ok, Value} ->
            {ok, Acc#{Elem => Value}};
          {error, _Reason} = Error ->
            Error
        end;
      (_Elem, {error, _Reason} = Error) ->
        Error
    end,
  lists:foldl(Fun, {ok, #{}}, lists:flatten(Layers)).

extract_dynamic_fields(Data, #dtrans_model_record{dynamic_fields = DynamicFields} = _Model) ->
  Proplist = maps:to_list(DynamicFields),
  Fold =
    fun(K, _V, Acc) ->
      FilterMap =
        fun({FieldKey, ModelField}) ->
          case FieldKey(K) of
            {ok, Value} ->
              {true, {K, Value, ModelField}};
            _Error ->
              false
          end
        end,
      case lists:filtermap(FilterMap, Proplist) of
        [{FieldRawName, FieldName, FieldModel} | _Tail] ->
          case dtrans_model_field:extract_dynamic(Data, FieldRawName, FieldModel) of
            {ok, Extracted} ->
              Acc#{FieldName => Extracted};
            _ ->
              Acc
          end;
        [] ->
          Acc
    end
  end,
  maps:fold(Fold, #{}, Data).

%%====================================================================
%% Helpers
%%====================================================================

-spec get_links(dtrans:model()) -> [dtrans:field_name()].
get_links(RawModel) ->
  Fun =
    fun({Key, Value}) ->
      Fields = get_depended_fields(Value),
      [{Key, Field} || Field <- Fields]
    end,
  IrregularLinks = lists:map(Fun, maps:to_list(RawModel)),
  lists:flatten(IrregularLinks).

-spec get_depended_fields(dtrans:field_model(any())) -> [dtrans:field_name()].
get_depended_fields(FieldModel) ->
  Constructor = maps:get(constructor, FieldModel, undefined),
  case Constructor of
    {depends_on, Fields, _Fun} ->
      Fields;
    _ -> []
  end.

-spec add_vertexes(Vertexes :: [dtrans:field_name()], digraph:graph()) -> ok.
add_vertexes(Vertexes, Digraph) ->
  Fun =
    fun(Elem) ->
      digraph:add_vertex(Digraph, Elem)
    end,
  lists:foreach(Fun, Vertexes).

-spec add_edges(Edges :: [{From :: dtrans:field_name(), To :: dtrans:field_name()}], digraph:graph()) ->
 ok | {error, {bad_edge, [dtrans:field_name()]}}.
add_edges(Edges, Digraph) ->
  Fun =
    fun
      ({_From, _To}, {error, {bad_edge, _Fields}} = Error) ->
        Error;
      ({From, To}, _Acc) ->
        digraph:add_edge(Digraph, From, To)
    end,
  case lists:foldr(Fun, ok, Edges) of
    {error, _Reason} = Error ->
      Error;
    _ ->
      ok
  end.

-spec check_guarantee_of_existence_dependencies(
    dtrans:field_name(),
    RequiredFlag :: boolean(),
    dtrans:model(),
    digraph:graph()) ->
  boolean().
check_guarantee_of_existence_dependencies(Field, RequiredFlag, RawModel, Digraph) ->
  case RawModel of
    #{Field := #{required := false}} when RequiredFlag =:= true ->
      false;
    #{Field := #{required := Required}} ->
      Fields = digraph:out_neighbours(Digraph, Field),
      FunAll =
        fun(Elem) ->
          check_guarantee_of_existence_dependencies(Elem, Required, RawModel, Digraph)
        end,
      lists:all(FunAll, Fields)
  end.

-spec build_dependency_layers([[dtrans:model_field_name()]], digraph:graph()) ->
  [[dtrans:model_field_name()]].
build_dependency_layers([] = _Acc, Digraph) ->
  Layer = digraph:source_vertices(Digraph),
  StaticLayer = lists:filter(fun(FieldKey) -> not is_function(FieldKey, 1) end, Layer),
  build_dependency_layers([StaticLayer], Digraph);
build_dependency_layers([[] | Tail] = _Acc, _Digraph) ->
  Tail;
build_dependency_layers([PreviousLayer | _Tail] = Acc, Digraph) ->
  Fun =
    fun(Elem) ->
      digraph:out_neighbours(Digraph, Elem)
    end,
  LayerWithDups = lists:flatmap(Fun, PreviousLayer),
  Layer = sets:to_list(sets:from_list(LayerWithDups)),
  build_dependency_layers([Layer | Acc], Digraph).

-spec prepare_model(dtrans:model()) ->
  PreparedModel :: #{dtrans:model_field_name() => dtrans_field:t()}.
prepare_model(RawModel) ->
  Fun =
    fun(K, V) ->
      ModelField = dtrans_model_field:new(K,  V),
      dtrans_model_field:to_map(ModelField)
    end,
  maps:map(Fun, RawModel).
