-module(dtrans_model).

-record(dtrans_model_record, {
  model  :: #{dtrans:model_field_name() => dtrans_field:t()},
  layers :: [[dtrans:model_field_name()]]
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
  case build_graph(RawModel) of
    {ok, Digraph} ->
      case check_required_dependencies(RawModel, Digraph) of
        true ->
          Fun =
            fun(Key, Value) ->
              dtrans_model_field:new(Key, Value)
            end,
          UpgradedModel = maps:map(Fun, RawModel),
          {ok, #dtrans_model_record{
            model  = UpgradedModel,
            layers = build_dependency_layers(Digraph)
          }};
        false ->
          {error, dependency_tree_model_cannot_be_resolved}
      end;
    {error, _Reason} = Error ->
      Error
  end.

-spec extract(Data :: dtrans:data(), Model :: t()) ->
  {ok, dtrans:data()} | {error, Reason :: term()}.
extract(Data, #dtrans_model_record{model = Model, layers = Layers} = _Model) ->
  Fun =
    fun
      (Elem, {ok, Acc}) ->
        case dtrans_model_field:extract(Data, Acc, maps:get(Elem, Model)) of
          {ok, Value} ->
            {ok, Acc#{Elem => Value}};
          {error, _Reason} = Error ->
            Error
        end;
      (_Elem, {error, _Reason} = Error) ->
        Error
    end,
  lists:foldl(Fun, {ok, #{}}, lists:flatten(Layers)).

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

-spec build_dependency_layers(digraph:graph()) ->
  [[dtrans:model_field_name()]].
build_dependency_layers(Digraph) ->
  build_dependency_layers([], Digraph).

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

build_dependency_layers([] = _Acc, Digraph) ->
  Layer = digraph:source_vertices(Digraph),
  build_dependency_layers([Layer], Digraph);
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
