-module(dtrans_model).

-type field_name()  :: any().

-type field_model(FieldType) :: #{
  required      => boolean(),
  validator     => fun((FieldType) -> boolean()),
  default_value => FieldType,
  constructor   =>
    fun(() -> FieldType)
    | {depends_on, [field_name()], fun((...) -> FieldType)}
}.

-export_type([field_model/1]).

-type t() :: #{field_name() => field_model(any())}.

-export_type([t/0]).

-record(dtrans_model_internals, {

}).

-opaque t_internals() :: #dtrans_model_internals{}.

-export_type([t_internals/0]).

%% API
-export([new/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec new(t()) -> t_internals().
new(RawModel) ->
  {ok, _Digraph} = build_graph(RawModel).

%%====================================================================
%% Internal functions
%%====================================================================

-spec build_graph(RawModel :: t()) ->
  {ok, Digraph :: digraph:graph()} | {error, cyclic_dependency}.
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

%%====================================================================
%% Helpers
%%====================================================================

-spec get_links(t()) -> [field_name()].
get_links(Model) ->
  Fun =
    fun({Key, Value}) ->
      Fields = get_depended_fields(Value),
      [{Key, Field} || Field <- Fields]
    end,
  IrregularLinks = lists:map(Fun, maps:to_list(Model)),
  lists:flatten(IrregularLinks).

-spec get_depended_fields(field_model(any())) -> [field_name()].
get_depended_fields(FieldModel) ->
  Constructor = maps:get(constructor, FieldModel),
  case Constructor of
    {depends_on, _Fun, Fields} ->
      Fields;
    _ -> []
  end.

-spec add_vertexes(Vertexes :: [field_name()], digraph:graph()) -> ok.
add_vertexes(Vertexes, Digraph) ->
  Fun =
    fun(Elem) ->
      digraph:add_vertex(Digraph, Elem)
    end,
  lists:foreach(Fun, Vertexes).

-spec add_edges(Edges :: [{From :: field_name(), To :: field_name()}], digraph:graph()) ->
 ok | {error, {bad_edge, [field_name()]}}.
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
