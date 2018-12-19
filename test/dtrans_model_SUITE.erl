-module(dtrans_model_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([cyclic_dependency_test/1]).
-export([required_dependency_of_not_required_field/1]).
-export([layers_test/1, extended_layers_test/1, layers_in_disconnected_graph/1]).

all() ->
  [
    cyclic_dependency_test,
    required_dependency_of_not_required_field,
    layers_test,
    extended_layers_test,
    layers_in_disconnected_graph
  ].

cyclic_dependency_test(_Config) ->
  Model = #{
    field1 => #{
      constructor => {depends_on, fun() -> true end, [field2]}
    },
    field2 => #{
      constructor => {depends_on, fun() -> true end, [field3]}
    },
    field3 => #{
      constructor => {depends_on, fun() -> true end, [field1]}
    }
  },
  ?assertMatch({error, {cyclic_dependency, _}}, dtrans_model:new(Model)).

required_dependency_of_not_required_field(_Config) ->
  Model = #{
    field1 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, []}
    },
    field2 => #{
      required    => true,
      constructor => {depends_on, fun() -> true end, [field1]}
    }
  },
  ?assertEqual({error, dependency_tree_model_cannot_be_resolved}, dtrans_model:new(Model)).

layers_test(_Config) ->
  Model = #{
    field1 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, []}
    },
    field2 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, [field1]}
    }
  },
  ?assertEqual([[field1], [field2]], dtrans_model:new(Model)).

extended_layers_test(_Config) ->
  Model = #{
    field1 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, []}
    },
    field2 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, [field1]}
    },
    field3 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, [field1]}
    }
  },
  ?assertEqual([[field1], [field2, field3]], dtrans_model:new(Model)).

layers_in_disconnected_graph(_Config) ->
  Model = #{
    field1 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, []}
    },
    field2 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, [field1]}
    },
    field3 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, [field1]}
    },
    field4 => #{
      required    => false,
      constructor => {depends_on, fun() -> true end, []}
    }
  },
  ?assertEqual([[field1], [field2, field3, field4]], dtrans_model:new(Model)).