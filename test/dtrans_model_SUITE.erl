-module(dtrans_model_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([cyclic_dependency_test/1]).
-export([required_dependency_of_not_required_field/1]).
-export([layers_test/1, extended_layers_test/1, layers_in_disconnected_graph/1]).
-export([simple_extract_data_test/1]).

all() ->
  [
    cyclic_dependency_test,

    required_dependency_of_not_required_field,

    layers_test,
    extended_layers_test,
    layers_in_disconnected_graph,

    simple_extract_data_test
  ].

cyclic_dependency_test(_Config) ->
  Model = #{
    field1 => #{
      constructor => {depends_on, [field2], fun() -> true end}
    },
    field2 => #{
      constructor => {depends_on, [field3], fun() -> true end}
    },
    field3 => #{
      constructor => {depends_on, [field1], fun() -> true end}
    }
  },
  ?assertMatch({error, {cyclic_dependency, _}}, dtrans:new(Model)).

required_dependency_of_not_required_field(_Config) ->
  Model = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [], fun() -> true end}
    },
    field2 => #{
      required      => true,
      default_value => 0,
      constructor   => {depends_on, [field1], fun() -> true end}
    }
  },
  ?assertEqual({error, dependency_tree_model_cannot_be_resolved}, dtrans:new(Model)).

layers_test(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [], fun() -> true end}
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun() -> true end}
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  %% Ugly access method to layers dtrans_model_field record
  ?assertEqual([[field1], [field2]], element(3, Model)).

extended_layers_test(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [], fun() -> true end}
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun() -> true end}
    },
    field3 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun() -> true end}
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  %% Ugly access method to layers dtrans_model_field record
  ?assertEqual([[field1], [field2, field3]], element(3, Model)).

layers_in_disconnected_graph(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [], fun() -> true end}
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun() -> true end}
    },
    field3 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun() -> true end}
    },
    field4 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [], fun() -> true end}
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  %% Ugly access method to layers dtrans_model_field record
  ?assertEqual([[field1], [field2, field3, field4]], element(3, Model)).

simple_extract_data_test(_config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 1}}, dtrans:extract(#{field1 => 1}, Model)).
