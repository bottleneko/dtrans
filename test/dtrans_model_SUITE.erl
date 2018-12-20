-module(dtrans_model_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([cyclic_dependency_test/1]).
-export([required_field_not_present/1]).
-export([required_dependency_of_not_required_field/1]).

-export([
  layers_test/1,
  extended_layers_test/1,
  layers_in_disconnected_graph/1
]).

-export([
  simple_extract_data_test/1,
  extract_data_with_constructor_test/1,
  extract_data_with_validator_test/1,
  extract_data_with_deps_test/1
]).

-export([
  validator_throws_error/1,
  constructor_throws_error/1
  ]).

-export([
  extract_required_field/1,
  extract_default_value/1
]).

all() ->
  [
    cyclic_dependency_test,

    required_dependency_of_not_required_field,

    required_field_not_present,

    layers_test,
    extended_layers_test,
    layers_in_disconnected_graph,

    simple_extract_data_test,
    extract_data_with_constructor_test,
    extract_data_with_validator_test,
    extract_data_with_deps_test,

    validator_throws_error,
    constructor_throws_error,

    extract_required_field,
    extract_default_value
  ].

cyclic_dependency_test(_Config) ->
  Model = #{
    field1 => #{
      constructor => {depends_on, [field3], fun() -> true end}
    },
    field2 => #{
      constructor => {depends_on, [field3], fun() -> true end}
    },
    field3 => #{
      constructor => {depends_on, [field2], fun() -> true end}
    }
  },
  ?assertMatch({error, {cyclic_dependency, _}}, dtrans:new(Model)).

required_dependency_of_not_required_field(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [], fun() -> true end}
    },
    field2 => #{
      required      => true,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_) -> true end}
    }
  },
  ?assertEqual({error, dependency_tree_model_cannot_be_resolved}, dtrans:new(RawModel)).

required_field_not_present(_Config) ->
  RawModel = #{
    field1 => #{
      required      => true,
      default_value => 0,
      constructor   => fun(_) -> 1 end
    },
    field2 => #{
      required      => true,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> true end}
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({error, {no_data, field1}}, dtrans:extract(#{field2 => 1}, Model)).

layers_test(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [], fun(_) -> true end}
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> true end}
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
      constructor   => fun(_) -> true end
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> true end}
    },
    field3 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> true end}
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
      constructor   => fun(_) -> true end
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_) -> true end}
    },
    field3 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> true end}
    },
    field4 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(_) -> true end
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

extract_data_with_constructor_test(_config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(Value) -> Value + 1 end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 2}}, dtrans:extract(#{field1 => 1}, Model)).

extract_data_with_validator_test(_config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      validator     =>
        fun
          (Value) when Value rem 2 =:= 1 -> ok;
          (_Value) -> {error, fail}
        end

    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({error, {validation_error, field1, fail}}, dtrans:extract(#{field1 => 2}, Model)),
  ?assertEqual({ok, #{field1 => 1}}, dtrans:extract(#{field1 => 1}, Model)).

extract_data_with_deps_test(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(Lhs, Rhs) -> Lhs + Rhs end}
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 1, field2 => 3}}, dtrans:extract(#{field1 => 1, field2 => 2}, Model)).

validator_throws_error(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      validator     => fun(_) -> erlang:error(test) end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({error, {validation_error, field1, test}}, dtrans:extract(#{field1 => 1}, Model)).

constructor_throws_error(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(_) -> erlang:error(test) end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({error, {construction_error, field1, test}}, dtrans:extract(#{field1 => 1}, Model)).

extract_required_field(_Config) ->
  RawModel = #{
    field1 => #{
      required      => true,
      default_value => 0
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 1}}, dtrans:extract(#{field1 => 1}, Model)).

extract_default_value(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 1
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 1}}, dtrans:extract(#{}, Model)).
