-module(dtrans_model_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() ->
  [
    cyclic_dependency_test,
    required_dependency_of_not_required_field,

    layers_test,
    extended_layers_test,
    layers_in_disconnected_graph,

    simple_extract_data_test,
    extract_data_with_constructor_test,
    extract_data_with_validator_test,
    extract_data_with_deps_test,
    extract_required_field,
    extract_not_required_field_with_default_value,
    extract_not_required_field_without_default_value,
    extract_internal_field_value,
    extract_internal_field_value_with_dependencies,
    extract_internal_field_with_ignored_shadowing_field_in_data,

    required_field_not_present,
    constructor_error,
    validator_throws_error,
    constructor_throws_error,
    validator_invalid_output,
    constructor_invalid_output
  ].

%%====================================================================
%% Build model errors
%%====================================================================

cyclic_dependency_test(_Config) ->
  Model = #{
    field1 => #{
      constructor => {depends_on, [field3], fun(_, _) -> {ok, true} end}
    },
    field2 => #{
      constructor => {depends_on, [field3], fun(_, _) -> {ok, true} end}
    },
    field3 => #{
      constructor => {depends_on, [field2], fun(_, _) -> {ok, true} end}
    }
  },
  ?assertMatch({error, {cyclic_dependency, _}}, dtrans:new(Model)).

required_dependency_of_not_required_field(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(_) -> {ok, true} end
    },
    field2 => #{
      required      => true,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> {ok, true} end}
    }
  },
  ?assertEqual({error, dependency_tree_model_cannot_be_resolved}, dtrans:new(RawModel)).

%%====================================================================
%% Build model
%%====================================================================

layers_test(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(_) -> {ok, true} end
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> {ok, true} end}
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
      constructor   => fun(_) -> {ok, true} end
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> {ok, true} end}
    },
    field3 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> {ok, true} end}
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
      constructor   => fun(_) -> {ok, true} end
    },
    field2 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_) -> {ok, true} end}
    },
    field3 => #{
      required      => false,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> {ok, true} end}
    },
    field4 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(_) -> {ok, true} end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  %% Ugly access method to layers dtrans_model_field record
  ?assertEqual([[field1], [field2, field3, field4]], element(3, Model)).

%%====================================================================
%% Extraction
%%====================================================================

simple_extract_data_test(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 1}}, dtrans:extract(#{field1 => 1}, Model)).

extract_data_with_constructor_test(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(Value) -> {ok, Value + 1} end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 2}}, dtrans:extract(#{field1 => 1}, Model)).

extract_data_with_validator_test(_Config) ->
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
      constructor   => {depends_on, [field1], fun(Lhs, Rhs) -> {ok, Lhs + Rhs} end}
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 1, field2 => 3}}, dtrans:extract(#{field1 => 1, field2 => 2}, Model)).

extract_required_field(_Config) ->
  RawModel = #{
    field => #{
      required => true
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field => 1}}, dtrans:extract(#{field => 1}, Model)).

extract_not_required_field_with_default_value(_Config) ->
  RawModel = #{
    field => #{
      required      => false,
      default_value => 1
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field => 1}}, dtrans:extract(#{}, Model)).

extract_not_required_field_without_default_value(_Config) ->
  RawModel = #{
    field => #{
      required => false
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{}}, dtrans:extract(#{}, Model)).

extract_internal_field_value(_Config) ->
  RawModel = #{
    field => #{
      internal    => true,
      constructor => fun() -> {ok, 1} end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field => 1}}, dtrans:extract(#{}, Model)).

extract_internal_field_value_with_dependencies(_Config) ->
  RawModel = #{
    field1 => #{},
    field2 => #{
      internal    => true,
      constructor => {depends_on, [field1], fun(Field1) -> {ok, Field1 + 1} end}
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field1 => 3, field2 => 4}}, dtrans:extract(#{field1 => 3}, Model)).

extract_internal_field_with_ignored_shadowing_field_in_data(_Config) ->
  RawModel = #{
    field => #{
      internal    => true,
      constructor => fun() -> {ok, 1} end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field => 1}}, dtrans:extract(#{field => 3}, Model)).

%%====================================================================
%% Extracting errors
%%====================================================================

required_field_not_present(_Config) ->
  RawModel = #{
    field1 => #{
      required      => true,
      default_value => 0,
      constructor   => fun(_) -> {ok, 1} end
    },
    field2 => #{
      required      => true,
      default_value => 0,
      constructor   => {depends_on, [field1], fun(_, _) -> {ok, true} end}
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({error, {no_data, field1}}, dtrans:extract(#{field2 => 1}, Model)).

constructor_error(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(_) -> {error, test} end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({error, {construction_error, field1, test}}, dtrans:extract(#{field1 => 1}, Model)).

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

validator_invalid_output(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      validator     => fun(_) -> invalid_output end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({error, {validator_invalid_output, field1, invalid_output}}, dtrans:extract(#{field1 => 1}, Model)).

constructor_invalid_output(_Config) ->
  RawModel = #{
    field1 => #{
      required      => false,
      default_value => 0,
      constructor   => fun(_) -> invalid_output end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({error, {constructor_invalid_output, field1, invalid_output}}, dtrans:extract(#{field1 => 1}, Model)).
