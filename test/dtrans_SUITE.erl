-module(dtrans_SUITE).

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
    extract_field_with_model,
    extract_optional_field_with_model,
    extract_field_with_model_internal_field,
    extract_field_with_model_error_data_not_present,
    extract_field_with_model_validation_error,
    extract_one_field,
    extract_many_fields,
    extract_many_fields_default,
    extract_many_fields_error,
    extract_dynamic_field,
    extract_dynamic_field_with_model,

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

extract_field_with_model(_Config) ->
  RawInnerModel = #{
    field => #{
      internal    => true,
      constructor => fun() -> {ok, 1} end
    }
  },
  {ok, InnerModel} = dtrans:new(RawInnerModel),
  RawOuterModel = #{
    field => #{
      required => true,
      model    => InnerModel
    }
  },
  {ok, OuterModel} = dtrans:new(RawOuterModel),
  ?assertEqual({ok, #{field => #{field => 1}}}, dtrans:extract(#{field => #{}}, OuterModel)).

extract_optional_field_with_model(_Config) ->
  RawInnerModel = #{
    inner_field => #{
      internal    => true,
      constructor => fun() -> {ok, 1} end
    }
  },
  {ok, InnerModel} = dtrans:new(RawInnerModel),
  RawOuterModel = #{
    outer_field => #{
      required => false,
      model    => InnerModel
    }
  },
  {ok, OuterModel} = dtrans:new(RawOuterModel),
  ?assertEqual({ok, #{}}, dtrans:extract(#{}, OuterModel)).

extract_field_with_model_internal_field(_Config) ->
  RawInnerModel = #{
    inner_field => #{}
  },
  {ok, InnerModel} = dtrans:new(RawInnerModel),
  RawOuterModel = #{
    outer_field => #{
      required => true,
      model    => InnerModel
    }
  },
  {ok, OuterModel} = dtrans:new(RawOuterModel),
  ?assertEqual({ok, #{outer_field => #{inner_field => 4}}},
    dtrans:extract(#{outer_field => #{inner_field => 4}}, OuterModel)).

extract_field_with_model_error_data_not_present(_Config) ->
  RawInnerModel = #{
    inner_field => #{
      required => true
    }
  },
  {ok, InnerModel} = dtrans:new(RawInnerModel),
  RawOuterModel = #{
    outer_field => #{
      required => true,
      model    => InnerModel
    }
  },
  {ok, OuterModel} = dtrans:new(RawOuterModel),
  ?assertEqual({error, {error_in_inner_model,outer_field,{no_data,inner_field}}}, dtrans:extract(#{outer_field => #{}}, OuterModel)).

extract_field_with_model_validation_error(_Config) ->
  RawInnerModel = #{
    inner_field => #{
      validator => fun(_Value) -> {error, "Some error"} end
    }
  },
  {ok, InnerModel} = dtrans:new(RawInnerModel),
  RawOuterModel = #{
    outer_field => #{
      model => InnerModel
    }
  },
  {ok, OuterModel} = dtrans:new(RawOuterModel),
  ?assertEqual({error,
    {error_in_inner_model,outer_field,
      {validation_error,inner_field,"Some error"}}},
    dtrans:extract(#{outer_field => #{inner_field => 4}}, OuterModel)).

extract_one_field(_Config) ->
  RawModel = #{
    field => #{
      count       => one,
      constructor => fun(Value) -> {ok, [1 | Value]} end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field => [1, 2, 3, 4]}}, dtrans:extract(#{field => [2, 3, 4]}, Model)).

extract_many_fields(_Config) ->
  RawModel = #{
    field => #{
      count       => many,
      constructor => fun(Value) -> {ok, Value + 1} end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field => [2, 3, 4]}}, dtrans:extract(#{field => [1, 2, 3]}, Model)).

extract_many_fields_error(_Config) ->
  RawModel = #{
    field => #{
      count       => many,
      constructor =>
        fun
          (1 = Value) -> {ok, Value + 1};
          (_Value)    -> {error, only_1}
        end
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual(
    {error,
      {{list_processing_error,3},
        field,
        {construction_error,field,only_1}}},
    dtrans:extract(#{field => [1, 2, 3]}, Model)).

extract_many_fields_default(_Config) ->
  RawModel = #{
    field => #{
      count         => many,
      constructor   => fun(Value) -> {ok, Value + 1} end,
      default_value => [1, 2, 3]
    }
  },
  {ok, Model} = dtrans:new(RawModel),
  ?assertEqual({ok, #{field => [2, 3, 4]}}, dtrans:extract(#{}, Model)).

extract_dynamic_field(_Config) ->
  DynamicFieldKey =
    fun(Value) ->
      Binary = iolist_to_binary(Value),
      case Binary of
        <<"test_", _/binary>> ->
          {ok, Binary};
        _ ->
          {error, wrong_dynamic_field_name}
      end
    end,
  RawDynamicModel = #{
    DynamicFieldKey => #{}
  },
  {ok, DynamicModel} = dtrans:new(RawDynamicModel),
  ?assertEqual({ok, #{<<"test_field">> => 1}},
    dtrans:extract(#{"test_field" => 1, "non_test_field" => 2}, DynamicModel)).

extract_dynamic_field_with_model(_Config) ->
  RawInnerModel = #{
    field => #{
      required => true,
      constructor => fun(Value) -> {ok, Value + 1} end
    }
  },
  {ok, InnerModel} = dtrans:new(RawInnerModel),
  DynamicFieldKey =
    fun(Value) ->
      Binary = iolist_to_binary(Value),
      case Binary of
        <<"test_", _/binary>> ->
          {ok, Binary};
        _ ->
          {error, wrong_dynamic_field_name}
      end
    end,
  RawDynamicModel = #{
    DynamicFieldKey => #{
      model => InnerModel
    }
  },
  {ok, DynamicModel} = dtrans:new(RawDynamicModel),
  ?assertEqual({ok, #{<<"test_field">> => #{field => 42}}},
    dtrans:extract(#{"test_field" => #{field => 41}, "non_test_field" => 2}, DynamicModel)).

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
