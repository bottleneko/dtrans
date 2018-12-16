-module(dtrans_model_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([cyclic_dependency_test/1]).

all() ->
  [
    cyclic_dependency_test
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
  ?assertEqual({error, {cyclic_dependency, [field2, field3, field1]}}, dtrans_model:new(Model)).
