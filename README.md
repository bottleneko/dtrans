dtrans [![Build Status](https://travis-ci.org/bottleneko/dtrans.svg?branch=master)](https://travis-ci.org/bottleneko/dtrans)
=====

Data transformation and validation micro library. Not production ready.
 Breaking changes is coming

Take a wow-effect from coverage
-----

```sh
$ rebar3 ct --cover && rebar3 cover --verbose
```

# Short documentation 

## Field properties

### Internal

Association `internal => true` means that this field be constructed 
without input data using only functions with /0 arity and other 
from fields values throught tuple:

`{depends_on, [other_field_name], fun(OtherFieldValue) -> {ok, 42} end}`

> Note: lambda-function above will be called with validated and constructed values 

> Warning: when this property is set as `true` - `required`, `validator`, `default_value` being ignored

```erlang
1> {ok, Model} = dtrans:new(#{
      timestamp =>
        #{internal => true,
          constructor => 
            fun() -> 
                {ok, erlang:apply(erlang, timestamp, [])} 
            end
        }
    }).
2> dtrans:extract(#{}, Model).
{ok,#{timestamp => {1545,494367,679728}}}
```

### Required

Correctly set `required` field give the opportunity checks model 
while building in future this will be used in compile-time models
 checks 

```erlang
1> dtrans:new(#{
      a =>
        #{required      => false,
          default_value => 0
        },
      b => 
        #{required      => false,
          default_value => 0
        },
      sum_of_a_and_b =>
        #{internal    => true,
          required    => true,
          constructor => 
            {depends_on, 
              [a, b], 
              fun(A, B) -> {ok, A + B} end}
        }
    }).
{error,dependency_tree_model_cannot_be_resolved}
```

And this field property using while data extracting

```erlang
1> {ok, Model} = dtrans:new(#{
      required_field =>
        #{required => true}
    }).
2> dtrans:extract(#{}, Model).
{error,{no_data,required_field}}
```
    
### Validator

This field may be set validation lambda-function with /1 arity.
This function should be return `ok` or `{error, Reason :: any()}` values 

```erlang
1> {ok, Model} = dtrans:new(#{
      invalid_field =>
        #{validator =>
            fun
                (42 = _Value) -> ok;
                (_Value)      ->  {error, "Expected value is 42"}
            end
        }
    }).
2> dtrans:extract(#{invalid_field => 43}, Model).
{error,{validation_error,invalid_field,"Expected value is 42"}}
```
  
### Default value

Default value working only for `required => false` not internal fields

```erlang
1> {ok, Model} = dtrans:new(#{
      field =>
        #{default_value => 0}
    }).
2> dtrans:extract(#{}, Model).
{ok,#{field => 0}}
3> dtrans:extract(#{field => 42}, Model).
{ok,#{field => 42}}
```

```erlang
1> {ok, Model} = dtrans:new(#{
      field => #{}
    }).
2> dtrans:extract(#{}, Model).
{ok,#{}}
3> dtrans:extract(#{field => 42}, Model).
{ok,#{field => 42}}
```

## Constructor

You can change final value of field throught set property `constructor` by lambda function with /1 arity 
or tuple for using other fields values:

`{depends_on, [other_field_name], fun(MainFieldValue, OtherFieldValue) -> {ok, 42} end}`

Construction functions must be returns `{ok, Value :: any()}` or `{error, Reason :: any()}` values

> Note: lambda-function above will be called with validated and constructed values 

> Warning: by using `depends_on` you may be show `{error, {cyclic_dependency, [...]} while building model`

```erlang
1> {ok, Model} = dtrans:new(#{
      field =>
        #{constructor => 
            fun(Value) -> 
              {ok, Value + 41}
            end
         }
    }).
2> dtrans:extract(#{field => 1}, Model).
{ok,#{field => 42}}
```

### Model

With `model` property you can inherit model in other model as field specification

```erlang
1> {ok, InnerModel} = dtrans:new(#{
      inner_field => #{}
    }).
2> {ok, OuterModel} = dtrans:new(#{
      outer_field => 
        #{required => true,
          model    => InnerModel
         }
    }).
3> dtrans:extract(#{outer_field => #{inner_field => 4}}, OuterModel).
{ok, #{outer_field => #{inner_field => 4}}}
```

# TODO

* [ ] Add compile-time build for models where possible
* [ ] Add more information to errors
* [ ] Use other model as field spec
    * [x] New model field property `model`
    * [ ] New model field property set count of models 