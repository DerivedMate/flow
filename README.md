# Flow

CPS styled, interpreted, dangerously dynamically typed programming language with no compile time checks, and exclusively runtime errors.

## Table of contents
- [Flow](#flow)
  - [Table of contents](#table-of-contents)
  - [History](#history)
  - [Installation](#installation)
  - [Syntax highlighting](#syntax-highlighting)
  - [Basic concepts](#basic-concepts)
  - [Data types](#data-types)
  - [Casting](#casting)
  - [Binary operations](#binary-operations)
  - [Variables](#variables)
    - [Via arguments](#via-arguments)
    - [Via let bindings](#via-let-bindings)
    - [Via labels](#via-labels)
  - [Functions](#functions)
    - [Currying](#currying)
    - [Conditionals](#conditionals)
    - [Modifiers](#modifiers)
      - [map](#map)
      - [keep](#keep)
      - [gen](#gen)
      - [fold](#fold)
      - [unfold](#unfold)
    - [let](#let)
    - [lazy](#lazy)
  - [IO](#io)
  - [Capturing](#capturing)
  - [Modules](#modules)

## History

The initial idea was born on the [Cave of Linguists](https://discord.me/cave-of-linguists) discord server. Pioneering work was done by the user of name waterboi who introduced the original syntax. His implementation can be found under [thomaskoppelaar/flow](https://github.com/thomaskoppelaar/flow).

The following implementation further diverged from the aforementioned one, yet has maintained the original spirit.

## Installation

The project requires [haskell](https://www.haskell.org/), and stack (included with haskell) to run. Once acquired, the installation is just:

```sh
# Clone the repo
git clone https://github.com/DerivedMate/flow.git
cd flow

# build & run
stack build; stack run
```

It can be also used to run individual files:

```sh
stack run -- file/path
```

Or as a repl:

```sh
stack run
```

The preferred file extension is `.hf`.

## Syntax highlighting

A vscode extension for syntax highlighting can be found under [flow-highlight](https://github.com/DerivedMate/flow-highlight). For the time being, the only way to install it is to clone it to `~/.vscode/extensions/flow-highlight`.

## Basic concepts

The main idea behind flow is the flow of data between _"cells"_. Each cell is either:

1. an expression: `{ 2 }`, `{ + 2 1 }`, `{(1, 3)}`;
1. a function: `{ ~add a Int, b Int. + a b }`;
1. or an IO operation: `{ <~ [Int] }`.

Flow from one cell to another is denoted by `=>`:

```
{(1, 3)} => { ~add a Int, b Int. + a b }
         => { <~ Int }
```

In the above example, the first cell returns a [pair](#data-types) of `(1, 3)`. That is further passed into a [function](#functions) of two [ints](#data-types) `a = 1, b = 3`. The result is passed into an output operation, which displays `4` onto the screen (_stdout_).

## Data types

Current data types include:

| Name    | Examples                |
| ------- | ----------------------- |
| Int     | `1`, `+3`, `-42`        |
| Float   | `3.1415`, `2.`          |
| Str     | `` `Hello` ``           |
| Bool    | `True`, `False`         |
| tuple\* | `(1, 3)`                |
| list    | `[1, 2, 3]`, `[4, 5.0]` |
| Any     | anything                |

> \*tuples, for the time being, are treated as data carriers instead of proper data types. Recall how we destructured `(1, 3)` in the previous example

In general, data is weakly typed, unless the type is explicitly stated:

```
{ a,     b.         + a b } => let ~combine
{ a Int, b Int. Int + a b } => let ~add %% first casts a, b into ints, and + a b - to int %%

{(1, 3)}              => ~combine => { <~ Int } %% => 4             %%
{(`Hello`, ` World`)} => ~combine => { <~ Str } %% => `Hello World` %%
{(`7`, `11`)}         => ~combine => { <~ Int } %% => 711           %%
{(`7`, `11`)}         => ~add     => { <~ Int } %% => 18            %%
```

## Casting

Flow allows for two kinds of casting: of arguments, and of variables. The former take form of specifying the arguments' types:

```
{ ~add a Int, b Int. %% ... %% }
```

The latter regard variables, and can be used like normal expressions:

```
{ + 2 5 :: Float } %% == { + 2 (5 :: Float) } %%
{ ~add a, b. + a b }
   => { &0 :: (Any -> Any -> Int) }
   => { f. {( [1, 2, 3], [4, 5] )} => ~f  }
   => { <~ Str } %% => 5 %%
```

## Binary operations

Flow uses prefix notation for binary operations. If an operation is not defined for a pair of types (let's say, `Int`, `String`), the latter is cast to the type of the former (`` (+ 1 `2`) `` is `3`, `` (+ `1` 2) `` is `` `12` ``). Current operators include:

| Name                  | Syntax |
| --------------------- | ------ |
| modulo                | `%`    |
| addition              | `+`    |
| subtraction           | `-`    |
| multiplication        | `*`    |
| division              | `/`    |
| exponentiation        | `^`    |
| concatenation         | `+`    |
| set difference        | `-`    |
| greater than or equal | `>=`   |
| greater than          | `>`    |
| less than or equal    | `<=`   |
| less than             | `<`    |
| equal                 | `==`   |
| not equal             | `/=`   |
| and                   | `&&`   |
| or                    | `\|\|` |

## Variables

There are three ways of bindings values:

### Via arguments

Bindings created this way exist only in the function's scope:

```
{ `Ann` } => { name Str. %% name is visible only here %% }
```

### Via let bindings

Let bindings always bind to the nearest scope. All previous bindings of the same name are affectively shadowed:

```
{ 5 } => let { a } %% a = 5                     %%
{ 6 } => let { a } %% a = 6, but is not mutated %%
```

It is highly encouraged to use `let ~name` for functions, and `let { name }` for values; nonetheless, the two are exactly equivalent.

### Via labels

Labels are used for self-referencing a function in its scope. Binding is created iff the function has not been defined in the current scope.

```
{ a, b. + a b } => let ~f
{ ~f. %% ~f refers to the above %% }
```

## Functions

Functions are cells that take arguments, and can be **locally** aliased with labels:

```
{ ~fact n, m.
   > n 0 | {( - n 1, * n m )} => ~fact
}
```

Type annotations for arguments, and return values are also optional:

```
{ ~add a Int, b Int. Int 
   + a b 
}
```

the above denotes a function of two integers that returns an integer. Optional parenthesis can be used around types for clarity:

```
{ greeting Str. (Str -> Str)
   { name Str. Str 
      + greeting name
   }
} => let ~makeGreeting
{ `Hello, ` } => ~makeGreeting => let ~welcome
{ `Bob`     } => ~welcome      => { <~ Str }
%% => Hello, Bob %%
```

### Currying

Functions are by design curried:

```
{ a, b. + a b }   => let ~add
{ 5 } => ~add     => let ~addFive
{ 6 } => ~addFive => let { eleven }

{ eleven } => { <~ Int }
```

Which also means that argumentless functions are immediately called:

```
{. { `secrets` } => { <~ Str } => { `ooops` } } => let { badNews }
```

This can be avoided by using the [lazy](#modifiers) modifier, which get executed only when referenced:

```
lazy {. { `secrets` } => { <~ Str } } => let ~PublishReynoldsPamphlet
~PublishReynoldsPamphlet 
```

### Conditionals

Flow uses guard-style conditionals, which can be expressions or even proper flows:

```
{ n Int.
    == 0 (% n 2) | True
                 | False
} => let ~isEven
{ n Int.
    { n } => ~isEven | `It's Even`
                     | `It's Odd`
} => let ~f

{ 4 } => ~f => { <~ Str } %% => It's Even %%
{ 1 } => ~f => { <~ Str } %% => It's Odd  %%
```

Needless to say, multiple guards can be used; only the first truthy one is executed:

```
{ n =
    > n 0 | 1
    < n 0 | -1
          | 0
} => let ~sgn
```

### Modifiers

The behavior of a cell can be changed with modifiers:

```
mod ~f
mod { ~f }
mod { ~f %% ... %% }
```

Current modifiers include:

| Signature | Description                                                                                                               |
| --------- | ------------------------------------------------------------------------------------------------------------------------- |
| map       | takes a collection of elements (list, tuple - single elements default to lists), and applies the function to each of them |
| keep      | uses the function as a predicate: if it's `False`, the flow stops; if `True` - continues unchanged                        |
| keep[]    | like `keep`, but acts on every element of a collection (same as in `map`)                                                 |
| gen       | yields a value, and calls itself. Creates new flows from a single element                                                 |
| fold      | folds (left to right) a list of elements together with an accumulator. Returns the accumulator                            |
| unfold    | collects yields of the inner generator                                                                                    |
| let       | binds the incoming value to the specified name(s) in the current scope                                                    |
| lazy      | defers cell's execution until it's explicitly referenced                                                                  |

#### map

```
{ a Int. + 1 a } => let ~inc
{[1, 2, 3]} => map ~inc => { <~ [Int] } %% => [2, 3, 4] %%
```

#### keep

```
{ a Int. == 0 ( % a 2 ) } => let ~isEven
{ 3 } => keep ~isEven => { `I'm never printed!` } => { <~ Str }
{ 2 } => keep ~isEven => { <~ Int } %% => 2 %%

{[1, 2, 3, 4]} => keep[] ~isEven => { <~ [Int] } %% => [2, 4] %%
```

#### gen

```
{ n Int.
    > n 0 | (n, - n 1)
} => let ~iter
{ 3 } => map { n. {n} => gen ~iter }
      => { <~ [Int] }        %% => [3, 2, 1] %%
{ 3 } => gen ~iter => { <~ Int } %% => 3 2 1     %%
```

#### fold

```
   {( 1, [1, 2, 3, 4] )} %% (initial, elements) %%
=> fold
   { ~prod a Int, n Int. * a n }
   %%
     | a | n | return   |
     | 1 | 1 | 1*1 = 1  |
     | 1 | 2 | 1*2 = 2  |
     | 2 | 3 | 2*3 = 6  |
     | 6 | 4 | 6*4 = 24 |

     => 24
   %%
=> { <~ Int }
```

Fold can also accept multiple arguments:

```
   {( ``, [(`Hi`, `Alice`), (`Hello`, `Bob`)] )}
=> fold
   { acc, greeting, name.
        + acc ( + (+ (+ greeting `, `) name) `! ` )
   }
=> { <~ Str }
   %%
    => Hi, Alice! Hello, Bob!
   %%
```

#### unfold

```
%%
    Finite Iterate version of factorial.
    Returns iterations of factorial
    from 1 to m.
%%

   {(1, ~> Int, 1)}
=> unfold
   { ~fact n, m, acc.
        <= n m | ( * n acc
                 , ( + n 1
                   , m
                   , * n acc
                   )
                 )
   }
=> { <~ [Int] }
```

### let

```
{ m Int, w Str, n Int. Str
   == 0 % n m | w
              | ``
} => let ~mkFB
{[ (3, `foo`)
 , (5, `bar`)
 , (7, `yo`) 
 ]} => map ~mkFB 
    => let ~fs
{ n Int. 
   > n 0 
   | {( ``, fs )} => fold { a, f. { n } => { + a f } } 
                  => let { r }
     {. r | (r, - n 1) 
          | (n, - n 1) 
     }
} => let ~foobar
{ ~> Int } => gen ~foobar => { <~ Str }
```

### lazy

Lazy is primarily intended for deferring argumentless functions/cells or functions with all arguments bound. 

```
lazy { ~> Int } => let { getInt }
~getInt => let { age }
```

## IO

At the moment, IO operations only support stdio, yet further extensions are planned. IO is clearly divided into:

1. Input - going _with_ the flow; therefore, the arrow points to the right: `{ ~> type }`, returning `type`;
2. Output - _against_ the flow with the arrow pointing to the left: `{ <~ type }`, returning `nil`.

All IO operations require explicit casting.
Currently IO operations are executed sequentially, in the order of occurrence:

```
{(~> Int, ~> Int)} => { a, b. - a b } => { <~ Int }
%%
input:
3
1
output:
2
%%
```

## Capturing

Capturing can be employed to use the results of the previous flow, without explicitly defining variables. For example, instead of defining the `~add` function, we can write:

```
{(1, 3)} => { + &0 &1 } => { <~ Int } %% => 4 %%
```

Expression `&i` for a natural number `i` refers to the (i+1)-th argument.
Slicing is also available through `&i:j`. Zero indexed, both inclusive. `j` can be omitted to capture the remainder.

```
{(1, 3)} => {[&0:, &0:]}     %% [(1, 3), (1, 3)] %%
         => map { + &0 &1 }  %% [4, 4]           %%
         => { <~ [Int] }
```

However, where this feature truly comes in handy is in nested generators. Consider:

```
{ n Int.
    > n 0 | (n, - n 1)
} => let ~iter

{[3, 2, 1]}
    => map { n. {n} => gen {
             m. {m} => gen ~iter
                    => { r. (r, - r 1) }
            }}
    => { <~ [Int] }

{[3, 2, 1]}
    => map {{&0} => gen {
            {&0} => gen ~iter
                 => {(&0, - &0 1)}
            }}
    => { <~ [Int] }
```

The latter gets rid of dummy variable declarations.

Capturing can also be used with non-literal, and non-integer values:

```
{ l, r, xs.
   { xs } => { &l:r }
} => let ~slice

%%
   Parenthesis can be employed 
   to increase the readability 
   of certain expressions:
%%
{ l, n, xs. 
   { xs } => { &l:(+ l n) }
} => let ~chop
```

Even captures can be used; however, every non-integer value is still cast to int:

```
{ xs. { xs } => { &(&1:) } 
} => let ~lastExn
%%
   `xs` is sliced into `xs[x_1..x_n]`;
   cast into an Int, which corresponds
   to the list length - 1;
   that is then used as an index for `xs`.
%%
%% Consider: %%
{[ 1, 2, 3 ]} 
   => ~lastExn 
   => { <~ Int } 
   %% 3              %%

%% However:  %%
{ [] }        
   => ~lastExn 
   => { <~ Int } 
   %% <scope error!> %%
```

## Modules

Separate files can be imported using `@import` statement with a **relative** module path:

```
@import(`./path/to/module.hf`)

%% ... %%
```

Modules are simply inlined, following the order of dependency. Cyclic dependencies are not supported, and will result in a compile time error:

```
$> Error: cyclic dependency tree. Root path: /absolute/root/module/path.hf
```