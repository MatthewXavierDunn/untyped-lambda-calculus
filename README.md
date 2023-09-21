# untyped-lambda-calculus

Language written in `haskell` that compiles down to pure, untyped lambda calculus.

## Representations

The program uses `Church encoding` to represent natural numbers, linked lists, and tuples.

## Syntax

### Commands

Commands must be prefixed by a colon, `:`.

| Command                           | Description                                                                                         |
|-----------------------------------|-----------------------------------------------------------------------------------------------------|
| `:q`                              | quits the program                                                                                   |
| `:b <exp>`                        | reduces the given `<exp>` printing each intermediate beta reduction                                 |
| `:a <exp> << <var> << <with-var>` | performs alpha conversion on the given `<exp>` changing all references to `<var>` with `<with-var>` |
| `:f <exp>`                        | gives a list of all free-variables in the given `<exp>`                                             |

### Expressions

Expressions can take the form of standard, untyped calculus with the exception that variables can consist of multiple characters and the lambda symbol is replaced with a `\`:

```
lam> \f x y . f y x
(\f x y . (f y x))
lam> (\f x y . f y x) (\x y . x) a b
b
```

There are additional, larger structures in the language that are desugared into this pure lambda calculus.

#### Natural Numbers

The natural numbers use the Church numerals encoding:

```
lam> 0
(\f x . x)
lam> 1
(\f x . (f x))
```

And therefore, all operations defined on them are defined in terms of this encoding:

```
lam> 4 + 5
(\f x . (f (f (f (f (f (f (f (f (f x))))))))))
lam> succ 2
(\f x . (f (f (f x))))
```

#### Booleans

As with the natural numbers, the language also uses the standard Church encoding:

```
lam> True
(\x y . x)
lam> False
(\x y . y)
lam> True && True
(\x y . x)
lam> False || False
(\x y . y)
```

#### Lists

Lists are structured as Church pairs with a single element as the first of the pair, and the rest of the list as the second. Additionally we have a `Nil` element which represents the end of the list.

```
lam> [1,2,3]
(\f . (f (\f x . (f x)) (\f . (f (\f x . (f (f x))) (\f . (f (\f x . (f (f (f x)))) (\x x y . x)))))))
lam> head [1,2,3]
(\f x . (f x))
lam> tail [1,2,3]
(\f . (f (\f x . (f (f x))) (\f . (f (\f x . (f (f (f x)))) (\x x y . x)))))
```

#### Larger Structures

There are several large expressions which make writing programs in this language much easier:

**if-then-else**

```
lam> if True then a else b
a
```

**let-in**

```
lam> let f x = x in f a
a
```

**letrec-in**
(for allowing recursion in a let expression)

```
lam> letrec length xs = if null xs then 0 else 1 + (length (tail xs)) in length [1,2,3]
(\f x . (f (f (f x))))
```

## Install

`clone` this repo then run `cabal run`.

### Prerequisites

You simply need to install `haskell` and `cabal` via `GHCup`.
