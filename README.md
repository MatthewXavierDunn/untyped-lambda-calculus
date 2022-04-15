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
lamb> 0
(\f x . x)
lamb> 1
(\f x . (f x))
```

And therefore, all operations defined on them are defined in terms of this encoding:

```
lamb> 4 + 5
(\f x . (f (f (f (f (f (f (f (f (f x))))))))))
lamb> succ 2
(\f x . (f (f (f x))))
```

#### Booleans

As with the natural numbers, the language also uses the standard Church encoding:

```
lamb> True
(\x y . x)
lamb> False
(\x y . y)
lamb> True && True
(\x y . x)
lamb> False || False
(\x y . x)
```

#### Lists

Lists are structured as Church pairs with a single element as the first of the pair, and the rest of the list as the second. Additionally we have a `Nil` element which represents the end of the list.

```
lamb> [1,2,3]
(\f . (f (\f x . (f x)) (\f . (f (\f x . (f (f x))) (\f . (f (\f x . (f (f (f x)))) (\x x y . x)))))))
lamb> head [1,2,3]
(\f x . (f x))
lamb> tail [1,2,3]
(\f . (f (\f x . (f (f x))) (\f . (f (\f x . (f (f (f x)))) (\x x y . x)))))
```

#### Larger Structures

There are several large expressions which make writing programs in this language much easier:

**if-then-else**

```
lamb> if True then a else b
a
```

**let-in**

```
lamb> let f x = x in f a
a
```

**letrec-in**
(for allowing recursion in a let expression)

```
lamb> letrec length xs = if null xs then 0 else 1 + (length (tail xs)) in length [1,2,3]
(\f x . (f (f (f x))))
```
