# Compound Haskell Types

### Compound Types

There are a number of ways of producing more complex types out of simpler types. These are some times called *compound types*.

#### List Types

The first example of that is the list type. As elements of the list all must have the same type, we can specify the type of a list with two pieces of information:

- The fact that it is a list. This is denoted by using a single pair of square brackets: `[...]`
- The fact that the entries have a certain type. That type goes between the brackets.

```haskell
[False, True, True, True] :: [Bool]
['a', 'b', 'c'] :: [Char]            -- Can also be called String
"abc" :: [Char]                      -- Same as above
["abc", "def"] :: [[Char]]           -- Or also [String]
```

**Practice**: Write a value of type `[[Int]]`.

#### Tuple Types

A **tuple** is a collection of values separated by commas and surrounded by parentheses. Unlike lists:

- A tuple has a fixed number of elements (fixed *arity*), either zero or at least two.
- The elements can have different types, from each other.
- The types of each of the elements collectively form the type of the tuple.

Examples:
```haskell
(False, 3) :: (Bool, Int)
(3, False) :: (Int, Bool)    -- This is different from the one above
(True, True, "dat")  :: (Bool, Bool, [Char])
() :: ()                     -- The empty tuple, with the empty tuple type
```

We can also mix list types and tuple types. For instance:

```haskell
[(1, 2), (0, 2), (3, 4)] :: [(Int, Int)]       --   A list of pairs of integers
 -- A list of pairs of strings and booleans
[("Peter", True), ("Jane", False)] :: [([Char], Bool)]
```

**Activity**: Think of uses for tuple types. What information might we choose to represent with tuples?

#### Function types

A function type is written as `A -> B` where `A` is the type of the input and `B` is the type of the output. For example:
```haskell
add3 x = x + 3           :: Int -> Int
add (x, y) = x + y       :: (Int, Int) -> Int
oneUpTo n = [1..n]       :: Int -> [Int]
range (a, b) = [a..b]    :: (Int, Int) -> [Int]
```

When writing functions, we tend to declare their type right before their definition, like so:
```haskell
range :: (Int, Int) -> [Int]
range (a, b) = [a..b]
```

You may be tempted to think of this function as a function of two variables. It technically is not, and we will discuss this topic on the next section.

### Type Practice

Work out the types of the following expressions:

1. `(5 > 3, 3 + head [1, 2, 3])`
2. `[length "abc"]`
3. The function `f` defined by `f lst = length lst + head lst`
4. The function `g` defined by `g lst = if head lst then 5 else 3`
