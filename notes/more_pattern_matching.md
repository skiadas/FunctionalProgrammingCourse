# More Practice with Pattern Matching

In this section we look at gaining more practice with pattern-matching techniques, by writing standard library methods using pattern matches.

## Pattern-matching examples

The most common use of pattern-matching is in writing functions that process a list. We already saw a number of examples in that direction. The main elements of the process are as follows:

1. We handle in some special way the "base" cases of the empty list, and possibly the list of one element.
2. We handle the general case of a list with a head and a tail. This typically involves calling the function recursively onto the tail, then doing some more work with the result.

The `map` function is a good example of this process:
```haskell
map :: (a -> b) -> [a] -> [b]
map f []       = []
map f (x:xs)   = f x : map f xs
```
Note the second case. We call `map f xs` to obtain the result for the tail of our list. Then we also compute `f x` and put it at the front of the list.

Let us also write the function `filter`: `filter` takes a predicate, which is a function of type `a -> Bool`. Then it takes a list of values, applies the predicate to them, and only returns those for which the predicate is `True`. Here's how that looks like:
```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter p []                 = []
filter p (x:xs) | p x       = x :: filter p xs
                | otherwise = filter p xs
```

Let us look at some more examples. For instance let us write the function `take` that returns the first however many elements from a list. The logic would go like this:

1. If we are asked to take 0 or less elements, then we simply return the empty list.
2. If we are asked to take a number of elements from the empty list, then we simply return the empty list.
3. If we are asked to take `n` elements from a non-empty list, then we will take `n-1` elements from its tail, then append the head element.

Let us translate that into code:
```haskell
take :: Int -> [a] -> [a]
take _ []                   = []
take n (x:xs) | n <= 0      = []
              | otherwise   = x : take (n-1) xs
```

### Practice Problems

You are expected to do these using pattern-matching and recursion as above, and not via other means.

1. Write a function `length :: [a] -> Int` which given a list returns its length.
2. Write a function `last :: [a] -> a` which returns the last element of a non-empty list. You should not worry about its behavior on an empty list.
3. Write the function `(++) :: [a] -> [a] -> a` which concatenates two lists. Write it using `++` as an infix operator in the definition (will not need the parentheses if it is infix).
4. Write a function `init :: [a] -> [a]` which given a list returns the list without the last element. If the list is empty it should return an empty list.
5. Write a function `zip :: [a] -> [b] -> [(a, b)]` that given two lists forms pairs (tuples) out of the corresponding values (i.e. the first elements go together, the second elements go together etc). Stop when either list runs out of elements.
6. (difficult. Skip for now) Write a function `unzip :: [(a, b)] -> ([a], [b])` that basically does the opposite of `zip`.
7. Write a function `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`. It takes a function that turns an `a` and a `b` into a value of type `c`, and also takes a list of `a`s and a list of `b`s. It then forms a list out of the result of applying the function to the corresponding pairs of elements.
8. Write a function `insertOrdered :: Ord t => t -> [t] -> [t]` that takes a list containing values in increasing order, possibly with duplicates, and a new element to insert into the list. It then inserts that element in the correct spot to preserve the order. For example `insertOrdered 4 [1, 3, 6] = [1, 3, 4, 6]`.
9. Write a function `searchOrdered :: Ord t => t -> [t] -> Bool` that takes a list containing values in increasing order, possibly with duplicates, and an element, and it checks to see if the element is in the list. *This function should only traverse as much of the list as it needs to.*
10. Write a function `interject :: [a] -> [a] -> [a]` that given two lists produces a new list with the values interjected. So the first value of the first list goes first, followed by the first value of the second list, followed by the second value of the first list and so on. If any list ends first, the remaining entries are formed from the remaiming elements. For example `interject [1, 2, 3] [4, 5, 6, 7, 8] = [1, 4, 2, 5, 3, 6, 7, 8]`.
11. (difficult) Write a function `splitAt :: Int -> [a] -> ([a], [a])` which takes an integer and a list, and splits the list in two at that integer and stores the two parts in a tuple. If the integer is 0 or less, then the first part of the tuple would be `[]`. If the integer is longer than the list length, then the second part of the tuple would be `[]`. Simple example: `splitAt 3 [1..5] = ([1, 2, 3], [4, 5])`
12. (difficult) Write a function `splitWith :: (a -> Bool) -> [a] -> ([a], [a])` which take as input a predicate and a list, and separates the list in two lists, with the first list containing those elements for which the predicate is `True` and the second list containing those elements for which the predicate is `False`. The order of elements must be maintained within each list.
