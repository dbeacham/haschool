~~~ {.haskell}
data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

intList :: List Int
intList = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))
~~~

Helper functions

~~~ {.haskell}
wrap :: a -> List a
wrap a = Cons a Nil

nil :: List a -> Bool
nil Nil = True
nil _   = False
~~~

Definition of fold on a list

~~~ {.haskell}
-- foldL (fold list) is the equivalent of foldr
foldL :: (a -> b -> b) -> b -> List a -> b
foldL f acc Nil = acc
foldL f acc (Cons x xs) = f x (foldL f acc xs)

Exercise 3.1
Using the universal property, prove the fusion law:

For strict h,
  h . (foldL f e) = foldL f' e'
where
  h (f a b) = f' a (h b), h e = e'

~~~ {.haskell}
-- Exercise 3.2
-- define mapL, appendL, concatL using foldL

mapL :: (a -> b) -> List a -> List b
mapL f = foldL (Cons . f) Nil

appendL :: List a -> List a -> List a
-- appendL xs ys = foldL Cons ys xs
appendL = flip (foldL Cons)

concatL :: List (List a) -> List a
concatL = foldL appendL Nil
~~~

Exercise 3.3
As an extension to the general fusion law, and using the answer to 3.2, prove the map fusion law:

foldL f e . map g = foldL (f . g) e

One classic application of foldL is the insertion sort algorithm, defined by

~~~ {.haskell}
insertionSort :: (Ord a) => List a -> List a
insertionSort = foldL insert Nil
  where insert y Nil = wrap y
        insert y (Cons x xs)
         | y < x = Cons y (Cons x xs)
         | otherwise = Cons x (insert y xs)
~~~

Because `insert y (Cons x xs)` sometimes requires `xs` as well as `insert y xs` means that `insert y` is difficult to write as an instance of foldL. But, the tupled function insert', satisfying

~~~ {.haskell}
insert' :: a -> List a -> (List a, List a)
insert' y xs = (xs, insert y xs)
~~~

can be written as a straighforwardly as a fold:

~~~ {.haskell}
-- Exercise 3.4
insert' :: (Ord a) => a -> List a -> (List a, List a)
insert' y xs = foldL go (Nil, wrap y) xs
  where
    go x (tot, Cons i is)
      | i < x = (Cons x tot, Cons i (Cons x is))
      | otherwise = (Cons x tot, Cons x (Cons i is))
~~~

The value of `insert y (Cons x xs)` depends not only on the result of recursive call `insert y xs` but also on the substructure `xs` itself. Our solution above is to define a function that returns both of these; afterwards, we can decide which one we do not want. An alternative is to capture this modified recursion pattern explicitly as a higher-order operator; in this case, the operator is known as a _paramorphism_. In the case of lists, we define

~~~ {.haskell}
paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL f e Nil = e
paraL f e (Cons x xs) = f x (xs, paraL f e xs)
~~~

Here, the argument, `f` takes a copy of the tail `xs` along with the result `paraL f e xs` of the recursive call on that tail.

~~~ {.haskell}
-- Exercise 3.5
-- Define `insert` as an instance of `paraL`

insert :: (Ord a) => a -> List a -> List a
insert y = paraL go (wrap y)
  where
    go x (xs, Cons i is)
      | i < x = Cons i (Cons x is)
      | otherwise = Cons x (Cons i is)
~~~

## Unfolds for lists

The dual of folding is _unfolding_. The haskell standard library provides the `unfoldr` function for generating lists. Our equivalent is

~~~ {.haskell}
unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f s =
  case f s of 
     Just (a, b) -> Cons a (unfoldL' f b)
     Nothing -> Nil

-- `stars' n` returns a string of n '*'
stars' = unfoldL' go
  where
    go 0 = Nothing
    go n = Just ('*', n-1)
~~~

Sometimes it is convenient to split the function argument of `unfoldL'` into three components

    - a predicate indicating when the argument should return `Nothing`
    - a function yielding the first component of the pair when the predicate doesn't hold
    - a function yielding the second component of the pair when the predicate doesn't hold

~~~ {.haskell}
unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f s z = if p z
                   then Nil
                   else f z : (unfoldL p f s (s z))

stars = unfoldL (== 0) (const '*') pred
~~~

~~~
-- Exercise 3.6
-- Express unfoldL in terms of unfoldL' and vice versa

derivedUnfoldL' f z = unfoldL p g h
  where
    p = f z == Nothing
    g = case f z of Just (a, b) -> a
    h = case f z of Just (a, b) -> b
    
derivedUnfoldL p f s z = unfoldL' f z
  where
    f z = if p z
          then Nothing
          else Just (f z, s z)
~~~

Exercise 3.7
Using the universal property, prove the _fusion law_
  unfoldL p f g . h = unfoldL p' f' g'
where
  (p . h = p'), (f . h = f') and (g . h = h . g')


Conversely, one could define a function `foldL'` taking a single argument of type `Maybe (a, b) -> b` in place of the two arguments to `foldL`

~~~ {.haskell}
foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil = f Nothing
foldL' f (Cons x xs) = f $ Just (x , foldL' f xs)
~~~

The primed versions of `foldL` and `unfoldL` make the duality between folding and unfolding very clear, although they may be less convenient to program with.

~~~ {.haskell}
-- Exercise 3.8
-- Define `foldL'` in terms of `foldL` and vice versa

derivedFoldL f z xs = foldL' g xs
  where
    g Nothing = z
    g (Just (a, b)) = f a b

derivedFoldl' g xs = foldL f z xs
  where
    f = \a b -> g $ Just (a, b)
    z = g Nothing
~~~

The adaptation of the single-argument fold and unfold to the multi-argument interface is simplified by functions of the following types

~~~ {.haskell}
foldLArgs :: (a -> b -> b) -> b -> (Maybe (a, b) -> b)
foldLArgs f z Nothing = z
foldLArgs f z (Just (a, b)) = f a b

unfoldLArgs :: (b -> Bool) -> (b -> a) -> (b -> b) -> (b -> Maybe (a, b))
unfoldLArgs p f s x = if p x
                      then Nothing
                      else Just (f x, s x)
~~~

### Selection sort

A _selection sort_ operates by removing the minimum element of the list to be sorted at each iteration, but leaving the other elements in the same order. This algorithm may be expressed as an unfold.

We define `delmin` to do this removal

~~~ {.haskell}
delmin :: (Ord a) => List a -> Maybe (a, List a)
delmin Nil = Nothing
delmin xs = Just (y, deleteL y xs)
  where y = minimumL xs

minimumL :: (Ord a) => List a -> a
minimumL (Cons x xs) = foldL min x xs

deleteL :: a -> List a -> List a
deleteL _ Nil = Nil
deleteL y (Cons x xs)
  | x == y = xs
  | otherwise = Cons x (deleteL y xs)
~~~

The selection sort is then straight forward to define. **Remember that an unfold takes a seed (the unordered list) and creates a new value to put on the unfolded structure (the minimum element), plus a new seed value (the list without its minimum element)**.

~~~ {.haskell}
ssort :: (Ord a) => List a -> List a
ssort = unfoldL' delmin
~~~

~~~
-- Exercise 3.10
-- The case `deleteL y (Cons x xs)` requies both the tail, `xs`
-- and the result, `delete y xs`, so this function is another
-- paramorphism. Define `deleteL` in terms of `paraL`.

deleteL' y = paraL go Nil
  where
    go x (xs, ls)
      -- no recursion as we don't need to calculate the
      -- second value in the pair, `ls`
      | x == y = xs
      -- we recurse in this case, as we need to run `paraL`
      -- to determine the structure of `ls`
      | otherwise = Cons x ls
~~~

In fact, `delmin` is itself a paramorphism! Redefine `delmin` using `paraL` as the only form of recursion, taking care to retain the order of the remainder of the list.

~~~ {.haskell}
delmin' :: (Ord a) -> List a -> Maybe (a, List a)
delmin' = paraL go Nothing
 where
   go x (xs, Nothing) = Just x xs
   go x (xs, Just m is)
     | x < m = Just x xs
     | otherwise = Just m (Cons x is)
~~~

### Bubble sort

A _bubble sort_ has a very similar structure to the selection sort - an unfold on lists - but the body is slightly different. The function `bubble` has the same type as `delmin` but it doesn't preserve the relative order of the remaining elements. This relaxation means that it is possible to define `bubble` as a fold

~~~ {.haskell}
bubble :: (Ord a) => List a -> Maybe (a, List a)
bubble = foldL step Nothing
  where
    step x Nothing = Just (x, Nil)
    step x (Just (y, ys))
      | x < y = Just (x, Cons y ys)
      | otherwise = Just (y, Cons x ys)
~~~

Of course, `Maybe (a, List a)` is isomorphic to `List a`. Use this fact to have `bubble` return `List a`.

~~~ {.haskell}
bubble' :: (Ord a) => List a -> List a
bubble' = foldL step Nil
  where
    step x Nil = wrap x
    step x (Cons y ys)
      | x < y = Cons x (Cons y ys)
      | otherwise = Cons y (Cons x ys)
~~~

Given `bubble`, the bubble sort alogrithm is simply

~~~ {.haskell}
bsort :: (Ord a) => List a -> List a
bsort = unfoldL' bubble
~~~

Infact, `insert` can also be described as an unfold. The state of the unfold consists of a pair: the list into which to insert, and _maybe_ an element to be inserted. Initially there is an element to insert, but once it has been inserted the rest of the list is merely copied.

~~~ {.haskell}
insert'' :: (Ord a) => a -> List a -> List a
insert'' y = unfoldL' ins
~~~
