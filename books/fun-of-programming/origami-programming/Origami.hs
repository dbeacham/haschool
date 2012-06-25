module Origami where

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

intList :: List Int
intList = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))

wrap :: a -> List a
wrap a = Cons a Nil

nil :: List a -> Bool
nil Nil = True
nil _   = False

-- foldL (fold list) is the equivalent of foldr
foldL :: (a -> b -> b) -> b -> List a -> b
foldL f acc Nil = acc
foldL f acc (Cons x xs) = f x (foldL f acc xs)

-- Exercise 3.1
-- Using the universal property, prove the fusion law:
--
-- For strict h,
--   h . (foldL f e) = foldL f' e'
-- where
--   h (f a b) = f' a (h b), h e = e'

-- Exercise 3.2
-- define mapL, appendL, concatL using foldL

mapL :: (a -> b) -> List a -> List b
mapL f = foldL (Cons . f) Nil

appendL :: List a -> List a -> List a
-- appendL xs ys = foldL Cons ys xs
appendL = flip (foldL Cons)

concatL :: List (List a) -> List a
concatL = foldL appendL Nil

-- Exercise 3.3
-- As an extension to the general fusion law, and using
-- the answer to 3.2, prove the map fusion law:
--
-- foldL f e . map g = foldL (f . g) e

-- One classic application of foldL is the insertion sort
-- algorithm, defined by
insertionSort :: (Ord a) => List a -> List a
insertionSort = foldL insert Nil
  where insert y Nil = wrap y
        insert y (Cons x xs)
         | y < x = Cons y (Cons x xs)
         | otherwise = Cons x (insert y xs)

-- Exercise 3.4
-- Because `insert y (Cons x xs)` sometimes requires
-- `xs` as well as `insert y xs` means that `insert y`
-- is difficult to write as an instance of foldL. But,
-- the tupled function insert', satisfying

-- insert' :: a -> List a -> (List a, List a)
-- insert' y xs = (xs, insert y xs)

-- can be written as a straighforwardly as a fold:

insert' :: (Ord a) => a -> List a -> (List a, List a)
insert' y xs = foldL go (Nil, wrap y) xs
  where
    go x (tot, Cons i is)
      | i < x = (Cons x tot, Cons i (Cons x is))
      | otherwise = (Cons x tot, Cons x (Cons i is))

-- Exercise 3.5
-- The value of `insert y (Cons x xs)` depends not only on the
-- result of recursive call `insert y xs` but also on the
-- substructure `xs` itself. Our solution above is to define
-- a function that returns both of these; afterwards, we can
-- decide which one we do not want. An alternative is to capture
-- this modified recursion pattern explicitly as a higher-order
-- operator; in this case, the operator is known as a
-- _paramorphism_. In the case of lists, we define

paraL :: (a -> (List a, b) -> b) -> b -> List a -> b
paraL f e Nil = e
paraL f e (Cons x xs) = f x (xs, paraL f e xs)

-- Here, the argument, `f` takes a copy of the tail `xs` along with the
-- result `paraL f e xs` of the recursive call on that tail. Define
-- `insert` as an instance of `paraL`

insert :: (Ord a) => a -> List a -> List a
insert y = paraL go (wrap y)
  where
    go x (xs, Cons i is)
      | i < x = Cons i (Cons x is)
      | otherwise = Cons x (Cons i is)


-- Unfolds for lists
unfoldL' :: (b -> Maybe (a, b)) -> b -> List a
unfoldL' f s =
  case f s of 
     Just (a, b) -> Cons a (unfoldL' f b)
     Nothing -> Nil

stars' = unfoldL' go
  where
    go 0 = Nothing
    go n = Just ('*', n-1)

unfoldL :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> List a
unfoldL p f s z = if p z
                   then Nil
                   else Cons (f z) (unfoldL p f s (s z))

stars = unfoldL (== 0) (const '*') pred

-- Exercise 3.6
-- Express unfoldL in terms of unfoldL' and vice versa

derivedUnfoldL' f = unfoldL p g h
  where
    p = \z -> f z == Nothing
    g = \z -> case f z of Just (a, b) -> a
    h = \z -> case f z of Just (a, b) -> b
    
derivedUnfoldL p f s = unfoldL' g
  where
    g z = if p z
          then Nothing
          else Just (f z, s z)


foldL' :: (Maybe (a, b) -> b) -> List a -> b
foldL' f Nil = f Nothing
foldL' f (Cons x xs) = f $ Just (x , foldL' f xs)
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

-- simplify conversion from multi- to single-argument functions
foldLArgs :: (a -> b -> b) -> b -> (Maybe (a, b) -> b)
foldLArgs f z Nothing = z
foldLArgs f z (Just (a, b)) = f a b

unfoldLArgs :: (b -> Bool) -> (b -> a) -> (b -> b) -> (b -> Maybe (a, b))
unfoldLArgs p f s x = if p x
                      then Nothing
                      else Just (f x, s x)

delmin :: (Ord a) => List a -> Maybe (a, List a)
delmin Nil = Nothing
delmin xs = Just (y, deleteL y xs)
  where y = minimumL xs

minimumL :: (Ord a) => List a -> a
minimumL (Cons x xs) = foldL min x xs

deleteL :: (Eq a) => a -> List a -> List a
deleteL _ Nil = Nil
deleteL y (Cons x xs)
  | x == y = xs
  | otherwise = Cons x (deleteL y xs)

deleteL' y = paraL go Nil
  where
    go x (xs, ls)
      -- no recursion as we don't need to calculate the
      -- second value in the pair, `ls`
      | x == y = xs
      -- we recurse in this case, as we need to run `paraL`
      -- to determine the structure of `ls`
      | otherwise = Cons x ls

delmin' :: (Ord a) => List a -> Maybe (a, List a)
delmin' = paraL go Nothing
 where
   go x (xs, Nothing) = Just (x, xs)
   go x (xs, Just (m, is))
     | x < m = Just (x, xs)
     | otherwise = Just (m, Cons x is)

bubble :: (Ord a) => List a -> Maybe (a, List a)
bubble = foldL step Nothing
  where
    step x Nothing = Just (x, Nil)
    step x (Just (y, ys))
      | x < y = Just (x, Cons y ys)
      | otherwise = Just (y, Cons x ys)

bubble' :: (Ord a) => List a -> List a
bubble' = foldL step Nil
  where
    step x Nil = wrap x
    step x (Cons y ys)
      | x < y = Cons x (Cons y ys)
      | otherwise = Cons y (Cons x ys)

insert'' :: (Ord a) => a -> List a -> List a
insert'' y xs = unfoldL' ins (Just y, xs)
  where
    ins (Nothing, Nil) = Nothing
    ins (Nothing, Cons x xs) = Just (x, (Nothing, xs))
    ins (Just y, Cons x xs)
      | y < x = Just (y, (Nothing, Cons x xs))
      | otherwise = Just (x, (Just y, xs))
    ins (Just y, Nil) = Just (y, (Nothing, Nil))

apoL' :: (b -> Maybe (a, Either b (List a))) -> b -> List a
apoL' f z = case f z of
              Nothing -> Nil
              Just (x, Left v) -> Cons x (apoL' f v)
              Just (x, Right xs) -> Cons x xs

insert''' :: (Ord a) => a -> List a -> List a
insert''' y xs = apoL' ins (y, xs)
  where
    ins (y, Cons x xs)
      | y < x = Just (y, Right (Cons x xs))
      | otherwise = Just (x, Left (y, xs))
    ins (y, Nil) = Just (y, Right Nil)
