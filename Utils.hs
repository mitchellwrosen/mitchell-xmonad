{-# LANGUAGE LambdaCase #-}

module Utils where

data Unsnoc1 a
  = Unsnoc1_0
  | Unsnoc1_1 [a] a

unsnoc1 :: [a] -> Unsnoc1 a
unsnoc1 =
  foldr step Unsnoc1_0
 where
  step :: a -> Unsnoc1 a -> Unsnoc1 a
  step x = \case
    Unsnoc1_0 -> Unsnoc1_1 [] x
    Unsnoc1_1 xs y -> Unsnoc1_1 (x:xs) y

data Unsnoc2 a
  = Unsnoc2_0
  | Unsnoc2_1 a
  | Unsnoc2_2 [a] a a

unsnoc2 :: [a] -> Unsnoc2 a
unsnoc2 =
  foldr step Unsnoc2_0
 where
  step :: a -> Unsnoc2 a -> Unsnoc2 a
  step x = \case
    Unsnoc2_0 -> Unsnoc2_1 x
    Unsnoc2_1 y -> Unsnoc2_2 [] x y
    Unsnoc2_2 xs y z -> Unsnoc2_2 (x:xs) y z
