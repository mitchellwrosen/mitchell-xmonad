{-# LANGUAGE FlexibleInstances, InstanceSigs, LambdaCase, MultiParamTypeClasses,
             ScopedTypeVariables #-}

module MitchellLayout
  ( MitchellLayout
  , mitchellLayout
  , NumMasters(..)
  , Selecting(..)
  ) where

import Bloop
import Data.Foldable
import Data.Int
import Data.Word
import Graphics.X11
import Numeric.Natural
import XMonad          (LayoutClass(..), X)
import XMonad.StackSet (Stack(Stack))

import qualified XMonad

-- Mitchell's layout!
--
-- * Master pane occupies right 70%, even with no other windows
-- * All slaves evenly split the left 30%
-- * When a slave is selected, it magnifies 10% (e.g. slave2 below)
-- * Two-master mode puts a second, smaller master beneath the first
-- * Main master's left/bottom border are adjustible
-- * Slave zoom is adjustible
--
-- +----------+---------------------------+
-- |  slave1 ←|→ master 1                 |
-- +----------+--+                        |
-- |  *slave2*  ←|→                       |
-- +----------+--+                       ↑|
-- |  ...     +---------------------------+
-- +----------+  master 2 (optional)     ↓|
-- |  slaveN  |                           |
-- +----------+---------------------------+

-- Resize ratio
rr :: Rational
rr = 2/100

data MitchellLayout a
  = MitchellLayout
      NumMasters -- One or two masters?
      Selecting  -- Selecting a master or slave?
      Rational   -- X% the master pane(s) occupy
      Rational   -- Y% the main master pane occupies, when there are two masters
      Natural    -- Slave magnify multiplier
      Natural    -- Master magnify multiplier
  deriving (Read, Show)

mitchellLayout :: Rational -> Rational -> Natural -> Natural -> MitchellLayout a
mitchellLayout =
  MitchellLayout OneMaster SelectingMaster1

data NumMasters
  = OneMaster
  | TwoMasters
  deriving (Read, Show)

flipNumMasters :: NumMasters -> NumMasters
flipNumMasters OneMaster  = TwoMasters
flipNumMasters TwoMasters = OneMaster

data Selecting
  = SelectingMaster1
  | SelectingMaster2
  | SelectingSlave
  deriving (Read, Show)

selecting :: NumMasters -> Stack a -> Selecting
selecting _ (Stack _ [] _)           = SelectingMaster1
selecting TwoMasters (Stack _ [_] _) = SelectingMaster2
selecting _ _                        = SelectingSlave

instance XMonad.LayoutClass MitchellLayout a where
  doLayout
    :: MitchellLayout a
    -> Rectangle
    -> Stack a
    -> X ([(a, Rectangle)], Maybe (MitchellLayout a))
  doLayout (MitchellLayout nm _ qx qy qs qm) rect stack =
    pure
      ( theLayout rect qx qy qs qm (stackLayout nm stack)
      , Just (MitchellLayout nm (selecting nm stack) qx qy qs qm)
      )

  pureMessage (MitchellLayout nm ss qx qy qs qm) msg =
    asum
      [ -- For handling expand/shrink, it depends whether we are selecting a
        -- master (adjust master/slave ratio) or a slave a slave (adjust slave
        -- magnification)
        (let
          selectingMaster = \case
            XMonad.Expand -> MitchellLayout nm ss (max 0 (qx - rr)) qy qs qm
            XMonad.Shrink -> MitchellLayout nm ss (min 1 (qx + rr)) qy qs qm
         in
          case ss of
            SelectingMaster1 -> selectingMaster
            SelectingMaster2 -> selectingMaster
            SelectingSlave ->
              \case
                XMonad.Expand -> MitchellLayout nm ss qx qy (qs + 1) qm
                XMonad.Shrink -> MitchellLayout nm ss qx qy (predNat qs) qm)
        <$> XMonad.fromMessage msg

        -- For handling j/k, it depends whether we are selecting master 1
        -- (adjust master 1/2 ratio) or master 2 (adjust master 2 magnification)
      , do
          Bloop c <- XMonad.fromMessage msg
          case c of
            'k' ->
              case ss of
                SelectingMaster1 -> Just (MitchellLayout nm ss qx (max 0 (qy - rr)) qs qm)
                SelectingMaster2 -> Just (MitchellLayout nm ss qx qy qs (qm + 1))
                SelectingSlave   -> Nothing

            'j' ->
              case ss of
                SelectingMaster1 -> Just (MitchellLayout nm ss qx (min 1 (qy + rr)) qs qm)
                SelectingMaster2 -> Just (MitchellLayout nm ss qx qy qs (predNat qm))
                SelectingSlave   -> Nothing

            'm' ->
              Just (MitchellLayout (flipNumMasters nm) ss qx qy qs qm)

            _ ->
              Nothing
      ]

  description _ =
    "Killer Tofu"

-- The overall shape of the layout
data TheLayout a
  -- Focused on master 1, with a possible second master and 0+ slaves
  = FocusingMaster1 a (Maybe a) [a]
                 -- ^ master 1
                 --   ^ master 2
                 --             ^ slaves

  -- Focused on master 2, with 0+ slaves
  | FocusingMaster2 a a [a]
                 -- ^ master 1
                 --   ^ master 2
                 --     ^ slaves

  -- Focused on a slave, with one master
  | FocusingSlave1 a (Stack a)
                -- ^ master
                --   ^ slaves (with focus)

  -- Focused on a slave, with two masters
  | FocusingSlave2 a a (Stack a)
                -- ^ master 1
                --   ^ master 2
                --     ^ slaves (with focus)

theLayout
  :: Rectangle
  -> Rational
  -> Rational
  -> Natural
  -> Natural
  -> TheLayout a
  -> [(a, Rectangle)]
theLayout rect qx qy qs qm = \case
  FocusingMaster1 master1 Nothing slaves ->
    (master1, master1Rect rect qx 1) :
      zip slaves (unfocusedSlaveRects rect qx (fromIntegral (length slaves)))

  FocusingMaster1 master1 (Just master2) slaves ->
    (master1, master1Rect rect qx qy) :
      (master2, master2Rect rect qx qy 0) :
      zip slaves (unfocusedSlaveRects rect qx (fromIntegral (length slaves)))

  FocusingMaster2 master1 master2 slaves ->
    (master2, master2Rect rect qx qy qm) :
      (master1, master1Rect rect qx qy) :
        zip slaves (unfocusedSlaveRects rect qx (fromIntegral (length slaves)))

  FocusingSlave1 master1 (Stack slave slaveups slavedowns) ->
    xx : yy : zzs
   where
    xx = (slave, focusedSlaveRect rect qx qs (nslaveups :/ nslaves))
    yy = (master1, master1Rect rect qx 1)
    zzs =
      map
        (\(i, sl) -> (sl, unfocusedSlaveRect rect qx (i :/ nslaves)))
        (zip [0..] (reverse slaveups) ++ zip [nslaveups+1 ..] slavedowns)

    nslaveups :: Int32
    nslaveups =
      fromIntegral (length slaveups)

    nslaves =
      nslaveups + 1 + fromIntegral (length slavedowns)

  FocusingSlave2 master1 master2 (Stack slave slaveups slavedowns) ->
    xx : yy : zz : zzs
   where
    xx = (slave, focusedSlaveRect rect qx qs (nslaveups :/ nslaves))
    yy = (master1, master1Rect rect qx qy)
    zz = (master2, master2Rect rect qx qy 0)
    zzs =
      map
        (\(i, sl) -> (sl, unfocusedSlaveRect rect qx (i :/ nslaves)))
        (zip [0..] (reverse slaveups) ++ zip [nslaveups+1 ..] slavedowns)

    nslaveups :: Int32
    nslaveups =
      fromIntegral (length slaveups)

    nslaves =
      nslaveups + 1 + fromIntegral (length slavedowns)

master1Rect :: Rectangle -> Rational -> Rational -> Rectangle
master1Rect (Rectangle x y w h) qx qy =
  Rectangle
    (x + fromIntegral (w - mw))
    y
    mw
    (round (qy * fromIntegral h))
 where
  mw :: Word32
  mw = round (qx * fromIntegral w)

master2Rect :: Rectangle -> Rational -> Rational -> Natural -> Rectangle
master2Rect (Rectangle x y w h) qx qy qm =
  Rectangle
    (x + fromIntegral (w - mw))
    (y + round ((qy - fromIntegral qm * rr) * fromIntegral h))
    mw
    (round ((1 - (qy - fromIntegral qm * rr)) * fromIntegral h))
 where
  mw :: Word32
  mw = round (qx * fromIntegral w)

data ℚ a
  = a :/ a

unfocusedSlaveRect :: Rectangle -> Rational -> ℚ Int32 -> Rectangle
unfocusedSlaveRect (Rectangle x y w h) q (i :/ n) =
  Rectangle
    x
    (y + i * fromIntegral (div h (fromIntegral n)))
    (round ((1-q) * fromIntegral w))
    (div h (fromIntegral n))

unfocusedSlaveRects :: Rectangle -> Rational -> Int32 -> [Rectangle]
unfocusedSlaveRects rect qx n =
  map (\i -> unfocusedSlaveRect rect qx (i :/ n)) [0..n]

focusedSlaveRect :: Rectangle -> Rational -> Natural -> ℚ Int32 -> Rectangle
focusedSlaveRect (Rectangle x y w h) qx qs (i :/ n) =
  Rectangle
    x
    (y + i * fromIntegral hh)
    (round ((1 - qx + fromIntegral qs * rr) * fromIntegral w))
    hh
 where
  hh = div h (fromIntegral n)

stackLayout :: NumMasters -> Stack a -> TheLayout a
stackLayout nmasters (Stack x ys zs) =
  case nmasters of
    OneMaster ->
      case unsnoc1 ys of
        Unsnoc1_0       -> FocusingMaster1 x Nothing zs
        Unsnoc1_1 ys' y -> FocusingSlave1 y (Stack x ys' zs)

    TwoMasters ->
      case unsnoc2 ys of
        Unsnoc2_0 ->
          case zs of
            []    -> FocusingMaster1 x Nothing []
            z:zs' -> FocusingMaster1 x (Just z) zs'

        Unsnoc2_1 y ->
          FocusingMaster2 y x zs

        Unsnoc2_2 ys' y z ->
          FocusingSlave2 z y (Stack x ys' zs)

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

predNat :: Natural -> Natural
predNat 0 = 0
predNat n = n - 1
