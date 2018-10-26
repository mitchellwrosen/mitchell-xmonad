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
      !NumMasters -- One or two masters?
      !Selecting  -- Selecting a master or slave?
      !Rational   -- X% the master pane(s) occupy
      !Rational   -- Y% the main master pane occupies, when there are two masters
      !Natural    -- Slave magnify multiplier
      !Natural    -- Master magnify X multiplier
      !Natural    -- Master magnify Y multiplier
  deriving (Read, Show)

mitchellLayout :: Rational -> Rational -> Natural -> Natural -> Natural -> MitchellLayout a
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
  | SelectingSlave !Int
  deriving (Read, Show)

selecting :: NumMasters -> Stack a -> Selecting
selecting _ (Stack _ [] _)                = SelectingMaster1
selecting OneMaster (Stack _ (_:xs) _)    = SelectingSlave (length xs)
selecting TwoMasters (Stack _ [_] _)      = SelectingMaster2
selecting TwoMasters (Stack _ (_:_:xs) _) = SelectingSlave (length xs)

instance XMonad.LayoutClass MitchellLayout a where
  doLayout
    :: MitchellLayout a
    -> Rectangle
    -> Stack a
    -> X ([(a, Rectangle)], Maybe (MitchellLayout a))
  doLayout (MitchellLayout nm _ qx qy qs qmx qmy) rect stack =
    pure
      ( theLayout rect qx qy qs qmx qmy (stackLayout nm stack)
      , Just (MitchellLayout nm (selecting nm stack) qx qy qs qmx qmy)
      )

  pureMessage ll@(MitchellLayout nm ss qx qy qs qmx qmy) msg =
    asum
      [ (\case
            XMonad.Expand -> decreaseMasterSlaveRatio ll
            XMonad.Shrink -> increaseMasterSlaveRatio ll)
        <$> XMonad.fromMessage msg

      , case XMonad.fromMessage msg of
          Just (Bloop 'j') ->
            Just (increaseMastersRatio ll)

          Just (Bloop 'k') ->
            Just (decreaseMastersRatio ll)

          Just (Bloop 'H') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (increaseMasterZoomX ll)
              SelectingSlave _ -> Just (decreaseSlaveZoom ll)

          Just (Bloop 'J') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (decreaseMasterZoomY ll)
              SelectingSlave _ -> Nothing

          Just (Bloop 'K') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (increaseMasterZoomY ll)
              SelectingSlave _ -> Nothing

          Just (Bloop 'L') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (decreaseMasterZoomX ll)
              SelectingSlave _ -> Just (increaseSlaveZoom ll)

          Just (Bloop '0') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (resetMasterZoom ll)
              SelectingSlave _ -> Just (resetSlaveZoom ll)

          Just (Bloop 'm') ->
            Just (MitchellLayout (flipNumMasters nm) ss qx qy qs qmx qmy)

          _ ->
            Nothing
      ]

  description _ =
    "Killer Tofu"

decreaseMastersRatio :: MitchellLayout a -> MitchellLayout a
decreaseMastersRatio (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss qx (max 0 (qy - rr)) qs qmx qmy

increaseMastersRatio :: MitchellLayout a -> MitchellLayout a
increaseMastersRatio (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss qx (min 1 (qy + rr)) qs qmx qmy

increaseMasterZoomX :: MitchellLayout a -> MitchellLayout a
increaseMasterZoomX (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss qx qy qs (qmx + 1) qmy

decreaseMasterZoomX :: MitchellLayout a -> MitchellLayout a
decreaseMasterZoomX (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss qx qy qs (predNat qmx) qmy

increaseMasterZoomY :: MitchellLayout a -> MitchellLayout a
increaseMasterZoomY (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss qx qy qs qmx (qmy + 1)

decreaseMasterZoomY :: MitchellLayout a -> MitchellLayout a
decreaseMasterZoomY (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss qx qy qs qmx (predNat qmy)

resetMasterZoom :: MitchellLayout a -> MitchellLayout a
resetMasterZoom (MitchellLayout nm ss qx qy qs _ _) =
  MitchellLayout nm ss qx qy qs 0 0

decreaseMasterSlaveRatio :: MitchellLayout a -> MitchellLayout a
decreaseMasterSlaveRatio (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss (max 0 (qx - rr)) qy qs qmx qmy

increaseMasterSlaveRatio :: MitchellLayout a -> MitchellLayout a
increaseMasterSlaveRatio (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss (min 1 (qx + rr)) qy qs qmx qmy

increaseSlaveZoom :: MitchellLayout a -> MitchellLayout a
increaseSlaveZoom (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss qx qy (qs + 1) qmx qmy

decreaseSlaveZoom :: MitchellLayout a -> MitchellLayout a
decreaseSlaveZoom (MitchellLayout nm ss qx qy qs qmx qmy) =
  MitchellLayout nm ss qx qy (predNat qs) qmx qmy

resetSlaveZoom :: MitchellLayout a -> MitchellLayout a
resetSlaveZoom (MitchellLayout nm ss qx qy _ qmx qmy) =
  MitchellLayout nm ss qx qy 0 qmx qmy

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
  -> Natural
  -> TheLayout a
  -> [(a, Rectangle)]
theLayout rect qx qy qs qmx qmy = \case
  FocusingMaster1 master1 Nothing slaves ->
    (master1, applyRectSpec (master1RectSpec qx 1) rect) :
      zip
        slaves
        (map
          (`applyRectSpec` rect)
          (unfocusedSlaveRectSpecs qx (fromIntegral (length slaves))))

  FocusingMaster1 master1 (Just master2) slaves ->
    (master1, applyRectSpec (master1RectSpec qx qy) rect) :
      (master2, applyRectSpec (master2RectSpec qx qy qmx 0) rect) :
      zip
        slaves
        (map
          (`applyRectSpec` rect)
          (unfocusedSlaveRectSpecs qx (fromIntegral (length slaves))))

  FocusingMaster2 master1 master2 slaves ->
    (master2, applyRectSpec (master2RectSpec qx qy qmx qmy) rect) :
      (master1, applyRectSpec (master1RectSpec qx qy) rect) :
        zip
          slaves
          (map
            (`applyRectSpec` rect)
            (unfocusedSlaveRectSpecs qx (fromIntegral (length slaves))))

  FocusingSlave1 master1 (Stack slave slaveups slavedowns) ->
    xx : yy : zzs
   where
    xx = (slave, applyRectSpec (focusedSlaveRectSpec qx qs (nslaveups :/ nslaves)) rect)
    yy = (master1, applyRectSpec (master1RectSpec qx 1) rect)
    zzs =
      map
        (\(i, sl) -> (sl, applyRectSpec (unfocusedSlaveRectSpec qx (i :/ nslaves)) rect))
        (zip [0..] (reverse slaveups) ++ zip [nslaveups+1 ..] slavedowns)

    nslaveups :: Int32
    nslaveups =
      fromIntegral (length slaveups)

    nslaves =
      nslaveups + 1 + fromIntegral (length slavedowns)

  FocusingSlave2 master1 master2 (Stack slave slaveups slavedowns) ->
    xx : yy : zz : zzs
   where
    xx = (slave, applyRectSpec (focusedSlaveRectSpec qx qs (nslaveups :/ nslaves)) rect)
    yy = (master2, applyRectSpec (master2RectSpec qx qy 0 qmy) rect)
    zz = (master1, applyRectSpec (master1RectSpec qx qy) rect)
    zzs =
      map
        (\(i, sl) -> (sl, applyRectSpec (unfocusedSlaveRectSpec qx (i :/ nslaves)) rect))
        (zip [0..] (reverse slaveups) ++ zip [nslaveups+1 ..] slavedowns)

    nslaveups :: Int32
    nslaveups =
      fromIntegral (length slaveups)

    nslaves =
      nslaveups + 1 + fromIntegral (length slavedowns)

-- Small helper type used to declaratively define a rectangle in terms of its
-- bounding box (the whole screen). Initially, rectangles share the top left
-- corner with the bounding box. Some examples:
--
-- AlignRight (Rect 0.2 1)          Rect 0.5 0.5
--      +--------xx                  xxxxxx----+
--      |        xx                  xxxxxx    |
--      |        xx                  xxxxxx    |
--      |        xx                  |         |
--      +--------xx                  +---------+
data RectSpec
  = AlignRight RectSpec
  | AlignBottom RectSpec
  | TranslateY Rational RectSpec
  | Rect Rational Rational -- % width, % height. Assumed to each be <= 1

applyRectSpec :: RectSpec -> Rectangle -> Rectangle
applyRectSpec spec r@(Rectangle x y w h) =
  case spec of
    AlignRight spec' ->
      let
        Rectangle _ y' w' h' =
          applyRectSpec spec' r
      in
        Rectangle (x + fromIntegral (w - w')) y' w' h'

    AlignBottom spec' ->
      let
        Rectangle x' _ w' h' =
          applyRectSpec spec' r
      in
        Rectangle x' (y + fromIntegral (h - h')) w' h'

    TranslateY qy spec' ->
      let
        Rectangle x' y' w' h' =
          applyRectSpec spec' r
      in
        Rectangle x' (y' + round (fromIntegral h * qy)) w' h'

    Rect qw qh ->
      Rectangle x y (round (fromIntegral w * qw)) (round (fromIntegral h * qh))

master1RectSpec :: Rational -> Rational -> RectSpec
master1RectSpec qx qy =
  AlignRight (Rect qx qy)

master2RectSpec
  :: Rational
  -> Rational
  -> Natural
  -> Natural
  -> RectSpec
master2RectSpec qx qy qmx qmy =
  AlignRight (AlignBottom (Rect w h))
 where
  w = min 1 (qx + fromIntegral qmx * rr)
  h = 1 - max 0 (qy - fromIntegral qmy * rr)

data ℚ a
  = a :/ a

unfocusedSlaveRectSpec :: Rational -> ℚ Int32 -> RectSpec
unfocusedSlaveRectSpec qx (i :/ n) =
  TranslateY
    (fromIntegral i / fromIntegral n)
    (Rect (1-qx) (1 / fromIntegral n))

unfocusedSlaveRectSpecs :: Rational -> Int32 -> [RectSpec]
unfocusedSlaveRectSpecs qx n =
  map (\i -> unfocusedSlaveRectSpec qx (i :/ n)) [0..n]

focusedSlaveRectSpec :: Rational -> Natural -> ℚ Int32 -> RectSpec
focusedSlaveRectSpec qx qs (i :/ n) =
  TranslateY
    (fromIntegral i / fromIntegral n)
    (Rect w h)
 where
  w = min 1 (1 - qx + fromIntegral qs * rr)
  h = 1 / fromIntegral n

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
