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
import Text.Printf     (printf)
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
      !MasterMode -- Main master above or below second master?
      !Selecting  -- Selecting a master or slave?
      !Rational   -- X% the master pane(s) occupy
      !Rational   -- Y% the main master pane occupies when there are two masters
      !Natural    -- Slave magnify multiplier
      !Natural    -- Master magnify X multiplier
      !Natural    -- Master magnify Y multiplier
  deriving (Read, Show)

mitchellLayout :: Rational -> Rational -> Natural -> Natural -> Natural -> MitchellLayout a
mitchellLayout =
  MitchellLayout OneMaster MasterAbove SelectingMaster1

data NumMasters
  = OneMaster
  | TwoMasters
  deriving (Read, Show)

flipNumMasters :: NumMasters -> NumMasters
flipNumMasters OneMaster  = TwoMasters
flipNumMasters TwoMasters = OneMaster

-- Main master relative to second master?
data MasterMode
  = MasterAbove
  | MasterBelow
  deriving (Read, Show)

flipMasterMode :: MasterMode -> MasterMode
flipMasterMode MasterAbove = MasterBelow
flipMasterMode MasterBelow = MasterAbove

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
  doLayout (MitchellLayout nm mode _ qx qy qs qmx qmy) rect stack =
    pure
      ( theLayout rect qx qy qs qmx qmy (stackLayout nm mode stack)
      , Just (MitchellLayout nm mode (selecting nm stack) qx qy qs qmx qmy)
      )

  pureMessage ll@(MitchellLayout nm mode ss qx qy qs qmx qmy) msg =
    asum
      [ (\case
            XMonad.Expand -> decreaseMasterSlaveRatio ll
            XMonad.Shrink -> increaseMasterSlaveRatio ll)
        <$> XMonad.fromMessage msg

      , case XMonad.fromMessage msg of
          Just (Bloop 'j') ->
            case mode of
              MasterAbove -> Just (increaseMastersRatio ll)
              MasterBelow -> Just (decreaseMastersRatio ll)

          Just (Bloop 'k') ->
            case mode of
              MasterAbove -> Just (decreaseMastersRatio ll)
              MasterBelow -> Just (increaseMastersRatio ll)

          Just (Bloop 'H') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (increaseMasterZoomX ll)
              SelectingSlave _ -> Just (decreaseSlaveZoom ll)

          Just (Bloop 'J') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 ->
                case mode of
                  MasterAbove -> Just (decreaseMasterZoomY ll)
                  MasterBelow -> Just (increaseMasterZoomY ll)
              SelectingSlave _ -> Nothing

          Just (Bloop 'K') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 ->
                case mode of
                  MasterAbove -> Just (increaseMasterZoomY ll)
                  MasterBelow -> Just (decreaseMasterZoomY ll)
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
            Just (MitchellLayout (flipNumMasters nm) mode ss qx qy qs qmx qmy)

          Just (Bloop 'n') ->
            Just (MitchellLayout nm (flipMasterMode mode) ss qx qy qs qmx qmy)

          _ ->
            Nothing
      ]

  description (MitchellLayout OneMaster _ _ qx _ _ _ _) =
    printf "Mitchell %d" (round (qx * 100) :: Int)
  description (MitchellLayout TwoMasters _ _ qx qy _ _ _) =
    printf "Mitchell %d %d" (round (qx * 100) :: Int) (round (qy * 100) :: Int)

decreaseMastersRatio :: MitchellLayout a -> MitchellLayout a
decreaseMastersRatio (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss qx (max 0 (qy - rr)) qs qmx qmy

increaseMastersRatio :: MitchellLayout a -> MitchellLayout a
increaseMastersRatio (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss qx (min 1 (qy + rr)) qs qmx qmy

increaseMasterZoomX :: MitchellLayout a -> MitchellLayout a
increaseMasterZoomX (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss qx qy qs (qmx + 1) qmy

decreaseMasterZoomX :: MitchellLayout a -> MitchellLayout a
decreaseMasterZoomX (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss qx qy qs (predNat qmx) qmy

increaseMasterZoomY :: MitchellLayout a -> MitchellLayout a
increaseMasterZoomY (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss qx qy qs qmx (qmy + 1)

decreaseMasterZoomY :: MitchellLayout a -> MitchellLayout a
decreaseMasterZoomY (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss qx qy qs qmx (predNat qmy)

resetMasterZoom :: MitchellLayout a -> MitchellLayout a
resetMasterZoom (MitchellLayout nm mode ss qx qy qs _ _) =
  MitchellLayout nm mode ss qx qy qs 0 0

decreaseMasterSlaveRatio :: MitchellLayout a -> MitchellLayout a
decreaseMasterSlaveRatio (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss (max 0 (qx - rr)) qy qs qmx qmy

increaseMasterSlaveRatio :: MitchellLayout a -> MitchellLayout a
increaseMasterSlaveRatio (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss (min 1 (qx + rr)) qy qs qmx qmy

increaseSlaveZoom :: MitchellLayout a -> MitchellLayout a
increaseSlaveZoom (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss qx qy (qs + 1) qmx qmy

decreaseSlaveZoom :: MitchellLayout a -> MitchellLayout a
decreaseSlaveZoom (MitchellLayout nm mode ss qx qy qs qmx qmy) =
  MitchellLayout nm mode ss qx qy (predNat qs) qmx qmy

resetSlaveZoom :: MitchellLayout a -> MitchellLayout a
resetSlaveZoom (MitchellLayout nm mode ss qx qy _ qmx qmy) =
  MitchellLayout nm mode ss qx qy 0 qmx qmy

-- The overall shape of the layout
data TheLayout a
  -- Focused on master 1, with no master 2 and 0+ slaves
  = FocusingMaster1of1 a [a]
                    -- ^ master 1
                    --   ^ slaves

  -- Focused on master 1, with a master 2 and 0+ slaves
  | FocusingMaster1of2 MasterMode a a [a]
                               -- ^ master 1
                               --   ^ master 2
                               --     ^ slaves

  -- Focused on master 2, with 0+ slaves
  | FocusingMaster2 MasterMode a a [a]
                            -- ^ master 1
                            --   ^ master 2
                            --     ^ slaves

  -- Focused on a slave, with one master
  | FocusingSlave1 a (Stack a)
                -- ^ master
                --   ^ slaves (with focus)

  -- Focused on a slave, with two masters
  | FocusingSlave2 MasterMode a a (Stack a)
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
  FocusingMaster1of1 master1 slaves ->
    (master1, applyRectSpec (master1RectSpec qx 1 MasterAbove) rect) :
      unfocusedSlaveRects slaves

  FocusingMaster1of2 mode master1 master2 slaves ->
    (master1, applyRectSpec (master1RectSpec qx qy mode) rect) :
      (master2, applyRectSpec (master2RectSpec qx qy qmx 0 mode) rect) :
        unfocusedSlaveRects slaves

  FocusingMaster2 mode master1 master2 slaves ->
    (master2, applyRectSpec (master2RectSpec qx qy qmx qmy mode) rect) :
      (master1, applyRectSpec (master1RectSpec qx qy mode) rect) :
        unfocusedSlaveRects slaves

  FocusingSlave1 master1 (Stack slave slaveups slavedowns) ->
    xx : yy : zzs
   where
    xx = (slave, applyRectSpec (focusedSlaveRectSpec qx qs (nslaveups :/ nslaves)) rect)
    yy = (master1, applyRectSpec (master1RectSpec qx 1 MasterAbove) rect)
    zzs =
      map
        (\(i, sl) -> (sl, applyRectSpec (unfocusedSlaveRectSpec qx (i :/ nslaves)) rect))
        (zip [0..] (reverse slaveups) ++ zip [nslaveups+1 ..] slavedowns)

    nslaveups :: Int32
    nslaveups =
      fromIntegral (length slaveups)

    nslaves =
      nslaveups + 1 + fromIntegral (length slavedowns)

  FocusingSlave2 mode master1 master2 (Stack slave slaveups slavedowns) ->
    xx : yy : zz : zzs
   where
    xx = (slave, xxrect)
    xxrect = applyRectSpec (focusedSlaveRectSpec qx qs (nslaveups :/ nslaves)) rect
    yy =
      ( master2
      , if overlapsY xxrect yyrect1
          then yyrect2
          else yyrect1
      )
    yyrect1 = applyRectSpec (master2RectSpec qx qy qmx qmy mode) rect
    yyrect2 = applyRectSpec (master2RectSpec qx qy 0 qmy mode) rect
    zz = (master1, applyRectSpec (master1RectSpec qx qy mode) rect)
    zzs =
      map
        (\(i, sl) -> (sl, applyRectSpec (unfocusedSlaveRectSpec qx (i :/ nslaves)) rect))
        (zip [0..] (reverse slaveups) ++ zip [nslaveups+1 ..] slavedowns)

    nslaveups :: Int32
    nslaveups =
      fromIntegral (length slaveups)

    nslaves =
      nslaveups + 1 + fromIntegral (length slavedowns)

 where
  unfocusedSlaveRects slaves =
    zip
      slaves
      (map
        (`applyRectSpec` rect)
        (unfocusedSlaveRectSpecs qx (fromIntegral (length slaves))))

overlapsY :: Rectangle -> Rectangle -> Bool
overlapsY (Rectangle _ y1 _ h1) (Rectangle _ y2 _ h2)
  | y1 <= y2  = y1 + fromIntegral h1 >= y2
  | otherwise = y2 + fromIntegral h2 >= y1

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

master1RectSpec :: Rational -> Rational -> MasterMode -> RectSpec
master1RectSpec qx qy = \case
  MasterAbove -> rect
  MasterBelow -> AlignBottom rect
 where
  rect = AlignRight (Rect qx qy)

master2RectSpec
  :: Rational
  -> Rational
  -> Natural
  -> Natural
  -> MasterMode
  -> RectSpec
master2RectSpec qx qy qmx qmy = \case
  MasterAbove -> AlignBottom rect
  MasterBelow -> rect
 where
  rect = AlignRight (Rect w h)
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

stackLayout :: NumMasters -> MasterMode -> Stack a -> TheLayout a
stackLayout nmasters mode (Stack x ys zs) =
  case nmasters of
    OneMaster ->
      case unsnoc1 ys of
        Unsnoc1_0       -> FocusingMaster1of1 x zs
        Unsnoc1_1 ys' y -> FocusingSlave1 y (Stack x ys' zs)

    TwoMasters ->
      case unsnoc2 ys of
        Unsnoc2_0 ->
          case zs of
            []    -> FocusingMaster1of1 x []
            z:zs' -> FocusingMaster1of2 mode x z zs'

        Unsnoc2_1 y ->
          FocusingMaster2 mode y x zs

        Unsnoc2_2 ys' y z ->
          FocusingSlave2 mode z y (Stack x ys' zs)

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
