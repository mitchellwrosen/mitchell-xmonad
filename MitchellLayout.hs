{-# LANGUAGE FlexibleInstances, InstanceSigs, LambdaCase, MultiParamTypeClasses,
             ScopedTypeVariables, TemplateHaskell #-}

module MitchellLayout
  ( MitchellLayout
  , mitchellLayout
  , NumMasters(..)
  , Selecting(..)
  , Zoom(..)
  ) where

import Bloop
import Utils

import Control.Lens    hiding (Zoom)
import Control.Lens.TH (makeLenses)
import Data.Foldable
import Data.Int
import Graphics.X11
import Numeric.Natural
import Text.Printf     (printf)
import XMonad          (Typeable, X)

import qualified XMonad          as X
import qualified XMonad.StackSet as X (Stack(Stack))

-- Mitchell's layout!
--
-- * Master pane occupies right 70%, even with no other windows
-- * All slaves evenly split the left 30%
-- * Two-master mode puts a second, smaller master beneath the first
-- * Slaves and second master can be zoomed when selected
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

data OnReset
  = OnReset
      !Rational
      !Rational
      !Natural
      !Zoom
  deriving (Read, Show)

mitchellLayout
  :: Rational
  -> Rational
  -> Natural
  -> Zoom
  -> MitchellLayout a
mitchellLayout qx qy qs qm =
  MitchellLayout
    { _layNumMasters = OneMaster
    , _laySelecting = SelectingMaster1
    , _layMasterX = qx
    , _layMasterY = qy
    , _laySlaveZ = qs
    , _layMaster2Z = qm
    , layOnReset = OnReset qx qy qs qm
    }

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

selecting :: NumMasters -> X.Stack a -> Selecting
selecting _          (X.Stack _ []       _) = SelectingMaster1
selecting OneMaster  (X.Stack _ (_:xs)   _) = SelectingSlave (length xs)
selecting TwoMasters (X.Stack _ [_]      _) = SelectingMaster2
selecting TwoMasters (X.Stack _ (_:_:xs) _) = SelectingSlave (length xs)

-- X multiplier, Y multiplier
data Zoom
  = Zoom
  { _zoomX :: !Natural
  , _zoomY :: !Natural
  } deriving (Read, Show)

data MitchellLayout a
  = MitchellLayout
  { _layNumMasters :: !NumMasters -- One or two masters?
  , _laySelecting  :: !Selecting  -- Selecting a master or slave?
  , _layMasterX    :: !Rational   -- X% the master pane(s) occupy
  , _layMasterY    :: !Rational   -- Y% the main master pane occupies when there are two masters
  , _laySlaveZ     :: !Natural    -- Slave magnify multiplier
  , _layMaster2Z   :: !Zoom       -- Second master zoom
  , layOnReset     :: !OnReset    -- Blob to use on reset
  } deriving (Read, Show)

makeLenses ''MitchellLayout
makeLenses ''Zoom

instance Typeable a => X.LayoutClass MitchellLayout a where
  doLayout
    :: MitchellLayout a
    -> Rectangle
    -> X.Stack a
    -> X ([(a, Rectangle)], Maybe (MitchellLayout a))
  doLayout lay@(MitchellLayout nm _ qx qy qs qm _) rect stack = do
    pure
      ( over (mapped . _2) (`applyRectSpec` rect)
          (theLayout qx qy qs qm (stackLayout nm stack))
      , Just (lay & laySelecting .~ selecting nm stack)
      )

  pureMessage lay@(MitchellLayout _ ss qx qy qs qm _) msg =
    asum
      [ (\case
            X.Expand -> decreaseMasterSlaveRatio lay
            X.Shrink -> increaseMasterSlaveRatio lay)
        <$> X.fromMessage msg

      , case X.fromMessage msg of
          Just (Bloop 'j') ->
            Just (increaseMastersRatio lay)

          Just (Bloop 'k') ->
            Just (decreaseMastersRatio lay)

          Just (Bloop 'H') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (increaseMasterZoomX lay)
              SelectingSlave _ -> Just (decreaseSlaveZoom lay)

          Just (Bloop 'J') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (decreaseMasterZoomY lay)
              SelectingSlave _ -> Nothing

          Just (Bloop 'K') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (increaseMasterZoomY lay)
              SelectingSlave _ -> Nothing

          Just (Bloop 'L') ->
            case ss of
              SelectingMaster1 -> Nothing
              SelectingMaster2 -> Just (decreaseMasterZoomX lay)
              SelectingSlave _ -> Just (increaseSlaveZoom lay)

          Just (Bloop '0') ->
            Just (resetZooms lay)

          Just (Bloop 'm') ->
            Just (lay & layNumMasters %~ flipNumMasters)

          _ ->
            Nothing

      , X.fromMessage msg >>= \case
          X.ReleaseResources ->
            Just (mitchellLayout qx qy qs qm)

          X.Hide ->
            Nothing
      ]

  description (MitchellLayout OneMaster _ qx _ _ _ _) =
    printf "Mitchell %d" (round (qx * 100) :: Int)
  description (MitchellLayout TwoMasters _ qx qy _ _ _) =
    printf "Mitchell %d %d" (round (qx * 100) :: Int) (round (qy * 100) :: Int)

decreaseMastersRatio :: MitchellLayout a -> MitchellLayout a
decreaseMastersRatio =
  over layMasterY (\qy -> max 0 (qy - rr))

increaseMastersRatio :: MitchellLayout a -> MitchellLayout a
increaseMastersRatio =
  over layMasterY (\qy -> min 1 (qy + rr))

increaseMasterZoomX :: MitchellLayout a -> MitchellLayout a
increaseMasterZoomX =
  over (layMaster2Z . zoomX) (+1)

decreaseMasterZoomX :: MitchellLayout a -> MitchellLayout a
decreaseMasterZoomX =
  over (layMaster2Z . zoomX) predNat

increaseMasterZoomY :: MitchellLayout a -> MitchellLayout a
increaseMasterZoomY =
  over (layMaster2Z . zoomY) (+1)

decreaseMasterZoomY :: MitchellLayout a -> MitchellLayout a
decreaseMasterZoomY =
  over (layMaster2Z . zoomY) predNat

decreaseMasterSlaveRatio :: MitchellLayout a -> MitchellLayout a
decreaseMasterSlaveRatio =
  over layMasterX (\qx -> max 0 (qx - rr))

increaseMasterSlaveRatio :: MitchellLayout a -> MitchellLayout a
increaseMasterSlaveRatio =
  over layMasterX (\qx -> min 1 (qx + rr))

increaseSlaveZoom :: MitchellLayout a -> MitchellLayout a
increaseSlaveZoom =
  over laySlaveZ (+1)

decreaseSlaveZoom :: MitchellLayout a -> MitchellLayout a
decreaseSlaveZoom =
  over laySlaveZ predNat

resetZooms :: MitchellLayout a -> MitchellLayout a
resetZooms lay =
  lay
    & laySlaveZ .~ 0
    & layMaster2Z .~ Zoom 0 0

-- The overall shape of the layout
data TheLayout a
  -- Focused on master 1, with no master 2 and 0+ slaves
  = FocusingMaster1of1 a [a]
                    -- ^ master 1
                    --   ^ slaves

  -- Focused on master 1, with a master 2 and 0+ slaves
  | FocusingMaster1of2 a a [a]
                    -- ^ master 1
                    --   ^ master 2
                    --     ^ slaves

  -- Focused on master 2, with 0+ slaves
  | FocusingMaster2 a a [a]
                 -- ^ master 1
                 --   ^ master 2
                 --     ^ slaves

  -- Focused on a slave, with one master
  | FocusingSlaveWith1Master a (X.Stack a)
                          -- ^ master
                          --   ^ slaves (with focus)

  -- Focused on a slave, with two masters
  | FocusingSlaveWith2Masters a a (X.Stack a)
                           -- ^ master 1
                          --   ^ master 2
                          --     ^ slaves (with focus)

theLayout
  :: Rational
  -> Rational
  -> Natural
  -> Zoom
  -> TheLayout a
  -> [(a, RectSpec)]
theLayout qx qy qs (Zoom qmx qmy) = \case
  FocusingMaster1of1 master1 slaves ->
    (master1, master1Rect qx 1) :
    unfocusedSlaveRects' slaves

  FocusingMaster1of2 master1 master2 slaves ->
    (master1, master1Rect qx qy) :
    (master2, master2Rect qx qy (Zoom qmx 0)) :
    unfocusedSlaveRects' slaves

  FocusingMaster2 master1 master2 slaves ->
    (master2, master2Rect qx qy (Zoom qmx qmy)) :
    (master1, master1Rect qx qy) :
    unfocusedSlaveRects' slaves

  FocusingSlaveWith1Master master1 (X.Stack slave slaveups slavedowns) ->
    xx : yy : zzs
   where
    xx = (slave, focusedSlaveRect qx qs (nslaveups :/ nslaves))
    yy = (master1, master1Rect qx 1)
    zzs =
      map
        (\(i, sl) -> (sl, unfocusedSlaveRect qx (i :/ nslaves)))
        (zip [0..] (reverse slaveups) ++ zip [nslaveups+1 ..] slavedowns)

    nslaveups :: Int32
    nslaveups =
      fromIntegral (length slaveups)

    nslaves =
      nslaveups + 1 + fromIntegral (length slavedowns)

  FocusingSlaveWith2Masters master1 master2 (X.Stack slave slaveups slavedowns) ->
    xx : yy : zz : zzs
   where
    xx = (slave, xxrect)
    xxrect = focusedSlaveRect qx qs (nslaveups :/ nslaves)
    -- yy = (master2, master2Rect qx qy (Zoom 0 0))
    yy =
      ( master2
      , IfOverlapsY yyrect1 xxrect yyrect2
      )
    yyrect1 = master2Rect qx qy (Zoom qmx qmy)
    yyrect2 = master2Rect qx qy (Zoom 0 qmy)
    zz = (master1, master1Rect qx qy)
    zzs =
      map
        (\(i, sl) -> (sl, unfocusedSlaveRect qx (i :/ nslaves)))
        (zip [0..] (reverse slaveups) ++ zip [nslaveups+1 ..] slavedowns)

    nslaveups :: Int32
    nslaveups =
      fromIntegral (length slaveups)

    nslaves =
      nslaveups + 1 + fromIntegral (length slavedowns)

 where
  unfocusedSlaveRects' slaves =
    zip
      slaves
      (unfocusedSlaveRects qx (fromIntegral (length slaves)))

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
  = AlignRight !RectSpec
  | AlignBottom !RectSpec
  | TranslateY !Rational !RectSpec
  | IfOverlapsY !RectSpec !RectSpec !RectSpec
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

    IfOverlapsY r1 r2 rt ->
      let
        r1' = applyRectSpec r1 r
        r2' = applyRectSpec r2 r
        rt' = applyRectSpec rt r
      in
        if overlapsY r1' r2'
          then rt'
          else r1'

    Rect qw qh ->
      Rectangle x y (round (fromIntegral w * qw)) (round (fromIntegral h * qh))

master1Rect :: Rational -> Rational -> RectSpec
master1Rect qx qy =
  AlignRight (Rect qx qy)

master2Rect
  :: Rational
  -> Rational
  -> Zoom
  -> RectSpec
master2Rect qx qy (Zoom qmx qmy) =
  AlignBottom (AlignRight (Rect w h))
 where
  w = min 1 (qx + fromIntegral qmx * rr)
  h = 1 - max 0 (qy - fromIntegral qmy * rr)

data ℚ a
  = a :/ a

unfocusedSlaveRect :: Rational -> ℚ Int32 -> RectSpec
unfocusedSlaveRect qx (i :/ n) =
  TranslateY
    (fromIntegral i / fromIntegral n)
    (Rect (1-qx) (1 / fromIntegral n))

unfocusedSlaveRects :: Rational -> Int32 -> [RectSpec]
unfocusedSlaveRects qx n =
  map (\i -> unfocusedSlaveRect qx (i :/ n)) [0..n]

focusedSlaveRect :: Rational -> Natural -> ℚ Int32 -> RectSpec
focusedSlaveRect qx qs (i :/ n) =
  TranslateY
    (fromIntegral i / fromIntegral n)
    (Rect w h)
 where
  w = min 1 (1 - qx + fromIntegral qs * rr)
  h = 1 / fromIntegral n

stackLayout :: NumMasters -> X.Stack a -> TheLayout a
stackLayout = \case
  OneMaster  -> stackLayoutOneMaster
  TwoMasters -> stackLayoutTwoMasters

stackLayoutOneMaster :: X.Stack a -> TheLayout a
stackLayoutOneMaster (X.Stack x ys zs) =
  case unsnoc1 ys of
    Unsnoc1_0       -> FocusingMaster1of1 x zs
    Unsnoc1_1 ys' y -> FocusingSlaveWith1Master y (X.Stack x ys' zs)

stackLayoutTwoMasters :: X.Stack a -> TheLayout a
stackLayoutTwoMasters (X.Stack x ys zs) =
  case unsnoc2 ys of
    Unsnoc2_0 ->
      case zs of
        []    -> FocusingMaster1of1 x []
        z:zs' -> FocusingMaster1of2 x z zs'

    Unsnoc2_1 y ->
      FocusingMaster2 y x zs

    Unsnoc2_2 ys' y z ->
      FocusingSlaveWith2Masters z y (X.Stack x ys' zs)

predNat :: Natural -> Natural
predNat 0 = 0
predNat n = n - 1
