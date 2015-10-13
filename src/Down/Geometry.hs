{-# LANGUAGE BangPatterns #-}
module Down.Geometry
       (vDotProd,
        vLength,
        vDist,
        vLengthSqared,
        vDistSquared,
        vNormalize,
        vShow,
        Intersection(..),
        segmentIntersect,
        SideOfLine(..),
        sideOfLine
        ) where

import Linear
import Linear.Affine

vDotProd :: Num a => V2 a -> V2 a -> a
vDotProd (V2 x0 y0) (V2 x1 y1) = x0 * x1 + y0 * y1

vLength :: Floating a => V2 a -> a
vLength v = sqrt (vLengthSqared v)

vLengthSqared :: Num a => V2 a -> a
vLengthSqared (V2 x y) = (x * x + y * y)

vDist :: Floating a => V2 a -> V2 a -> a
vDist v1 v2 = sqrt (vDistSquared v1 v2)

vDistSquared :: Num a => V2 a -> V2 a -> a
vDistSquared (V2 x0 y0) (V2 x1 y1) = (dx * dx + dy * dy)
 where
   !dx = x1 - x0
   !dy = y1 - y0

vNormalize :: Floating a => V2 a -> V2 a
vNormalize v = (1 / vLength v) *^ v

vShow :: Show a => V2 a -> String
vShow (V2 x y) = "(" ++ show x ++ "," ++ show y ++ ")"

data Intersection a
  = None
  | Intersection (V2 a)
 deriving (Eq, Ord, Show)

segmentIntersect :: (Eq a, Ord a, Fractional a) => V2 a -> V2 a -> V2 a -> V2 a -> Intersection a
segmentIntersect p0@(V2 x0 y0) p1@(V2 x1 y1) p2@(V2 x2 y2) p3@(V2 x3 y3) =
  let a1 = y1 - y0
      b1 = x0 - x1
      c1 = a1 * x0 + b1 * y0
      a2 = y3 - y2
      b2 = x2 - x3
      c2 = a2 * x2 + b2 * y2
      denominator = a1 * b2 - a2 * b1
  in
   if denominator == 0
   then None
   else
     let intersectX = (b2 * c1 - b1 * c2) / denominator
         intersectY = (a1 * c2 - a2 * c1) / denominator
         rx0 = (intersectX - x0) / (x1 - x0)
         ry0 = (intersectY - y0) / (y1 - y0)
         rx1 = (intersectX - x2) / (x3 - x2)
         ry1 = (intersectY - y2) / (y3 - y2)
     in
      if ((rx0 >= 0 && rx0 <= 1) || (ry0 >= 0 && ry0 <= 1)) &&
         ((rx1 >= 0 && rx1 <= 1) || (ry1 >= 0 && ry1 <= 1))
      then
        Intersection (V2 intersectX intersectY)
      else
        None

data SideOfLine
  = LeftOfLine
  | RightOfLine
  | OnTopOfLine
 deriving (Eq, Ord, Show)

sideOfLine :: (Ord a, Num a) => V2 a -> V2 a -> V2 a -> SideOfLine
sideOfLine p@(V2 px py) l0@(V2 l0x l0y) l1@(V2 l1x l1y) =
  let s = (l1x - l0x)*(py - l0y) - (l1y - l0y)*(px - l0x)
  in if s < 0
     then RightOfLine
     else if s > 0
          then LeftOfLine
          else OnTopOfLine
