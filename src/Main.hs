{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Down.Geometry

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import SDL (($=))
import qualified SDL
import Foreign.C.Types
import Linear
import Linear.Affine
import Data.Time.Clock

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

data State = State {
  stateX :: CInt,
  stateY :: CInt,
  stateLines :: Map (Id Line) Line,
  stateSides :: Map (Id Side) Side
  }

initialState :: State
initialState =
  let s = sideMap lineMap
  in
   State {
     stateX = 10,
     stateY = 10,
     stateLines = lineMap,
     stateSides = s
     }

main = do
  putStrLn "DOWN operating system starting..."
  let a = V2 0 3
      b = V2 4 5.2
      d = 3.8
  putStrLn $ "a = " ++ vShow a ++ ", b = " ++ vShow b ++ "; a + b = " ++ vShow (a + b)
  putStrLn $ "a = " ++ vShow a ++ ", b = " ++ vShow b ++ "; a - b = " ++ vShow (a - b)
  putStrLn $ "a = " ++ vShow a ++ ", d = " ++ show d ++ "; d * a = " ++ vShow (d *^a)

  let p1 = V2 0 0
      p2 = V2 1 1
      ps = [V2 0 0, V2 1 1, V2 0 1, V2 1 0]
  forM_ ps $ \ p ->
    putStrLn $ vShow p ++ " to line " ++ vShow p1 ++ "--" ++ vShow p2 ++ ": " ++ show (sideOfLine p p1 p2)

  putStrLn $ "a = " ++ vShow a ++ ", ^a = " ++ vShow (vNormalize a) ++ ", |^a| = " ++ show (vLength $ vNormalize a)
  putStrLn $ "b = " ++ vShow a ++ ", ^b = " ++ vShow (vNormalize b) ++ ", |^b| = " ++ show (vLength $ vNormalize b)

  let lines = [(V2 (-1) 1, V2 1 1),
               (V2 1 1, V2 1 (-1)),
               (V2 1 (-1), V2 (-1) (-1)),
               (V2 (-1) (-1), V2 (-1) 1)]

  forM_ lines $ \ (p1, p2) ->
    let p = V2 0 0 in
    putStrLn $ vShow p ++ " to line " ++ vShow p1 ++ "--" ++ vShow p2 ++ ": " ++ show (sideOfLine p p1 p2)

  forM_ lines $ \ (p1, p2) ->
    let l0 = V2 (-4) (-2)
        l1 = V2 8 5 in
    putStrLn $ "intersection between " ++ vShow l0 ++ "--" ++ vShow l1 ++ " and " ++ vShow p1 ++ "--" ++ vShow p2 ++ ": " ++
      (show $ segmentIntersect l0 l1 p1 p2)

  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "DOWN 0.1.0.0"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      (SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedRenderer
         , SDL.rendererTargetTexture = False
         })

  let updateFun state =
        return $ state{
          stateX = stateX state + 1,
          stateY = stateY state + 1
          }

  let state = initialState
  gameLoop renderer updateFun renderScene state

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

renderScene :: SDL.Renderer -> State -> IO ()
renderScene renderer state@State{..} = do
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  SDL.rendererDrawColor renderer $= V4 minBound minBound minBound maxBound
  SDL.drawLine renderer (P (V2 0 0)) (P (V2 stateX stateY))

  forM_ (Map.elems stateLines) $ \ Line{..} -> do
    SDL.drawLine renderer (P (V2 (screenWidth`div`2) (screenHeight`div`2) + lineP1))
      (P (V2 (screenWidth`div`2) (screenHeight`div`2) + lineP2))
  SDL.present renderer

gameLoop :: SDL.Renderer -> (State -> IO State) -> (SDL.Renderer -> State -> IO ()) -> State -> IO ()
gameLoop renderer updateFun renderFun state = do
  now <- getCurrentTime
  loop now 0 0 now 0.0 0 state
 where
   ms_per_update :: Double
   ms_per_update = 1/30

   loop :: UTCTime -> Int -> Int -> UTCTime -> Double -> Int -> State -> IO ()
   loop !lastReportTime !lastReportFrames !lastReportTicks !previousTime !lag !gameTicksIn stateIn = do

     now <- getCurrentTime
     let elapsed = now `diffUTCTime` previousTime
         previousTime' = now
         lag' = lag + fromRational (toRational elapsed)

     events <- SDL.pollEvents
     let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events


     let updateLoop theLag gameTicks state
           | theLag >= ms_per_update = do
             state' <- updateFun state
             updateLoop (theLag - ms_per_update) (gameTicks + 1) state'
           | otherwise = return (theLag, gameTicks, state)

     (lagOut, gameTicksOut, stateOut) <- updateLoop lag' gameTicksIn stateIn

     renderFun renderer stateOut

     let diff = now `diffUTCTime` lastReportTime
     unless quit $ do
          if diff < 5
            then
               loop lastReportTime (lastReportFrames + 1) (lastReportTicks + (gameTicksOut - gameTicksIn))
                  previousTime' lagOut gameTicksOut stateOut
            else do
              putStrLn $ show (fromIntegral lastReportFrames / diff) ++ " FPS"
              putStrLn $ show (fromIntegral lastReportTicks / diff) ++ " TPS"
              loop now 0 0 previousTime' lagOut gameTicksOut stateOut

data Id a = Id Int
  deriving (Show, Eq, Ord)

data Side = Side {
  sideP1 :: V2 CInt,
  sideP2 :: V2 CInt,
  sideDelta  :: V2 CInt,
--  sideNormal :: V2 CInt,
  sideLine   :: Id Line
  }
  deriving (Show)

data Line = Line {
  lineP1 :: V2 CInt,
  lineP2 :: V2 CInt,
  lineDelta  :: V2 CInt,
--  lineNormal :: V2 CInt,
  lineRight  :: Id Side,
  lineLeft   :: Maybe (Id Side)
  }
  deriving (Show)

mkLine :: V2 CInt -> V2 CInt -> Id Side -> Maybe (Id Side) -> Line
mkLine p1 p2 right mbLeft =
  let delta@(V2 dx dy) = p2 - p1
  in
   Line {
     lineP1 = p1,
     lineP2 = p2,
     lineDelta = delta,
--     lineNormal = vNormalize (perp delta),
     lineRight = right,
     lineLeft = mbLeft
     }

mkSide :: Map (Id Line) Line -> Id Line -> Bool -> Side
mkSide lineMap lineId onRight =
  let Just (Line{..}) = Map.lookup lineId lineMap
      p1 = if onRight then lineP1 else lineP2
      p2 = if onRight then lineP2 else lineP1
      delta@(V2 dx dy) = p2 - p1
  in
   Side {
     sideP1 = p1,
     sideP2 = p2,
     sideDelta = delta,
--     sideNormal = vNormalize (perp delta),
     sideLine = lineId
     }

lineMap :: Map (Id Line) Line
lineMap = Map.fromList(
  [(Id 0, mkLine (V2 (-100) 100) (V2 100 130) (Id 0) Nothing),
   (Id 1, mkLine (V2 100 130) (V2 100 (-100)) (Id 1) Nothing),
   (Id 2, mkLine (V2 100 (-100)) (V2 (-100) (-100)) (Id 2) Nothing),
   (Id 3, mkLine (V2 (-100) (-100)) (V2 (-100) 100) (Id 3) Nothing)])

sideMap :: Map (Id Line) Line -> Map (Id Side) Side
sideMap lineMap = Map.fromList(
  [(Id 0, mkSide lineMap (Id 0)  True),
   (Id 1, mkSide lineMap (Id 1)  True),
   (Id 2, mkSide lineMap (Id 2)  True),
   (Id 3, mkSide lineMap (Id 3)  True)])
