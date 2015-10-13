{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Game.Waddle as Waddle

import Data.ByteString(ByteString)
import Data.CaseInsensitive(mk)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad
import SDL (($=))
import qualified SDL
import Foreign.C.Types
import Linear
import Linear.Affine
import Data.Time.Clock
import System.Environment
import System.Exit
import System.IO

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640*2, 480*2)

data State = State {
  stateX :: CInt,
  stateY :: CInt,
  stateLines :: Map (Id Line) Line,
  stateSides :: Map (Id Side) Side,
  playerPos  :: V2 CInt,
  playerAngle :: Double
  }

initialState :: Map (Id Line) Line -> Map (Id Side) Side -> State
initialState lineMap sideMap =
   State {
     stateX = 10,
     stateY = 10,
     stateLines = lineMap,
     stateSides = sideMap,
     playerPos = V2 0 0,
     playerAngle = pi
     }

main :: IO ()
main = do
  putStrLn "DOWN operating system starting..."
  args <- getArgs
  wadFile <- case args of
    [wadFile] -> do
      putStrLn $ "Loading WAD " ++ wadFile ++ "..."
      Waddle.load wadFile
    _ -> do
      hPutStrLn stderr "usage: down WADFILE"
      exitFailure

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
          stateY = stateY state + 1,
          playerAngle = playerAngle state + 0.02
          }

  let (lineMap, sideMap, minx, maxx, miny, maxy) = convertLevel wadFile "E1M1"

  putStrLn $ show (Map.size lineMap) ++ " lines, " ++ show (Map.size sideMap) ++ " sides, minx: " ++ show minx ++ ", maxx: " ++ show maxx ++ ", miny: " ++ show miny ++ ", maxy: " ++ show maxy
  mapM_ print (Map.toList lineMap)
  let state = initialState lineMap sideMap
  gameLoop renderer (1/30) updateFun renderScene state

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

convertLevel :: Wad -> ByteString -> (Map (Id Line) Line, Map (Id Side) Side, CInt, CInt, CInt, CInt)
convertLevel wad lumpName =
  let Just (Level{..}) = Map.lookup (mk lumpName) (wadLevels wad)
      (minx, maxx, miny, maxy) = foldr (\ Vertex{..} (minx', maxx', miny', maxy') ->
                                         (min minx' (fromIntegral vertexX),
                                          max maxx' (fromIntegral vertexX),
                                          min miny' (fromIntegral vertexY),
                                          max maxy' (fromIntegral vertexY)))
                                 (60000 :: CInt, -60000 :: CInt, 60000 :: CInt, -60000 :: CInt)
                                 levelVertices
      lineList = zipWith (\ idx LineDef{..} ->
                       (Id idx,
                        mkLine
                        (((toV2 minx maxx miny maxy $ levelVertices !! (fromIntegral lineDefStartVertex))))
                        (((toV2 minx maxx miny maxy $ levelVertices !! (fromIntegral lineDefEndVertex))))
                        (Id (fromIntegral lineDefRightSideDef))
                        (fmap (Id . fromIntegral) lineDefLeftSideDef)))
             [0..]
             levelLineDefs
      lmap = Map.fromList lineList
      smap = concatMap (\ (idx, Line{..}) -> 
               (lineRight, mkSide lmap idx True) :
               (case lineLeft of
                   Nothing -> []
                   Just l -> [(l, mkSide lmap idx False)]))
               lineList
  in (lmap, Map.fromList smap, minx, maxx, miny, maxy)
 where
   toV2 minx maxx miny maxy (Vertex {..}) =
     V2 ((fromIntegral vertexX + (minx `div` 2)) `div` 4) (((negate (fromIntegral vertexY - (miny `div` 2)))) `div` 4)

renderScene :: SDL.Renderer -> State -> IO ()
renderScene renderer State{..} = do
  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  SDL.rendererDrawColor renderer $= V4 minBound minBound minBound maxBound

  let offset = V2 (screenWidth `div` 2) (screenHeight `div` 2)

  SDL.drawLine renderer (P (V2 (-1000) 0 + offset)) (P (V2 1000 0 + offset))
  SDL.drawLine renderer (P (V2 0 (-1000) + offset)) (P (V2 0 1000 + offset))

  forM_ (Map.elems stateLines) $ \ Line{..} -> do
    SDL.drawLine renderer (P (lineP1 + offset)) (P (lineP2 + offset))

  SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 (-5) (-5) + playerPos + offset)) (V2 10 10)))
  let V2 dx dy = angle playerAngle
      idx = round (dx * 30)
      idy = round (dy * 30)
  SDL.drawLine renderer (P (playerPos + offset)) (P (V2 idx idy + playerPos + offset))

  SDL.present renderer

-- | This is a generic game loop.  It is based on the adaptive game
-- loop in Robert Nystrom, "Game Programming Patterns", see
-- http://gameprogrammingpatterns.com/game-loop.html for details.
--
gameLoop :: SDL.Renderer -> Double -> (state -> IO state) -> (SDL.Renderer -> state -> IO ()) -> state -> IO ()
gameLoop renderer ms_per_update updateFun renderFun startState = do
  now <- getCurrentTime
  loop now (0 :: Int) (0 :: Int) now (0.0 :: Double) (0 :: Int) startState
 where
--   loop :: UTCTime -> Int -> Int -> UTCTime -> Double -> Int -> State -> IO ()
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
  let delta = p2 - p1
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
      delta = p2 - p1
  in
   Side {
     sideP1 = p1,
     sideP2 = p2,
     sideDelta = delta,
--     sideNormal = vNormalize (perp delta),
     sideLine = lineId
     }

-- lineMap :: Map (Id Line) Line
-- lineMap = Map.fromList(
--   [(Id 0, mkLine (V2 (-100) 100) (V2 100 130) (Id 0) Nothing),
--    (Id 1, mkLine (V2 100 130) (V2 100 (-100)) (Id 1) Nothing),
--    (Id 2, mkLine (V2 100 (-100)) (V2 (-100) (-100)) (Id 2) Nothing),
--    (Id 3, mkLine (V2 (-100) (-100)) (V2 (-100) 100) (Id 3) Nothing)])

-- sideMap :: Map (Id Line) Line -> Map (Id Side) Side
-- sideMap lineMap = Map.fromList(
--   [(Id 0, mkSide lineMap (Id 0)  True),
--    (Id 1, mkSide lineMap (Id 1)  True),
--    (Id 2, mkSide lineMap (Id 2)  True),
--    (Id 3, mkSide lineMap (Id 3)  True)])
