{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Game.Waddle as Waddle

import qualified Data.Text as Text
import Text.Printf
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
(screenWidth, screenHeight) = (1200, 800)

data Input = Input {
  inputMoveForward :: Bool,
  inputMoveBackward :: Bool,
  inputStrafeLeft :: Bool,
  inputStrafeRight :: Bool,
  inputTurnLeft :: Bool,
  inputTurnRight :: Bool
  }

initialInput :: Input
initialInput = Input {
  inputMoveForward = False,
  inputMoveBackward = False,
  inputStrafeLeft = False,
  inputStrafeRight = False,
  inputTurnLeft = False,
  inputTurnRight = False
  }

data State = State {
  stateRenderer :: SDL.Renderer,
  stateX :: CInt,
  stateY :: CInt,
  stateLines :: Map (Id Line) Line,
  stateSides :: Map (Id Side) Side,
  playerPos  :: V2 CInt,
  playerAngle :: Double,
  stateScaleFactor :: CInt,
  stateInput :: Input
  }

initialState :: SDL.Renderer -> Map (Id Line) Line -> Map (Id Side) Side -> V2 CInt -> State
initialState renderer lineMap sideMap playerPos =
   State {
     stateRenderer = renderer,
     stateX = 10,
     stateY = 10,
     stateLines = lineMap,
     stateSides = sideMap,
     playerPos = playerPos,
     playerAngle = pi,
     stateScaleFactor = 5,
     stateInput = initialInput
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

  rInfo <- SDL.getRendererInfo renderer
  printf "Renderer: %s (%s)\n" (Text.unpack (SDL.rendererInfoName rInfo))
    (show $ SDL.rendererType (SDL.rendererInfoFlags rInfo))

  let updateFun state =
        return $ state{
          stateX = stateX state + 1,
          stateY = stateY state + 1,
          playerAngle = if inputTurnLeft (stateInput state)
                        then playerAngle state - 0.1
                        else
                          if inputTurnRight (stateInput state)
                          then playerAngle state + 0.1
                          else
                            playerAngle state,
          playerPos = playerPos state +
                      sum [if inputMoveForward (stateInput state)
                           then V2 (round $ cos (playerAngle state) * 10) (round $ sin (playerAngle state) * 10)
                           else zero,
                           if inputMoveBackward (stateInput state)
                           then V2 (round $ cos (playerAngle state + pi) * 10) (round $ sin (playerAngle state + pi) * 10)
                           else zero,
                           if inputStrafeLeft (stateInput state)
                           then V2 (round $ cos (playerAngle state + pi/2) * 10) (round $ sin (playerAngle state + pi/2) * 10)
                           else zero,
                           if inputStrafeRight (stateInput state)
                           then V2 (round $ cos (playerAngle state - pi/2) * 10) (round $ sin (playerAngle state - pi/2) * 10)
                           else zero
                           ]
          }

  let (lineMap, sideMap, playerPos) = convertLevel wadFile "E1M1"

  putStrLn $ show (Map.size lineMap) ++ " lines, " ++ show (Map.size sideMap) ++ " sides"
  print playerPos
  let state = initialState renderer lineMap sideMap (maybe (V2 0 0) id playerPos)
  gameLoop (1/30) processInput updateFun renderScene state

  putStrLn "DOWN operating system shutting down..."
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
  putStrLn "ENDOWN."

convertLevel :: Wad -> ByteString -> (Map (Id Line) Line, Map (Id Side) Side, Maybe (V2 CInt))
convertLevel wad lumpName =
  let Just (Level{..}) = Map.lookup (mk lumpName) (wadLevels wad)
      lineList = zipWith (\ idx LineDef{..} ->
                       (Id idx,
                        mkLine
                        (((toV2 $ levelVertices !! (fromIntegral lineDefStartVertex))))
                        (((toV2 $ levelVertices !! (fromIntegral lineDefEndVertex))))
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
      playerPos = foldr (\ Thing{..} acc ->
                          case thingType of
                            Player1StartPos ->  Just (V2 (fromIntegral thingX) (negate (fromIntegral thingY)))
                            _ -> acc) Nothing levelThings
  in (lmap, Map.fromList smap, playerPos)
 where
   toV2  (Vertex {..}) =
     V2 (fromIntegral vertexX) (negate (fromIntegral vertexY))

vDiv :: V2 CInt -> CInt -> V2 CInt
vDiv (V2 x y) c = V2 (x `div` c) (y `div` c)

renderScene :: State -> IO ()
renderScene State{..} = do
  let renderer = stateRenderer

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  let offset = negate (playerPos `vDiv` stateScaleFactor) + V2 (screenWidth `div` 2) (screenHeight `div` 2)

  -- Draw grid.
  SDL.rendererDrawColor renderer $= V4 128 128 128 10

  forM_ [-10..10] $ \ col ->
    SDL.drawLine renderer (P (V2 (col * 128) (-2000) + offset)) (P (V2 (col * 128) 2000 + offset))

  forM_ [-10..10] $ \ row ->
    SDL.drawLine renderer (P (V2 (-2000) (row * 128) + offset)) (P (V2 2000 (row * 128) + offset))

  -- Render map.
  SDL.rendererDrawColor renderer $= V4 minBound minBound minBound maxBound

  forM_ (Map.elems stateLines) $ \ Line{..} -> do
    SDL.drawLine renderer (P (lineP1 `vDiv` stateScaleFactor + offset)) (P (lineP2 `vDiv` stateScaleFactor + offset))

  -- Render player.
  SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 (-5) (-5) + offset + playerPos `vDiv` stateScaleFactor)) (V2 10 10)))
  let V2 dx dy = angle playerAngle
      idx = round (dx * 30)
      idy = round (dy * 30)
  SDL.drawLine renderer (P (offset + playerPos `vDiv` stateScaleFactor))
    (P (V2 idx idy + offset + playerPos `vDiv` stateScaleFactor))

  SDL.present renderer

processInput :: [SDL.Event] -> State -> IO (State, Bool)
processInput events state = do
  return $ foldl processEvent (state, False) events
 where
   processEvent (stateIn, quit) (SDL.Event _ SDL.QuitEvent) =
     (state, True)
   processEvent (stateIn, quit) (SDL.Event _ (SDL.KeyboardEvent ke)) =
     case SDL.keyboardEventKeyMotion ke of
       SDL.Pressed ->
         case SDL.keysymKeycode (SDL.keyboardEventKeysym ke) of
           SDL.KeycodeA ->
             (stateIn{stateInput = (stateInput stateIn){inputStrafeLeft = True}}, quit)
           SDL.KeycodeD ->
             (stateIn{stateInput = (stateInput stateIn){inputStrafeRight = True}}, quit)
           SDL.KeycodeW ->
             (stateIn{stateInput = (stateInput stateIn){inputMoveForward = True}}, quit)
           SDL.KeycodeUp ->
             (stateIn{stateInput = (stateInput stateIn){inputMoveForward = True}}, quit)
           SDL.KeycodeS ->
             (stateIn{stateInput = (stateInput stateIn){inputMoveBackward = True}}, quit)
           SDL.KeycodeDown ->
             (stateIn{stateInput = (stateInput stateIn){inputMoveBackward = True}}, quit)
           SDL.KeycodeLeft ->
             (stateIn{stateInput = (stateInput stateIn){inputTurnLeft = True}}, quit)
           SDL.KeycodeRight ->
             (stateIn{stateInput = (stateInput stateIn){inputTurnRight = True}}, quit)
           _ ->
             (stateIn, quit)
       SDL.Released ->
         case SDL.keysymKeycode (SDL.keyboardEventKeysym ke) of
           SDL.KeycodeEscape ->
             (stateIn, True)
           SDL.KeycodeA ->
             (stateIn{stateInput = (stateInput stateIn){inputStrafeLeft = False}}, quit)
           SDL.KeycodeD ->
             (stateIn{stateInput = (stateInput stateIn){inputStrafeRight = False}}, quit)
           SDL.KeycodeW ->
             (stateIn{stateInput = (stateInput stateIn){inputMoveForward = False}}, quit)
           SDL.KeycodeUp ->
             (stateIn{stateInput = (stateInput stateIn){inputMoveForward = False}}, quit)
           SDL.KeycodeS ->
             (stateIn{stateInput = (stateInput stateIn){inputMoveBackward = False}}, quit)
           SDL.KeycodeDown ->
             (stateIn{stateInput = (stateInput stateIn){inputMoveBackward = False}}, quit)
           SDL.KeycodeLeft ->
             (stateIn{stateInput = (stateInput stateIn){inputTurnLeft = False}}, quit)
           SDL.KeycodeRight ->
             (stateIn{stateInput = (stateInput stateIn){inputTurnRight = False}}, quit)
           _ ->
             (stateIn, quit)
   processEvent (stateIn, quit) _ =
     (stateIn, quit)

-- | This is a generic game loop.  It is based on the adaptive game
-- loop in Robert Nystrom, "Game Programming Patterns", see
-- http://gameprogrammingpatterns.com/game-loop.html for details.
--
gameLoop :: Double -> ([SDL.Event] -> state -> IO (state, Bool)) -> (state -> IO state) -> (state -> IO ()) -> state -> IO ()
gameLoop ms_per_update inputFun updateFun renderFun startState = do
  now <- getCurrentTime
  loop now (0 :: Int) (0 :: Int) now (0.0 :: Double) (0 :: Int) startState
 where
   loop !lastReportTime !lastReportFrames !lastReportTicks !previousTime !lag !gameTicksIn stateIn1 = do

     now <- getCurrentTime
     let elapsed = now `diffUTCTime` previousTime
         previousTime' = now
         lag' = lag + fromRational (toRational elapsed)

     events <- SDL.pollEvents
     (stateIn, quit) <- inputFun events stateIn1

     let updateLoop theLag gameTicks state
           | theLag >= ms_per_update = do
             state' <- updateFun state
             updateLoop (theLag - ms_per_update) (gameTicks + 1) state'
           | otherwise = return (theLag, gameTicks, state)

     (lagOut, gameTicksOut, stateOut) <- updateLoop lag' gameTicksIn stateIn

     renderFun stateOut

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
