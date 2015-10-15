{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Game.Waddle as Waddle

import Down.Geometry

import Data.List
import Data.Maybe
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
(screenWidth, screenHeight) = (600, 400)

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
  stateAutomap :: Bool,
  stateX :: CInt,
  stateY :: CInt,
  stateLines :: Map (Id Line) Line,
  stateSides :: Map (Id Side) Side,
  playerPos  :: V2 Double,
  playerAngle :: Double,
  playerAccel :: V2 Double,
  playerVelocity :: V2 Double,
  playerMaxSpeed :: Double,
  playerMaxAccel :: Double,
  playerTurnAngle :: Double,
  playerMoveAccel :: Double,
  playerFov :: Double,
  playerViewRange :: Double,
  stateScaleFactor :: Double,
  stateInput :: Input
  }

initialState :: SDL.Renderer -> Map (Id Line) Line -> Map (Id Side) Side -> V2 CInt -> State
initialState renderer lineMap sideMap (V2 playerPosX playerPosY) =
   State {
     stateRenderer = renderer,
     stateAutomap = False,
     stateX = 10,
     stateY = 10,
     stateLines = lineMap,
     stateSides = sideMap,
     playerPos = V2 (fromIntegral playerPosX) (fromIntegral playerPosY),
     playerAngle = pi,
     playerAccel = V2 0 0,
     playerVelocity = V2 0 0,
     playerMaxSpeed = 20,
     playerMaxAccel = 2,
     playerTurnAngle = 0.05,
     playerMoveAccel = 1,
     playerFov = 60 / 180 * pi,
     playerViewRange = 2000,
     stateScaleFactor = 7,
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

  let (lineMap, sideMap, playerPos) = convertLevel wadFile "E1M1"

  putStrLn $ show (Map.size lineMap) ++ " lines, " ++ show (Map.size sideMap) ++ " sides"
  print playerPos
  let state = initialState renderer lineMap sideMap (maybe (V2 0 0) id playerPos)
  gameLoop (1/60) processInput updateState renderScene state

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

updateState ::  Monad m => State -> m State
updateState state@State{..} = do
  let accel = playerMoveAccel
      moving = inputMoveForward (stateInput) ||
               inputMoveBackward (stateInput) ||
               inputStrafeLeft (stateInput) ||
               inputStrafeRight (stateInput)
  return $ state{
    stateX = stateX + 1,
    stateY = stateY + 1,
    playerAngle = if inputTurnLeft (stateInput)
                  then playerAngle - playerTurnAngle
                  else
                    if inputTurnRight (stateInput)
                    then playerAngle + playerTurnAngle
                    else
                      playerAngle,
    playerPos = playerPos + playerVelocity,
    playerVelocity = if moving
                     then
                       let newVelocity = playerVelocity + playerAccel
                           newSpeed = sqrt (quadrance newVelocity)
                       in
                        if newSpeed <= playerMaxSpeed
                        then newVelocity
                        else newVelocity ^* (playerMaxSpeed / newSpeed)
                     else if quadrance playerVelocity < 0.5
                          then zero
                          else playerVelocity ^* 0.8,
    playerAccel = if moving
                  then
                    let newAccel = playerAccel +
                                   sum [if inputMoveForward stateInput
                                        then V2 (cos playerAngle * accel) (sin playerAngle * accel)
                                        else zero,
                                        if inputMoveBackward stateInput
                                        then V2 (cos (playerAngle + pi) * accel) (sin (playerAngle + pi) * accel)
                                        else zero,
                                        if inputStrafeLeft stateInput
                                        then V2 (cos (playerAngle - pi/2) * accel) (sin (playerAngle - pi/2) * accel)
                                        else zero,
                                        if inputStrafeRight stateInput
                                        then V2 (cos (playerAngle + pi/2) * accel) (sin (playerAngle + pi/2) * accel)
                                        else zero
                                       ]
                        newAcc = sqrt (quadrance newAccel)
                    in
                     if newAcc <= playerMaxAccel
                     then newAccel
                     else newAccel ^* (playerMaxAccel / newAcc)
                  else
                    zero
    }

renderScene :: State -> IO ()
renderScene state@State{..} = do
  if stateAutomap
    then renderAutomap state
    else renderFirstPerson state
  let renderer = stateRenderer
  SDL.present renderer

renderFirstPerson :: State -> IO ()
renderFirstPerson State{..} = do
  let renderer = stateRenderer

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  let angleInc = playerFov / fromIntegral screenWidth
      horizon = screenHeight `div` 2
      threeDscale = 8
  SDL.rendererDrawColor renderer $= V4 minBound minBound minBound maxBound

  forM_ [0.. (screenWidth `div` threeDscale) - 1] $ \ col -> do
    let ang = playerAngle + (fromIntegral $ (col * threeDscale) - screenWidth `div` 2) * angleInc
    let V2 dx dy = angle ang
        idx = dx * playerViewRange
        idy = dy * playerViewRange

        rayP1 = playerPos
        rayP2 = playerPos + V2 dx dy ^* playerViewRange

    let iss = catMaybes $ map (\ Line{..} ->
                               case lineLeft of
                                 Nothing ->
                                   let p3 = fmap fromIntegral lineP1
                                       p4 = fmap fromIntegral lineP2
                                       mbIs = segmentIntersect rayP1 rayP2 p3 p4
                                   in
                                    case mbIs of
                                      Nothing -> Nothing
                                      Just (is@(Intersection p)) ->
                                        Just (distance playerPos p * cos (ang - playerAngle), is)
                                 Just _ ->
                                   Nothing
                              )
           (Map.elems stateLines)
        iss' = sortBy (\ (pdist, _) (qdist, _) ->
                        pdist `compare` qdist) iss
    case iss' of
      [] -> return ()
      ((dist, Intersection p) : _) -> do
        let disp = round (15 * playerViewRange / dist)
            shade = round ((dist / playerViewRange) * 255)
        SDL.rendererDrawColor renderer $= V4 shade shade shade maxBound
        SDL.fillRect renderer $
          Just (SDL.Rectangle (P (V2 (col * threeDscale) (horizon - disp)))
                (V2 threeDscale (disp * 2)))
    return()
  return ()

renderAutomap :: State -> IO ()
renderAutomap State{..} = do
  let renderer = stateRenderer

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  let offset = fmap round (negate (playerPos ^/ stateScaleFactor)) + V2 (screenWidth `div` 2) (screenHeight `div` 2)

  -- Draw grid.
  SDL.rendererDrawColor renderer $= V4 128 128 128 10

  forM_ [-10..10] $ \ col ->
    SDL.drawLine renderer (P (V2 (col * 128) (-2000) + offset)) (P (V2 (col * 128) 2000 + offset))

  forM_ [-10..10] $ \ row ->
    SDL.drawLine renderer (P (V2 (-2000) (row * 128) + offset)) (P (V2 2000 (row * 128) + offset))

  -- Render map.
  SDL.rendererDrawColor renderer $= V4 minBound minBound minBound maxBound

  forM_ (Map.elems stateLines) $ \ Line{..} -> do
    SDL.drawLine renderer (P (fmap round (fmap fromIntegral lineP1 ^/ stateScaleFactor) + offset))
      (P (fmap round (fmap fromIntegral lineP2 ^/ stateScaleFactor) + offset))

  -- Render player.
  SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 (-5) (-5) + offset + (fmap round (playerPos ^/ stateScaleFactor)))) (V2 10 10)))
  let V2 dx1 dy1 = angle (playerAngle - playerFov / 2)
      idx1 = (dx1 * playerViewRange)
      idy1 = (dy1 * playerViewRange)
  let V2 dx2 dy2 = angle (playerAngle + playerFov / 2)
      idx2 = (dx2 * playerViewRange)
      idy2 = (dy2 * playerViewRange)
  SDL.drawLine renderer (P (offset + fmap round (playerPos ^/ stateScaleFactor)))
    (P (offset + fmap round ((V2 idx1 idy1 + playerPos) ^/ stateScaleFactor)))
  SDL.drawLine renderer (P (offset + fmap round (playerPos ^/stateScaleFactor)))
    (P (offset + fmap round ((V2 idx2 idy2 + playerPos) ^/ stateScaleFactor)))


processInput :: [SDL.Event] -> State -> IO (State, Bool)
processInput events state = do
  return $ foldl processEvent (state, False) events
 where
   processEvent (stateIn, _) (SDL.Event _ SDL.QuitEvent) =
     (stateIn, True)
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
           SDL.KeycodeTab ->
             (stateIn{stateAutomap = not (stateAutomap stateIn)}, quit)
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
