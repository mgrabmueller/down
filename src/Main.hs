{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Game.Waddle hiding (Sector)
import qualified Game.Waddle as Waddle

import Down.Geometry

import Data.Word
import Foreign
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
(screenWidth, screenHeight) = (800, 600)

renderScale :: CInt
renderScale = 4

renderWidth, renderHeight :: CInt
(renderWidth, renderHeight) = (screenWidth `div` renderScale, screenHeight `div` renderScale)

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

data RenderMode
  = RenderNone
  | RenderPrimitives
  | RenderTexture
 deriving (Eq, Ord)

data State = State {
  stateRenderMode :: RenderMode,
  stateRenderer :: SDL.Renderer,
  stateBuffer :: SDL.Texture,
  stateAutomap :: Bool,
  stateX :: CInt,
  stateY :: CInt,
  stateLines :: Map (Id Line) Line,
  stateSides :: Map (Id Side) Side,
  stateSectors :: Map (Id Sector) Sector,
  playerSector :: Id Sector,
  playerPos  :: V2 Double,
  playerAngle :: Double,
  playerAccel :: V2 Double,
  playerVelocity :: V2 Double,
  playerMaxSpeed :: Double,
  playerMaxAccel :: Double,
  playerTurnAngle :: Double,
  playerMoveAccel :: Double,
  playerHeight :: Double,
  playerEyeLevel :: Double,
  playerFov :: Double,
  playerViewRange :: Double,
  stateScaleFactor :: Double,
  stateInput :: Input
  }

initialState :: SDL.Renderer -> SDL.Texture -> Map (Id Line) Line -> Map (Id Side) Side ->
                Map (Id Sector) Sector -> V2 CInt -> Double -> Id Sector -> State
initialState renderer texture lineMap sideMap sectorMap (V2 playerPosX playerPosY) playerAngle playerSector =
   State {
     stateRenderMode = RenderTexture,
     stateRenderer = renderer,
     stateBuffer = texture,
     stateAutomap = False,
     stateX = 10,
     stateY = 10,
     stateLines = lineMap,
     stateSides = sideMap,
     stateSectors = sectorMap,
     playerSector = playerSector,
     playerPos = V2 (fromIntegral playerPosX) (fromIntegral playerPosY),
     playerAngle = playerAngle,
     playerAccel = V2 0 0,
     playerVelocity = V2 0 0,
     playerMaxSpeed = 20,
     playerMaxAccel = 1.5,
     playerTurnAngle = 0.015,
     playerMoveAccel = 0.5,
     playerHeight = maybe 0 secFloorHeight $ Map.lookup playerSector sectorMap,
     playerEyeLevel = 46,
     playerFov = 80 / 180 * pi,
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

  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming
             (V2 renderWidth renderHeight)

  let (lineMap, sideMap, sectorMap, playerPos, playerAngle, playerSector) = convertLevel wadFile "E1M1"

  putStrLn $ show (Map.size lineMap) ++ " lines, " ++ show (Map.size sideMap) ++ " sides"
--  print playerPos
--  print (playerSector, maybe Nothing (flip Map.lookup sectorMap) playerSector)
  -- forM_ (Map.toList sectorMap) $ \ (i, Sector{..}) -> print (fromId i, secBbox)
  let state = initialState renderer texture lineMap sideMap sectorMap (maybe (V2 0 0) id playerPos) playerAngle (maybe (Id 0) id playerSector)
  gameLoop (1/60) processInput updateState prerenderScene renderScene state

  putStrLn "DOWN operating system shutting down..."
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
  putStrLn "ENDOWN."

convertLevel :: Wad -> ByteString -> (Map (Id Line) Line, Map (Id Side) Side, Map (Id Sector) Sector, Maybe (V2 CInt), Double, Maybe (Id Sector))
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
      sList = concatMap (\ (idx, Line{..}) ->
                          let rSide = levelSideDefs !! fromId lineRight in
                          (lineRight, mkSide lmap idx (Id $ fromIntegral $ sideDefSector rSide) True) :
                          (case lineLeft of
                              Nothing -> []
                              Just l -> 
                                let lSide = levelSideDefs !! fromId l in
                                [(l, mkSide lmap idx (Id $ fromIntegral $ sideDefSector lSide) False)]))
                lineList
      smap = Map.fromList sList
      secList = zipWith (\ idx Waddle.Sector{..} ->
                          let sides = filter (\ (_, Side{..}) -> fromId sideSector == idx) sList
                          in
                          (Id idx,
                           Sector {
                             secFloorHeight = fromIntegral sectorFloorHeight,
                             secCeilingHeight = fromIntegral sectorCeilingHeight,
                             secSides = sides,
                             secBbox = mkSectorBbox (map snd sides) emptyBbox
                             }))
                [0..]
                levelSectors
      secmap = Map.fromList secList
      (playerPos, playerAngle) = foldr (\ Thing{..} acc ->
                                         case thingType of
                                           Player1StartPos ->
                                             (Just (V2 (fromIntegral thingX) (negate (fromIntegral thingY))),
                                              (fromIntegral thingAngle / 180 * pi + pi))
                                           _ -> acc) (Nothing, 0) levelThings
  in (lmap, smap, secmap, playerPos, playerAngle, maybe Nothing (findSector secmap . fmap fromIntegral) playerPos)
 where
   toV2  (Vertex {..}) =
     V2 (fromIntegral vertexX) (negate (fromIntegral vertexY))
   mkSectorBbox [] acc = acc
   mkSectorBbox (Side{..} : rest) acc =
     mkSectorBbox rest (bboxExtend (bboxExtend acc sideP1) sideP2)

updateState ::  Monad m => State -> m State
updateState state@State{..} = do
  let accel = playerMoveAccel
      moving = inputMoveForward (stateInput) ||
               inputMoveBackward (stateInput) ||
               inputStrafeLeft (stateInput) ||
               inputStrafeRight (stateInput)
      newSec = maybe playerSector id (findSector stateSectors $ playerPos + playerVelocity)
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
    playerSector = newSec,
    playerHeight = maybe playerHeight secFloorHeight (Map.lookup newSec stateSectors),
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
renderFirstPerson State{..} | stateRenderMode == RenderNone = do
  return ()

renderFirstPerson State{..} | stateRenderMode == RenderPrimitives = do
  let renderer = stateRenderer

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  let angleInc = playerFov / fromIntegral screenWidth
      horizon = screenHeight `div` 2
  SDL.rendererDrawColor renderer $= V4 minBound minBound minBound maxBound

  forM_ [0.. renderWidth - 1] $ \ col -> do
    let ang = playerAngle + (fromIntegral $ (col * renderScale) - renderWidth) * angleInc
    let V2 dx dy = angle ang

        rayP1 = playerPos
        rayP2 = playerPos + V2 dx dy ^* playerViewRange

    let iss = catMaybes $ map (\ Line{..} ->
                               case lineLeft of
                                 Nothing ->
                                   let p3 = lineP1
                                       p4 = lineP2
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
      ((dist, Intersection _) : _) -> do
        let disp = round (8 * playerViewRange / dist) * renderScale
            shade = round ((dist / playerViewRange) * 255)
        SDL.rendererDrawColor renderer $= V4 shade shade shade maxBound
        SDL.fillRect renderer $
          Just (SDL.Rectangle (P (V2 (col * renderScale) (horizon - disp)))
                (V2 renderScale (disp * 2)))
    return()
  return ()

renderFirstPerson State{..} | stateRenderMode == RenderTexture = do
  let renderer = stateRenderer
  SDL.copy renderer stateBuffer Nothing Nothing
  return ()

renderFirstPerson State{..} = do
  let renderer = stateRenderer

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  SDL.clear renderer

  return ()

vertLine :: Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> Word32 -> IO ()
vertLine ptr pitch x top' bot' color = do
  let top = max 0 (min (renderHeight - 1) top')
      bot = max 0 (min (renderHeight - 1) bot')
      loop !row _ | row > bot = return ()
      loop !row !aptr = do
        let pixPtr = castPtr aptr
        poke pixPtr color
        loop (row + 1) (aptr `plusPtr` (fromIntegral pitch))
  loop top (ptr `plusPtr` (fromIntegral (top * pitch + x * 4)))

prerenderScene :: State -> IO State
prerenderScene state@State{..} | stateRenderMode == RenderTexture && not stateAutomap = do
  (dataPtr, pitch) <- SDL.lockTexture stateBuffer Nothing
  let data8Ptr = castPtr dataPtr
      white = 0xffffffff

  let angleInc = playerFov / fromIntegral renderWidth
      Just currentSector = Map.lookup playerSector stateSectors

  forM_ [0 .. renderWidth - 1] $ \ col ->
    vertLine data8Ptr pitch col 0 (renderHeight - 1) white

  let loop col _ | col > renderWidth - 1 = return ()
      loop !col !ang = do
        let currAng = ang + playerAngle
            V2 !dx !dy = angle currAng
            !rayEnd = playerPos + V2 dx dy ^* playerViewRange

        renderColumn state data8Ptr pitch currentSector playerPos playerPos rayEnd currAng dx dy col 0 (fromIntegral $ renderHeight - 1) 0
        loop (col + 1) (ang + angleInc)
  loop 0 (negate (playerFov/2))

  SDL.unlockTexture stateBuffer
  return state

prerenderScene state@State{..} =
  return state

renderColumn :: State -> Ptr Word8 -> CInt -> Sector -> V2 Double -> V2 Double -> V2 Double -> Double -> Double -> Double -> CInt -> Double -> Double -> Int -> IO ()
renderColumn state@State{..} data8Ptr pitch currentSector startPos rayStart rayEnd ang dx dy col windowTop windowBot depth | depth < 100 = do
  let horizon = (fromIntegral renderHeight :: Double) / 2

  let handleSide result (sideId, side@Side{..}) =
        if dot sideNormal (V2 dx dy) < 0
        then
          case segmentIntersect rayStart rayEnd sideP1 sideP2 of
            Nothing -> result
            Just (is@(Intersection p)) ->
              (distance startPos p * cos (ang - playerAngle), is, sideId, side) : result
        else
          result
  let intersectons = foldl' handleSide [] (secSides currentSector)
      sortedIntersections = sortBy (\ (pdist, _, _, _) (qdist, _, _, _) ->
                                     pdist `compare` qdist)
                            intersectons
  case sortedIntersections of
    [] -> return ()
    ((dist, Intersection intersection, sideId, side) : _) -> do
      let dispFactor =  0.1 * playerViewRange / dist
          eyeLevel = playerHeight + playerEyeLevel
          wallTop = horizon + (dispFactor * (eyeLevel - secCeilingHeight currentSector)) 
          wallBot = horizon + (dispFactor * (eyeLevel - secFloorHeight currentSector))
          shade = round (235 - (dist / playerViewRange) * 235) .&. 255 :: Word32
          colTop = max wallTop windowTop
          colBot = min wallBot windowBot
      case otherSectorOfSide state sideId of
        Nothing ->
          vertLine data8Ptr pitch col (round colTop) (round colBot)
            ((shade `shiftL` 24) .|. (shade `shiftL` 16) .|. (shade `shiftL` 8) .|. 255)
        Just otherSector -> do
          let otherWallTop = horizon + (dispFactor * (eyeLevel - secCeilingHeight otherSector))
              otherWallBot = horizon + (dispFactor * (eyeLevel - secFloorHeight otherSector))
              midWallTop = max wallTop otherWallTop
              midWallBot = min wallBot otherWallBot
              midColTop = max midWallTop windowTop
              midColBot = min midWallBot windowBot
          if otherWallTop < wallTop
            then
            vertLine data8Ptr pitch col (round colTop) (round midColTop)
              ((255 `shiftL` 24) .|. (shade `shiftL` 16) .|. (shade `shiftL` 8) .|. 255)
--              ((shade `shiftL` 24) .|. (shade `shiftL` 16) .|. (shade `shiftL` 8) .|. 255)
            else
            return()
          if otherWallBot > wallBot
            then
            vertLine data8Ptr pitch col (round midColBot) (round colBot)
              ((shade `shiftL` 24) .|. (shade `shiftL` 16) .|. (255 `shiftL` 8) .|. 255)
--              ((shade `shiftL` 24) .|. (shade `shiftL` 16) .|. (shade `shiftL` 8) .|. 255)
            else
            return()
          if midColTop < midColBot
            then
            renderColumn state data8Ptr pitch otherSector startPos intersection rayEnd ang dx dy col midColTop midColBot (depth + 1)
            else
            return ()
renderColumn _ _ _ _ _ _ _ _ _ _ _ _ _ depth = do
  putStrLn $ "depth overflow: " ++ show depth
          

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
    SDL.drawLine renderer (P (fmap round (lineP1 ^/ stateScaleFactor) + offset))
      (P (fmap round (lineP2 ^/ stateScaleFactor) + offset))
    SDL.drawLine renderer (P (fmap round ((lineP1 + (lineDelta ^/ 2)) ^/ stateScaleFactor) + offset))
      (P (fmap round ((lineP1 + (lineDelta ^/ 2) + lineNormal ^* 10) ^/ stateScaleFactor) + offset))

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
           SDL.KeycodeR ->
             (stateIn{stateRenderMode = case stateRenderMode stateIn of
                         RenderNone -> RenderPrimitives
                         RenderPrimitives -> RenderTexture
                         _ -> RenderNone}, quit)
           SDL.KeycodePlus ->
             (stateIn{stateScaleFactor = min (stateScaleFactor stateIn * 1.2) 10}, quit)
           SDL.KeycodeMinus ->
             (stateIn{stateScaleFactor = max (stateScaleFactor stateIn / 1.2) 0.1}, quit)
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
gameLoop :: Double -> ([SDL.Event] -> state -> IO (state, Bool)) -> (state -> IO state) ->
            (state -> IO state) -> (state -> IO ()) -> state -> IO ()
gameLoop ms_per_update inputFun updateFun prerenderFun renderFun startState = do
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
           | otherwise = do
             state' <- prerenderFun state
             return (theLag, gameTicks, state')

     (lagOut, gameTicksOut, stateOut) <- updateLoop lag' gameTicksIn stateIn

     renderFun stateOut

     let diff = now `diffUTCTime` lastReportTime
     unless quit $ do
          if diff < 3
            then
               loop lastReportTime (lastReportFrames + 1) (lastReportTicks + (gameTicksOut - gameTicksIn))
                  previousTime' lagOut gameTicksOut stateOut
            else do
              putStrLn $ show (fromIntegral lastReportFrames / diff) ++ " FPS"
              putStrLn $ show (fromIntegral lastReportTicks / diff) ++ " TPS"
              loop now 0 0 previousTime' lagOut gameTicksOut stateOut

data Id a = Id {fromId :: Int}
  deriving (Show, Eq, Ord)

data Sector = Sector {
  secFloorHeight :: Double,
  secCeilingHeight :: Double,
  secSides :: [(Id Side, Side)],
  secBbox :: Bbox
  }
  deriving (Show)

data Side = Side {
  sideP1 :: V2 Double,
  sideP2 :: V2 Double,
  sideDelta  :: V2 Double,
  sideNormal :: V2 Double,
  sideLine   :: Id Line,
  sideSector :: Id Sector
  }
  deriving (Show)

data Line = Line {
  lineP1 :: V2 Double,
  lineP2 :: V2 Double,
  lineDelta  :: V2 Double,
  lineNormal :: V2 Double,
  lineRight  :: Id Side,
  lineLeft   :: Maybe (Id Side)
  }
  deriving (Show)

mkLine :: V2 Double -> V2 Double -> Id Side -> Maybe (Id Side) -> Line
mkLine p1 p2 right mbLeft =
  let delta = p2 - p1
  in
   Line {
     lineP1 = p1,
     lineP2 = p2,
     lineDelta = delta,
     lineNormal = signorm (perp delta),
     lineRight = right,
     lineLeft = mbLeft
     }

mkSide :: Map (Id Line) Line -> Id Line -> Id Sector -> Bool -> Side
mkSide lineMap lineId sectorId onRight =
  let Just (Line{..}) = Map.lookup lineId lineMap
      p1 = if onRight then lineP1 else lineP2
      p2 = if onRight then lineP2 else lineP1
      delta = p2 - p1
  in
   Side {
     sideP1 = p1,
     sideP2 = p2,
     sideDelta = delta,
     sideNormal = signorm (perp delta),
     sideLine = lineId,
     sideSector = sectorId
     }

data Bbox = Bbox {
  bboxLeft :: Double,
  bboxTop  :: Double,
  bboxRight :: Double,
  bboxBottom :: Double
 }
 deriving (Show)

-- | An empty bounding box.
--
emptyBbox :: Bbox
emptyBbox = Bbox {
  bboxLeft = 1e12,
  bboxTop = 1e12,
  bboxRight = -1e12,
  bboxBottom = -1e12
  }

-- | Return 'True' if the given bounding box is empty, and 'False'
-- otherwise.
--
bboxIsEmpty :: Bbox -> Bool
bboxIsEmpty (Bbox left top right bottom) =
  left >= right || top >= bottom

-- | Extend the given bounding box so that the result contains both
-- the original bounding box and the given point.
--
bboxExtend :: Bbox -> V2 Double -> Bbox
bboxExtend (Bbox left top right bottom) (V2 x y) =
  Bbox {
    bboxLeft = min left x,
    bboxTop = min top y,
    bboxRight = max right x,
    bboxBottom = max bottom y
  }

-- | Return 'True' if the given bounding box contains the given point.
--
bboxContains :: Bbox -> V2 Double -> Bool
bboxContains (Bbox left top right bottom) (V2 x y) =
  x >= left && x <= right && y >= top && y <= bottom

-- | Return the area enclosed in the given bounding box.  Returns 0
-- for any bounding box @bbox@ when 'bboxIsEmpty' @bbox@ = 'True'.
--
bboxArea :: Bbox -> Double
bboxArea bbox@(Bbox left top right bottom) =
  if bboxIsEmpty bbox
  then 0
  else (right - left) * (bottom - top)

-- | Find the smallest sector that contains the given point (if there
-- is any).
--
findSector :: Map (Id Sector) Sector -> V2 Double -> Maybe (Id Sector)
findSector secMap p =
  let (_, res) = Map.foldrWithKey' (\ k Sector{..} acc@(area, _) ->
                                     if bboxContains secBbox p
                                     then
                                       let a = bboxArea secBbox
                                       in if a > area
                                          then (a, Just k)
                                          else acc
                                     else acc) (0, Nothing) secMap
  in res

               
-- | Return a list of all sides of a sector (in no particular order).
--
sidesOfSector :: State -> Id Sector -> [(Id Side, Side)]
sidesOfSector State{..} secId =
  maybe (error "sidesOfVector: impossible") secSides $ Map.lookup secId stateSectors

-- | Given a side ID, return the sector on the other side (if there is
-- any).
--
otherSectorOfSide :: State -> Id Side -> Maybe Sector
otherSectorOfSide State{..} sideId =
  let thisSide = maybe (error "otherSector: impossible (1)") id $ Map.lookup sideId stateSides
      Line{..} = maybe (error "otherSector: impossible (2)") id $ Map.lookup (sideLine thisSide) stateLines
      otherSide = if lineRight == sideId
                  then
                    case lineLeft of
                      Nothing -> Nothing
                      Just l -> Map.lookup l stateSides
                  else Map.lookup lineRight stateSides
  in case otherSide of
    Just otherSide' -> Map.lookup (sideSector otherSide') stateSectors
    _ -> Nothing

