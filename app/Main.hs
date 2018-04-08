{-# LANGUAGE RecordWildCards #-}
import           Linear.V2 (V2(V2), perp)
import           Polynomial.Roots
import           Data.Complex
import           Text.Printf (printf)
import qualified Physics.Light as Light
import           Helm
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Cmd as Cmd
import qualified Helm.Mouse as Mouse
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import           Helm.Time (Time, second)
import qualified Helm.Engine.SDL as SDL

data Action
  = Idle
  | Animate Double 
  | Player1Left
  | Player1Right
  | Player1Jump
  | Player2Left
  | Player2Right
  | Player2Jump
  | Stop1Vel
  | Stop2Vel
  | BeginGame

-- | Represents the status of the player (i.e. where they're at).
data GameStatus
  = Playing  -- ^ The player is playing the game.
  | Paused   -- ^ The game is paused.
  | Waiting  -- ^ The player is waiting and needs to click to start the game.
  | Dead     -- ^ The player is dead and needs to hit space to get to the waiting state.
  deriving (Eq, Ord, Show)

data Model = Model
  { player1Pos    :: V2 Double
  , player1Vel    :: V2 Double
  , player2Pos    :: V2 Double
  , player2Vel    :: V2 Double
  , ballPos       :: V2 Double
  , ballVel       :: V2 Double
  , gameStatus    :: GameStatus
  , player1Score  :: Integer
  , player2Score  :: Integer
  , player1Chance :: Integer
  , player2Chance :: Integer
  , lastWin       :: Integer
  , lastCollide   :: Integer
  }

xof :: V2 Double -> Double
xof (V2 x y) = x

yof :: V2 Double -> Double
yof (V2 x y) = y

gravity :: V2 Double
gravity = V2 0 30

screenWidth :: Double
screenWidth = 1920

screenHeight :: Double
screenHeight = 900

groundHeight :: Double
groundHeight = 180

netWidth :: Double
netWidth = 30

netHeight :: Double
netHeight = screenHeight*0.6

playerRadius :: Double
playerRadius = 50

ballRadius :: Double
ballRadius = 30

baseline :: Double
baseline = screenHeight - playerRadius

dampX :: V2 Double -> V2 Double
dampX (V2 x y) = (V2 0 y)

dampY :: V2 Double -> V2 Double
dampY (V2 x y) = (V2 x 0)

getCollisionTime :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Double
getCollisionTime (V2 x' y') (V2 vx' vy') (V2 px py) (V2 pvx pvy) = minimum $ map realPart $ filter (\x -> (imagPart x == 0) && (realPart x >= 0)) $ roots 1.0e-6 300 $ map (\x -> x :+ 0)
    [ (x^2) + (y^2) - ((ballRadius + playerRadius)^2),
      (2*x*vx) + (2*y*vy),
      (vx^2) + (vy^2) + (y*g),
      vy*g,
      (g^2)/4]
    where x = x' - px
          y = y' - py
          vx = vx' - pvx
          vy = vy' - pvy
          g = if py < baseline then 0 else yof gravity


initial :: (Model, Cmd SDLEngine Action)
initial =
  ( Model
      { player1Pos    = V2 200 baseline
      , player1Vel    = V2 0 0
      , player2Pos    = V2 (screenWidth - 200) baseline
      , player2Vel    = V2 0 0
      , ballPos       = V2 300 100
      , ballVel       = V2 0 0
      , gameStatus    = Waiting
      , player1Score  = 0
      , player2Score  = 0
      , player1Chance = 3
      , player2Chance = 3
      , lastWin       = 0
      , lastCollide   = 0
      }
  , Cmd.none
  )

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model { .. } Idle = (model, Cmd.none)

update model@Model { .. } BeginGame =
  if gameStatus == Playing then (model, Cmd.none)
  else
    ( model
      { gameStatus = Playing
      , player1Pos    = V2 200 baseline
      , player1Vel    = V2 0 0
      , player2Pos    = V2 (screenWidth - 200) baseline
      , player2Vel    = V2 0 0
      , ballPos       = V2 (if lastWin /= 2 then 300 else screenWidth - 300) 100
      , ballVel       = V2 0 0
      }
    , Cmd.none
    )

update model@Model { .. } (Animate dt) =
  if gameStatus /= Playing then (model, Cmd.none)
  else
    ( model
      { ballPos       = ballFinalPos
      , ballVel       = ballFinalVel
      , player1Pos    = player1FinalPos
      , player1Vel    = player1FinalVel
      , player2Pos    = player2FinalPos
      , player2Vel    = player2FinalVel
      , player1Score  = score1
      , player2Score  = score2
      , player1Chance = chance1
      , player2Chance = chance2
      , gameStatus    = status
      , lastWin       = lw
      , lastCollide   = lc
      }
    , Cmd.none
    )
  where
    t = pure $ dt * 0.005
    magn v = sqrt $ sum $ v * v
    distance a b = magn $ a - b
    dot a b = sum $ a * b
    c2c a b = (a - b) * (pure (1 / (distance a b)))
    pc2c a b = perp $ c2c a b

    reflect ppos bpos bvel = ((pure (dot bvel y)) * y) - ((pure (dot bvel x)) * x)
      where x = c2c bpos ppos
            y = pc2c bpos ppos  

    playerUpdate pos vel leftLimit rightLimit
      | (yof (pos + (vel*t)) < baseline) && ((xof (pos + (vel*t)) >= leftLimit) && (xof (pos + (vel*t)) <= rightLimit)) =  (pos + (vel*t) + (gravity*0.5*t*t), vel + (gravity*t))
      | yof (pos + (vel*t)) < baseline = (pos + (dampX (vel * t)) + (gravity*0.5*t*t), dampX (vel + (gravity*t)))
      | (xof (pos + (vel*t)) >= leftLimit) && (xof (pos + (vel*t)) <= rightLimit) = ((\(V2 x y) -> (V2 x baseline)) (pos + (vel * t) + (gravity*0.5*t*t)), dampY (vel + (gravity*t)))
      | otherwise = ((\(V2 x y) -> (V2 x baseline)) pos, (V2 0 0))
    
    (player1FinalPos, player1FinalVel) = playerUpdate player1Pos player1Vel playerRadius (((screenWidth - netWidth)/2) - playerRadius)
    
    (player2FinalPos, player2FinalVel) = playerUpdate player2Pos player2Vel (((screenWidth + netWidth)/2) + playerRadius) (screenWidth - playerRadius)
    
    doesCollide ppos
      | (distance ballTempPos ppos) <= (ballRadius + playerRadius) = True
      | otherwise = False
      where
        (ballTempPos, ballTempVel) = (ballPos + (ballVel * t) + (0.5 * gravity * t * t), ballVel + (gravity * t))

    ballAfterCollision ppos pvel = let
        tcol = pure $ getCollisionTime ballPos ballVel ppos pvel
        (x, y) = (ballPos + (ballVel * tcol) + (0.5 * gravity * tcol * tcol), ballVel + (gravity * tcol))
        v = reflect ppos x y
        tf = t - tcol
        in (x + (v * tf) + (0.5 * gravity * tf * tf), v + (gravity * tf))

    (ballFinalPos, ballFinalVel)
      | doesCollide player1FinalPos = ballAfterCollision player1Pos player1Vel
      | doesCollide player2FinalPos = ballAfterCollision player2Pos player2Vel
      | (xof expBallPos <= ballRadius) || (xof expBallPos >= (screenWidth - ballRadius)) = (ballPos, reverseXVel ballVel)
      | ((xof expBallPos >= (screenWidth - netWidth)/2 - ballRadius) && (xof expBallPos <= (screenWidth + netWidth)/2 + ballRadius)) && (yof expBallPos >= screenHeight - netHeight) = (ballPos, reverseXVel ballVel) 
      | ((xof expBallPos >= (screenWidth - netWidth)/2 - ballRadius) && (xof expBallPos <= (screenWidth + netWidth)/2 + ballRadius)) && ((yof expBallPos <= screenHeight - netHeight) && (yof expBallPos >= screenHeight - netHeight - ballRadius)) = (ballPos, reflect expBallPos ballPos ballVel) 
      | yof expBallPos >= (screenHeight - ballRadius) = (ballPos, (V2 0 0))
      | otherwise = (expBallPos, expBallVel)
      where
        expBallPos = ballPos + (ballVel * t) + (0.5 * gravity * t * t)
        expBallVel = ballVel + (gravity * t)
        reverseXVel (V2 x y) = (V2 (-x) y)
    
    (score1, score2, chance1, chance2, status, lc, lw)
      | touchGround =
        if ballInLeft then (player1Score, player2Score + 1, 3, 3, Waiting, 0, 2)
        else (player1Score + 1, player2Score, 3, 3, Waiting, 0, 1)
      | chance1' < 0 = (player1Score, player2Score + 1, 3, 3, Waiting, 0, 2)
      | chance2' < 0 = (player1Score + 1, player2Score, 3, 3, Waiting, 0, 1)
      | otherwise = (player1Score, player2Score, chance1', chance2', Playing, lc', lastWin)
      where
        touchGround = yof (ballPos + (ballVel*t) + (0.5 * gravity * t * t)) >= (screenHeight - ballRadius)
        ballInLeft = (xof (ballPos+(ballVel * t)) > ballRadius) && (xof (ballPos+(ballVel * t)) < ((screenWidth - netWidth) / 2 - ballRadius))
        (chance1', chance2', lc')
          | doesCollide player1FinalPos = (player1Chance - 1, player2Chance, 1) 
          | doesCollide player2FinalPos = (player1Chance, player2Chance - 1, 2)
          | otherwise = (player1Chance, player2Chance, lastCollide)

update model@Model { .. } Player1Left
  | gameStatus == Playing =
    if False then (model, Cmd.none) else
      ( model
        { player1Vel = if (xof player1Vel) /= (-100) then player1Vel - (V2 100 0)
                       else player1Vel
        }
      , Cmd.none
      )
  | otherwise = (model, Cmd.none)

update model@Model { .. } Player2Left
  | gameStatus == Playing =
    if False then (model, Cmd.none) else
      ( model
        { player2Vel = if (xof player2Vel) /= (-100) then player2Vel - (V2 100 0)
                       else player2Vel
        }
      , Cmd.none
      )
  | otherwise = (model, Cmd.none)

update model@Model { .. } Player1Right
  | gameStatus == Playing =
    if False then (model, Cmd.none) else
      ( model
        { player1Vel = if (xof player1Vel) /= 100 then player1Vel + (V2 100 0)
                       else player1Vel
        }
      , Cmd.none
      )
  | otherwise = (model, Cmd.none)

update model@Model { .. } Player2Right
  | gameStatus == Playing =
    if False then (model, Cmd.none) else
      ( model
        { player2Vel = if (xof player2Vel) /= 100 then player2Vel + (V2 100 0)
                       else player2Vel
        }
      , Cmd.none
      )
  | otherwise = (model, Cmd.none)

update model@Model { .. } Player1Jump
  | gameStatus == Playing =
    if False then (model, Cmd.none) else
      ( model
        { player1Vel = if (yof player1Pos) < baseline then player1Vel else player1Vel - (V2 0 150)
        }
      , Cmd.none
      )
  | otherwise = (model, Cmd.none)

update model@Model { .. } Player2Jump
  | gameStatus == Playing =
    if False then (model, Cmd.none) else
      ( model
        { player2Vel = if (yof player2Pos) < baseline then player2Vel else player2Vel - (V2 0 150)
        }
      , Cmd.none
      )
  | otherwise = (model, Cmd.none)

update model@Model { .. } Stop1Vel
  | gameStatus == Playing =
    if False then (model, Cmd.none) else
      ( model
        { player1Vel = dampX player1Vel
        }
      , Cmd.none
      )
  | otherwise = (model, Cmd.none)

update model@Model { .. } Stop2Vel
  | gameStatus == Playing =
    if False then (model, Cmd.none) else
      ( model
        { player2Vel = dampX player2Vel
        }
      , Cmd.none
      )
  | otherwise = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Keyboard.downs $ \key -> (case key of
      Keyboard.AKey -> Player1Left
      Keyboard.DKey -> Player1Right
      Keyboard.WKey -> Player1Jump
      Keyboard.LeftKey -> Player2Left
      Keyboard.RightKey -> Player2Right
      Keyboard.UpKey -> Player2Jump
      Keyboard.SpaceKey -> BeginGame
      _ -> Idle)
  , Keyboard.ups $ \key -> (case key of
      Keyboard.AKey -> Stop1Vel
      Keyboard.DKey -> Stop1Vel
      Keyboard.LeftKey -> Stop2Vel
      Keyboard.RightKey -> Stop2Vel
      _ -> Idle)
  , Time.fps 600 Animate
  ]

playingOverlay :: Color -> Model -> Form SDLEngine
playingOverlay color Model { .. } =
  group $
    [
      move (V2 (screenWidth / 2) 25) $ text $ Text.height 30 $
                                         Text.color color $
                                         Text.toText score
    ] ++
    (if gameStatus == Waiting then [centerNotice "Press Spcebar to resume"] else [])

  where
    score = printf "%d:%d" player1Score player2Score
    centerNotice msg = move (V2 (screenWidth / 2) 60) $ text $
                       Text.height 16 $ Text.color color $ Text.toText msg

view :: Model -> Graphics SDLEngine
view  model@Model { .. } = 
  Graphics2D $ collage [backdrop, ground, player1, player2, ball, net, overlay model]
  where
    backdrop = move (V2 (screenWidth/2) (screenHeight/2)) $ filled (rgb 0.85 0.85 1.0) $ rect (V2 screenWidth screenHeight)
    ground = move (V2 (screenWidth/2) (screenHeight + (groundHeight/2))) $ filled (rgb 0.05 1.0 0.05) $ rect (V2 screenWidth groundHeight)
    player1 = move player1Pos $ filled (rgb 0.95 0.05 0.05) $ circle playerRadius
    player2 = move player2Pos $ filled (rgb 0.05 0.05 0.95) $ circle playerRadius
    ball = move ballPos $ filled (rgb 0.95 0.05 0.95) $ circle ballRadius
    net = move (V2 (screenWidth/2) (screenHeight - (netHeight/2))) $ filled (rgb 0.2 0.2 0.2) $ rect (V2 netWidth netHeight)
    overlay model = playingOverlay (rgb 1 1 1) model

main :: IO ()
main = do
  engine <- SDL.startup

  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
