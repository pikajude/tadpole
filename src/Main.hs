{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Lens
import           Control.Monad.Fix                  (MonadFix)
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Interface.Pure.Game as G
import           Reflex                             hiding (toggle)
import           Reflex.Gloss

makePrisms ''G.Event

data Force = Force
           { _dUp    :: Bool
           , _dDown  :: Bool
           , _dLeft  :: Bool
           , _dRight :: Bool
           } deriving Show

makeLenses ''Force

newtype Momentum = Momentum Point deriving Show

main :: IO ()
main = playReflex (InWindow "Tadpole" (640, 480) (0, 0)) (greyN 0.1) 100 game

watchDirection :: (Reflex t, MonadFix m, MonadHold t m)
               => Event t (Either G.SpecialKey Char, G.KeyState) -> m (Behavior t Force)
watchDirection = accum toggleKey (Force False False False False) where
    toggleKey ks (Left G.KeyUp, x)    = toggle ks dUp x
    toggleKey ks (Right 'w', x)       = toggle ks dUp x

    toggleKey ks (Left G.KeyDown, x)  = toggle ks dDown x
    toggleKey ks (Right 's', x)       = toggle ks dDown x

    toggleKey ks (Left G.KeyRight, x) = toggle ks dRight x
    toggleKey ks (Right 'd', x)       = toggle ks dRight x

    toggleKey ks (Left G.KeyLeft, x)  = toggle ks dLeft x
    toggleKey ks (Right 'a', x)       = toggle ks dLeft x

    toggleKey ks _                    = ks
    toggle ks l x = ks & l .~ (x == G.Down)

applyForce :: Momentum -> (Force, Float) -> Momentum
applyForce (Momentum (x,y)) (Force up down left right, f) =
    Momentum $ over both cutoff $ cap 10 $ mulSV (1 - (3 * f)) $ foldr ($) (x,y) $
        mconcat [ [ over _2 plus  | up ]
                , [ over _2 minus | down ]
                , [ over _1 plus  | right ]
                , [ over _1 minus | left ]
                ]
    where
        delta = 120 * f
        plus = (+ delta)
        minus = subtract delta
        cutoff n | abs n <= 0.01 = 0
                 | otherwise = n
        cap lim v
            | magV v <= lim = v
            | otherwise = mulSV (lim / magV v) v

addDirection :: (Point, t) -> (Momentum, Float) -> (Point, Float)
addDirection ((x,y), _) (Momentum (dx, dy), f) =
    ((x + dx * 60 * f, y + dy * 60 * f), f)

game :: forall t m . GlossApp t m
game tick input = do
    let buttonEvents = fmapMaybe getButtonEvent input

    force <- watchDirection buttonEvents

    momentum <- accum applyForce (Momentum (0, 0)) $ attach force tick

    pos <- accum addDirection ((0, 0), 0) $ attach momentum tick

    trail :: Behavior t [(Float, Float)] <-
        accum (\ y (pair, f) -> take (round $ 0.5 / f) $ pair : y) [(0, 0)] pos

    let bubblePic = ffor trail $ \ ps -> mconcat $ reverse
            $ ffor (zip (iterate (+ (1 / fromIntegral (length ps))) 0) ps)
                $ \ (c, (x,y)) -> translate x y $ color (withAlpha (1 - c) red)
                    $ circleSolid (30 * (1 - c))
        directionPic = drawDirections <$> force
        debug = ffor momentum $ \ (Momentum m) -> color white $ line [(0,0), mulSV 5 m]

    return $ mconcat [bubblePic, directionPic, debug]
    where
        getButtonEvent (G.EventKey (G.SpecialKey m) ks _ _) = Just (Left m, ks)
        getButtonEvent (G.EventKey (G.Char c) ks _ _)       = Just (Right c, ks)
        getButtonEvent _                                    = Nothing

        drawDirections (Force up down left right)
            = mconcat [ tint up $ translate 0 15 $ rotate 270 triangle
                      , tint down $ translate 0 (negate 15) $ rotate 90 triangle
                      , tint left $ translate (negate 15) 0 $ rotate 180 triangle
                      , tint right $ translate 15 0 triangle
                      ]
        tint True  = color white
        tint False = color (greyN 0.5)

        triangle = polygon [(0, 10), (0, negate 10), (10, 0)]
