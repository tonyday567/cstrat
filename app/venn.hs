{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Protolude
-- import Readme.Lhs
import NumHask.Space
import Chart
import Control.Lens
-- import Codec.Picture.Types
import qualified Data.Map.Strict as Map
-- import Lucid.Svg.Path
import Lucid.Svg
import Data.Generics.Labels ()
import qualified Data.Text as Text
import Data.Text (Text)

xs :: Map.Map Text (Point Double)
xs = Map.fromList
     [ ("origin", Point 0 0) -- origin
     , ("circle1", Point 0.5 (-0.5 + cos (pi/6)))  -- center of circle 1
     , ("circle2", Point 0 -0.5)                  -- center of circle 2
     , ("circle3", Point -0.5 (-0.5 + cos (pi/6))) -- center of circle 3
     , ("corner1", Point 0 (-0.5 + 2 * cos (pi/6))) -- corner 1
     , ("corner2", Point 1 -0.5)                   -- corner 2
     , ("corner3", Point -1 -0.5)                  -- corner 3
     ]

p :: Text -> (Double, Double)
p k = let (Point x y) = xs Map.! k in (x, -y)

moveA :: Double -> Double -> Text
moveA x y = "M" <> show x <> "," <> show y

data Arc = Arc { arcXr :: Double, arcYr :: Double, arcRot :: Double, arcLargeArcFlag :: Bool, arcSweepFlag :: Bool, arcX :: Double, arcY :: Double} deriving (Eq, Show, Generic)

arcA_ :: Arc -> Text
arcA_ a = show (view #arcXr a) <> " " <> show (view #arcYr a) <> " " <> show (view #arcRot a) <> " " <> bool "0" "1" (view #arcLargeArcFlag a) <> " " <> bool "0" "1" (view #arcSweepFlag a) <> " " <> show (view #arcX a) <> "," <> show (view #arcY a)

arcA :: [Arc] -> Text
arcA as = "A" <> Text.intercalate " " (arcA_ <$> as)

outerseg1 :: Text
outerseg1 = Text.intercalate " "
  [ uncurry moveA (p "corner1")
  , arcA 
   [ uncurry (Arc 0.5 0.5 0 True True) (p "corner2")
   , uncurry (Arc 1 1 0 False False) (p "circle1")
   , uncurry (Arc 1 1 0 False False) (p "corner1")
   ]
  , "Z"
  ]

outerseg2 :: Text
outerseg2 = Text.intercalate " "
  [ uncurry moveA (p "corner3")
  , arcA 
   [ uncurry (Arc 0.5 0.5 0 True False) (p "corner2")
   , uncurry (Arc 1 1 0 False True) (p "circle2")
   , uncurry (Arc 1 1 0 False True) (p "corner3")
   ]
  , "Z"
  ]

outerseg3 :: Text
outerseg3 = Text.intercalate " "
  [ uncurry moveA (p "corner3")
  , arcA 
   [ uncurry (Arc 0.5 0.5 0 True True) (p "corner1")
   , uncurry (Arc 1 1 0 False False) (p "circle3")
   , uncurry (Arc 1 1 0 False False) (p "corner3")
   ]
  , "Z"
  ]

innerseg :: Text
innerseg = Text.intercalate " "
  [ uncurry moveA (p "circle1")
  , arcA 
   [ uncurry (Arc 1 1 0 False True) (p "circle2")
   , uncurry (Arc 1 1 0 False True) (p "circle3")
   , uncurry (Arc 1 1 0 False True) (p "circle1")
   ]
  , "Z"
  ]

midseg1 :: Text
midseg1 = Text.intercalate " "
  [ uncurry moveA (p "corner1")
  , arcA 
   [ uncurry (Arc 1 1 0 False True) (p "circle1")
   , uncurry (Arc 1 1 0 False False) (p "circle3")
   , uncurry (Arc 1 1 0 False True) (p "corner1")
   ]
  , "Z"
  ]

midseg2 :: Text
midseg2 = Text.intercalate " "
  [ uncurry moveA (p "circle1")
  , arcA 
   [ uncurry (Arc 1 1 0 False True) (p "corner2")
   , uncurry (Arc 1 1 0 False True) (p "circle2")
   , uncurry (Arc 1 1 0 False False) (p "circle1")
   ]
  , "Z"
  ]

midseg3 :: Text
midseg3 = Text.intercalate " "
  [ uncurry moveA (p "circle2")
  , arcA 
   [ uncurry (Arc 1 1 0 False True) (p "corner3")
   , uncurry (Arc 1 1 0 False True) (p "circle3")
   , uncurry (Arc 1 1 0 False False) (p "circle2")
   ]
  , "Z"
  ]

-- <path stroke-width="4.0e-2" stroke="#FFFFFF" fill="#032F74" fill-opacity="0.7" stroke-opacity="1.0" d="M0.0,-1.2320508075688774 A0.5 0.5 0.0 1 1 1.0,0.5 1.0 1.0 0.0 0 0 0.5,-0.3660254037844387 1.0 1.0 0.0 0 0 0.0,-1.2320508075688774 Z" />

seg :: Text -> Text -> Svg ()
seg ps c = path_ [stroke_width_ "0.04"
               , stroke_ "#FFFFFF"
               , fill_ c
               , fill_opacity_ "0.7"
               , stroke_opacity_ "1.0"
               , d_ ps
               ]

svg' :: Point Double -> Rect Double -> [Svg ()] -> Svg ()
svg' (Point w' h') (Rect x x' y y') cs =
  with (svg11_ (mconcat cs)) [version_ "1.1", width_ (show w') , height_ (show h'), viewBox_ (show x <> " " <> show (-y') <> " " <> show (x' - x) <> " " <> show (y' - y))]

vennGlyphs :: [Text]
vennGlyphs = [outerseg1, outerseg2, outerseg3, midseg1, midseg2, midseg3, innerseg]

venns :: [Svg ()]
venns = zipWith seg vennGlyphs (toHex <$> d3Palette1)

main :: IO ()
main = writeFile "other/venn.svg" $ toStrict $ prettyText $ svg' (Point 600 400) (Rect -2 2 -2 2) venns

