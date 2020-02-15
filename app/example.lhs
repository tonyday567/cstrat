---
pagetitle: cstrat
---

[cstrat](https://github.com/tonyday567/cstrat) example
===

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

> {-# OPTIONS_GHC -Wall #-}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> 

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [readme-lhs](https://www.hackage.org/package/readme-lhs)

> import Protolude
> -- import Readme.Lhs
> import Graphics.Svg.Types hiding (Point, Text)
> import NumHask.Space
> import Linear.V2
> import Chart
> import Control.Lens
> import Codec.Picture.Types
> import qualified Data.Map.Strict as Map

code
---

> xs = Map.fromList
>      [ ("origin", Point 0 0) -- origin
>      , ("circle1", Point 0.5 (-0.5 + cos (pi/6)))  -- center of circle 1
>      , ("circle2", Point 0 -0.5)                  -- center of circle 2
>      , ("circle3", Point -0.5 (-0.5 + cos (pi/6))) -- center of circle 3
>      , ("corner1", Point 0 (-0.5 + 2 * cos (pi/6))) -- corner 1
>      , ("corner2", Point 1 -0.5)                   -- corner 2
>      , ("corner3", Point -1 -0.5)                  -- corner 3
>      ]
>
> p :: Text -> V2 Double
> p k = let (Point x y) = xs Map.! k in V2 x (-y)
> 
> 
> outerseg1 :: [PathCommand]
> outerseg1 =
>   [ MoveTo OriginAbsolute [p "corner1"]
>   , EllipticalArc OriginAbsolute
>     [ (0.5,0.5,0,True,True,p "corner2")
>     , (1,1,0,False,False,p "circle1")
>     , (1,1,0,False,False,p "corner1")
>     ]
>   , EndPath
>   ]
> 
> outerseg2 :: [PathCommand]
> outerseg2 =
>   [ MoveTo OriginAbsolute [p "corner3"]
>   , EllipticalArc OriginAbsolute
>     [ (0.5,0.5,0,True,False,p "corner2")
>     , (1,1,0,False,True,p "circle2")
>     , (1,1,0,False,True,p "corner3")
>     ]
>   , EndPath
>   ]
> 
> outerseg3 :: [PathCommand]
> outerseg3 =
>   [ MoveTo OriginAbsolute [p "corner3"]
>   , EllipticalArc OriginAbsolute
>     [ (0.5,0.5,0,True,True,p "corner1")
>     , (1,1,0,False,False,p "circle3")
>     , (1,1,0,False,False,p "corner3")
>     ]
>   , EndPath
>   ]
> 
> innerseg :: [PathCommand]
> innerseg =
>   [ MoveTo OriginAbsolute [p "circle1"]
>   , EllipticalArc OriginAbsolute
>     [ (1,1,0,False,True,p "circle2")
>     , (1,1,0,False,True,p "circle3")
>     , (1,1,0,False,True,p "circle1")
>     ]
>   , EndPath
>   ]
> 
> midseg1 :: [PathCommand]
> midseg1 =
>   [ MoveTo OriginAbsolute [p "corner1"]
>   , EllipticalArc OriginAbsolute
>     [ (1,1,0,False,True,p "circle1")
>     , (1,1,0,False,False,p "circle3")
>     , (1,1,0,False,True,p "corner1")
>     ]
>   , EndPath
>   ]
> 
> midseg2 :: [PathCommand]
> midseg2 =
>   [ MoveTo OriginAbsolute [p "circle1"]
>   , EllipticalArc OriginAbsolute
>     [ (1,1,0,False,True,p "corner2")
>     , (1,1,0,False,True,p "circle2")
>     , (1,1,0,False,False,p "circle1")
>     ]
>   , EndPath
>   ]
> 
> midseg3 :: [PathCommand]
> midseg3 =
>   [ MoveTo OriginAbsolute [p "circle2"]
>   , EllipticalArc OriginAbsolute
>     [ (1,1,0,False,True,p "corner3")
>     , (1,1,0,False,True,p "circle3")
>     , (1,1,0,False,False,p "circle2")
>     ]
>   , EndPath
>   ]

> pt1 :: [PathCommand] -> DrawAttributes -> Tree
> pt1 s d = PathTree $ defaultSvg & drawAttr .~ d & pathDefinition .~ s
>
> o1 :: Tree
> o1 = CircleTree $ Circle (da red 0.3) (Num 0, Num 0) (Num 0.1)
>
> c1 :: [Tree]
> c1 = 
>   [ CircleTree $ Circle (da red 0.3) (Num 0.5, Num (0.5 - cos (pi/6))) (Num 1)
>   , CircleTree $ Circle (da blue 0.3) (Num 0, Num 0.5) (Num 1)
>   , CircleTree $ Circle (da grey 0.3) (Num -0.5, Num (0.5 - cos (pi/6))) (Num 1)
>   ]
> 
> c0 :: [Tree]
> c0 = 
>   [ CircleTree $ Circle (da red 1) (Num 0.5, Num (0.5 - cos (pi/6))) (Num 0.01)
>   , CircleTree $ Circle (da blue 1) (Num 0, Num 0.5) (Num 0.01)
>   , CircleTree $ Circle (da grey 1) (Num -0.5, Num (0.5 - cos (pi/6))) (Num 0.01)
>   , CircleTree $ Circle (da grey 1) (Num 0, Num 0) (Num 0.01)
>   , CircleTree $ Circle (da grey 1) (Num 0, Num $ 0.5 - 2 * cos (pi/6)) (Num 0.01)
>   , CircleTree $ Circle (da grey 1) (Num -1, Num $ 0.5) (Num 0.01)
>   , CircleTree $ Circle (da grey 1) (Num 1, Num $ 0.5) (Num 0.01)
>   ]
>
> ch1 :: (Num a) => [Tree] -> ChartSvg a
> ch1 ts = ChartSvg (Rect (-2) 2 (-2) 2) ts
>
> t1 :: [Tree] -> IO ()
> t1 s = writeChartSvg "other/t1.svg" (Point 400.0 400) (ch1 (s <> [o1]))
>
>
> da c o = mempty & fillColor .~ Last (Just $ ColorRef $ promotePixel c) & fillOpacity ?~ realToFrac o


> vennda c = mempty & fillColor .~ Last (Just $ ColorRef $ promotePixel c) & fillOpacity ?~ realToFrac 0.7 & strokeWidth .~ (Last $ Just $ Num 0.04) & strokeColor .~ Last (Just $ ColorRef $ promotePixel (PixelRGB8 255 255 255)) & strokeOpacity .~ (Just 1)
>
> venn = zipWith (\s c -> pt1 c s) (vennda <$> d3Palette1) [outerseg1, outerseg2, outerseg3, midseg1, midseg2, midseg3, innerseg]
>
>
> vennGlyphs = [outerseg1, outerseg2, outerseg3, midseg1, midseg2, midseg3, innerseg]
>
> vgs :: [Chart Double]
> vgs = zipWith (\x y -> Chart (GlyphA (defaultGlyphStyle & #shape .~ PathGlyph x & #color .~ y)) [SP 0 0]) vennGlyphs d3Palette1
> 



> main :: IO ()
> main = pure ()

