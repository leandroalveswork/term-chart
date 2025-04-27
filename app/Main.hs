module Main where

newtype CNumReader = CNumReader (Rational -> Rational)

-- grafico 40x20 (numeros)
-- grafico 80x40 (tamanho em pixels)

data GraphView = GraphView { vWidth :: Int , vHeight :: Int }

defaultGraphView :: GraphView
defaultGraphView = GraphView
  { vWidth = 40
  , vHeight = 20
  }

resultInPixel :: CNumReader -> Int -> Int
resultInPixel (CNumReader f) x = let x' :: Rational
                                     x' = fromRational (fromIntegral x) 
                                     absurdSafe :: Int -> Int
                                     absurdSafe = max (-10000) . min 10000
                                     in (absurdSafe . round) ((f (x' / 2)) * 2) 

drawColumnVarying :: CNumReader -> Int -> [(Int, Int)]
drawColumnVarying rf x = let y0 = resultInPixel rf (x - 1)
                             y = resultInPixel rf x
                             intermediatePixels :: [(Int, Int)]
                             intermediatePixels = case compare y y0 of
                                                       GT -> map 
                                                               (\yIm -> if yIm - y0 > y - yIm
                                                                           then (x, yIm)
                                                                           else (x - 1, yIm)) 
                                                               [y0..y]
                                                       LT -> map
                                                               (\yIm -> if yIm - y > y0 - yIm
                                                                           then (x - 1, yIm)
                                                                           else (x, yIm)) 
                                                               [y..y0]
                                                       EQ -> []
                         in intermediatePixels ++ [(x, y)] 

lineIntro :: Int -> String 
lineIntro yNumber
  | yNumber `mod` 5 == 0 = (\text -> if length text == 1 then " " ++ text else text) (show yNumber)
  | otherwise = "▀▀"

columnIntro :: Int -> String
columnIntro xNumber
  | xNumber `mod` 5 == 0 = (\text -> if length text == 1 then text ++ " " else text) (show xNumber)
  | otherwise = "| "

drawGraph :: GraphView -> CNumReader -> [String]
drawGraph g rf = let totalDrawing :: [(Int, Int)]
                     totalDrawing = foldMap 
                                      (filter ((\y -> y > 0 && y <= vHeight g * 2).snd) . drawColumnVarying rf) 
                                      [1..(vWidth g * 2)]
                 in (foldMap 
                      (\yPx -> [lineIntro (yPx `div` 2) ++ foldMap 
                        (\xPx -> case (any (==(xPx, yPx)) totalDrawing, any (==(xPx, yPx - 1)) totalDrawing) of
                                      (False, False) -> " "
                                      (False, True)  -> "▄"
                                      (True, False)  -> "▀"
                                      (True, True)   -> "█"
                        )
                        [1..(vWidth g * 2)]
                      ])
                      [(vHeight g * 2),(vHeight g * 2) - 2..1])
                      ++ ["   " ++ foldMap columnIntro [1..(vWidth g)]]

autoDraw :: (Rational -> Rational) -> IO ()
autoDraw f = 
  putStrLn (unlines (drawGraph defaultGraphView (CNumReader f)))

byFractional :: (Fractional a, Real a) => (a -> a) -> Rational -> Rational
byFractional f = toRational . f . fromRational 

main :: IO ()
main = do
  {-
  (autoDraw . byFractional) (\x -> 10 + sin (x * (pi / 2) / 5) * 10)
  (autoDraw . byFractional) (\x -> 10 + tan (x * (pi / 2) / 5) * 3)
  autoDraw (\x -> if x == 0 then 1000000 else 20 / x)
  autoDraw (\x -> let integX :: Int
                      integX = floor x
                      xFraction :: Rational
                      xFraction = x - fromIntegral integX
                  in 5 + toRational (integX `mod` 10) + xFraction)
                  -}
  autoDraw (\x -> toRational (floor (x / 5) * 5))

  (autoDraw . byFractional) (\x -> 20 / (1.12 ** x))
  (autoDraw . byFractional) (\x -> 0.2 * (1.12 ** x))
  
  --(autoDraw . byFractional) (\x -> 10 + sinDiff x * (2 - x / 20))
  (autoDraw . byFractional) (\x -> 10 + sinDiff x)
  (autoDraw . byFractional) (\x -> 10 + modDiff x)
  
  (autoDraw . byFractional) (\x -> 10 + (sinDiff x + modDiff x) / 2)
  --(autoDraw . byFractional) (\x -> 10 + (tanDiff x + modDiff x) / 2)
  --(autoDraw . byFractional) (\x -> 10 + sinDiff x * modDiff x)
  where sinDiff :: (RealFrac a, Floating a) => a -> a
        sinDiff w = sin (w * (pi / 2) / 5) * 5
        modDiff :: (RealFrac a, Floating a) => a -> a
        modDiff w = let integW :: Int
                        integW = floor w
                        wFraction = w - fromIntegral integW
                    in (fromIntegral (integW `mod` 10) + wFraction) - 5
        tanDiff :: (Fractional a, Real a, Floating a) => a -> a
        tanDiff w = tan (w * (pi / 2) / 5) * 3

