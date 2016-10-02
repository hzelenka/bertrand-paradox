module Main where

import Data.List (splitAt)
import System.Random (getStdGen, newStdGen, randomRs)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

type Radians = Double
type CircPoint = (Double, Double)

-- Corresponding point along edge of circle
radToPt :: Radians -> CircPoint
radToPt rad = (cos rad, sin rad)

-- Distance in the Cartesian plane
dist :: CircPoint -> CircPoint -> Double
dist (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

-- Method 1: Choose two random endpoints and draw a chord between them
drawChord1 :: Radians -> Radians -> [CircPoint]
drawChord1 endpt1 endpt2 = map radToPt [endpt1, endpt2]

ex1 :: IO ()
ex1 = toFile def "ex1.png" $ do
  plot (line "" [circle, map radToPt [pi/2,7*pi/6,11*pi/6,pi/2]])
  plot (line "Shorter" [ drawChord1 (pi/2) (k*pi/9) | k <- [5..10] ])
  plot (line "Longer" [ drawChord1 (pi/2) (k*pi/9) | k <- [11..13] ])

-- Method 2: Choose a random radius and a point on the radius, then draw a perpendicular chord
drawChord2 :: Radians -> Double -> [CircPoint]
drawChord2 radius radDist = [(x' + dx, y' + dy),(x' - dx, y' - dy)]
  where (x, y)   = radToPt radius
        (x', y') = (radDist * x, radDist * y)
        len      = sqrt $ 1 - radDist^2 -- Pythagorean formula
        (dx, dy) = (negate len * y, len * x)

ex2 :: IO ()
ex2 = toFile def "ex2.png" $ do
  plot (line "" [circle, map radToPt [pi/2,7*pi/6,11*pi/6,pi/2], [(0,0),(0,-1)]])
  plot (line "Shorter" [ drawChord2 (3*pi/2) k | k <- [0.55,0.65..0.95]])
  plot (line "Longer" [ drawChord2 (3*pi/2) k | k <- [0.05,0.15..0.45]])
                           
-- Method 3: Choose a random interior point and draw a chord through it
drawChord3 :: CircPoint -> [CircPoint]
drawChord3 midpt = [(x + dx, y + dy), (x - dx, y - dy)]
  where (x, y)   = midpt
        radDist  = dist midpt (0,0)
        (x', y') = let k = sqrt (1 / (x^2 + y^2)) in (k * x, k * y)
        len      = sqrt $ 1 - radDist^2 -- Pythagorean formula
        (dx, dy) = (negate len * y', len * x')

ex3 :: IO ()
ex3 = toFile def "ex3.png" $ do
  let smallCircle = map (\(x,y) -> (x/2,y/2)) circle
  plot (line "" [circle, smallCircle])
  plot (line "Shorter" [ drawChord3 mid | mid <- [(-0.5,0.5),(-0.65,0.35),(-0.35,0.65),
                                                  (-0.75,0.1),(-0.1,0.75),(-0.4,0.4)] ])
  plot (line "Longer" (map drawChord3 [(-0.25,0.1),(-0.1,0.25)] ))

circle :: [CircPoint]
circle = [ radToPt rad | rad <- [0.00,0.01..2*pi] ]

drawStuff :: [[CircPoint]] -> FilePath -> IO ()
drawStuff points filename = toFile def filename $ do
  let count   = length $ filter (\l -> dist (head l) (l !! 1) >= sqrt 3) points
      caption = show count ++ " out of 1000 points were longer than an equilateral side"
  setColors [opaque black, blue `withOpacity` 0.1]
  plot (line "" [circle])
  plot (line caption points)

main :: IO ()
main = do
  gen1 <- getStdGen
  gen2 <- newStdGen
  putStr "Enter desired method: "
  entry <- getLine
  let rands1  = let randRdns = splitAt 1000 $ take 2000 (randomRs (0, 2*pi) gen1 :: [Double])
                in zip (fst randRdns) (snd randRdns)
      rands2  = let (randRdns, randDbs) = (take 1000 (randomRs (0, 2*pi) gen2),
                                           take 1000 (randomRs (0.0, 1.0) gen1))
                                          :: ([Double], [Double])
                in zip randRdns randDbs
      rands3  = let (x, y) = (randomRs (-1.0, 1.0) gen1,
                              randomRs (-1.0, 1.0) gen2)
                in take 1000 $ filter (\(a, b) -> a^2 + b^2 <= 1 && (a, b) /= (0, 0)) $ zip x y
      chords1 = map (uncurry drawChord1) rands1
      chords2 = map (uncurry drawChord2) rands2
      chords3 = map drawChord3 rands3
      entry'  = case entry of "1" -> Right (drawStuff chords1 "method1.png")
                              "2" -> Right (drawStuff chords2 "method2.png")
                              "3" -> Right (drawStuff chords3 "method3.png")
                              "1'" -> Right ex1
                              "2'" -> Right ex2
                              "3'" -> Right ex3
                              _ -> Left "Bad method, go talk to Joseph Bertrand about it"
  case entry' of Right procedure -> procedure >>
                                    putStrLn "Drew something, check your folders"
                 Left err -> putStrLn err
