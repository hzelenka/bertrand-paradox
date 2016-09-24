module Main where

import Data.List (splitAt)
import System.Random
import System.Environment
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

type Radians = Double
type CircPoint = (Double, Double)

-- Given a measurement in radians, return the coordinates of the corresponding edge point
radToPt :: Radians -> CircPoint
radToPt rad = (cos rad, sin rad)

-- Distance in the Cartesian plane
distance :: CircPoint -> CircPoint -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

-- Method 1: Choose two random endpoints and draw a chord between them
drawChord1 :: Radians -> Radians -> [CircPoint]
drawChord1 endpt1 endpt2 = map radToPt [endpt1, endpt2]

-- Method 2: Choose a random radius and a point on the radius, then draw a perpendicular chord
drawChord2 :: Radians -> Double -> [CircPoint]
drawChord2 radius radDist = [(x' + dx, y' + dy),(x' - dx, y' - dy)]
  where (x, y)   = radToPt radius
        (x', y') = (radDist * x, radDist * y)
        len      = sqrt $ 1 - radDist^2 -- Pythagorean formula
        (dx, dy) = (negate len * y, len * x)
                           
-- Method 3: Choose a random interior point and draw a chord through it
drawChord3 :: CircPoint -> [CircPoint]
drawChord3 midpt = [(x + dx, y + dy), (x - dx, y - dy)]
  where (x, y)   = midpt
        radDist  = distance midpt (0,0)
        (x', y') = let k = sqrt (1 / (x^2 + y^2)) in (k * x, k * y)
        len      = sqrt $ 1 - radDist^2 -- Pythagorean formula
        (dx, dy) = (negate len * y', len * x')

circle :: [CircPoint]
circle = [ radToPt rad | rad <- [0.00,0.01..2*pi] ]

drawStuff :: [[CircPoint]] -> FilePath -> IO ()
drawStuff points filename = toFile def filename $ do
    layout_left_axis_visibility.axis_show_ticks .= False
    layout_left_axis_visibility.axis_show_labels .= False
    layout_bottom_axis_visibility.axis_show_ticks .= False
    layout_bottom_axis_visibility.axis_show_labels .= False
    let op = min 1 (100 / fromIntegral (length points))
    setColors [opaque black, blue `withOpacity` 0.1]
    plot (line "" [circle])
    plot (line "" points)

main :: IO ()
main = do
  -- We need >= 5 StdGens to ensure freshness of random numbers
  gen1 <- getStdGen
  gen2 <- newStdGen
  gen2' <- newStdGen
  gen3 <- newStdGen
  gen3' <- newStdGen
  putStr "Enter desired method: "
  entry <- getLine
  putStr "Enter desired number of repetitions: "
  reps <- getLine
  let reps'    = read reps
      rands1  = let randRdns = splitAt reps' $ take (reps'*2) (randomRs (0, 2*pi) gen1 :: [Double])
                in zip (fst randRdns) (snd randRdns)
      rands2  = let (randRdns, randDbs) = (take reps' (randomRs (0, 2*pi) gen2),
                                           take reps' (randomRs (0.0, 1.0) gen2'))
                                          :: ([Double], [Double])
                in zip randRdns randDbs
      rands3  = let (x, y) = (randomRs (-1.0, 1.0) gen3, randomRs (-1.0, 1.0) gen3')
                in take reps' $ filter (\(a, b) -> a^2 + b^2 <= 1 && (a, b) /= (0, 0)) $ zip x y
      chords1 = map (uncurry drawChord1) rands1
      chords2 = map (uncurry drawChord2) rands2
      chords3 = map drawChord3 rands3
      entry' = case entry of "1" -> (chords1, "method1.png")
                             "2" -> (chords2, "method2.png")
                             "3" -> (chords3, "method3.png")
                             _ -> error "Bad method, go talk to Joseph Bertrand about it"
  uncurry drawStuff entry'
  putStrLn "Drew something, check your folders"

