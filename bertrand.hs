module Main where

import System.Random
import System.Environment

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

type Radians = Double
type CircPoint = (Double, Double)

-- Given a measurement in radians, return the coordinates of the corresponding edge point
radToPt :: Radians -> (Double, Double)
radToPt rad = (cos rad, sin rad)

-- Distance in the Cartesian plane
distance :: CircPoint -> CircPoint -> Double
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

-- Method 1: Choose two random endpoints and draw a chord between them
drawChord1 :: Radians -> Radians -> [CircPoint]
drawChord1 endpt1 endpt2 = map radToPt [endpt1, endpt2]

-- Method 2: Choose a random radius and a point on the radius, then draw a perpendicular chord
drawChord2 :: Radians -> Double -> [CircPoint]
drawChord2 radius radDist = [(x'+dx,y'+dy),(x'-dx,y'-dy)]
  where (x, y)   = radToPt radius
        (x', y') = (radDist * x, radDist * y)
        len      = sqrt $ 1 - radDist^2 -- Pythagorean formula
        (dx, dy) = (negate len*y, len*x)
                           
-- Method 3: Choose a random interior point and draw a chord through it
drawChord3 :: CircPoint -> [CircPoint]
drawChord3 midpt = [(x+dx,y+dy),(x-dx,y-dy)]
  where (x, y)   = midpt
        radDist  = distance midpt (0,0)
        len      = sqrt $ 1 - radDist^2 -- Pythagorean formula
        (dx, dy) = (negate len*y, len*x)

main :: IO ()
main = undefined

