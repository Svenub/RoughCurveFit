module RoughCurveFit where

data Point = Point
    { getX :: Double -- x coordinate
    , getY :: Double -- y coordinate
    }
    deriving (Eq, Show)


-- This will return the incline value of two given points.
getK :: Point -> Point -> Double
getK (Point x1 y1) (Point x2 y2) = (y2 - y1) / (x2 - x1)


{- This is used to get the starting point where the line is cutting the y-axis. This is needed to
decrease the so called area where the fitted line should be. The function is not customized though to handle 
list of points where the first point is cutting the x-axis. -}
getStartPoint :: [Point] -> Point
getStartPoint []  = error "List is empty"
getStartPoint [x] = Point 0 0
getStartPoint xs  = Point 0 m
  where
    firstP = head xs
    lastP  = last xs
    k      = getK firstP lastP
    m      = getY firstP - (getX firstP * k)


-- This returns the m value of a given list with points.
getM :: [Point] -> Double
getM [] = error "empty list"
getM xs = getY (getStartPoint xs)

-- By using a starting point we can calculate all the different inclines from it with a given list of different points.
findK :: Point -> [Point] -> [Double]
findK startP = map (getK startP)


{- This will give us the list of different inclines by using a starting point, that cuts the y-axis, as a basevalue. This list will be used to
find the biggest and smallest k-values to narrow the search-area down. -}
kList :: [Point] -> [Double]
kList xs = findK (getStartPoint xs) (init xs)

-- Gives the tuple of the biggest and smallest k-values.
getHighestLowestK :: [Point] -> (Double, Double)
getHighestLowestK xs =
    let ks = kList xs
    in (maximum ks, minimum ks) 


{- This function will create a list with the diffence in the y-value between a
given k - and m-value and the points of a given list. -}
diffList :: Double -> Double -> [Point] -> [Double]
diffList k m  = map getDiff
  where
    getDiff p = abs ((k * getX p) + m - getY p)

{- This will give the average value of the elements in a list. -}
sumAverageError :: (Fractional a, Foldable t) => t a -> a
sumAverageError xs = sum xs / fromIntegral (length xs)


{- To actually check which line that fits the points best, we have to go through all the different k-values, 
starting from the highest incline to the lowest incline, and check the diffence of all the y-values in the list. The precision is 
currently hardcoded into the function as "0.01" but could be further extended to be able to choose the amount of precision before
the function is called. The y-values will be summed up and the avarage value will be calculated. The calculated value will
be stored in a tuple together with its corresponded incline value. -}
everyErrorList :: Double -> Double -> Double -> [Point] -> [(Double, Double)]
everyErrorList _ _ _ [] = error "empty List"
everyErrorList startK endK m xs
    | startK >= endK = averageError : everyErrorList (startK - 0.01) endK m xs
    | otherwise      = [averageError]
  where
    averageError = (sumAverageError (diffList startK m xs), startK)

{- This will give a list of all the different y-values, using the everyErrorList-function. The given arguments will be based on 
the highest and lowest k-values that is calculated. The m value is also calculated based on the points given from the list. -}
giveErrorList :: [Point] -> [(Double, Double)]
giveErrorList xs = everyErrorList start end m xs
  where
    (start, end) = getHighestLowestK xs
    m            = getM xs

{- Now, to actually find the best fitted line, it should be the line with the least amount of the average differences of the y-values, aka. the errors.
The k-value is stored together with the preferred result and so is selected from the tuple. The m value is never changed and so is selected based on 
the given points in the list. -}
averageLine :: [Point] -> String
averageLine xs = concat ["y = ", show k, "x + ", show m]
  where
    k = snd (minimum (giveErrorList xs))
    m = getM xs


points =
    [ Point 88.0  57.9
    , Point 224.7 108.2
    , Point 365.3 149.6
    , Point 687.0 228.07
    , Point 4332  778.434
    , Point 10760 1428.74
    , Point 30684 2839.08
    , Point 60188 4490.8
  , Point 90467 5879.13
  ]

points2 = [Point 0.2 0.3, Point 1 1, Point 2 2, Point 3 2, Point 4 2.5]

points3 = [Point 15 10300, Point 17 8100, Point 18 7400]


test = averageLine points

-- Prints out the line equations of the search-area (not needed).
printHandL xs = do
    print ("highest: y = " ++ show highK ++ "x" ++ " + " ++ show m)
    print ("lowest: y = " ++ show lowK ++ "x" ++ " + " ++ show m)
  where
    highK = fst (getHighestLowestK xs)
    lowK  = snd (getHighestLowestK xs)
    m     = getM xs
