import Data.List (zipWith4)
import Data.Maybe (fromJust)

type Interval = Int   -- Interval represents the number of semi-tones
                      -- in an interval

-- | intervalWeight maps from the number of semi-tones in an interval to
-- its weight.
intervalWeight :: [(Interval, Double)]
intervalWeight = [ (0,  0)  -- unison
                 , (1, 1)   -- minor second
                 , (2, 0.8) -- major second
                 , (3, 0.225) -- minor third
                 , (4, 0.2) -- major third
                 , (5, 0.55)
                 ]
                 -- etc

-- | Possibly, this should rather be the list of the interval weights
--   in both cases it should by replaced by map ?? intervalWeight
intervalList :: [Interval]
intervalList = [0 .. 4*12]

metricTensionTable :: [(Int, [(Int, Int)])]
metricTensionTable = [ (4, [(1, 2), (2, 2), (3, 6), (4, 2)])
                     , (2, [(1, 2), (2, 2)])
                     , (3, [(1, 2), (2, 2), (3, 2)])
                     , (6, [(1, 2), (2, 2), (3, 2), (4, 8), (5, 4), (6, 3)])
                     , (9, [(1, 2), (2, 2), (3, 2), (4, 8), (5, 4), (6, 3)
                           , (7, 14), (8, 8), (9, 4)])
                     ]

-- | Interval, root placement, strength with 1 being strongest.
rootStrengthsAndRoots :: [(Interval, Int, Int)]
rootStrengthsAndRoots = []

-- | Maps addition across the various parameters of the analysis.
mapAdd :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
mapAdd = zipWith4 (\a b c d -> a + b + c + d)

-- | Maps the metric tensions in a given meter.
mapMetricTensions :: Int -> Int -> Int -> [Double]
mapMetricTensions startBeat totalBeats meter = take totalBeats $
  map (lookupAndFigureMetricTension meter) $ [startBeat..meter] ++ cycle [1..meter]

-- | Looks up the relevant metric weight.
lookupAndFigureMetricTension :: Int -> Int -> Double
lookupAndFigureMetricTension meter beatNumber =
    (0.1 * fromIntegral beatNumber / fromIntegral tension)
    where
    tension = fromJust $ lookup beatNumber =<< lookup meter metricTensionTable
