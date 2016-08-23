import Data.List

data Section = Section { getA :: Int
                       , getB :: Int
                       , getC :: Int
                       } deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0
                   ]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath


roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwordTimeToA = timeA + a
        crosstimeToA = timeA + b + c
        forwordTimeToB = timeB + b
        crosstimeToB = timeB + a + c
        newPathToA = if forwordTimeToA <= crosstimeToA
                        then (A, a):pathA
                        else (C, c):(B, b):pathB
        newPathToB = if forwordTimeToB <= crosstimeToB
                        then (B, b):pathB
                        else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

groupsOf3 :: Int -> [a] -> [[a]]
groupsOf3 0 _ = undefined
groupsOf3 _ [] = []
groupsOf3 n xs = take n xs:groupsOf3 n (drop n xs)

main = do
  contents <- getContents
  let threes = groupsOf3 3 $ map read $ lines contents
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathTime = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "Time taken: " ++ show pathTime
