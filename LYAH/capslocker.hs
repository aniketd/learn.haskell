import System.IO

main = do
    handle <- openFile "opeth.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines
