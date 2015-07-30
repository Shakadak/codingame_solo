import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    il <- getLine
    let i = words il
    let lx = read (i!!0) :: Int
    let ly = read (i!!1) :: Int
    let tx = read (i!!2) :: Int
    let ty = read (i!!3) :: Int
    loop $ readablePath (lx - tx, ly - ty)

loop :: [String] -> IO ()
loop [] = return ()
loop (x:xs) = do
    getLine
    putStrLn x
    loop xs

readablePath :: (Int, Int) -> [String]
readablePath (0, 0) = []
readablePath position = readableStep position : readablePath (nextStep position)

readableStep :: (Int, Int) -> String
readableStep (px, py) = lat py ++ long px

next :: Int -> Int
next 0 = 0
next n
    | n > 0 = n - 1
    | otherwise = n + 1

nextStep :: (Int, Int) -> (Int, Int)
nextStep (x, y) = (next x, next y)

long :: Int -> String
long l
    | l > 0 = "E"
    | l < 0 = "W"
    | otherwise = []

lat :: Int -> String
lat l
    | l > 0 = "S"
    | l < 0 = "N"
    | otherwise = []
