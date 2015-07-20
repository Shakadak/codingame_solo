import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let input = words input_line
    let lx = read (input!!0) :: Int -- the X position of the light of power
    let ly = read (input!!1) :: Int -- the Y position of the light of power
    let tx = read (input!!2) :: Int -- Thor's starting X position
    let ty = read (input!!3) :: Int -- Thor's starting Y position
    loop $ readable_path (lx - tx, ly - ty)

loop :: [String] -> IO ()
loop (step:path) = do
    input_line <- getLine
    let _ = read input_line :: Int -- The level of Thor's remaining energy, representing the number of moves he can still make.

    -- A single line providing the move to be made: N NE E SE S SW W or NW
    putStrLn step

    loop path
loop [] = return ()

readablePath :: (Int, Int) -> [String]
readablePath (0, 0) = []
readablePath position = readable_step position : readable_path (next_step position)

readableStep :: (Int, Int) -> String
readableStep (px, py) = latitude py ++ longitude px

next :: Int -> Int
next 0 = 0
next n
    | n > 0 = n - 1
    | otherwise = n + 1

nextStep :: (Int, Int) -> (Int, Int)
nextStep (x, y) = (next x, next y)

longitude :: Int -> String
longitude l
    | l > 0 = "E"
    | l < 0 = "W"
    | otherwise = []

latitude :: Int -> String
latitude l
    | l > 0 = "S"
    | l < 0 = "N"
    | otherwise = []
