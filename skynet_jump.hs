import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    hPutStrLn stderr input_line
    let r = read input_line :: Int -- the length of the road before the gap.
    input_line <- getLine
    hPutStrLn stderr input_line
    let g = read input_line :: Int -- the length of the gap.
    input_line <- getLine
    hPutStrLn stderr input_line
    let l = read input_line :: Int -- the length of the landing platform.
    input_line <- getLine
    --hPutStrLn stderr input_line
    let speed = read input_line :: Int -- the starting speed of the motorbike.
    loop

loop :: IO ()
loop = do
    input_line <- getLine
    hPutStrLn stderr input_line
    hPutStrLn stderr ""
    let s = read input_line :: Int -- the position on the road of the motorbike.

    -- hPutStrLn stderr "Debug messages..."

    -- A single line containing one of 4 keywords: SPEED, SLOW, JUMP, WAIT.
    putStrLn "SPEED"
    input_line <- getLine
    let x = read input_line :: Int -- the motorbike's speed.

    loop

stop_distance :: Int -> Int
stop_distance 0 = 0
stop_distance speed = speed - 1 + stop_distance (speed - 1)

max_speed_in_platform :: Int -> Int
max_stop_distance platform_size = maximum $ takeWhile (`stop_distance_is_less_than` platform_size) [1,2..]

stop_distance_is_less_than :: Int -> Int -> Bool
speed `stop_distance_is_less_than` platform_size = stop_distance speed <= platform_size

min_jump_speed :: Int -> Int
min_jump_speed gap_size = gap_size + 1
