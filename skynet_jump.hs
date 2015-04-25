import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let r = read input_line :: Int -- the length of the road before the gap.
    input_line <- getLine
    let g = read input_line :: Int -- the length of the gap.
    input_line <- getLine
    let l = read input_line :: Int -- the length of the landing platform.
    loop g r

loop :: Int -> Int -> IO ()
loop gap gap_position = do
    input_line <- getLine
    let speed = read input_line :: Int -- the motorbike's speed.
    input_line <- getLine
    let bike_position = read input_line :: Int -- the position on the road of the motorbike.

    -- A single line containing one of 4 keywords: SPEED, SLOW, JUMP, WAIT.
    putStrLn $ action gap speed gap_position bike_position

    loop gap gap_position

action :: Int -> Int -> Int -> Int -> String
action _ _ gap_position bike_position
	|bike_position > gap_position = "SLOW"
	|bike_position == gap_position - 1 = "JUMP"
action gap speed _ _
    |speed < gap + 1 = "SPEED"
    |speed > gap + 1 = "SLOW"
	|otherwise = "WAIT"
