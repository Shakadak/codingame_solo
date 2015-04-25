import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let gb = words input_line
    let sx = read (gb!!0) :: Int
    let sy = read (gb!!1) :: Int

    input <- replicateM 8 $ do
        input_line <- getLine
        let mh = read input_line :: Int
        return mh

    -- either:  FIRE (ship is firing its phase cannons) or HOLD (ship is not firing).
    putStrLn $ fire_or_hold (input!!sx) (maximum input)

    loop

fire_or_hold :: Int -> Int -> String
fire_or_hold here max_height
    | here == max_height = "FIRE"
    | otherwise = "HOLD"
