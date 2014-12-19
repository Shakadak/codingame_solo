import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- The code below will read all the game information for you.
    -- On each game turn, information will be available on the standard input, you will be sent:
    -- -> the total number of visible enemies
    -- -> for each enemy, its name and distance from you
    -- The system will wait for you to write an enemy name on the standard output.
    -- Once you have designated a target:
    -- -> the cannon will shoot
    -- -> the enemies will move
    -- -> new info will be available for you to read on the standard input.

    loop


loop :: IO ()
loop = do
    input_line <- getLine
    let count = read input_line :: Int -- The number of current enemy ships within range

    let input = replicateM count $ do
        input_line <- getLine
        return input_line
    workable <- input
    putStrLn $ fst $ nearest_target $ as_tuple $ split_input workable -- The name of the most threatening enemy (HotDroid is just one example)

    loop

split_input :: [String] -> [[String]]
split_input input = [words slice | slice <- input]

as_tuple :: [[String]] -> [(String, Int)]
as_tuple input = [(slice!!0, read (slice!!1) :: Int) | slice <- input]

nearest_target :: [(String, Int)] -> (String, Int)
nearest_target [] = ("", 0)
nearest_target [singleton] = singleton
nearest_target (top:rest)
    | snd top < snd nearest_rest = top
    | otherwise = nearest_rest
    where nearest_rest = nearest_target rest
