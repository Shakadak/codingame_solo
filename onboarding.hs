import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    loop


loop :: IO ()
loop = do
    input_line <- getLine
    let count = read input_line :: Int

	input <- replicateM count $ do
		input_line <- getLine
		return input_line
    putStrLn $ fst $ nearest_target $ as_tuple $ split_input input
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
