import Control.Monad (liftM)
import Data.List (sort)
import Data.Char (ord)
import Data.List.Split (splitOn)

getAsSortedList :: IO [String]
getAsSortedList = 
    liftM (sort . concatMap (splitOn ",") . lines . filter (/= '"') ) $ readFile "names.txt"

toNum :: String -> [Int]
toNum = map chst 
  where
    chst x = ord x - 64

main :: IO ()
main = do 
  l <- getAsSortedList 
  let tlist = zip l [1..]
      rlist = map (\x -> snd x * (sum . toNum $ fst x)) tlist
  print $ sum rlist
