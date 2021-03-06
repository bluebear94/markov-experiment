import Markov
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import System.IO
import System.Random

parseSCName :: String -> [String]
parseSCName name = let
  (prequote, inquote') = break (=='「') name
  inquote = tail $ init inquote'
  inwords = map (:[]) inquote
  in prequote : inwords

nodesToStrs :: [MSNode String] -> [String]
nodesToStrs = let
  f (Elem s) = Just s
  f _ = Nothing
  in mapMaybe f

unparseSCName :: [String] -> String
unparseSCName (prequote : inwords) = prequote ++ "「" ++ (join inwords) ++ "」"

readSCNames :: String -> FreqTally (MSNode String) -> IO (FreqTally (MSNode String))
readSCNames character tally = do
  let path = "superu/" ++ character ++ ".txt"
  handle <- openFile path ReadMode
  hSetEncoding handle utf8
  text <- hGetContents handle
  let lns = filter (not . null) (lines text)
  let sclens = map parseSCName lns
  return $ tallyNN tally sclens

readManySCNames :: [String] -> IO (FreqTally (MSNode String))
readManySCNames = foldM (flip readSCNames) Map.empty

main :: IO ()
main = do
  args <- getArgs
  let leaky = "-l" `elem` args
  let tm = if leaky then traverseMarkovLeaky else traverseMarkov
  input' <- getLine
  let input = words input'
  tally <- readManySCNames input
  let chain = fromFreqTable tally
  input'' <- getLine
  let nameCount = read input'' :: Int
  forM_ [1 .. nameCount] (\_ -> do
    gen <- newStdGen
    let str = tm (chain, Start, gen)
    putStrLn $ unparseSCName $ nodesToStrs str
    )