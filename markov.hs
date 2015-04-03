-- My brain hurts Hasuike. My brain hurts.

module Fluffy.Markov where
  import qualified Data.Map as Map
  import Data.Map (Map, (!))
  import System.Random
  
  type Prob = Double
  -- A ProbabilityStructure describes a set of possibilities with arbitrary probabilities.
  data ProbabilityStructure a = Surely a | Ehh a Prob (ProbabilityStructure a) deriving Show
  
  -- fromFreqs takes a list of pairs representing frequencies and yields a probability structure paired with the cumulative frequency.
  fromFreqsCF :: Map a Prob -> (ProbabilityStructure a, Prob)
  fromFreqsCF = fromFreqsCFT . Map.assocs
  fromFreqsCFT :: [(a, Prob)] -> (ProbabilityStructure a, Prob)
  fromFreqsCFT [(val, freq)] = (Surely val, freq)
  fromFreqsCFT ((val1, freq) : rest) = let
    (psRest, freqRest) = fromFreqsCFT rest
    totalFreq = freq + freqRest
    in (Ehh val1 (freq / totalFreq) psRest, totalFreq)
  fromFreqs :: Map a Prob -> ProbabilityStructure a
  fromFreqs = fst . fromFreqsCF
  
  -- psApply takes a probability structure and an "index" and yields the element at that index.
  psApply :: ProbabilityStructure a -> Prob -> a
  psApply (Surely value) _ = value
  psApply (Ehh val1 probability next) roll
    | roll < probability = val1
    | otherwise = psApply next ((roll - probability) / (1 - probability))
  
  type MarkovChain a = Map a (ProbabilityStructure a)
  markovNext :: (Ord a) => MarkovChain a -> a -> Prob -> Maybe a
  markovNext chain prev roll = do
    possibilities <- prev `Map.lookup` chain
    return $ psApply possibilities roll
  
  markovNextSS :: (RandomGen g, Ord a) => (MarkovChain a, a, g) -> Maybe (MarkovChain a, a, g)
  markovNextSS (chain, prev, gen) = do
    let (roll, gen') = randomR (0.0, 1.0) gen
    next <- markovNext chain prev roll
    return (chain, next, gen')
  
  type FreqTally a = Map a (Map a Prob)
  
  -- takes a map from keys to frequency tallies, and returns a recognized Markov chain
  fromFreqTable :: FreqTally a -> MarkovChain a
  fromFreqTable = Map.map fromFreqs
  
  data MSNode a = Elem a | Start | End deriving (Eq, Ord, Show)
  
  tallyN :: (Ord a) => FreqTally (MSNode a) -> [a] -> FreqTally (MSNode a)
  tallyN tl str = tallyN' tl (Start : (map Elem str))
  tallyN' :: (Ord a) => FreqTally (MSNode a) -> [MSNode a] -> FreqTally (MSNode a)
  tallyN' tl [last] = tally2 tl (last, End)
  tallyN' tl (first:second:rest) = let
    tl' = tally2 tl (first, second)
    in tallyN' tl' (second:rest)
  
  tally2 :: (Ord a) => FreqTally a -> (a, a) -> FreqTally a
  tally2 tl (from, to) = let
    subtally = Map.findWithDefault Map.empty from tl
    count = Map.findWithDefault 0 to subtally -- Confusing yet?
    subtally' = Map.insert to (count + 1) subtally
    in Map.insert from subtally' tl
  
  tallyNN :: (Ord a) => FreqTally (MSNode a) -> [[a]] -> FreqTally (MSNode a)
  tallyNN tl strs = foldl tallyN tl strs -- leaving the arguments in for clarity