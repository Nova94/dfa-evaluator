{- Author - Lisa Gray
   Version - 1.0
   Date - 4/24/15
-}

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import DFAJSON

--(set of States, Sigma, delta function, start state, isFinal)
type DFA = ([[Char]], [Char], (([Char], Char) -> [Char]), [Char], ([Char] -> [[Char]] -> Bool))

--sets deltaMapping to an empty map
deltaMapping :: Map.Map ([Char], Char) [Char]
deltaMapping = Map.empty
accs = []

--Checks whether current state is an accept state
inFinal :: Eq a => a -> [a] -> Bool
inFinal st l = st `elem` l

main = do
    dfa <- readJSON
    let q = states dfa --get Q from JSON
        s = sigma dfa -- get Sigma from JSON
        dmap = deltamap dfa --Get mapping of states from JSON
        ss = startstate dfa --get start state from dfa
        accs = acceptstates dfa --get list of Accept states
        --get mapping from json and store in delta mapping
        deltaMapping = Map.fromList (getDeltaMapJSON dmap)
        delta (q, x) = Maybe.fromJust ( Map.lookup (q,x) deltaMapping )
        --I had to define the evaluateDFA here because it didn't like it elsewhere
        evaluateDFA :: DFA -> [Char] -> Bool
        evaluateDFA (qset, sigma, d, ss, inFinal) [] = inFinal ss accs
        evaluateDFA (qset, sigma, d, ss, inFinal) [x] = inFinal (d (ss, x)) accs
        evaluateDFA (qset, sigma, d, ss, inFinal) (x:xs) =
            evaluateDFA (qset, sigma, d, ns, inFinal) xs
            where ns = d (ss, x)
    --empty Sigma = empty set language otherwise get string
    if (null s)
        then print True
        else do
            x <- getLine
            print deltaMapping
            print accs
            print (evaluateDFA (q,s,delta,ss,inFinal) x)

{- Original declarations to test evaluation
--myDFA
q = ["q0", "q1", "q2", "q3"]
s = ['0' , '1']

m :: DFA --[Char] Char
m = (q,s, delta, "q0", inFinal)

--make delta function
delta :: ([Char], Char) -> [Char]
delta (q, x) = Maybe.fromJust ( Map.lookup (q,x) deltaMapping )


evaluateDFA :: DFA -> [Char] -> Bool
evaluateDFA (qset, sigma, d, ss, inFinal) [] = inFinal ss accs
evaluateDFA (qset, sigma, d, ss, inFinal) [x] = inFinal (d (ss, x)) accs
evaluateDFA (qset, sigma, d, ss, inFinal) (x:xs) =
    evaluateDFA (qset, sigma, d, ns, inFinal) xs
    where ns = d (ss, x)
-}
