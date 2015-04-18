import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

--(set of States, Sigma, delta function, start state, isFinal)
type DFA = ([[Char]], [Char], (([Char], Char) -> [Char]), [Char], ([Char] -> [[Char]] -> Bool))
--type Q = [Char]
--data X a = Xcons a

--myDFA
q = ["q0", "q1", "q2", "q3"]
s = ['0' , '1']
deltaMapping = Map.fromList [ (("q0", '0'), "q0")
                            , (("q0", '1'), "q1")
                            {-, (("q1", '0'), "q3")
                            , (("q1", '1'), "q2")
                            , (("q2", '0'), "q2")
                            , (("q2", '1'), "q2")
                            , (("q3", '0'), "q3")
                            , (("q3", '1'), "q3")]-}

                            , (("q1", '0'), "q0")
                            , (("q1", '1'), "q2")
                            , (("q2", '0'), "q2")
                            , (("q2", '1'), "q1") ]

accs = ["q2"]

m :: DFA --[Char] Char
m = (q,s, delta, "q0", inFinal)

--make delta function
--delta :: (Q, Eq a) => (Q, a) -> Q
--delta :: (Ord a, Ord b) => (a, b) -> Map.Map (a, b) a -> a
delta (q, x) = Maybe.fromJust ( Map.lookup (q,x) deltaMapping )

inFinal st l = st `elem` l


evaluateDFA :: DFA -> [Char] -> Bool
evaluateDFA (qset, sigma, d, ss, inFinal) [] = inFinal ss accs
evaluateDFA (qset, sigma, d, ss, inFinal) [x] = inFinal (d (ss, x)) accs
evaluateDFA (qset, sigma, d, ss, inFinal) (x:xs) =
--  | null xs = inFinal ss accs
--  | null (x:xs) && inFinal ss accs = True
--  | null (x:xs) && not (inFinal ss accs) = False
--  | otherwise = 
    evaluateDFA (qset, sigma, d, ns, inFinal) xs
    where ns = d (ss, x)
