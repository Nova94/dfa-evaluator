import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

--(set of States, Sigma, delta function, start state, isFinal)
type DFA = ([[Char]], [Char], (([Char], Char) -> [Char]), [Char], ([Char] -> [[Char]] -> Bool))

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
delta (q, x) = Maybe.fromJust ( Map.lookup (q,x) deltaMapping )

inFinal st l = st `elem` l


evaluateDFA :: DFA -> [Char] -> Bool
evaluateDFA (qset, sigma, d, ss, inFinal) [] = inFinal ss accs
evaluateDFA (qset, sigma, d, ss, inFinal) [x] = inFinal (d (ss, x)) accs
evaluateDFA (qset, sigma, d, ss, inFinal) (x:xs) =
    evaluateDFA (qset, sigma, d, ns, inFinal) xs
    where ns = d (ss, x)
