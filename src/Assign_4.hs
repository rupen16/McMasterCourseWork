{- Assignment 4
 - Name: Rupen Patel
 - Date: 11/17/2019
 -}
module Assign_4 where

import Test.QuickCheck

macid :: String
macid = "pater45"

data MathExpr a = X
                | Coef a
                | Sum (MathExpr a) (MathExpr a)
                | Prod (MathExpr a) (MathExpr a)
                | Quot (MathExpr a) (MathExpr a)
                | Exp (MathExpr a)
                | Log (MathExpr a)
                deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - value
 - -----------------------------------------------------------------
 - Description: This function take an expression of type MathExpr and
 - a number and finds the value of the expression at that number.
 -}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value X n = n
value (Coef c) n = c
value (Sum a b) n = (value a n) + (value b n)
value (Prod a b) n = (value a n) * (value b n)
value (Quot a b) n = (value a n) / (value b n)
value (Exp a) n = exp(value a n)
value (Log a) n = log(value a n)

propValue :: Double -> Double -> Bool
propValue a b = value X b == b &&
                value (Coef a) b == a &&
                value (Sum (Coef a) (Coef a)) b == 2 * a &&
                value (Prod (Coef a) (Coef a)) b == a ** 2 &&
                value (Exp (Coef a)) b == exp(a) 
                


{- -----------------------------------------------------------------
 - simp
 - -----------------------------------------------------------------
 - Description: This function takes an expression of type MathExpr
 - and applies the simplication rules of 1s and 0s and outputs a 
 - simplified version of the expression.
 -}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp (Sum u (Coef 0)) = simp u
simp (Sum (Coef (0)) u) = simp u 
simp (X) = X
simp (Coef c) = Coef c
simp (Prod u (Coef 0)) = Coef 0
simp (Prod (Coef 0) u) = Coef 0
simp (Prod u (Coef 1)) = simp u
simp (Prod (Coef 1) u) = simp u
simp (Quot u (Coef 1)) = simp u
simp (Exp (Coef 0)) = Coef 1
simp (Log (Coef 1)) = Coef 0
simp (Sum u v) 
    | simp u == u && simp v == v = (Sum u v)
    | otherwise = simp (Sum (simp u) (simp v))
simp (Prod u v)
    | simp u == u && simp v == v = (Prod u v)
    | otherwise = simp (Prod (simp u) (simp v))
simp (Quot u v)
    | simp u == u && simp v == v = (Quot u v)
    | otherwise = simp (Quot (simp u) (simp v))
simp (Exp u)
    | simp u == u = (Exp u)
    | otherwise = simp (Exp (simp u))
simp (Log u)
    | simp u == u = (Log u)
    | otherwise = simp (Log (simp u))





{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description: This function takes an expression of type MathExpr and
 - finds the derivative of that expression and outputs the derivative as
 - an expression of type MathExpr.
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff (X) = (Coef 1)
diff (Coef c) = (Coef 0)
diff (Sum x y) = (Sum (diff x) (diff y))
diff (Prod x y) = (Sum (Prod (diff x) (y)) (Prod (x) (diff y)))
diff (Quot x y) = (Quot (Sum (Prod (diff x) (y)) (Prod (Prod x (Coef (-1))) (diff y))) (Prod (y) (y)))
diff (Exp x) = (Prod (Exp x) (diff x))
diff (Log x) = (Quot (diff x) (x))



{- -----------------------------------------------------------------
 - readDiffWrite
 - -----------------------------------------------------------------
 - Description: This function takes 2 files, in which one of them 
 - contains a math expression. It then converts that expression to
 - type MathExpr Double, differentiates and simplifies it, converts it 
 - back to a string and writes the simplified derivative to the other
 - file.
 --}

readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite f g =
    do a <- readFile f
       let b = read a :: MathExpr Double
       let c = diff b
       let d = simp c 
       let e = show d
       writeFile g e
       

    
{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 1
 - - Input: (Prod (Coef 3) (Sum X (Exp (Prod (X)(X))))) 2
 - - Expected Output: 169.7944501
 - - Acutal Output: 169.7944501
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 2
 - - Input: (Sum (Log (Prod (Coef 3) X)) (Quot (Prod (Coef 2) X) (Coef 7))) 5 
 - - Expected Output: 4.13662163
 - - Acutal Output: 4.13662163
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 3
 - - Input: (Exp (Sum (Prod X X) (Log (Quot (Coef (-2) (X)))) (-3)
 - - Expected Output: 5402.055952
 - - Acutal Output: 5402.055952
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 4
 - - Input: (Prod (Sum (Prod (X) (Coef 5)) Coef 0) (Quot (Prod (Coef 2) (X)) X))
 - - Expected Output: Prod (Prod X (Coef 5)) (Quot (Prod (Coef 2) X) X)
 - - Acutal Output: Prod (Prod X (Coef 5)) (Quot (Prod (Coef 2) X) X)
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 5
 - - Input: (Prod (Coef 1) (Sum (Coef 0) (Exp (0))))
 - - Expected Output: Coef 1
 - - Acutal Output: Coef 1
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 6
 - - Input: (Sum (Prod (Log (Coef 1)) X) (Quot (Prod X X) (Coef 1)))
 - - Expected Output: Prod X X
 - - Acutal Output: Prod X X
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 7
 - - Input: Exp (Sum (Prod X (Coef 0)) (Log (Sum (Coef 5) (Coef 0))))
 - - Expected Output: Exp (Log (Coef 5))
 - - Acutal Output: Exp (Log (Coef 5))
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 8
 - - Input: (Sum (X) (Coef 6))
 - - Expected Output: Sum (Coef 1.0) (Coef 0.0)
 - - Acutal Output: Sum (Coef 1.0) (Coef 0.0)
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 9
 - - Input: (Prod (Sum (Exp X) (Log (Coef 5))) (X))
 - - Expected Output: Sum (Prod (Sum (Prod (Exp X) (Coef 1.0)) (Quot (Coef 0.0) (Coef 5.0))) X) (Prod (Sum (Exp X) (Log (Coef 5.0))) (Coef 1.0))
 - - Acutal Output: Sum (Prod (Sum (Prod (Exp X) (Coef 1.0)) (Quot (Coef 0.0) (Coef 5.0))) X) (Prod (Sum (Exp X) (Log (Coef 5.0))) (Coef 1.0))
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 10
 - - Input: (Quot (Prod (X) (Sum (Prod (Coef 3) (X)) (Coef 5))) (X))
 - - Expected Output: Quot (Sum (Prod (Sum (Prod (Coef 1.0) (Sum (Prod (Coef 3.0) X) (Coef 5.0))) (Prod X (Sum (Coef 3.0) (Coef 0.0)))) X) (Prod (Prod (Prod X (Sum (Prod (Coef 3.0) X) (Coef 5.0))) (Coef (-1.0))) (Coef 1.0))) (Prod X X)
 - - Acutal Output: Quot (Sum (Prod (Sum (Prod (Coef 1.0) (Sum (Prod (Coef 3.0) X) (Coef 5.0))) (Prod X (Sum (Coef 3.0) (Coef 0.0)))) X) (Prod (Prod (Prod X (Sum (Prod (Coef 3.0) X) (Coef 5.0))) (Coef (-1.0))) (Coef 1.0))) (Prod X X)
 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 11
 - - Input: writeFile "test.txt" "(Sum (X) (Coef 6))"
 - -        readDiffWrite "test.txt" "new.txt"
 - -        readFile "new.txt"
 - - Expected Output: "Coef 1"
 - - Acutal Output: "Coef 1"
 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 12
 - - Input: writeFile "test1.txt" "(Sum (Prod (X) (Coef 0)) (Prod (Coef 3) (X)))"
 - -        readDiffWrite "test1.txt" "new1.txt"
 - -        readFile "new1.txt"
 - - Expected Output: "Coef 3"
 - - Acutal Output: "Coef 3"
 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 13
 - - Input: writeFile "test2.txt" "(Quot (Prod (X) (Sum (Prod (Coef 3) (X)) (Coef 5))) (X))"
 - -        readDiffWrite "test2.txt" "new2.txt"
 - -        readFile "new2.txt"
 - - Expected Output: "Quot (Sum (Prod (Sum (Sum (Prod (Coef 3.0) X) (Coef 5.0)) (Prod X (Coef 3.0))) X) (Prod (Prod X (Sum (Prod (Coef 3.0) X) (Coef 5.0))) (Coef (-1.0)))) (Prod X X)"
 - - Acutal Output: "Quot (Sum (Prod (Sum (Sum (Prod (Coef 3.0) X) (Coef 5.0)) (Prod X (Coef 3.0))) X) (Prod (Prod X (Sum (Prod (Coef 3.0) X) (Coef 5.0))) (Coef (-1.0)))) (Prod X X)"
 - -----------------------------------------------------------------
 - - Function: value
 - - Property: propValue :: Double -> Double -> Bool
 - -           propValue a b = value X b == b &&
 - -                           value (Coef a) b == a &&
 - -                           value (Sum (Coef a) (Coef a)) b == 2 * a &&
 - -                           value (Prod (Coef a) (Coef a)) b == a ** 2 &&
 - -                           value (Exp (Coef a)) b == exp(a) 
 - - Actual Test Result: Pass               
 -}

