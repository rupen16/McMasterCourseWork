{- Assignment 3
 - Name: Rupen Patel
 - Date: TODO add of completion
 -}
module Assign_3 where

import qualified Data.Map.Strict as IM

macid :: String
macid = "pater45"

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show


newtype PolyList a = PolyList [a]
  deriving Show


{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: Function that takes an input of type Poly and a number n
 - and subs in n for any instance of x in the poly. It returns the value 
 - of the Poly at n. 
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue X n = n
polyValue (Coef p) n = p
polyValue (Sum p1 p2) n =  polyValue p1 n + polyValue p2 n
polyValue (Prod p1 p2) n = polyValue p1 n * polyValue p2 n
  


{- -----------------------------------------------------------------
 - polyHead and polyTail
 - -----------------------------------------------------------------
 - Description: These helper function take a PolyList and find its
 - head and tail, respectively and return it as a number and list,
 - respectively. 
 -}

polyHead :: (Num a) => PolyList a -> a
polyHead (PolyList (x:xs)) = x

polyTail :: (Num a) => PolyList a -> [a]
polyTail (PolyList (x:xs)) = xs


{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: This function takes a PolyList and a number and finds
 - the value of the PolyList at number n, using Horner's method.
 -}
polyListValue :: (Num a,Eq a) => PolyList a -> a -> a
polyListValue (PolyList []) n = 0
polyListValue  p1 n = polyHead (p1) + n * (polyListValue (PolyList (polyTail p1)) n)
 

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: This function takes two PolyLists and returns a PolyList
 - of the sum of two inputted PolyLists. It calls the helper function, 
 - polyListSumHelper, that runs through both lists, until it reaches one
 - of the base cases and sends it back to polyListSum.  
 -}

polyListSum :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum p1 q1 = PolyList (polyListSumHelper p1 q1)


polyListSumHelper :: (Num a,Eq a) => PolyList a -> PolyList a -> [a]
polyListSumHelper (PolyList []) (PolyList []) = []
polyListSumHelper p1 (PolyList []) =  (polyHead (p1)) : polyListSumHelper (PolyList (polyTail (p1))) (PolyList ([]))
polyListSumHelper (PolyList []) q1 =  (polyHead (q1)) : polyListSumHelper (PolyList (polyTail (q1))) (PolyList ([]))
polyListSumHelper p1 q1 = (polyHead (p1) + polyHead (q1)) : polyListSumHelper (PolyList (polyTail (p1))) (PolyList (polyTail (q1)))





{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: This function determines the degree of an inputted
 - PolyList, by adding 1 for everytime it runs through the recursion and
 - returning the final number.
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Integer
polyListDegree (PolyList ([])) = undefined
polyListDegree (PolyList ([_])) = 0
polyListDegree p1 = 1 + polyListDegree (PolyList (polyTail (p1)))


{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: This function doesn't work, as I couldn't figure it
 - out.
 -}

polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd p1 q1 = error " "




{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: This function takes an inputted polyList and return it
 - in Poly form. 
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList ([])) = Coef 0
polyListToPoly p1 = Sum (Coef (polyHead(p1))) (Prod (X) (polyListToPoly (PolyList (polyTail (p1)))))


{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: This function takes a Poly and returns it in PolyList
 - form. Note: This function won't be able to convert Prod functions, as
 - you would need the polyListProd function, which is not completed. 
 -}
polyToPolyList :: (Num a,Eq a) => Poly a -> PolyList a
polyToPolyList X = (PolyList [0,1])
polyToPolyList (Coef c) = (PolyList [c])
polyToPolyList (Sum b c) = polyListSum (polyToPolyList (b)) (polyToPolyList (c))
polyToPolyList (Prod b c) = error " "

{------------            Test Case Plan                         
-- -----------------------------------------------------------------
- - Function: polyValue
- - Test Case Number: 1
- - Input: (Sum (Prod X X) (Coef 3)) (4)
- - Expected Output: 19
- - Acutal Output: 19
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyValue
- - Test Case Number: 2
- - Input: (Sum (Prod X (Coef 4)) (Coef 5)) (2)
- - Expected Output: 19
- - Acutal Output: 19
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyValue
- - Test Case Number: 3
- - Input: (Sum (Sum (Coef 6) (Coef (-1))) (Coef 3)) (5)
- - Expected Output: 8
- - Acutal Output: 8
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListValue
- - Test Case Number: 4
- - Input: (PolyList [1,5,6]) 2
- - Expected Output: 35
- - Acutal Output: 35
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListvalue
- - Test Case Number: 5
- - Input: (PolyList [-3, 2, 3]) 5
- - Expected Output: 97
- - Acutal Output: 19
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListValue
- - Test Case Number: 6
- - Input: (PolyList [4, 0, 7]) 4
- - Expected Output: 116
- - Acutal Output: 116
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListSum
- - Test Case Number: 7
- - Input: (PolyList [6,0,2]) (PolyList [1,3,2])
- - Expected Output: PolyList [7,3,4]
- - Acutal Output: PolyList [7,3,4]
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListSum
- - Test Case Number: 8
- - Input: (PolyList [1,5,4]) (PolyList [3,0,2])
- - Expected Output: PolyList [4,5,6]
- - Acutal Output: PolyList [4,5,6]
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListSum
- - Test Case Number: 9
- - Input: (PolyList [3,0,1]) (PolyList [-2,5,6])
- - Expected Output: PolyList [1,5,7]
- - Acutal Output: PolyList [1,5,7]
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListDegree
- - Test Case Number: 10
- - Input: PolyList [0,2,3]
- - Expected Output: 2
- - Acutal Output: 2
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListDegree
- - Test Case Number: 11
- - Input: PolyList [4,6,5,4,3]
- - Expected Output: 4
- - Acutal Output: 4
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListDegree
- - Test Case Number: 12
- - Input: PolyList [0,1]
- - Expected Output: 1
- - Acutal Output: 1
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListToPoly
- - Test Case Number: 13
- - Input: PolyList [4,5,6]
- - Expected Output: Sum (Coef 4) (Prod X (Sum (Coef 5) (Prod X (Sum (Coef 6) (Prod X (Coef 0))))))
- - Acutal Output: Sum (Coef 4) (Prod X (Sum (Coef 5) (Prod X (Sum (Coef 6) (Prod X (Coef 0))))))
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListToPoly
- - Test Case Number: 14
- - Input: PolyList [-2, 2]
- - Expected Output: (Coef (-2)) (Prod X (Sum (Coef 2) (Prod X (Coef 0))))
- - Acutal Output: (Coef (-2)) (Prod X (Sum (Coef 2) (Prod X (Coef 0))))
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyListToPoly
- - Test Case Number: 15
- - Input: PolyList [0,0,1]
- - Expected Output: Sum (Coef 0) (Prod X (Sum (Coef 0) (Prod X (Sum (Coef 1) (Prod X (Coef 0))))))
- - Acutal Output: Sum (Coef 0) (Prod X (Sum (Coef 0) (Prod X (Sum (Coef 1) (Prod X (Coef 0))))))
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyToPolyList
- - Test Case Number: 16
- - Input: Sum (Coef 2) (X)
- - Expected Output: PolyList [2,1]
- - Acutal Output: PolyList [2,1]
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyToPolyList
- - Test Case Number: 17
- - Input: Sum (Sum (Coef 4) (Coef 3)) (X)
- - Expected Output: PolyList [7, 1]
- - Acutal Output: 19
- -----------------------------------------------------------------
-- -----------------------------------------------------------------
- - Function: polyToPolyList
- - Test Case Number: 18
- - Input: (Sum (Coef 3) (Coef 4))
- - Expected Output: PolyList [7]
- - Acutal Output: PolyList [7]
- -----------------------------------------------------------------
- -}











