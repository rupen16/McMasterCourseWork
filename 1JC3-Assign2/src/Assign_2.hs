{- Assignment 2
 - Name: Rupen Patel    
 - Date: 10/21/2019
 -}
module Assign_2 where

macid :: String
macid = "pater45"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: This function returns the real part of an inputted
 - Gaussian integer.
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x,y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description: This function returns the imaginary part of an inputted
 - Gaussian integer.
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (x,y) = y


{- -----------------------------------------------------------------
 - gausConj
 - -----------------------------------------------------------------
 - Description: This function returns the conjugate of an inputted
 - Gaussian Integer. 
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj g = (gaussReal g, gaussImag g * (-1))

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: This function adds two inputted Gaussian integers
 - together, by adding the real parts of both integers together and the 
 - imaginary parts of both integers together.
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = (gaussReal g0 + gaussReal g1 , gaussImag g0 + gaussImag g1)

{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: This function multiplies two inputted Gaussian 
 - integers together. I used a let statement to simplify the 
 - code.
 -}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult g0 g1 = 
    let
        a0 = gaussReal g0
        a1 = gaussReal g1
        b0 = gaussImag g0
        b1 = gaussImag g1
        in (a0*a1 - b0*b1, a0*b1 + b0*a1)


{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: This function implements the norm of an inputted
 - Gaussian integer. To find the norm, I used the gaussMult function
 - and multiplied the inputted Gaussian integer with its conjugate. 
 - Since the Norm is type Integer, I used a let statement to add the 
 - real and imaginary part of the Gaussian integer together.  
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm g =
    let 
        a = gaussMult g (gaussConj g)
        in gaussReal a + gaussImag a
        

{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: This function takes in a list of Gaussian integers 
 - and returns the Gaussian Integer with the biggest norm from the 
 - list. If an empty list is inputed, then (0,0) is returned, otherwise
 - it calls the helper function maxHelperGauss.
 -}
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm gs
    | gs == [] = (0,0)
    | otherwise = maxHelperGauss (tail gs) (head gs)

{- -----------------------------------------------------------------
 - maxHelperGauss
 - -----------------------------------------------------------------
 - Description: This function is a helper function for maxGaussNorm.
 - This function performs recursion where it will take the tail of
 - initial list (gs) and the head of the initial list (calls it max) 
 - and compares their norms. If the norm of the head of the list is 
 - less than or equal to the norm of the max, it will run the function 
 - again with the tail of gs as the list and the original max as the
 - max. If the norm of the head is greater than the norm of the max, it
 - will run the function with the tail of gs as the list and the head of
 - gs as the max. This continues to run until gs becomes an empty list,
 - in which the function will return the max, the Gaussian integer with
 - the largest norm.
 -}

maxHelperGauss :: [GaussianInt] -> GaussianInt -> GaussianInt
maxHelperGauss gs max
    | gs == [] = max
    | gaussNorm (head gs) <= gaussNorm (max) = maxHelperGauss (tail gs) max
    | gaussNorm (head gs) > gaussNorm (max) = maxHelperGauss (tail gs) (head gs)
  

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: (0, 0)
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 2
 - - Input: (1,5)
 - - Expected Output: (1,-5)
 - - Acutal Output: (1,-5)
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 3
 - - Input: (1,-5)
 - - Expected Output: (1,5)
 - - Acutal Output: (1,5)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 4
 - - Input: (1,5) (-2,6)
 - - Expected Output: (-1,11) 
 - - Acutal Output: (-1,11)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 5
 - - Input:(752555, 2365) (80000, 5025)
 - - Expected Output:(832555, 7390)
 - - Acutal Output: (832555, 7390)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 6
 - - Input:(-5005, 22) (5005, -22)
 - - Expected Output:(0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 7
 - - Input: (25, 13) (0,0)
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 8
 - - Input: (23,-2) (-5, 16)
 - - Expected Output:(832555, 7390)
 - - Acutal Output: (832555, 7390)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 9
 - - Input: (-2,7) (-5, 3)
 - - Expected Output: (-11,-41)
 - - Acutal Output: (-11,-41)
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 10
 - - Input: (2, 6)
 - - Expected Output: 40
 - - Acutal Output: 40
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 11
 - - Input: (-20, -30)
 - - Expected Output: 1300
 - - Acutal Output: 1300
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 12
 - - Input: (5555,6666)
 - - Expected Output: 75293581
 - - Acutal Output: 75293581
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 13
 - - Input: []
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 14
 - - Input: [(6,1), (2,4), (1,6), (0,2)]
 - - Expected Output: (6,1)
 - - Acutal Output: (6,1)
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 15
 - - Input: [(1,3), (-5,-6), (4,0), (5,4)]
 - - Expected Output: (-5,-6)
 - - Acutal Output: (-5,-6)
 - -----------------------------------------------------------------
 -}

