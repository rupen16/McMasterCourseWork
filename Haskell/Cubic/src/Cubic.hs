{- Assignment 1
 - Name: Rupen Patel
 - Date: 09/29/2019
 -}
module Assign_1 where

macid :: String
macid = "pater45"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: cubicQ is a function that takes values a, b and c
 from the cubic equation ax^3+bx^2+cx+d = 0 and computes a value (q)
 that is needed to figure out the roots of a cubic equation.
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = (3*a*c - b**2)/(9*(a**2))

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: cubicR is a function that takes values a, b, c and d
 from the cubic equation ax^3+bx^2+cx+d = 0 and computes a value (r)
 that is needed to figure out the roots of a cubic equation. 
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = (9*a*b*c - 27*(a**2)*d - 2*(b**3))/(54*(a**3))

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: cubicDisc is a function that determines the discriminant 
 of a cubic equation. It takes the values q and r, which were calculated in
 other functions, and computes the discriminant of the equation, which helps
 us figure out how many roots the equation has. 
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = (q ** 3) + (r ** 2)

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: cubicS is a function that takes values q and r, which were
 calculated in the functions cubicQ and cubicR, and computes a value (s)
 that is needed to figure out the roots of a cubic equation.
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cbcR (r + sqrt(cubicDisc q r)) 

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: cubicT is a function that takes values q and r, which were
 calculated in the functions cubicQ and cubicR, and computes a value (t)
 that is needed to figure out the roots of a cubic equation.
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cbcR (r - sqrt(cubicDisc q r))

{- -----------------------------------------------------------------
 - cbcR
 - -----------------------------------------------------------------
 - Description: cbcR is a function that simply finds the cubic root
 of a function. However, since Haskell can't directly take the cubic
 root of negative numbers, there has to be some restrictions. If the 
 argument (a) is greater than or equal to 0, then proceed by finding
 the cube root of a. Otherwise, multiply a by -1 (to turn it positive), 
 find the cube root of that and then multiply it by -1.  
 -}
cbcR :: Double -> Double 
cbcR a = if a >= 0 then (a) ** (1/3) else ((a*(-1)) ** (1/3)) * (-1)

{- -----------------------------------------------------------------
 - x1
 - -----------------------------------------------------------------
 - Description: x1 is a function that is used to calculate the first
 root of a cubic equation. It takes the arguments, s, t , a and b 
 (the latter 2 from the cubic equation ax^3+bx^2+cx+d = 0) and computes 
 the value of the first root of the cubic function.
 -}
x1 :: Double -> Double -> Double -> Double -> Double
x1 s t a b = (s + t - (b/(3*a)))

{- -----------------------------------------------------------------
 - x2
 - -----------------------------------------------------------------
 - Description: x2 is a function that is used to calculate the second
 root of a cubic equation. It takes the arguments, s, t , a and b 
 (the latter 2 from the cubic equation ax^3+bx^2+cx+d = 0) and computes 
 the value of the second root of the cubic function.
 -}
x2 :: Double -> Double-> Double -> Double -> Double
x2 s t a b = (-1)*((s + t)/2) - (b/(3*a))


{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: cubicRealSolutions is a function that displays all the 
 roots of a cubic equation in a list. It takes the values of a, b, c and
 d from the cubic equation ax^3+bx^2+cx+d = 0 and places them in the cubicDisc
 function, so it can return the right number of roots. If the discriminant >= 0, 
 it'll return a list containing 1 value [function x1], if the discriminant == 0, 
 it'll return a list containing 3 values [funxtion x1, function x2, function x2],
 otherwise it'll return [] (empty list).  
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
    | cubicDisc q r > 0 = [x1 s t a b ]
    | cubicDisc (cubicQ a b c) (cubicR a b c d) == 0 = [x1 s t a b, x2 s t a b, x2 s t a b] 
    | otherwise  = []
    where 
        s = cubicS (cubicQ a b c) (cubicR a b c d)
        t = cubicT (cubicQ a b c) (cubicR a b c d)
        q = cubicQ a b c
        r = cubicR a b c d 
    
  
{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}

-- Requirements stated that test cases were not required