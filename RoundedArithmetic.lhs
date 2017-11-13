> module RoundedArithmetic where
> import Numeric.IEEE
>

  The IEEE standard 754-2008 requires availability of a function nextUp and similarily nextDown, 
  where nextUp(a) is the least floating-point number in the format of a that compares greater than a.
  The function nextUp thus computes the successor of a floating point number. This just amounts to adding eta, 
  the smallest subnormal positive number, if directed rounding is available(as requested by IEEE754).
  We use this idea a proposed in the 2009 paper"Computing predecessor and successor in rounding to the nearest"
  by Rump, Zimmermann et al. to give intervals that contain the results of the arithmetic operation.
  For add we produce a lower bound and an upper bound with the functions add_u (upper bound for add)
  and add_d for the lower bound. Our procedures will work in any rounding mode, i.e. the result of fl(a°b) is 
  garanteed to lie between those boundaries provided it is finite, where fl: Real -> Float
  denotes rounding to the nearest.
  The advantage of the given procedures are that they are fast and efficient in that they avoid a 
  switching of rounding modes.  The disadvantage is tha they do not always yield the tightest possible interval.
  For details see the proofs in the above mentioned paper.
  
 eps is the  distance from 1 to the next smaller floating point number
  
> eps :: Double
> eps = epsilon

 Let u denote the relative error unit, defined as half of the distance from 1 to its successor, i.e. 
 in binary representation
 u= 2^(-p) where p is the precision, then phi1 denotes its predecessor
 
> phi1 :: Double
> phi1 = eps * (1 + 2*eps)
 
  eta is the smallest positive subnormal floating point number

> eta :: Double
> eta = eps * (2 *  minNormal)
  
 
> add_u :: Double -> Double -> Double
> add_u x y = (x + y) + (((abs (x + y))* phi1) + eta)

> add_d :: Double -> Double -> Double
> add_d x y = (x + y) - (((abs (x + y))* phi1) + eta)

> sub_u :: Double -> Double -> Double
> sub_u x y = (x - y) + (((abs (x - y))* phi1) + eta)

> sub_d :: Double -> Double -> Double
> sub_d x y = (x - y) - (((abs (x - y))* phi1) + eta)

> mul_u :: Double -> Double -> Double
> mul_u x y = (x * y) + (((abs (x * y))* phi1) + eta)

> mul_d :: Double -> Double -> Double
> mul_d x y = (x * y) - (((abs (x * y))* phi1) + eta)

> div_u :: Double -> Double -> Double
> div_u x y = (x / y)  + (((abs (x / y))* phi1) + eta)

> div_d :: Double -> Double -> Double
> div_d x y = (x / y) - (((abs (x / y))* phi1) + eta)
>
> 



