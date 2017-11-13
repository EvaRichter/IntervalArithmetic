> module IntervalType where
> import Numeric.IEEE
> import RoundedArithmetic
> import Data.List

 implements operations for set-based intervals, i.e. also unbounded
 intervals and empty set are allowed,

 -- validInt gives True when left border is smaller right border (inclusive infinities)
 -- the empty interval will be represented by [nan,nan]

 the empty interval is represented by [nan,nan]


> data Interval = MkInterval Double Double
>
 

> instance Show Interval where
>      show (MkInterval a b) = "["++(show a)++";" ++ (show b)++"]"
>

 instance ReadS Interval where
      readInt (']':s) = [((MkInterval a b), t)| ( a,  r)    <- readInt  s ,
                                                  ( ',', r2)  <- readInt  r ,
                                                  (b, '[':t) <- readInt  r2  ]

 readInt :: ReadS Interval
 readInt (']':s) = [((MkInterval a b), t)| ( a,  r)    <- read  s ,
                                           ( ',', r2)  <- read  r ,
                                           (b, '[': t) <- read r2  ]

> lb :: Interval -> Double 
> lb (MkInterval a b) = a
> 
> rb :: Interval -> Double
> rb (MkInterval a b) = b
>

check whether an interval is valid, i.e. left border is smaller than right one and returns False
if one of the borders is nan

> validInt :: Interval -> Bool
> validInt (MkInterval a b) =
>   if ( a <= b ) then True else False
>

 
 Helper functions to classify intervals. We have 4 distinct classes of intervals: mixed, positive, negative, zero

> isIntM :: Interval -> Bool
> isIntM z
>        | a < 0 && b > 0
>         = True
>        | otherwise
>         = False where
>        a = lb z
>        b = rb z
>
> isIntZ :: Interval -> Bool
> isIntZ z
>        | a == 0 && b == 0
>         = True
>        | otherwise
>         = False where
>        a = lb z
>        b = rb z


> isIntP :: Interval -> Bool
> isIntP z
>        | (validInt z) && a > 0
>         = True
>        | otherwise
>         = False where
>        a = lb z
>        b = rb z
>
> isIntN :: Interval -> Bool
> isIntN z
>        | (validInt z) && b < 0
>         = True
>        | otherwise
>         = False where
>        a = lb z
>        b = rb z
  


 

 
 


  
-------function to produce an Interval from two Doubles,
       derive  bounds of an interval,
       test for empty intervals

> num2Int :: Double -> Double -> Interval
> num2Int a b = MkInterval a b
>
> givebs :: Interval -> (Double, Double)
> givebs (MkInterval a b) = (a,b)
>
> isEmpty :: Interval -> Bool
> isEmpty z
>     | lb z > rb z
>        = True
>     | isNaN (lb z) == True
>        = True
>     | isNaN (rb z) == True
>        = True
>     | otherwise
>        = False

> 
> 
> 
> 
>
>
>
>



