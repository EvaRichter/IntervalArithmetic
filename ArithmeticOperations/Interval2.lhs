> module IntervalArithmetic.ArithmeticOperations.Interval2 where
> import Numeric.IEEE
> import Data.List

 implements operations for set-based intervals, i.e. also unbounded
 intervals and empty set are allowed,

 -- validInt gives True when left border is smaller right border (inclusive infinities)
 -- the empty interval will be represented by [nan,nan]

 the empty interval is represented by [nan,nan]


> data Interval = Empty|IV Double Double
>
 

> instance Show Interval where
>      show Empty            = "[e]"
>      show (IV a b) = "["++(show a)++";" ++ (show b)++"]"
>

 instance ReadS Interval where
      readInt (']':s) = [((IV a b), t)| ( a,  r)    <- readInt  s ,
                                                  ( ',', r2)  <- readInt  r ,
                                                  (b, '[':t) <- readInt  r2  ]

 readInt :: ReadS Interval
 readInt (']':s) = [((IV a b), t)| ( a,  r)    <- read  s ,
                                           ( ',', r2)  <- read  r ,
                                           (b, '[': t) <- read r2  ]

> lb :: Interval -> Double 
> lb (IV a b) = a
> lb Empty = nan
> 
> rb :: Interval -> Double
> rb (IV a b) = b
> rb Empty = nan
>
> bounds :: Interval -> (Double, Double)
> bounds z = (lb z, rb z)
>
> num2Int :: Double -> Double -> Interval
> num2Int a b = IV a b
>
> emptyInt :: Interval
> emptyInt = Empty
>
> {-
> while nan == nan returns false for Doubles we want to have
> Empty = Empty to return True, as we  use it as representation
> for the empty interval, but now [nan,infty]=[infty,nan], not good.
> 
> 
> instance Eq Interval where
>   z == z'= ((not (lb z  <= rb z)  && not (rb z  <= lb z)) &&
>             (not (lb z' <= rb z') && not (rb z' <= lb z'))) ||
>            (lb z == lb z' && rb z == rb z')
> -}
> 
> instance Eq Interval where
>   Empty == Empty = True
>   z == z'        = (lb z == lb z' && rb z == rb z')
