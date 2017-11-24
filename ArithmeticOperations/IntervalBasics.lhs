> module IntervalBasics where
> import Numeric.IEEE
> import Interval
> import RoundedArithmetic
> import Data.List

> len :: Interval -> Double
> len z =  rb z - lb z
> {-
> measures the length of an interval, is NaN for the empty interval
> and infinity for[-infinity,infinity]
> -}
>
> dist :: Interval -> Interval -> Double
> dist z z' = max (lb z - rb z') (lb z'- rb z)
> {- gives the distance of two intervals-}

> {-
> The following function checks whether an interval is valid,
> it is True, when the left border is smaller than or equal to the right  and 
> returns False if rb < lb or if one of the borders is nan
> -}
> isIntValid :: Interval -> Bool
> isIntValid z =
>   if ( lb z <= rb z ) then True else False
>
> {-
> helper functions to classify intervals.
> We have 4 distinct classes of valid (i.e. also nonempty) intervals:
> mixed, positive, negative, zero
> -}

> isIntM :: Interval -> Bool
> isIntM z
>        | a < 0 && b > 0
>           = True
>        | otherwise
>           = False where
>        a = lb z
>        b = rb z
>
> isIntZ :: Interval -> Bool
> isIntZ z
>        | a == 0 && b == 0
>           = True
>        | otherwise
>           = False where
>        a = lb z
>        b = rb z

> isIntP :: Interval -> Bool
> isIntP z
>        | (isIntValid z) && ((a > 0) || (b > 0))
>           = True
>        | otherwise
>           = False where
>        a = lb z
>        b = rb z
        
> isIntN :: Interval -> Bool
> isIntN z
>        | (isIntValid z) && ((a < 0) || (b < 0))
>         = True
>        | otherwise
>         = False where
>        b = rb z
>        a = lb z
 
>
> isIntEmpty :: Interval -> Bool
> isIntEmpty z
>     | lb z > rb z
>        = True
>     | isNaN (lb z) == True
>        = True
>     | isNaN (rb z) == True
>        = True
>     | otherwise
>        = False
>
> {-
> refinement (partial) order on intervals:
> isIncl returns true only when the first interval is contained in the second
> -}
> 
> isIncl :: Interval -> Interval -> Bool
> isIncl z z'
>     | z == MkInterval nan nan
>       = True
>     | lb z' <= lb z && rb z <= rb z'
>       = True
>     | otherwise
>       = False




