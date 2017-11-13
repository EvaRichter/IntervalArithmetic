> module IntervalLists where
> import IntervalType
> import IntervalArith
> import Numeric.IEEE
> import RoundedArithmetic
> import Data.List


Arithmetic operations on intervals mostly  yield intervals, the exception is  when a division by a
zero containing interval is performed: then the result is a pair of intervals. This may also be the case in
general functions applied to intervals. Resulting lists of intervals are interpreted as "or".
In this module we extend arithmetics of intervals to arithmetic on lists of intervals.

Intervals form a downwards bounded lattice wrt. set inclusion, the smallest element (bottom) is the empty interval.

 meet is the infimum in wrt. this order

> meet:: Interval -> Interval -> Interval
> meet (MkInterval a b) (MkInterval a' b')
>  = if validInt z'' then z'' else MkInterval nan nan
>      where a'' = max a a'
>            b'' = min b b'
>            z'' = (MkInterval a'' b'')


  melt is either the supremum of two intervals or the first component. It aggregates intervals with nonempty
  meet, and gives the first interval in case of disjoint intervals.


> melt:: Interval -> Interval -> Interval
> melt z z' = if validInt( meet z z') then z'' else z
>                                 where a'' = min (lb z) (lb z')
>                                       b'' = max (rb z) (rb z')
>                                       z'' = (MkInterval a'' b'')
> 
 


If we have as a result of some calculations a list of intervals we want to clean it, i.e. remove duplicates
and melt together overlapping intervals to get a shorter list of disjoint intervals.

cleanList creates a list of disjoint intervals from a list of intervals(and removes duplicates)

> cleanList :: [Interval] -> [Interval]
> cleanList [] = []
> cleanList (z : zs) = (foldl melt z zsdm) : zsdn where
>   zsd = cleanList zs 
>   (zsdm, zsdn) = partition (validInt.(meet z)) zsd


arithmetic on interval lists


> addIL :: [Interval] -> [Interval] -> [Interval]
> addIL [] zs = []
> addIL zs [] = []
> addIL (z : zs) zs' = cleanList ((map (add z)  zs') ++ (addIL zs zs'))
>

> subIL :: [Interval] -> [Interval] -> [Interval]
> subIL [] zs = []
> subIL zs [] = []
> subIL (z : zs) zs' = cleanList ((map (sub z)  zs') ++ (subIL zs zs'))
> 
> mulIL :: [Interval] -> [Interval] -> [Interval]
> mulIL [] zs = []
> mulIL zs [] = []
> mulIL (z : zs) zs' = cleanList ((map (mul z)  zs') ++ (mulIL zs zs'))
>
> divIL :: [Interval] -> [Interval] -> [Interval]
> divIL [] zs = []
> divIL zs [] = []
> divIL (z : zs) zs' = cleanList (concat ((map (divI z)  zs')) ++ (divIL zs zs'))


