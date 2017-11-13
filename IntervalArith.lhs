> module IntervalArith where
> import IntervalType
> import Numeric.IEEE
> import RoundedArithmetic
> import Data.List
>

 implementation of arithmetic operations +,-,*,/ for intervals using the directed rounded operations add_u,etc.

> add :: Interval -> Interval -> Interval
> add x y = MkInterval (add_d (lb x)(lb y)) (add_u (rb x)(rb y))
>
> sub :: Interval -> Interval -> Interval
> sub x y = MkInterval (sub_d (lb x)(lb y)) (sub_u (rb x)(rb y))
>
> leftIntList :: Interval -> Interval -> [ Double]
> leftIntList x y =( mul_d (lb x)(lb y)): ((mul_d (lb x)(rb y)): ((mul_d (rb x)(lb y)):(( mul_d (rb x)(rb y)):[])))

> rightIntList :: Interval -> Interval -> [ Double]
> rightIntList x y =( mul_u (lb x)(lb y)): ((mul_u (lb x)(rb y)): ((mul_u (rb x)(lb y)):(( mul_u (rb x)(rb y)):[])))
>

------ nan cannot serve as the value of min/max on the empty list

> mul :: Interval -> Interval -> Interval
> mul x y = MkInterval (foldr (min) (infinity) (leftIntList x y)) (foldr (max)(-infinity) (rightIntList x y))
>

> divI ::  Interval -> Interval -> [Interval]
> divI z z' 
>   |isIntZ z' == False
>        = [mul z (MkInterval (div_d 1 d) ( div_u 1 c))]
>   |isIntZ z && isIntZ z'
>        = [MkInterval (-infinity)(infinity)]
>   | (b < 0) && (c < d) && (d == 0)
>        = [MkInterval (div_d b c) infinity]
>   | (b < 0) && isIntM z'
>        = [(MkInterval (-infinity)(div_u b d)), (MkInterval (div_d b c) infinity)]
>   | (b < 0) && (c == 0) && (d > 0)
>        = [MkInterval (-infinity) (div_u b d)]
>   | (0 < a) && (c < d)&& (d == 0) 
>        = [MkInterval (-infinity) (div_u a c)]
>   | (0 < a) && isIntM z'
>        = [(MkInterval (-infinity)(div_u a c)), (MkInterval (div_d a d) infinity)]
>   | (0 < a) && (c == 0) && (c < d)
>        = [MkInterval (div_d a d) (infinity)]
>   | otherwise = [MkInterval nan nan]
>   where a = lb z
>         b = rb z
>         c = lb z'
>         d = rb z'
>
