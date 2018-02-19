> module IntervalArithmetic.IntervalArithmetic where
> import Intervals.IntervalType
> import Intervals.IntervalProp
> import Intervals.IntervalOps
> import Numeric.IEEE
> import IntervalArithmetic.RoundedArithmetic
> import Data.List
> import Control.Exception
>

 implementation of arithmetic operations +,-,*,/ for intervals using the
 directed rounded operations add_u,etc.

> add :: Interval -> Interval -> Interval
> add x y = num2Int (add_d (lb x)(lb y)) (add_u (rb x)(rb y))
>
> sub :: Interval -> Interval -> Interval
> sub x y = num2Int (sub_d (lb x)(rb y)) (sub_u (rb x)(lb y))
>
> leftIntList :: Interval -> Interval -> [ Double]
> leftIntList x y =( mul_d (lb x)(lb y)): ((mul_d (lb x)(rb y)):
>                  ((mul_d (rb x)(lb y)):(( mul_d (rb x)(rb y)):[])))

> rightIntList :: Interval -> Interval -> [ Double]
> rightIntList x y =( mul_u (lb x)(lb y)): ((mul_u (lb x)(rb y)):
>                   ((mul_u (rb x)(lb y)):(( mul_u (rb x)(rb y)):[])))
>

------ nan cannot serve as the value of min/max on the empty list

> mul :: Interval -> Interval -> Interval
> mul x y = num2Int (foldr (min) (infinity) (leftIntList x y))
>                      (foldr (max)(-infinity) (rightIntList x y))
>
> -- division where divisor does not contain zeros, result is always a single interval,
> -- the exception of dividing by zero is handled by the control.exception module
> 
> divplus :: Interval -> Interval -> Interval
> divplus z z' = mul z (num2Int (div_d 1 d) ( div_u 1 c))
>         where c = lb z'
>               d = rb z' 

> divI ::  Interval -> Interval -> [Interval]
> divI z z' 
>   |isIntZ z' == False
>        = [mul z (num2Int (div_d 1 d) ( div_u 1 c))]
>   |isIntZ z && isIntZ z'
>        = [num2Int (-infinity)(infinity)]
>   | (b < 0) && (c < d) && (d == 0)
>        = [num2Int (div_d b c) infinity]
>   | (b < 0) && isIntM z'
>        = [(num2Int (-infinity)(div_u b d)), (num2Int (div_d b c) infinity)]
>   | (b < 0) && (c == 0) && (d > 0)
>        = [num2Int (-infinity) (div_u b d)]
>   | (0 < a) && (c < d)&& (d == 0) 
>        = [num2Int (-infinity) (div_u a c)]
>   | (0 < a) && isIntM z'
>        = [(num2Int (-infinity)(div_u a c)), (num2Int (div_d a d) infinity)]
>   | (0 < a) && (c == 0) && (c < d)
>        = [num2Int (div_d a d) (infinity)]
>   | otherwise = [num2Int nan nan]
>   where a = lb z
>         b = rb z
>         c = lb z'
>         d = rb z'
>
> sqrtI :: Interval -> Interval
> sqrtI z
>     = num2Int (sqrt (lb z)) (sqrt (rb z))
>   
>
> instance Num Interval where
>  negate z    = num2Int (- (rb z))(- (lb z))
>
>  z + z' = add z z'
> 
>  z * z' = mul z z'
> 
>  abs z
>   | isIntP z
>     = z
>   | isIntM z
>     = num2Int 0 (len z)
>   | isIntN z
>     = negate z
>   | isIntZ z
>     = num2Int 0 0
>   | otherwise
>     = num2Int nan nan
>
>  signum z
>   | isIntP z
>     = num2Int 1 1
>   | isIntM z
>     =num2Int ((lb z)/len z)((rb z)/len z) 
>   | isIntN z
>     = num2Int (-1) (-1)
>   | isIntZ z
>     = num2Int 0 0
>   | otherwise
>     = num2Int nan nan
> 
>  fromInteger k = num2Int (fromInteger k) (fromInteger k)
