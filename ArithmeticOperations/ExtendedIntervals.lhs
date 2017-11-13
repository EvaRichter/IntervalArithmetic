> module ExtendedIntervals where
> import Intervals
> import Numeric.IEEE
> import RoundedArithmetic

** extended intervals is an extension of the data type Interval, containing halfopen, empty and full interval 


> data ExtInt =
>    MkTrueInt Interval
>  | MkLOInt LeftOpen
>  | MkROInt RightOpen
>  | EmptyInt
>  | FullInt

> leftb :: ExtInt -> Double
> leftb (MkTrueInt z) = lb z
> leftb (MkLOInt z) = -infinity
> leftb (MkROInt (MkRiOp x)) = x
> leftb EmptyInt = nan
> leftb FullInt = -infinity

> rightb :: ExtInt -> Double
> rightb (MkTrueInt z) = rb z
> rightb (MkLOInt (MkLeOp x)) = x
> rightb (MkROInt z) =  infinity
> rightb EmptyInt = nan
> rightb FullInt = infinity

> haszeroE :: ExtInt -> Bool
> haszeroE (MkTrueInt z) = if  (lb z)*(rb z) <= 0 then True else False
> haszeroE (MkLOInt (MkLeOp y)) = if ( y < -0)then False else True
> haszeroE (MkROInt (MkRiOp x)) = if (x > 0)then False else True
> haszeroE EmptyInt = False
> haszeroE FullInt = True

 div :: ExtInt -> ExtInt -> ExtInt 
> div (MkInterval a )(MkInterval b ) 
>   | haszero (MkTrueInt a) == True
>        = FullInt
>   | ((lb b) == 0) && ((rb b)) == 0
>        = EmptyInt
>   | (rb a < 0) && (lb b < rb b) && (rb b == 0)
>        = MkROInt (MkRiOp (rb a /lb b))
>   | (rb a < 0) &&  (lb b == 0) && (lb b < rb b)
>        = MkLOInt (MkLeOp (rb a /rb b))
>   | (lb a > 0) && (lb b < rb b) && (rb b == 0)
>        = MkLOInt (MkLeOp (lb a /lb b))
>   | (lb a > 0) &&  (lb b == 0) && (lb b < rb b)
>        = MkROInt (MkRiOp (lb a /rb b))
>
> 
> divI :: ExtInt -> ExtInt -> ExtInt
> divI (MkTrueInt a)(MkTrueInt b) 
>   |haszero (MkTrueInt b) == False
>           = MkTrueInt (mul a (MkInterval (1/(rb b)) (1/(lb b))))
>   |otherwise = divzero (MkTrueInt a)(MkTrueInt b)
> divI EmptyInt EmptyInt = EmptyInt
> divI EmptyInt (MkTrueInt z) = EmptyInt
> divI (MkTrueInt z) EmptyInt = EmptyInt

ALLE ANDEREN FÄLLE ERGÄNZEN!!!!           

