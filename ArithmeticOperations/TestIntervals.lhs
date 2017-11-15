> module TestIntervals where
> import IntervalType
> import Numeric.IEEE


intervals with names for testpurposes

> intv :: Double -> Double -> Interval
> intv x y =MkInterval x y
> 
> i_i_5 = MkInterval (-infinity) 5
> i_12-7 = MkInterval (-12) (-7)
> i_1-4 = MkInterval 1 (-4)
> i_1+1.5 = MkInterval 1 1.5
> i_3+6 = MkInterval 3 6

