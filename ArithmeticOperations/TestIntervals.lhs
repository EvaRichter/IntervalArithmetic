> module TestIntervals where
> import IntervalType


intervals with names for testpurposes

> 
> i-i5 = MkInterval (-infinity) 5
> i-12-7 = MkInterval (-12) (-7)
> i-4+2 = MkInterval (-4) 2
> i-2+5 = MkInterval (-2) 5
> i-2+i = MkInterval (-2) infinity
> i-1+1 = MkInterval (-1) 1
> i+1-4 = MkInterval 1 (-4)
> i+1+1.5 = MkInterval 1 1.5
> i+3+6 = MkInterval 3 6
