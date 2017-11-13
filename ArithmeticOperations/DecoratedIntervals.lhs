> module DecoratedIntervals where
> import IntervalType

% defines a type of decorated intervals

Decorations attached to an interval reflects the available knowledge about
whether this intermediate function is guaranteed to be everywhere defined,
continuous, bounded etc. on given inputs.  There are 7 different qualities:
bounded, safe, defined, containing, empty, ill-formed, empty input.

> data Dec = Bnd | Saf | Def | Con | Emp | Ill | Ein
> data DecInt = MkDecInt Interval Dec

> initDec :: Interval -> DecInt
> initDec z
>         | isNaN (lb z) =  MkDecInt z Ein
>         | isNaN (rb z) = MkDecInt z Ein
>         | lb z > rb z = MkDecInt z Ill
>         | isInfinite (lb z) &&  (lb z) == (rb z) = MkDecInt z Emp
>         | otherwise = MkDecInt z Saf
