class SumRes r where 
    sumOf :: Integer -> r

instance SumRes Integer where
    sumOf = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
    sumOf x = sumOf . (x +) . toInteger



class SumSum r where
    sumSum :: Integer -> r

instance SumSum Integer where
    sumSum = id
