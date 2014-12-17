radius :: Integer -> Double  
radius r = 2 * pi * fromIntegral(r)

push :: (Num t) => t -> [t] -> [t]
push a l = a : l

(>>>) :: (Num t) => t -> [t] -> [t]
(>>>) = push
