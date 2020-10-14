module Utils (
    chunksOf,
    indexedFilter,
    flatten,
    reduce,
    replaceAtWith,
    repeatEach
) where

chunksOf :: Int -> [u] -> [[u]]
chunksOf n l
    | (length l == 0) = []
    | (n > 0) = (take n l) : (chunksOf n (drop n l))
    | otherwise = error "Non positive size of chunk"

flatten :: [[a]] -> [a]
flatten [] = []
flatten (h:t) = h ++ flatten t 

indexedFilter :: ((a, Int, [a]) -> Bool) -> [a] -> [a]
indexedFilter f l = (map (\(el,_, _) -> el) [x | x <- (mapIdx 0 l l), (f x) == True])
    where 
        mapIdx i (e:r) a = [(e, i, a)] ++ (mapIdx (i + 1) r a)
        mapIdx _ [] _ = []

reduce :: (a -> n -> a) -> a -> [n] -> a
reduce _ init [] = init
reduce f init (h:t) =
    (iteration (f init h) t f)
    where
        iteration acc [] _ = acc
        iteration acc (next:tail) f = iteration (f acc next) tail f

replaceAtWith :: [t] -> Int -> t -> [t]
replaceAtWith l i v
    | (length l <= i || i < 0) = error "Index out of bounds"
    | otherwise = let lenlmo = (length l) - 1 in (map (\(e, id) -> if id == i then v else e) (zip l [0..lenlmo]))

repeatEach :: [t] -> Int -> [t]
repeatEach [] _ = []
repeatEach _ 0 = []
repeatEach (h:t) n = (take n (repeat h)) ++ (repeatEach t n)