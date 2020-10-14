module Utils (
    chunksOf,
    indexedFilter,
    flatten,
    reduce,
    replaceAtWith,
    repeatEach,
    splitWhen,
    splitWhenF,
    slice,
    transposeList,
    indexOnTransposedSqMatrix
) where

import Data.List(findIndex, transpose)
import Data.Maybe (isJust, fromJust)

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

slice :: Int -> Int -> [a] -> [a]
slice _ _ [] = []
slice i e l = (take (e-i) (drop i l))

splitWhen :: (t -> Bool) -> [t] -> [[t]]
splitWhen f l = internalSplit f l
    where
        internalSplit _ [] = []::[[t]]
        internalSplit f l =
            let idx = (findIndex f l)
            in
                if (isJust idx) then
                    let qtd = ((fromJust idx) + 1) in
                    ((take (qtd - 1) l): (internalSplit f (drop qtd l)))
                else
                    [l]

splitWhenF :: (t -> Bool) -> [t] -> [[t]]
splitWhenF f l = (internalSplit f l)
    where
        internalSplit _ [] = []::[[t]]
        internalSplit f l =
            let idx = (findIndex f l)
            in
                if (isJust idx) then
                    let qtd = ((fromJust idx) + 1) in
                        if qtd > 1 then
                            ((take (qtd - 1) l): (internalSplit f (drop qtd l)))
                        else
                            (internalSplit f (drop qtd l))
                else
                    [l]

transposeList :: [t] -> Int -> [t]
transposeList l 0 = l
transposeList [] _ = []
transposeList l s = flatten (transpose (chunksOf s l))

indexOnTransposedSqMatrix :: Int -> Int -> Int
indexOnTransposedSqMatrix k n = (k `div` n) + n * (k `mod` n)