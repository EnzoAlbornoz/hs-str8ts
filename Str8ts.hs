module Str8ts where
import Slot
import Utils(reduce, replaceAtWith, flatten, repeatEach, chunksOf, splitWhenF, slice, transposeList, indexOnTransposedSqMatrix)
import Data.List(transpose, sort, elemIndex, delete, notElem, find)
import Data.Maybe(isNothing)

class IStr8ts s where
    isFullfiled :: s -> Bool
    isValidSolution :: s -> Bool
    backTrack :: s -> Maybe s
    getSlots :: s -> [Slot]
    getSize :: s -> Int
    getRows :: s -> [[Slot]]
    getWhiteConsecutiveRows :: s -> [[Slot]]
    getColumns :: s -> [[Slot]]
    getWhiteConsecutiveColumns :: s -> [[Slot]]
    getLineOfIdx :: s -> Int -> [Slot]
    getWCLineOfIdx :: s -> Int -> [Slot]
    getColumnOfIdx :: s -> Int -> [Slot]
    getWCColumnOfIdx :: s -> Int -> [Slot]

data Str8ts = Str8ts [Slot] Int

instance IStr8ts Str8ts where
    isFullfiled (Str8ts dt _) = length (filter (\e -> ((getValue e) == 0 && (getColor e) == White)) (dt)) == 0
    getSlots (Str8ts dt _) = dt
    getSize (Str8ts _ sz) = sz
    getRows (Str8ts dt sz) = (chunksOf sz dt)
    getWhiteConsecutiveRows m = concatMap (\r -> (splitWhenF (\re -> (getColor re) == Black) r)) (getRows m)
    getColumns (Str8ts dt sz) = (transpose (getRows (Str8ts dt sz)))
    getWhiteConsecutiveColumns m = concatMap (\r -> (splitWhenF (\re -> (getColor re) == Black) r)) (getColumns m)
    isValidSolution (Str8ts dt sz)
        -- Nesta parte, primeiro verificamos se o sistema está preenchido
        | (not (isFullfiled (Str8ts dt sz))) = False
        -- Agora, verificamos se ele não fere a nenhum dos sistemas de validação
        | otherwise = 
            (not 
                -- Verifica por inconsistencias nas Linhas
                ((length (filter checkError (
                    map (\r -> (
                        filter (\e -> (
                            (getColor e) == White)
                        ) r)
                    ) (getWhiteConsecutiveRows (Str8ts dt sz))
                )) > 0)
                    ||
                -- Verifica por inconsistencias nas Colunas
                (length (filter checkError (
                    map (\r -> (
                        filter (\e -> (
                            (getColor e) == White)
                        ) r)
                    ) (getWhiteConsecutiveColumns (Str8ts dt sz))
                )) > 0))
            )
            where
                -- A inconsistencia é constatada ao ordernar os valores
                checkError wr =
                    let sortedArr = (sort wr) in 
                        length 
                            (filter
                                (\x -> x == True)
                                (zipWith
                                    (\e i -> 
                                        if i == 0 then
                                            False
                                        else
                                            ((getValue (sortedArr!!(i - 1))) /= ((getValue e) - 1))
                                                || 
                                            (getValue e) < 1
                                                || 
                                            (getValue e) > sz
                                    )
                                    sortedArr
                                    [0..((length sortedArr) - 1)]
                                )
                            ) > 0
    backTrack (Str8ts dt sz) =
        let 
            infSzPos = (repeat ([(Slot White x) | x <- [1..sz]]))
            numOfBlankSlots = length (filter (\s -> (getValue s) == 0 && (getColor s) == White) dt)
            bsPos = take numOfBlankSlots infSzPos
            blankPossibilities = 
                reduce (\a n -> 
                    let 
                        Just iob = (elemIndex (Slot White 0) (a!!0))
                        son = (length n)
                        in (
                            concatMap (\sll -> 
                                let optn = ([l |
                                        l <- n,
                                        let line = (getLineOfIdx (Str8ts (sll!!0) sz) iob),
                                        let column = (getColumnOfIdx (Str8ts (sll!!0) sz) iob),
                                        let lineB = (filter (\e -> (getColor e) == White) line),
                                        let colB = (filter (\e -> (getColor e) == White) column),
                                        let lineZB =  (filter (/=0) (map getValue lineB)),
                                        let colZB = (filter (/=0) (map getValue colB)),
                                        (notElem (getValue l) (map getValue line)),
                                        (notElem (getValue l) (map getValue column)),
                                        (length lineZB == 0 || (getValue l) - (foldr1 min lineZB) < (length lineB)),
                                        (length colZB == 0 || (getValue l) - (foldr1 min colZB) < (length colB))
                                        ]) in
                                zipWith (\sl rv -> 
                                    replaceAtWith sl iob rv
                                ) sll optn
                            )
                            (chunksOf son (repeatEach a son))
                        )
                ) [dt] bsPos
        in
            (backTrackStep sz blankPossibilities)
        where
            backTrackStep s b = 
                let 
                    sln = (take 1 b)!!0
                    m = (Str8ts sln s) 
                in 
                    if (isValidSolution m) then 
                        Just m 
                    else (backTrackStep s (delete sln b))
    
    getLineOfIdx (Str8ts dt sz) idx =
        let indexedData = zip dt [0..((length dt) - 1)]
        in
            [e | (e,i) <- indexedData, i `div` sz == idx `div` sz]

    getWCLineOfIdx (Str8ts dt sz) idx =
        let indexedData = zip dt [0..((length dt) - 1)]
        in
            [e | (e,i) <- indexedData, i `div` sz == idx `div` sz, (isNothing (find (\s -> (getColor s) == Black) (slice (min i idx) ((max i idx) + 1) dt)))]

    getWCColumnOfIdx (Str8ts dt sz) idx =
        let indexedData = zip dt [0..((length dt) - 1)]
            trdt = transposeList dt sz
            idxt = indexOnTransposedSqMatrix idx sz
        in
            [e | (e,i) <- indexedData, i `mod` sz == idx `mod` sz, let it = indexOnTransposedSqMatrix i sz in (isNothing (find (\s -> (getColor s) == Black) (slice (min it idxt) ((max it idxt) + 1) trdt)))]

    getColumnOfIdx (Str8ts dt sz) idx =
        let indexedData = zip dt [0..((length dt) - 1)]
        in
            [e | (e,i) <- indexedData, i `mod` sz == idx `mod` sz]