module Str8ts where
import Slot
    ( ISlot(getColor, getValue), Slot(..), SlotColor(Black, White) )
import Utils
    ( chunksOf, reduce, repeatEach, replaceAtWith, splitWhenF )
import Data.List ( sort, delete, elemIndex, transpose )

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
    getColumnOfIdx :: s -> Int -> [Slot]

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
        -- Check if board has no empty slots 
        | (not (isFullfiled (Str8ts dt sz))) = False
        -- Check if not error occurs
        | otherwise = 
            (not 
                -- Try find inconsistencies on lines
                ((length (filter checkError (
                    map (\r -> (
                        filter (\e -> (
                            (getColor e) == White)
                        ) r)
                    ) (getWhiteConsecutiveRows (Str8ts dt sz))
                )) > 0)
                    ||
                -- Try find inconsistencies on columns
                (length (filter checkError (
                    map (\r -> (
                        filter (\e -> (
                            (getColor e) == White)
                        ) r)
                    ) (getWhiteConsecutiveColumns (Str8ts dt sz))
                )) > 0))
            )
            where
                -- Inconsistencies are detected when sorting the values of 
                -- slots and then checking for values bounds and sequence forming 
                checkError wr =
                    -- Sort array
                    let sortedArr = (sort wr) in 
                        length 
                            (filter
                                (\x -> x == True)
                                (zipWith
                                    (\e i -> 
                                        if i == 0 then
                                            False
                                        else
                                            -- Check for sequence
                                            ((getValue (sortedArr!!(i - 1))) /= ((getValue e) - 1))
                                                || 
                                            -- Check for lower bound
                                            (getValue e) < 1
                                                || 
                                            -- Check for upper bound
                                            (getValue e) > sz
                                    )
                                    sortedArr
                                    [0..((length sortedArr) - 1)]
                                )
                            ) > 0
    backTrack (Str8ts dt sz) =
        let 
            -- Infinite list of possible values for an White Slot in NxN board
            infSzPos = (repeat ([(Slot White x) | x <- [1..sz]])) 
            -- Qty of blank White Slots on the board
            numOfBlankSlots = length (filter (\s -> (getValue s) == 0 && (getColor s) == White) dt)
            -- Finite list os all possible values (Blank Slots Possibilities - BSP)
            bsPos = take numOfBlankSlots infSzPos
            -- Generate List of Possible Solutions LPS
            blankPossibilities = 
                -- Reduce over BSP with the LPS as accumulated value
                reduce (\a n -> 
                    let 
                        -- Compute the index of the next blank slot (that will be replaced)
                        Just iob = (elemIndex (Slot White 0) (a!!0))
                        son = (length n)
                        in (
                            -- For each element of LPS, replace the blank value with an possible value
                            -- (Going to next level on the Possibilities Tree)
                            concatMap (\sll -> 
                                -- Filter values of BSP based on heuristics
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
                                -- Compute replacement
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
            -- If there's no possibility, then return no value
            backTrackStep _ [] = Nothing
            backTrackStep s b = 
                let 
                    -- Take one solution from BSP
                    sln = (take 1 b)!!0
                    m = (Str8ts sln s) 
                in 
                    -- Check if it is valid, returning it if true 
                    -- or removing the value from the backTrack list
                    -- then trying to get the next value
                    if (isValidSolution m) then 
                        Just m 
                    else (backTrackStep s (delete sln b))
    
    getLineOfIdx (Str8ts dt sz) idx =
        let indexedData = zip dt [0..((length dt) - 1)]
        in
            [e | (e,i) <- indexedData, i `div` sz == idx `div` sz]

    getColumnOfIdx (Str8ts dt sz) idx =
        let indexedData = zip dt [0..((length dt) - 1)]
        in
            [e | (e,i) <- indexedData, i `mod` sz == idx `mod` sz]