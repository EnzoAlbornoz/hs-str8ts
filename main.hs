import Str8ts ( IStr8ts(getRows, backTrack), Str8ts(Str8ts) )
import Slot ( Slot(Slot), SlotColor(White, Black) )
import Data.List

main = do
    let size = 6
    -- let ft = [
    --         (Slot Black 0), (Slot White 0), (Slot White 0), (Slot Black 1), (Slot Black 0), (Slot Black 0),
    --         (Slot Black 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 5), (Slot White 0),
    --         (Slot Black 0), (Slot White 0), (Slot White 1), (Slot White 0), (Slot White 0), (Slot White 0),
    --         (Slot White 4), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot Black 0),
    --         (Slot White 0), (Slot White 6), (Slot White 5), (Slot White 0), (Slot White 0), (Slot Black 0),
    --         (Slot Black 0), (Slot Black 0), (Slot Black 0), (Slot White 0), (Slot White 1), (Slot Black 4)
    --         ]
    -- let st = [
    --         (Slot White 3), (Slot White 4), (Slot Black 0), (Slot White 0), (Slot White 0), (Slot Black 0),
    --         (Slot White 0), (Slot White 0), (Slot Black 1), (Slot White 6), (Slot White 0), (Slot Black 0),
    --         (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot Black 0),
    --         (Slot Black 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 1), (Slot White 0),
    --         (Slot Black 0), (Slot White 0), (Slot White 3), (Slot Black 0), (Slot White 0), (Slot White 0),
    --         (Slot Black 0), (Slot White 0), (Slot White 0), (Slot Black 0), (Slot White 0), (Slot White 0)
    --         ]
    let tt = [
            (Slot Black 0), (Slot White 0), (Slot White 0), (Slot Black 0), (Slot Black 0), (Slot Black 0),
            (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 1), (Slot Black 0),
            (Slot White 0), (Slot White 1), (Slot Black 0), (Slot White 0), (Slot White 0), (Slot White 0),
            (Slot White 0), (Slot White 0), (Slot White 0), (Slot Black 0), (Slot White 0), (Slot White 6),
            (Slot Black 1), (Slot White 0), (Slot White 5), (Slot White 0), (Slot White 0), (Slot White 0),
            (Slot Black 0), (Slot Black 0), (Slot Black 0), (Slot White 0), (Slot White 0), (Slot White 3)
            ]
    let toBeResolved = tt
    let m = (Str8ts toBeResolved size)
    let Just bt = backTrack m
    putStrLn "Solution:"
    putStrLn (intercalate "\n" (map show (getRows bt)))