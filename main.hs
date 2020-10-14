import Str8ts ( IStr8ts(getRows, backTrack), Str8ts(Str8ts) )
import Slot ( Slot(Slot), SlotColor(White, Black) )
import Data.List

main = do
    let sz = 6
    -- let dt = [
    --         (Slot Black 0), (Slot White 0), (Slot White 0), (Slot Black 1), (Slot Black 0), (Slot Black 0),
    --         (Slot Black 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 5), (Slot White 0),
    --         (Slot Black 0), (Slot White 0), (Slot White 1), (Slot White 0), (Slot White 0), (Slot White 0),
    --         (Slot White 4), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot Black 0),
    --         (Slot White 0), (Slot White 6), (Slot White 5), (Slot White 0), (Slot White 0), (Slot Black 0),
    --         (Slot Black 0), (Slot Black 0), (Slot Black 0), (Slot White 0), (Slot White 1), (Slot Black 4)
    --         ]
    let dt = [
            (Slot White 3), (Slot White 4), (Slot Black 0), (Slot White 0), (Slot White 0), (Slot Black 0),
            (Slot White 0), (Slot White 0), (Slot Black 1), (Slot White 6), (Slot White 0), (Slot Black 0),
            (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot Black 0),
            (Slot Black 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 1), (Slot White 0),
            (Slot Black 0), (Slot White 0), (Slot White 3), (Slot Black 0), (Slot White 0), (Slot White 0),
            (Slot Black 0), (Slot White 0), (Slot White 0), (Slot Black 0), (Slot White 0), (Slot White 0)
            ]
    -- let ft = [
    --         (Slot White 3),(Slot White 4),(Slot Black 0),(Slot White 5),(Slot White 2),(Slot Black 0),
    --         (Slot White 4),(Slot White 3),(Slot Black 1),(Slot White 6),(Slot White 5),(Slot Black 0),
    --         (Slot White 2),(Slot White 5),(Slot White 4),(Slot White 3),(Slot White 6),(Slot Black 0),
    --         (Slot Black 0),(Slot White 2),(Slot White 5),(Slot White 4),(Slot White 1),(Slot White 3),
    --         (Slot Black 0),(Slot White 6),(Slot White 3),(Slot Black 0),(Slot White 4),(Slot White 5),
    --         (Slot Black 0),(Slot White 1),(Slot White 2),(Slot Black 0),(Slot White 3),(Slot White 4)
    --         ]
    let m = (Str8ts dt sz)
    let Just bt = backTrack m
    putStrLn (intercalate "\n" (map show (getRows bt)))