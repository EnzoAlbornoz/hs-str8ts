import Str8ts ( IStr8ts(getRows, backTrack), Str8ts(Str8ts) )
import Slot ( Slot(Slot), SlotColor(White, Black) )
import Data.Time ( diffUTCTime, getCurrentTime )
import Data.List(intercalate)

main = do
    let sz = 6
    let dt = [
            (Slot Black 0), (Slot White 0), (Slot White 0), (Slot Black 1), (Slot Black 0), (Slot Black 0),
            (Slot Black 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 5), (Slot White 0),
            (Slot Black 0), (Slot White 0), (Slot White 1), (Slot White 0), (Slot White 0), (Slot White 0),
            (Slot White 4), (Slot White 0), (Slot White 0), (Slot White 0), (Slot White 0), (Slot Black 0),
            (Slot White 0), (Slot White 6), (Slot White 5), (Slot White 0), (Slot White 0), (Slot Black 0),
            (Slot Black 0), (Slot Black 0), (Slot Black 0), (Slot White 0), (Slot White 1), (Slot Black 4)
            ]
    let m = (Str8ts dt sz)
    ti <- getCurrentTime
    let Just bt = backTrack m
    te <- getCurrentTime
    putStrLn (intercalate "\n" (map show (getRows bt)))
    print ("Time Elapsed: " ++ (show (diffUTCTime te ti)))