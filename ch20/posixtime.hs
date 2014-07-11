import System.Posix.Files
import System.Time
import System.Posix.Types

getTimes :: FilePath -> IO (ClockTime, ClockTime, ClockTime)
getTimes fp = do
    stat <- getFileStatus fp
    return (toct (accessTime stat), toct (modificationTime stat), toct (statusChangeTime stat))

toct :: EpochTime -> ClockTime
toct et = TOD (truncate (toRational et)) 0
