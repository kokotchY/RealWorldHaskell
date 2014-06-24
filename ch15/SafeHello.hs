module SafeHello where
import System.IO (IOMode(..))
import MonadHandle
safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h