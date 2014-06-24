{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
import SafeHello
import MonadHandle
import System.IO (IOMode(..))
import Control.Monad.Writer

data Event = Open FilePath IOMode
    | Put String String
    | Close String
    | GetContents String
    deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a}
    deriving (Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

instance MonadHandle [Char] WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return "fake contents"
