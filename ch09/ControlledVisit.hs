{-# LANGUAGE ScopedTypeVariables #-}

module ControlledVisit where

import Control.Monad
import Control.Exception
import System.Directory
import System.FilePath
import System.IO
import Data.Time.Clock(UTCTime(..))

data Info = Info {
		infoPath :: FilePath
		, infoPerms :: Maybe Permissions
		, infoSize :: Maybe Integer
		, infoModTime :: Maybe UTCTime
		} deriving (Eq, Ord, Show)
	

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(e::SomeException) -> return Nothing) (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
	perms <- maybeIO (getPermissions path)
	size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
	modified <- maybeIO (getModificationTime path)
	return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]

traverse order path = do
	names <- getUsefulContents path
	contents <- mapM getInfo (path : map (path </>) names)
	liftM concat $ forM (order contents) $ \info -> do
		if isDirectory info && infoPath info /= path
				then traverse order (infoPath info)
				else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
	names <- getDirectoryContents path
	return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
