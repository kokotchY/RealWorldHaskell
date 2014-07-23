module PodDownload where

import PodTypes
import PodDB
import PodParser
import Network.HTTP
import Network.URI
import System.IO
import Database.HDBC
import Data.Maybe

downloadURL :: String -> IO (Either String String)
downloadURL url = do
    resp <- simpleHTTP request
    case resp of
        Left x -> return $ Left ("Error connecting: " ++ show x)
        Right r ->
            case rspCode r of
                (2,_,_) -> return $ Right (rspBody r)
                (3,_,_) ->
                    case findHeader HdrLocation r of
                        Nothing -> return $ Left (show r)
                        Just url -> downloadURL url
                _ -> return $ Left (show r)
    where
        request = Request { rqURI = uri,
                            rqMethod = GET,
                            rqHeaders = [],
                            rqBody = ""}
        uri = fromJust $ parseURI url

updatePodcastFromFeed :: IConnection conn => conn -> Podcast -> IO ()
updatePodcastFromFeed dbh pc = do
    resp <- downloadURL (castURL pc)
    case resp of
        Left x -> putStrLn x
        Right doc -> updateDB doc
    where
        updateDB doc = do
            mapM_ (addEpisode dbh) episodes
            commit dbh
            where
                feed = parse doc (castURL pc)
                episodes = map (item2ep pc) (items feed)

getEpisode :: IConnection conn => conn -> Episode -> IO (Maybe String)
getEpisode dbh ep = do
    resp <- downloadURL (epURL ep)
    case resp of
        Left x -> do
            putStrLn x
            return Nothing
        Right doc -> do
            file <- openBinaryFile filename WriteMode
            hPutStr file doc
            hClose file
            updateEpisode dbh (ep {epDone = True })
            commit dbh
            return $ Just filename
    where
        filename = "pod." ++ (show . castId . epCast $ ep) ++ "." ++ (show (epId ep)) ++ ".mp3"
