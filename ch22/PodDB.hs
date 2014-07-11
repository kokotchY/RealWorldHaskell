module PodDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import PodTypes
import Control.Monad (when)
import Data.List (sort)

connect :: FilePath -> IO Connection
connect fp = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    return dbh

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh = do
    tables <- getTables dbh
    when (not ("podcasts" `elem` tables)) $
        do
            run dbh "CREATE TABLE podcasts (\
                    \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \castURL TEXT NOT NULL UNIQUE)" []
            return ()
    when (not ("episodes" `elem` tables)) $
        do
            run dbh "CREATE TABLE episodes (\
                    \epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                    \epcastid INTEGER NOT NULL,\
                    \epurl TEXT NOT NULL,\
                    \epdone INTEGER NOT NULL,\
                    \UNIQUE(epcastid, epurl),\
                    \UNIQUE(epcastid, epid))" []
            return ()
    commit dbh

addPodcast :: IConnection conn => conn -> Podcast -> IO Podcast
addPodcast dbh podcast = handleSql errorHandler $ do
    run dbh "INSERT INTO podcasts (castURL) VALUES (?)" [toSql (castURL podcast :: String)]
    r <- quickQuery' dbh "SELECT castid FROM podcasts WHERE castURL = ?" [toSql (castURL podcast :: String)]
    case r of
        [[x]] -> return $ podcast {castId = fromSql x}
        y -> fail $ "addPodcast: unexpected reuslt: " ++ show y
    where
        errorHandler e = do
            fail $ "Error adding podcast; does this URL already exist?\n" ++ show e

addEpisode :: IConnection conn => conn -> Episode -> IO ()
addEpisode dbh ep =
    run dbh "INSERT OR IGNORE INTO episodes (epCastId, epURL, epDone) \
                \VALUES (?, ?, ?)"
            [toSql (castId . epCast $ ep), toSql (epURL ep), toSql (epDone ep)]
    >>  return ()


updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
updatePodcast dbh podcast =
    run dbh "UPDATE podcasts SET castURL = ? WHERE castId = ?"
        [toSql (castURL podcast), toSql (castId podcast)]
    >> return ()

updateEpisode :: IConnection conn => conn -> Episode -> IO ()
updateEpisode dbh episode =
    run dbh "UPDATE episodes SET epCastId = ?, epUrl = ?, epDone = ? \
        \WHERE epId = ?"
    [toSql (castId . epCast $ episode), toSql (epURL episode), toSql (epDone episode), toSql (epId episode)]
    >> return ()

removePodcast :: IConnection conn => conn -> Podcast -> IO ()
removePodcast dbh podcast = do
    run dbh "DELETE FROM episodes WHERE epCastId = ?"
        [toSql (castId podcast)]
    run dbh "DELETE FROM podcasts WHERE castid = ?"
        [toSql (castId podcast)]
    return ()

getPodcasts :: IConnection conn => conn -> IO [Podcast]
getPodcasts dbh = do
    res <- quickQuery' dbh "SELECT castid, castURL FROM podcasts ORDER BY castid" []
    return (map convPodcastRow res)

getPodcast :: IConnection conn => conn -> Integer -> IO (Maybe Podcast)
getPodcast dbh wantedId = do
    res <- quickQuery' dbh "SELECT castid, casturl FROM podcasts WHERE castid = ?" [toSql wantedId]
    case res of
        [x] -> return $ Just (convPodcastRow x)
        [] -> return Nothing
        x -> fail $ "Really bad error; more than one podcast with ID"

convPodcastRow :: [SqlValue] -> Podcast
convPodcastRow [svId, svURL] = Podcast {castId = fromSql svId,
                                        castURL = fromSql svURL}
convPodcastRow x = error $ "Can't convert podcast row " ++ show x

getPodcastEpisodes :: IConnection conn => conn -> Podcast -> IO [Episode]
getPodcastEpisodes dbh pc = do
    r <- quickQuery' dbh "SELECT epId, epURL, epDone FROM episodes WHERE epCastId = ?" [toSql (castId pc)]
    return (map convEpisdeRow r)
    where
        convEpisdeRow [svId, svURL, svDone] = Episode { epId = fromSql svId, epURL = fromSql svURL, epDone = fromSql svDone, epCast = pc}
