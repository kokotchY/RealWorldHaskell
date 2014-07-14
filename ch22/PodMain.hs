module Main where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)

main = withSocketsDo $ handleSqlError $ do
    args <- getArgs
    dbh <- connect "pod.db"
    case args of
        ["add", url] -> add dbh url
        ["list"] -> list dbh
        ["listEpisodes", id] -> listEpisodes dbh (read id :: Integer)
        ["update"] -> update dbh
        ["download"] -> download dbh
        ["remove", id] -> remove dbh (read id :: Integer)
        ["fetch"] -> do
            update dbh
            download dbh
        _ -> syntaxError
    disconnect dbh

add dbh url = do
    addPodcast dbh pc
    commit dbh
    where
        pc = Podcast {castId = 0, castURL = url}

remove dbh id = do
    removePodcast dbh $ podcastForId id
    commit dbh

listEpisodes dbh id = do
    podcast <- getPodcast dbh id
    case podcast of
        Just p -> do
            episodes <- getPodcastEpisodes dbh p
            putStrLn $ "Episodes of podcast " ++ show (castId p) ++ " - " ++ castURL p
            mapM_ displayEpisode episodes
            where
                displayEpisode episode = putStrLn $ show (epId episode) ++ " => " ++ epURL episode

podcastForId :: Integer -> Podcast
podcastForId id = Podcast { castId = id, castURL = "" }

list dbh = do
    pclist <- getPodcasts dbh
    mapM_ displayPodcast pclist
    where
        displayPodcast podcast = putStrLn $ show (castId podcast) ++ " => " ++ castURL podcast

update dbh = do
    pclist <- getPodcasts dbh
    mapM_ procPodcast pclist
    where
        procPodcast pc = do
            putStrLn $ "Updating from " ++ (castURL pc)
            updatePodcastFromFeed dbh pc

download dbh = do
    pclist <- getPodcasts dbh
    mapM_ procPodcast pclist
    where
        procPodcast pc = do
            putStrLn $ "Considering " ++ (castURL pc)
            episodelist <- getPodcastEpisodes dbh pc
            let dleps = filter (\ep -> epDone ep == False) episodelist
            mapM_ procEpisode dleps
            where
                procEpisode ep = do
                    putStrLn $ "Downloading " ++ (epURL ep)
                    getEpisode dbh ep

syntaxError = putStrLn
    "Usage: pod command [args\n\
    \\n\
    \pod add url    Adds a new podcast with the given URL\n\
    \pod download   Downloads all pending episodes\n\
    \pod fetch      Updates, then downloads\n\
    \pod list       List podcasts\n\
    \pod remove id  Remove the given podcast\n\
    \pod update     Downloads podcast feeds, looks for new episodes\n"
