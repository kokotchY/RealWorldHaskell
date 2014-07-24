module PodMainGUI where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade


import Control.Concurrent


data GUI = GUI {
    mainWin :: Window,
    mwAddBt :: Button,
    mwUpdateBt :: Button,
    mwDownloadBt :: Button,
    mwFetchBt :: Button,
    mwExitBt :: Button,
    statusWin :: Dialog,
    swOKBt :: Button,
    swCancelBt :: Button,
    swLabel :: Label,
    addWin :: Dialog,
    awOKBt :: Button,
    awCancelBt :: Button,
    awEntry :: Entry}

main gladepath = withSocketsDo $ handleSqlError $ do
    initGUI
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    gui <- loadGlade gladepath
    dbh <- connect "pod.db"
    connectGui gui dbh
    mainGUI
    disconnect dbh

loadGlade gladepath = do
    Just xml <- xmlNew gladepath
    mw <- xmlGetWidget xml castToWindow "mainWindow"
    [mwAdd, mwUpdate, mwDownload, mwFetch, mwExit, swOK, swCancel,
     auOK, auCancel] <- mapM (xmlGetWidget xml castToButton)
        ["addButton", "updateButton", "downloadButton",
        "fetchButton", "exitButton", "okButton",
        "cancelButton", "auOK", "auCancel"]

    sw <- xmlGetWidget xml castToDialog "statusDialog"
    swl <- xmlGetWidget xml castToLabel "statusLabel"

    au <- xmlGetWidget xml castToDialog "addDialog"
    aue <- xmlGetWidget xml castToEntry "auEntry"
    return $ GUI mw mwAdd mwUpdate mwDownload mwFetch mwExit
        sw swOK swCancel swl au auOK auCancel aue

connectGui gui dbh = do
    onDestroy (mainWin gui) mainQuit
    onClicked (mwAddBt gui) (guiAdd gui dbh)
    onClicked (mwUpdateBt gui) (guiUpdate gui dbh)
    onClicked (mwDownloadBt gui) (guiDownload gui dbh)
    onClicked (mwFetchBt gui) (guiFetch gui dbh)
    onClicked (mwExitBt gui) mainQuit

guiAdd gui dbh = do
    entrySetText (awEntry gui) ""
    onClicked (awCancelBt gui) (widgetHide (addWin gui))
    onClicked (awOKBt gui) procOK
    windowPresent (addWin gui)
    where
        procOK = do
            url <- entryGetText (awEntry gui)
            widgetHide (addWin gui)
            PodMainGUI.add dbh url

guiUpdate :: IConnection conn => GUI -> conn -> IO ()
guiUpdate gui dbh = statusWindow gui dbh "Pod: Update" (update dbh)

guiDownload :: IConnection conn => GUI -> conn -> IO ()
guiDownload gui dbh = statusWindow gui dbh "Pod: Download" (download dbh)

guiFetch :: IConnection conn => GUI -> conn -> IO ()
guiFetch gui dbh = statusWindow gui dbh "Pod: fetch" (\logf -> update dbh logf >> download dbh logf)

statusWindow :: IConnection conn => GUI -> conn -> String -> ((String -> IO ()) -> IO ()) -> IO ()
statusWindow gui dbh title func = do
    labelSetText (swLabel gui) ""
    widgetSetSensitivity (swOKBt gui) False
    widgetSetSensitivity (swCancelBt gui) True
    windowSetTitle (statusWin gui) title
    childThread <- forkIO childTasks
    onClicked (swCancelBt gui) (cancelChild childThread)
    windowPresent (statusWin gui)
    where
        childTasks = do
            updateLabel "Starting thread..."
            func updateLabel
            enableOK
        enableOK = do
            widgetSetSensitivity (swCancelBt gui) False
            widgetSetSensitivity (swOKBt gui) True
            onClicked (swOKBt gui) (widgetHide (statusWin gui))
            return ()
        updateLabel text = labelSetText (swLabel gui) text
        cancelChild childThread = do
            killThread childThread
            yield
            updateLabel "Action has been cancelled."
            enableOK

add dbh url = do
    addPodcast dbh pc
    commit dbh
    where
        pc = Podcast {castId = 0, castURL = url}

update :: IConnection conn => conn -> (String -> IO ()) -> IO ()
update dbh logf = do
    pclist <- getPodcasts dbh
    mapM_ procPodcast pclist
    logf "Update complete."
    where
        procPodcast pc = do
            logf $ "Updating from " ++ (castURL pc)
            updatePodcastFromFeed dbh pc

download dbh logf = do
    pclist <- getPodcasts dbh
    mapM_ procPodcast pclist
    logf "Download complete."
    where
        procPodcast pc = do
            logf $ "Considering " ++ (castURL pc)
            episodelist <- getPodcastEpisodes dbh pc
            let dleps = filter (\ep -> epDone ep == False) episodelist
            mapM_ procEpisode dleps
        procEpisode ep = do
            logf $ "Downloading " ++ (epURL ep)
            getEpisode dbh ep
