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
            add abh url

guiUpdate = undefined
guiDownload = undefined
guiFetch = undefined
