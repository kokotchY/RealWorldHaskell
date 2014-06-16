import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Printf(printf)
import System.Environment(getArgs)
import System.Exit
import Control.Monad(when)

data PasswdEntry = PasswdEntry {
    userName :: String,
    password :: String,
    uid :: Integer, 
    gid :: Integer, 
    gecos :: String,
    homeDir :: String,
    shell :: String}
    deriving (Eq, Ord)

instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s"
        (userName pe) (password pe) (uid pe) (gid pe)
        (gecos pe) (homeDir pe) (shell pe)

instance Read PasswdEntry where
    readsPrec _ value =
        case split ':' value of
            [f1, f2, f3, f4, f5, f6, f7] -> [(PasswdEntry f1 f2 (read f3) (read f4) f5  f6 f7, [])]
            x -> error $ "Invalid number of fields in input: " ++ show x
        where
            split :: Eq a => a -> [a] -> [[a]]
            split _ [] = [[]]
            split delim str =
                let (before, remainder) = span (/= delim) str
                in before : case remainder of
                    [] -> []
                    x -> split delim (tail x)

type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp =
    (uid, usermap)
    where
        uidmap = Map.fromList . map (\pe -> (uid pe, pe)) $ entries
        usermap = Map.fromList . map (\pe -> (userName pe, pe)) $ entries
        entries = map read (lines inp)

main = do
    args <- getArgs

    when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap filename"
        exitFailure

    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps

mainMenu maps@(uidmap, usermap) = do
    putStr optionText
    hFlush stdout
    sel <- getLine
    case sel of
        "1" -> lookupUserName >> mainMenu maps
        "2" -> lookupUID >> mainMenu maps
        "3" -> displayFile >> mainMenu maps
        "4" -> return ()
        "_" -> putStrLn "Invalid selection" >> mainMenu maps
    where
        lookupUserName = do
            putStrLn "Username: "
            username <- getLine
            case Map.lookup username usermap of
                Nothing -> putStrLn "Not found."
                Just x -> print x
        lookupUID = do
            putStrLn "UID: "
            uidstring <- getLine
            case Map.lookup (read uidstring) uidmap of
                Nothing -> putStrLn "Not found."
                Just x ->  print x
        displayFile =
            putStr . unlines . map (show . snd) . Map.toList $ uidmap
        optionText =
            "\npasswdmap options:\n\
            \\n\
            \1  Look up a user name\n\
            \2  Look up a UID\n\
            \3  Display entire file\n\
            \4  Quit\n\n\
            \Your selection: "
