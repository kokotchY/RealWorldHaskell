import System.FilePath (replaceExtension)
import System.Directory (doesFileExist, renameDirectory, renameFile)
import Glob (namesMatching)

renameWith :: (FilePath -> FilePath) -> FilePath -> IO FilePath

renameWith f path = do
	let path' = f path
	rename path path'
	return path'

rename :: FilePath -> FilePath -> IO ()
rename old new = do
	isFile <- doesFileExist old
	let f = if isFile then renameFile else renameDirectory
	f old new


glob2 :: String -> String -> Bool
glob2 [] [] = True
glob2 (f:fs) ('*':ps) = matchStar fs ps
glob2 (f:fs) ('?':ps) = glob2 fs ps
glob2 (f:fs) ('[':ps) = matchCharClass fs ps
glob2 (f:fs) (p:ps) = (f == p) && glob2 fs ps
glob2 "" _ = False

matchCharClass :: String -> String -> Bool
matchCharClass _ pat | not (']' `elem` pat) = error "Unmatched '['"
matchCharClass (c:fp) ('!':pat) = (c `notElem` (getCharClass pat)) && glob2 fp (afterCharClass pat)
matchCharClass (c:fp) pat = (c `elem` (getCharClass pat)) && glob2 fp (afterCharClass pat)

getCharClass :: String -> String
getCharClass = takeWhile ((/=) ']')

afterCharClass :: String -> String
afterCharClass pat = tail $ dropWhile ((/=) ']') pat

matchStar :: String -> String -> Bool
matchStar "" pat = glob2 "" pat
matchStar fp pat = glob2 fp pat || matchStar (tail fp) pat
