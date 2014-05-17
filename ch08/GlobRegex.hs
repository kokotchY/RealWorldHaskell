module GlobRegex 
	(
		globToRegex
		, matchesGlob
	) where

import Text.Regex.Posix ((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = 
	case (globToRegex' cs) of
		Left error -> Left error
		Right regex -> Right $ '^' : regex ++ "$" 

globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""
globToRegex' ('*':cs) = prepend ".*" (globToRegex' cs)
globToRegex' ('?':cs) = prepend "." (globToRegex' cs)
globToRegex' ('[':'!':c:cs) = prepend ("[^" ++ [c]) (globToRegex' cs)
globToRegex' ('[':c:cs) = prepend ['[', c] (charClass cs)
globToRegex' ('[':_) = Left "unterminated character class"
globToRegex' (c:cs) = prepend (escape c) (globToRegex' cs)

escape :: Char -> String
escape c
	| c `elem` regexChars = '\\' : [c]
	| otherwise = [c]
	where regexChars = "\\+()^$.{}|"

charClass :: String -> Either GlobError String
charClass (']':cs) = prepend "]" (globToRegex' cs)
charClass (c:cs) = prepend [c] (charClass cs)
charClass [] = Left "undeterminated character class"

prepend :: String -> Either GlobError String -> Either GlobError String
prepend prefix (Left error) = Left error
prepend prefix (Right result) = Right $ prefix ++ result

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat =
	case (globToRegex pat) of
		Left err -> False
		Right regex -> name =~ regex
