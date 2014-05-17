import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int
import Data.Word

data Greymap = Greymap {
	greyWidth :: Int
	, greyHeight :: Int
	, greyMax :: Int
	, greyData :: L.ByteString
} deriving (Eq)

data ParseState = ParseState {
	string :: L.ByteString
	, offset :: Int64
} deriving (Show)

newtype Parse a = Parse {
	runParse :: ParseState -> Either String (a, ParseState)
}

instance Show Greymap where
	show (Greymap w h m _) = "Greymap " ++ show w ++ " x " ++ show h ++ " " ++ show m

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
	case matchHeader (L8.pack "P5") s of
		Nothing -> Nothing
		Just s1 ->
			case getNat s1 of
				Nothing -> Nothing
				Just (width, s2) ->
					case getNat (L8.dropWhile isSpace s2) of
						Nothing -> Nothing
						Just (height, s3) ->
							case getNat (L8.dropWhile isSpace s3) of
								Nothing -> Nothing
								Just (maxGrey, s4)
									| maxGrey > 255 -> Nothing
									| otherwise ->
										case getBytes 1 s4 of
											Nothing -> Nothing
											Just (_, s5) ->
												case getBytes (width * height) s5 of
													Nothing -> Nothing
													Just (bitmap, s6) ->
														Just (Greymap width height maxGrey bitmap, s6)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
	| prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
	| otherwise = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
	Nothing -> Nothing
	Just (num, rest)
		| num <= 0 -> Nothing
		| otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str =
    let count = fromIntegral n
        both@(prefix,_) = L.splitAt count str
    in if L.length prefix < count
        then Nothing
        else Just both

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
	matchHeader (L8.pack "P5") s >>?
	\s -> skipSpace ((), s) >>?
	(getNat . snd) >>?
	skipSpace >>?
	\(width, s) -> getNat s >>?
	skipSpace >>?
	\(height, s) -> getNat s >>?
	\(maxGrey, s) -> getBytes 1 s >>?
	(getBytes (width * height) . snd) >>?
	\(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
	case runParse parser (ParseState initState 0) of
		Left err -> Left err
		Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }

parseByte :: Parse Word8
parseByte =
	getState ==> \initState ->
	case L.uncons (string initState) of
		Nothing -> bail "no more input"
		Just (byte, remainder) ->
			putState newState ==> \_ ->
			identity byte
		where
			newState = initState { string = remainder, offset = newOffset }
			newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
	"byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b

firstParser ==> secondParser = Parse chainedParser
	where chainedParser initState =
		case runParse firstParser initState of
			Left errMessage ->
				Left errMessage
			Right (firstResult, newState) ->
				runParse (secondParser firstResult) newState