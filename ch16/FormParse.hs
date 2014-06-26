import Text.ParserCombinators.Parsec
import Numeric
import Control.Monad
import Control.Applicative hiding (many, optional, (<|>))

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
    name <- many1 p_char
    value <- optionMaybe (char '=' >> many p_char)
    return (name, value)

p_pair_app1 =
    liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
    <|> (char '+' >> return ' ')
    <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'0']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
    char '%'
    a <- hexDigit
    b <- hexDigit
    let ((d, _):_) = readHex [a,b]
    return . toEnum $ d

a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
    where hexify a b = toEnum . fst . head . readHex $ [a,b]

a_char = oneOf urlBaseChars
    <|> (' ' <$ char '+')
    <|> a_hex
