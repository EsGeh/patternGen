module Protocol where

import Text.Parsec
--import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Error

import Control.Monad.Error

parseRequest :: Monad m => String -> ErrorT String m Request
parseRequest = execParser requestParser

execParser :: Monad m => GenParser Char () res-> String -> ErrorT String m res
execParser parser str = do
	let eitherRequest = parse parser "request" str
	case eitherRequest of
		Left parseError -> throwError $ strMsg $ show parseError
		Right req -> return $ req
	--throwError $ strMsg $ "not yet implemented"

requestParser :: GenParser Char () Request
requestParser = do
	ret <- (try getParser <|> setParser)
	eof
	return ret

getParser = do
	string "get" 
	sepParser
	varName <- varNameParser 
	return $ Get varName

--testParser :: GenParser Char () ()
setParser = do
	string "set"
	sepParser
	varName <- varNameParser
	sepParser
	value <- valueParser
	return $ Set varName value

valueParser = do
	try floatParser <|> stringParser

floatParser = do
	try (string "float") <|> string "f"
	sepParser
	value <- many1 digit
	return $ FloatVal $ read value
stringParser = do
	try (string "symbol") <|> string "s"
	sepParser
	str <- many1 $ (letter <|> digit)
	return $ StringVal $ str

varNameParser = many1 $ letter
sepParser = do
	space
	try spaces



data Request =
	Set Param Value |
	Get Param
	deriving( Read, Show )
data Answer = Answer
	deriving( Read, Show )

data Value = FloatVal Float | StringVal String
	deriving( Read, Show )
type Param = String

data ProtocolError = PEUnknownReq
