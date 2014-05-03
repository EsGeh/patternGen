module Protocol where

import Text.Parsec
--import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Error

import Control.Monad.Error

parseRequest :: Monad m => String -> ErrorT String m Request
parseRequest = execParser requestParser

execParser :: Monad m => GenParser Char () res -> String -> ErrorT String m res
execParser parser str = do
	let eitherRequest = parse parser "request" str
	case eitherRequest of
		Left parseError -> throwError $ strMsg $ show parseError
		Right req -> return $ req
	--throwError $ strMsg $ "not yet implemented"

requestParser :: GenParser Char () Request
requestParser = do
	--ret <- option (try getParser) setParser
	let getParser' = parsecMap Left $ getParser
	let setParser' = parsecMap Right $ setParser
	ret <- getParser' <|> setParser'
	eof
	return ret

-- get <varname>
getParser = do
	string "get" 
	sepParser
	varName <- varNameParser 
	return $ Get varName

-- set <varname> [<value>]
setParser = do
	string "set"
	sepParser
	varName <- varNameParser
	values <- (try $ do
		sepParser
		sepBy valueParser sepParser)
		<|>
		(return [])
	--value <- valueParser
	return $ Set varName values

valueParser = do
	try floatParser <|> stringParser

floatParser = do
	--try (string "float") <|> string "f"
	--sepParser
	value <- many1 digit
	return $ FloatVal $ read value
stringParser = do
	--try (string "symbol") <|> string "s"
	--sepParser
	str <- many1 $ (letter <|> digit)
	return $ StringVal $ str

varNameParser = many1 $ letter
sepParser = do
	space
	try spaces

type Request = Either Get Set

data Set = Set Param [Value]
	deriving( Read, Show )
data Get = Get Param
	deriving( Read, Show )

{-
data Request =
	Set Param Value |
	Get Param
	deriving( Read, Show )-}
data Answer = Answer [Value]
	deriving( Read, Show )

{-
type Value = 

data FloatVal = FloatVal { fromFloatVal :: Float }
	deriving( Read, Show )
data StringVal = StringVal { fromStringVal :: String }
	deriving( Read, Show )
-}
data Value =
	FloatVal Float
	| StringVal String
	deriving( Read, Show )
type Param = String

data ProtocolError = PEUnknownReq
