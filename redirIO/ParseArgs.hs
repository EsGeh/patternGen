module ParseArgs where


import Text.Read
import System.Console.GetOpt


parseArgs :: [String] -> Either Error Options
parseArgs args = do 
	let (optionFunctions, nonOptions, errors) = getOpt argOrder listOptDescr args
	options <- case errors of
		[] -> case nonOptions of
			[] -> Left $ usageInfo usageStr listOptDescr
			_ -> concM (return defaultOptions) optionFunctions
		_ -> Left $ foldl (++) "" errors
	return $ (options{ applicationAndParams = nonOptions } :: Options)

usageStr = "Syntax: " ++ progName ++ " [OPTIONS] application [...arguments]"

progName = "redirIO"

concM :: Monad m => m a -> [a -> m a] -> m a
concM start [] = start 
concM start (x:xs) = (start >>= x) `concM` xs


argOrder = RequireOrder

listOptDescr :: [OptDescr (Options -> Either Error Options)]
listOptDescr = [
	Option ['p'] ["port"] (ReqArg setPort "PORT") "the port to listen on"]
	where
		setPort string settings = case readMaybe string of
			Nothing -> Left "Error while parsing port!"
			Just port -> Right $ settings{ tcpPort=Just port }

data Options = Options {
	applicationAndParams :: [String],
	tcpPort :: Maybe Port
}
	deriving( Show )

defaultOptions = Options {
	applicationAndParams = [],
	tcpPort = Nothing
}
type Port = Int
type Error = String
