-- {-# LANGUAGE FlexibleInstances #-}
module ParseArgs where

import ProgramTypes

import System.Console.GetOpt

parseArgs :: [String] -> Either String Settings
parseArgs args = do
	let (options, nonOptions, errors) = getOpt argOrder listOptDescr args
	case errors of
		[] -> Right $ settingsFromFlags options
		_ -> Left $
			foldl (++) "" errors

settingsFromFlags :: [Flag] -> Settings
settingsFromFlags flags = 
	let eitherPort = portFromFlags flags
	in Settings {
		port = case eitherPort of
			Left _ -> Nothing
			Right port -> Just port
	}

portFromFlags :: [Flag] -> Either String Int
portFromFlags options = case options of
	[] -> Left "port not found!"
	(x:xs) -> case x of
		Port port -> return $ port
		--_ -> portFromFlags xs

argOrder = RequireOrder

listOptDescr :: [OptDescr Flag]
listOptDescr = [
	Option ['p'] ["port"] (ReqArg toFlag "PORT") "the port to listen on"]
	where
		toFlag string = Port $ read string

data Flag =
	Port Int
	deriving( Show )
