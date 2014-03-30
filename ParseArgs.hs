{-# LANGUAGE FlexibleInstances #-}
module ParseArgs where

import ProgramTypes

import System.Console.GetOpt

parseArgs :: [String] -> Either String Settings
parseArgs args = do
	let (options, nonOptions, errors) = getOpt argOrder listOptDescr args
	case errors of
		[] -> settingsFromFlags options
		_ -> Left $
			foldl (++) "" errors

settingsFromFlags :: [Flag] -> Either String Settings
settingsFromFlags flags = do
	port_ <- portFromFlags flags
	return $ Settings {
		port = port_
	}

{-
test = case portFromFlags [] of
	Left error -> 
-}

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
