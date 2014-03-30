{-# LANGUAGE FlexibleInstances #-}
module ParseArgs where

import Prelude hiding( Either, Left, Right)


import ProgramTypes
--import Control.Monad.Error
--import Data.Either

import System.Console.GetOpt

data Either a b = Left a | Right b
	deriving( Show )

instance Monad (Either String) where
	return = Right
	l >>= r = case l of
		Left a -> Left a
		Right b -> r b
	fail = Left

parseArgs :: [String] -> Either String Settings
parseArgs args = do
	let (options, nonOptions, errors) = getOpt argOrder listOptDescr args
	case errors of
		[] -> settingsFromFlags options
		_ -> fail $
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
	[] -> fail "port not found!"
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
