module Main where

import Protocol

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Trans

import System.IO


main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stdin LineBuffering
	interaction
	
	
interaction :: IO ()
interaction = do
	requests <- liftM lines $ getContents

	let stringAnswers = map stringAnswersFromInput requests

	putStrLn $ unlines $ stringAnswers

stringAnswersFromInput ::  String -> String
stringAnswersFromInput str = handleError $ runIdentity $ runErrorT $ answerFromInput str 

handleError :: Either String String -> String 
handleError answerOrErr = case answerOrErr of
		Right str -> str
		Left error -> "error " ++ error

answerFromInput :: Monad m => String -> ErrorT String m String
answerFromInput str =
	reqFromStr str >>= answerFromReq >>= return . strFromAnswer

reqFromStr :: Monad m => String -> ErrorT String m Request
reqFromStr = parseRequest

answerFromReq :: Monad m => Request -> ErrorT String m Answer
answerFromReq req = do
	return Answer

strFromAnswer :: Answer -> String
strFromAnswer answer = "accepted"
