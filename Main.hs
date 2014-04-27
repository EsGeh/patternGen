module Main where

import Protocol

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.State

import System.IO

main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stdin LineBuffering
	interaction


interaction :: IO ()
interaction = do
	requests <- liftM lines $ getContents

	-- stringAnswers :: String -> String
	let stringAnswers = map (
		handleError			-- Either ErrMsg String -> String
		. runIdentity . runErrorT	
		. answerFromInput		-- String -> ErrT m String
		) requests

	putStrLn $ unlines $ stringAnswers

handleError :: Either ErrMsg String -> String 
handleError answerOrErr = case answerOrErr of
		Right str -> str
		Left error -> "error"

answerFromInput :: Monad m => String -> ErrT m String
answerFromInput str =
	runMaybeT (
		(lift $ reqFromStr str) >>= answerFromReq -- :: MaybeT (ErrT m) Answer
	)
	>>= strFromAnswer -- :: Maybe Answer -> ErrT m String


-- code relevant for the protocol

reqFromStr :: Monad m => String -> ErrT m Request
reqFromStr = parseRequest

strFromAnswer :: Monad m => Maybe Answer -> ErrT m String
strFromAnswer maybeAnswer = return $ case maybeAnswer of
	Just answer -> show answer
	Nothing -> "void"

-- the real pattern generator...:

answerFromReq :: Monad m => Request -> MaybeT (ErrT m) Answer
answerFromReq req = case req of
	Left (Get var) -> return $ Answer $ StringVal $ "got " ++ show (Get var)
	Right (Set var value) -> MaybeT $ return $ Nothing


type ErrT m = ErrorT ErrMsg m
type ErrMsg = String

setVal :: Set -> State genState ()
setVal req = do
	return ()	

getVal :: Get -> State genState Value
getVal req = do
	return $ StringVal "<return value>"
