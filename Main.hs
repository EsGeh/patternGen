module Main where

import Protocol

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Trans


main = runErrorT interaction
	
	
interaction :: ErrorT String IO String
interaction = do
	content <- lift getContents
	answers <- mapM answerFromInput $ lines $ content
	return $ unlines answers

answerFromInput :: Monad m => String -> ErrorT String m String
answerFromInput str =
	reqFromStr str >>= answerFromReq >>= return . strFromAnswer

reqFromStr :: Monad m => String -> ErrorT String m Request
reqFromStr = parseRequest

answerFromReq :: Monad m => Request -> ErrorT (String) m Answer
answerFromReq req = do
	throwError $ strMsg $ "not yet implemented"

strFromAnswer answer = ""
