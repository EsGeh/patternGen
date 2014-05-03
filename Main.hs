{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Protocol
import PatternGen

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

import System.IO

main = do
	hSetBuffering stdout LineBuffering
	hSetBuffering stdin LineBuffering
	interaction


interaction :: IO ()
interaction = do
	requests <- liftM ( split . filter (/='\n')) $ getContents

	let actions = map actionFromReq requests
	(evalStateT $ sequence_ actions) initState

split [] = [[]]
split xs = start : rest
	where
		(start,rem) = span (/=';') xs
		rest = case rem of
			[] -> []
			(';':rem') -> split rem'

actionFromReq :: String -> (GenStateT IO) ()
actionFromReq str =
	StateT $ \s -> do -- IO
		let answerFromStr_State = answerFromString str :: GenStateT IO ((MaybeT (ErrT Identity)) Answer)

		(res, s') <- (runStateT $ answerFromStr_State) s
		
		case (runIdentity $ runErrorT $ runMaybeT res) of
			Left error -> putStrLn error
			Right Nothing -> return ()
			Right (Just answer) -> putStrLn $ strFromAnswer answer
		
		return $ ((), s')

answerFromString :: forall mS . (Monad mS)=> String -> GenStateT mS (MaybeT (ErrT Identity) Answer)
answerFromString str = 
	(return $ lift $ reqFromStr str)
	>>=
	answerFromReq' -- :: GenStateT mS (MaybeT (ErrT m) Answer)


answerFromReq' :: forall mS . (Monad mS) => MaybeT (ErrT Identity) Request -> StateT GeneratorState mS (MaybeT (ErrT Identity) Answer) 
answerFromReq' maybeReq =
	case (runIdentity $ runErrorT $ runMaybeT maybeReq) of
		Right (Just req) -> (return $ req) >>= answerFromReq
		Right (Nothing) -> state $ \s -> (ans, s)
			where
				ans = MaybeT $ return Nothing
		Left error -> state $ \s -> (ans, s)
			where
				ans = lift $ ErrorT $ return $ Left error
	

--answerFromReq :: forall m mS. (Monad m, Monad mS) => Request -> StateT GeneratorState mS (MaybeT (ErrT m) Answer) --GenStateT (MaybeT (ErrT m)) Answer

-- code relevant for the protocol

strFromAnswer :: Answer -> String
strFromAnswer (Answer values) = (unwords $ map show' values) ++ ";"
	where
		show' (FloatVal f) = show f
		show' (StringVal f) = show f

reqFromStr :: Monad m => String -> ErrT m Request
reqFromStr = parseRequest
