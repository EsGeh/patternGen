{-# LANGUAGE ScopedTypeVariables #-}
module PatternGen where

import Protocol

import Data.Map as M


import Control.Monad.Identity
import Control.Monad.Error

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State


type GeneratorState = M.Map String [Value]

type GenStateT m a = StateT GeneratorState m a
type GenState a = GenStateT Identity a

initState = M.singleton "testVar" [FloatVal 7]

{-
answerTemp :: GeneratorState -> Request -> (Either ErrMsg (Maybe Answer), GeneratorState)
answerTemp initS req = let (ansM, s) = runIdentity  $ (runStateT $ answerFromReq req) initS 
	in
		(runIdentity $ runErrorT $ runMaybeT ansM, s)
-}

answerFromReq :: forall m mS. (Monad m, Monad mS) => Request -> GenStateT mS (MaybeT (ErrT m) Answer)
answerFromReq req = case req of
	Left get -> StateT $ \s -> return $ 
		let
			(ret, s') = runState (getVal get) s :: ((ErrT m) [Value], GeneratorState)
		in
			(MaybeT $		-- MaybeT (ErrT m) Answer
			liftM Just $ 		-- (ErrT m) (Maybe Answer)
			liftM Answer $		-- (ErrT m) Answer
			ret,			-- (ErrT m) Value
			s')
	Right set -> StateT $ \s -> return $
		let (_, s') = (runState (setVal set) s) :: ((ErrT m) (), GeneratorState)
		in (
			MaybeT $
			return $
			Nothing
			, s')

setVal :: Monad m => Set -> GenState ((ErrT m) ())
setVal (Set varName values) = state $ \oldMap ->
	(return (), M.insert varName values oldMap)

getVal :: Monad m => Get -> GenState ((ErrT m) [Value])
getVal (Get varName) = state $ \map ->
	let 
		maybeValue = M.lookup varName map
	in
		case maybeValue of
			Nothing -> (throwError "variable lookup failed", map)
			Just val -> (return val, map)

type ErrT m = ErrorT ErrMsg m
type ErrMsg = String
