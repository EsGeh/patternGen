{-# LANGUAGE ScopedTypeVariables #-}
module PatternGen where

import Protocol

import Data.Map as M


import Control.Monad.Identity
--import Control.Monad.State
import Control.Monad.Error

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State


type GeneratorState = M.Map String Value

type GenStateT m a = StateT GeneratorState m a
type GenState a = GenStateT Identity a
--type GenState m a = StateT GeneratorState m a

--test req = runErrorT $ runMaybeT $ (runStateT $ answerFromReq req) initState

initState = M.singleton "testVar" (FloatVal 7)

answerTemp :: GeneratorState -> Request -> (Either ErrMsg (Maybe Answer), GeneratorState)
answerTemp initS req = let (ansM, s) = runIdentity  $ (runStateT $ answerFromReq req) initS 
	in
		(runIdentity $ runErrorT $ runMaybeT ansM, s)

answerFromReq :: forall m mS. (Monad m, Monad mS) => Request -> StateT GeneratorState mS (MaybeT (ErrT m) Answer) --GenStateT (MaybeT (ErrT m)) Answer
answerFromReq req = case req of
	Left get -> StateT $ \s -> return $ 
		--undefined :: Identity (MaybeT (ErrT m) Answer, GeneratorState)
		let
			(ret, s') = runState (getVal get) s :: ((ErrT m) Value, GeneratorState) -- ((ErrT mX) Value, s)
		in
			(MaybeT $		-- MaybeT (ErrT mX) Answer
			liftM Just $ 		-- (ErrT mX) (Maybe Answer)
			liftM Answer $		-- (ErrT mX) Answer
			ret,
			s')
	Right set -> StateT $ \s -> return $
		let (_, s') = (runState (setVal set) s) :: ((ErrT m) (), GeneratorState)
		in (
			MaybeT $
			return $
			Nothing
			, s')
			{-(
			MaybeT $ -- (MaybeT m) t
			return $  -- m (Maybe t)
			Nothing,
			s')-}


setVal :: Monad m => Set -> GenState ((ErrT m) ())
setVal (Set varName value) = state $ \oldMap ->
	(return (), M.insert varName value oldMap)

getVal :: Monad m => Get -> GenState ((ErrT m) Value)
getVal (Get varName) = state $ \map ->
	let
		maybeValue = M.lookup varName map
	in
		case maybeValue of
			Nothing -> (throwError "variable lookup failed", map)
			Just val -> (return val, map)

{-
answerFromReq :: forall m . Monad m => Request -> GenStateT Identity (MaybeT (ErrT m) Answer) --GenStateT (MaybeT (ErrT m)) Answer
answerFromReq req = case req of
	Left get -> liftM Answer $	-- GenStateT (MaybeT (ErrT m)) Value -> GenStateT (MaybeT (ErrT m)) Answer
		StateT $ \s -> lift $ runStateT (getVal get) s
	Right set -> StateT $ \s ->
			MaybeT $ -- (MaybeT m) t
			return $  -- m (Maybe t)
			Nothing

setVal :: Monad m => Set -> GenStateT (ErrT m) ()
setVal (Set varName value) = state $ \oldMap ->
	((), M.insert varName value oldMap)

getVal :: Monad m => Get -> GenStateT (ErrT m) Value
getVal (Get varName) = StateT $ \m ->
	let
		maybeValue = M.lookup varName m
	in
		case maybeValue of
			Nothing -> throwError "variable lookup failed"
			Just val -> return (val, m)
-}

type ErrT m = ErrorT ErrMsg m
type ErrMsg = String
