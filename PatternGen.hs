{-# LANGUAGE ScopedTypeVariables #-}
module PatternGen where

import Protocol

import Data.Map as M


import Control.Monad.Identity
import Control.Monad.Error

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

data PatternParams = PatternParams {
	testVar :: Float
}
	deriving( Show, Read )

data Pattern = Pattern

algorithm :: PatternParams -> Pattern
algorithm = undefined



--type GeneratorState = M.Map String Value
type GeneratorState = PatternParams

type GenStateT m a = StateT GeneratorState m a
type GenState a = GenStateT Identity a

--type ParamFromName = M.Map String (PatternParams -> Maybe 

initState = PatternParams {
	testVar = 7
}

{-
testAnswerFromReq :: Monad m => (MaybeT (ErrT m) Answer, GeneratorState)
testAnswerFromReq = runIdentity $ (runStateT $ answerFromReq (Right $ Set "testVar" (StringVal "asdf"))) initState
-}

answerFromReq :: forall m mS. (Monad m, Monad mS) => Request -> GenStateT mS (MaybeT (ErrT m) Answer)
answerFromReq req = case req of
	Left get -> StateT $ \s -> return $ 
		let
			(ret, s') = runState (getVal get) s :: ((ErrT m) Value, GeneratorState)
		in
			(MaybeT $		-- MaybeT (ErrT m) Answer
			liftM Just $ 		-- (ErrT m) (Maybe Answer)
			liftM Answer $		-- (ErrT m) Answer
			ret,			-- (ErrT m) Value
			s')
	Right set -> StateT $ \s -> return $
		let (ret, s') = (runState (setVal set) s) :: ((ErrT m) (), GeneratorState)
		in (
			MaybeT $		-- MaybeT (ErrT m) Answer
			liftM (const Nothing) $ -- ErrT m (Maybe Answer)
			ret 			-- ErrT m ()
			, s')

{-
testSetVal :: Monad m => ((ErrT m) (), GeneratorState)
testSetVal = runIdentity $ (runStateT $ setVal (Set "testVar" (StringVal "asdf"))) initState


testSetVal2 :: Monad m => ((ErrT m) (), GeneratorState)
testSetVal2 = runIdentity $ (runStateT $ setVal (Set "asdf" (StringVal "asdf"))) initState
-}

setVal :: Monad m => Set -> GenState ((ErrT m) ())
setVal (Set varName value) = state $ \params ->
	case runIdentity $ runErrorT $ nameToField varName of
		Right (getter, setter) ->
			case runIdentity $ runErrorT $ setter value params of
				Left err -> (throwError err, params)
				Right newParams -> (return (), newParams)
		Left err -> (throwError err, params)

getVal :: Monad m => Get -> GenState ((ErrT m) Value)
getVal (Get varName) = state $ \params ->
	case runIdentity $ runErrorT $ nameToField varName of
		Right (getter, setter) -> (return $ getter params, params)
		Left err -> (throwError err, params)


nameToField :: Monad m => String -> ErrT m (GetPatternParams, SetPatternParams)
nameToField str =
	case M.lookup str mapNameToField of
		Nothing -> throwError "variable lookup failed"
		Just (getter, setter) -> return (getter, setter)

setF :: (Float -> PatternParams -> PatternParams) -> (Value -> PatternParams -> ErrT Identity PatternParams)
setF f val params = case val of
	FloatVal float -> return $ f float params
	_ -> throwError "expected type 'float'"
	

mapNameToField :: M.Map String (GetPatternParams, SetPatternParams)
mapNameToField = M.fromList
	[
		("testVar", (
			FloatVal . testVar,
			setF (\val params -> params{ testVar = val })
		)) 
	]

type GetPatternParams = PatternParams -> Value
type SetPatternParams = Value -> PatternParams -> ErrT Identity PatternParams
		

{-
getVal :: Monad m => Get -> GenState ((ErrT m) Value)
getVal (Get varName) = state $ \map ->
	let 
		maybeValue = M.lookup varName map
	in
		case maybeValue of
			Nothing -> (throwError "variable lookup failed", map)
			Just val -> (return val, map)
-}

type ErrT m = ErrorT ErrMsg m
type ErrMsg = String
