{-# LANGUAGE InstanceSigs #-}

module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    getState,
    putState,
    modifyState,
    evalKvGet,
    evalKvPut,
    evalPrint,
    failure,
    catch,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ (f <$>) <$> g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  (>>=) :: (Functor e) => Free e a -> (a -> Free e b) -> Free e b
  Pure x >>= f = f x
  Free g >>= f = Free $ (>>= f) <$> g

data EvalOp a
  = ReadOp (Env -> a)
  | StateGetOp (State -> a)
  | StatePutOp State a
  | PrintOp String a
  | ErrorOp Error

instance Functor EvalOp where
  fmap :: (a -> b) -> EvalOp a -> EvalOp b
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (StateGetOp k) = StateGetOp $ f . k
  fmap f (StatePutOp s a) = StatePutOp s $ f a
  fmap f (PrintOp str a) = PrintOp str $ f a
  fmap _ (ErrorOp err) = ErrorOp err

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects :: (Functor e, Functor h) => (e (Free e a) -> h (Free e a)) -> Free e a -> Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g op = op

getState :: EvalM State
getState = Free $ StateGetOp $ \s -> pure s

putState :: State -> EvalM ()
putState s = Free $ StatePutOp s $ pure ()

modifyState :: (State -> State) -> EvalM ()
modifyState f = do
  s <- getState
  putState $ f s

evalPrint :: String -> EvalM ()
evalPrint str = Free $ PrintOp str $ pure ()

failure :: String -> EvalM a
failure err = Free $ ErrorOp err

catch :: EvalM a -> EvalM a -> EvalM a
catch = error "To be completed in assignment 4."

evalKvGet :: Val -> EvalM Val
evalKvGet = error "To be completed in assignment 4."

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut = error "To be completed in assignment 4."
