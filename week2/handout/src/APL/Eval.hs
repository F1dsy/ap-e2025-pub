
module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun  VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

type Error = String

newtype EvalM a
  = EvalM (Env -> Either Error a)

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
         in y env

instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x
  (<*>) = ap

instance Functor EvalM where
  fmap = liftM

catch ::  EvalM a -> EvalM a -> EvalM a
catch (EvalM e1) (EvalM e2) =
 EvalM $ \env -> 
  case e1 env of
    (Left _) -> e2 env
    ( Right r) ->   Right r


askEnv :: EvalM Env
askEnv = EvalM $ \env -> Right env

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

evalIntBinOp :: (Integer -> Integer -> EvalM Val) -> Exp -> Exp -> EvalM Val
evalIntBinOp f  e1 e2 = do
  x <- eval  e1
  y <- eval  e2
  case (x, y) of
    (ValInt x', ValInt y') -> f x' y'
    _ -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f = evalIntBinOp f'
  where
    f' x y = pure $ ValInt (f x y)

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

runEval :: EvalM a -> Either Error a
runEval (EvalM a) = a envEmpty

eval :: Exp -> EvalM Val
eval  (CstInt x) = pure $ ValInt x
eval  (CstBool b) = pure $ ValBool b
eval  (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval  (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval  (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval  (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval  (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ ValInt (x `div` y)
eval  (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ ValInt (x ^ y)
eval  (Eql e1 e2) = do
  x <- eval e1
  y <- eval e2
  case (x, y) of
    (ValInt x', ValInt y') -> pure $ ValBool $ x' == y'
    (ValBool x', ValBool y') -> pure $ ValBool $ x' == y'
    (_, _) -> failure "Invalid operands to equality"
eval  (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval  (Let var e1 e2) = do
  v <- eval e1
  localEnv (envExtend var v) $ eval e2
eval  (ForLoop (p, initial) (i, bound) body) = do
  v <- eval  initial
  n <- eval  bound
  case n of

    (ValInt n') -> pure $ ValInt $ 1
      -- if n' <= 0
      --   then
      --     failure "Bound must be positive integer"
      --   else

      --     let env' = envExtend i (ValInt 0) (envExtend p v )
      --     in  loop  0
      -- where
        -- loop k
        --   | True = eval env' (Var p)
        --   | otherwise = do
        --     v' <- eval env' body
        --     let env'' = envExtend p v' (envExtend i (ValInt (k + 1)) env')
        --       in loop env'' (k + 1)
    _ -> failure "Non integral loop bound"

eval  (Lambda var e1) =
   pure $ ValFun  var e1
eval  (Apply  fun arg) = do
  fun' <- eval  fun
  case fun' of
    (ValFun  var e1) ->  eval  (Let var arg e1)
    _ -> failure "Function is not of ValFun type"
eval  (TryCatch e1 e2) = eval e1 `catch` eval e2
