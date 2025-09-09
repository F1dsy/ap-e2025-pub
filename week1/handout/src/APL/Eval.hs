module APL.Eval
  ( Val (..),
    eval,
    envEmpty,
  )
where

import APL.AST (Exp (..), VName)

-- import Distribution.TestSuite (Result (Error))

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

-- | Empty environment, which contains no variable bindings.
envEmpty :: Env
envEmpty = []

-- | Extend an environment with a new variable binding,
-- producing a new environment.
envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

-- | Look up a variable name in the provided environment.
-- Returns Nothing if the variable is not in the environment.
envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

eval :: Env -> Exp -> Either Error Val
eval env (CstInt x) = Right $ ValInt x
eval env (CstBool b) = Right $ ValBool b
eval env (Add env e1 env e2) =
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt e1), Right (ValInt e2)) -> Right $ ValInt $ e1 + e2
eval env (Sub env e1 env e2) =
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt e1), Right (ValInt e2)) -> Right $ ValInt $ e1 - e2
eval env (Mul env e1 env e2) = case (eval e1, eval e2) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt e1), Right (ValInt e2)) -> Right $ ValInt $ e1 * e2
eval env (Div env e1 env e2) = case (eval e1, eval e2) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt e1), Right (ValInt e2)) -> Right $ ValInt $ e1 `div` e2
eval env (Pow env e1 env e2) = case (eval e1, eval e2) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValInt e1), Right (ValInt e2)) -> Right $ ValInt $ e1 ^ e2
eval env (Eql env e1 env e2) =
  case (eval e1, eval e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (If cond e1 e2) =
  case eval cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Non-boolean conditional."
eval env (Var v) =
  case envLookup v env of
    Just x -> Right x
    Nothing -> Left $ "No value in: " ++ v
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2