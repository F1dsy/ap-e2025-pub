module APL.InterpPure (runEval) where

import APL.Monad

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], Right x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (StateGetOp k)) = runEval' r s $ k s
    runEval' r _ (Free (StatePutOp s a)) = runEval' r s a
    runEval' r s (Free (PrintOp str a)) =
      let (strl, a') = runEval' r s a
       in (str : strl, a')
    runEval' _ _ (Free (ErrorOp err)) = ([], Left err)