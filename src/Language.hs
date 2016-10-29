-- {-# LANGUAGE DataKinds #-}
module Language where

import qualified Data.Map as M


type Variable = String
type Environment = M.Map Variable ExpressedValue
type FinalAnswer = ExpressedValue
type Cont = ExpressedValue -> FinalAnswer


data ExpressedValue
  = IntVal  { intVal :: Int }
  | BoolVal { boolVal :: Bool }
  | ProcVal { var :: Variable, body :: Expression, savedEnv :: Environment }
  | ListVal { values :: [ExpressedValue] }
  | PrimPlus
  | PrimZero
  deriving (Show, Eq, Ord)


data Expression
  = CstExpr    { val :: ExpressedValue }
  | VarExpr    { var :: Variable }
  | CondExpr   { cond :: Expression, consequence :: Expression, fallback :: Expression }
  | LetExpr    { var :: Variable, binding :: Expression, body :: Expression }
  | LambdaExpr { var :: Variable, body :: Expression }
  | CallExpr   { operator :: Expression, operand :: Expression }
  deriving (Show, Eq, Ord)


--
-- Environment implementation
--

emptyEnv :: Environment
emptyEnv = M.empty

fromEnv :: Environment -> Variable -> ExpressedValue
fromEnv env var = env M.! var

assocEnv :: Environment -> Variable -> ExpressedValue -> Environment
assocEnv env var val = M.insert var val env


--
-- Continuation passing style interpreter
--

run :: Expression -> FinalAnswer
run expr = eval expr emptyEnv id

eval :: Expression -> Environment -> Cont -> FinalAnswer
eval CstExpr{..}    env cont = cont val
eval VarExpr{..}    env cont = cont (fromEnv env var)
eval LambdaExpr{..} env cont = cont (ProcVal var body env)
eval CondExpr{..}   env cont =
  eval cond env $ \(BoolVal b) ->
    eval (if b then consequence else fallback) env cont
eval LetExpr{..} env cont =
  eval binding env $ \val ->
    eval body (assocEnv env var val) cont
eval CallExpr{..} env cont =
  eval operator env $ \fct ->
    eval operand env $ \arg ->
      applyProc fct arg cont


applyProc :: ExpressedValue -> ExpressedValue -> Cont -> FinalAnswer
applyProc ProcVal{..} val cont =
  eval body (assocEnv savedEnv var val) cont
applyProc PrimZero val cont = cont (BoolVal $ intVal val == 0)
applyProc PrimPlus val cont = cont (IntVal (sum $ map intVal (values val)))



-- TEST DRIVER

testLanguage :: IO ()
testLanguage = do
  let prog = LetExpr {
              var = "x",
              binding = CstExpr (IntVal 5),
              body = LetExpr {
                var = "y",
                binding = CstExpr (IntVal 10),
                body = CondExpr {
                        cond = (CallExpr (CstExpr PrimZero) (VarExpr "x")),
                        consequence = (VarExpr "x"),
                        fallback = (VarExpr "y")
                      }
              } }
  print $ run prog


--
