module Eval where

import Expr
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid

eval :: Expr -> Expr
eval = evalC M.empty

evalC :: Ctx -> Expr -> Expr
evalC c e0 = case e0 of
  EZ _ _ -> e0
  ES _ _ -> e0
  EPlus _ e1 e2 -> intOp c "+" (+) e1 e2
  EMinus _ e1 e2 -> intOp c "-" (-) e1 e2
  ETimes _ e1 e2 -> intOp c "*" (*) e1 e2
  ECat _ e1 e2 -> case (evalC c e1, evalC c e2) of
    (ES _ s1, ES _ s2) -> ES undefined (mappend s1 s2)
    (ES _ _, _) -> error "right operand of ++ not a string"
    (_, _) -> error "left operand of ++ not a string"
  ELen _ e -> case evalC c e of
    ES _ s -> (EZ undefined . fromIntegral . T.length) s
    _ -> error "operand of len is not a string"
  EL _ v e1 e2 -> evalC (M.insert v e1 c) e2
  EV _ v -> case M.lookup v c of
    Just e -> e
    Nothing -> error ("variable "++v++" not defined")

intOp :: Ctx -> Name -> (Integer -> Integer -> Integer) -> Expr -> Expr -> Expr
intOp c name op e1 e2 = case (evalC c e1, evalC c e2) of
  (EZ _ z1, EZ _ z2) -> EZ undefined (op z1 z2) 
  (EZ _ _, _) -> error ("right operand of "++ name ++" not an integer")
  (_, _) -> error ("left operand of "++ name ++" not an integer")
