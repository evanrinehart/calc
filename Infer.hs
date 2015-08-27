module Infer where

import Expr
import qualified Data.Map as M

data Type = TZ | TS
type TCtx = M.Map Name Type

instance Show Type where
  show TZ = "Integer"
  show TS = "String"

infer :: Expr -> Type
infer e = inferC M.empty e

inferC :: TCtx -> Expr -> Type
inferC c e0 = case e0 of
  EZ l _ -> TZ
  ES l _ -> TS
  EV l v -> case M.lookup v c of
    Nothing -> error ("line "++show l++": variable "++v++" not defined")
    Just t -> t
  EL l v e1 e2 ->
    let t1 = inferC c e1 in
    inferC (M.insert v t1 c) e2
  EPlus l e1 e2 -> intOp l c "+" e1 e2
  EMinus l e1 e2 -> intOp l c "-" e1 e2
  ETimes l e1 e2 -> intOp l c "*" e1 e2
  ECat l e1 e2 -> case (inferC c e1, inferC c e2) of
    (TS, TS) -> TS
    (TS, _) -> error ("line "++show l++": right operand of ++ not a string")
    (_, _) -> error ("line "++show l++": left operand of ++ not a string")
  ELen l e -> case inferC c e of
    TS -> TZ
    _ -> error ("line "++show l++": operand of len not a string")
  EDec l e -> case inferC c e of
    TZ -> TS
    _ -> error ("line "++show l++": operand of decimal not an integer")

intOp :: Int -> TCtx -> Name -> Expr -> Expr -> Type
intOp l c name e1 e2 = case (inferC c e1, inferC c e2) of
  (TZ, TZ) -> TZ
  (TZ, _) -> error ("line "++show l++": right operand of "++name++" not an integer")
  (_, _) -> error ("line "++show l++": left operand of "++name++" not an integer")

