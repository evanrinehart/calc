module Expr where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text hiding (map, unwords)

type Name = String
type Ctx = Map Name Expr

data Expr =
  EZ Int Integer |
  ES Int Text |
  EV Int Name |
  EL Int Name Expr Expr |
  EPlus Int Expr Expr |
  EMinus Int Expr Expr |
  ETimes Int Expr Expr |
  ECat Int Expr Expr |
  ELen Int Expr
    deriving (Eq)

instance Show Expr where
  show = showExpr

data LExpr = LExpr Int Expr deriving (Show)

showExpr :: Expr -> String
showExpr e0 = case e0 of
  EZ _ n -> show n
  ES _ t -> show t
  EV _ s -> s
  EPlus _ e1 e2 -> showExpr e1 ++ " + " ++ showExpr e2
  EMinus _ e1 e2 -> showExpr e1 ++ " - " ++ showExpr e2
  ETimes _ e1 e2 -> showExpr e1 ++ " * " ++ showExpr e2
  ECat _ e1 e2 -> showExpr e1 ++ " ++ " ++ showExpr e2
  ELen _ e -> "len("++showExpr e++")"
  EL _ v e1 e2 -> "let "++v++" = "++showExpr e1++" in\n"++showExpr e2

