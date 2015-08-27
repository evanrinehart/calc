module C where

import Expr
import Infer
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as M

type RCtx = M.Map String String
type Compiler = StateT [String] (Writer String)

names :: [String]
names = map (("__reg"++) . show) [1..]

compile :: Expr -> String
compile e = code where
  code = unlines
    ["#include <stdio.h>"
    ,"#include <string.h>"
    ,"#include <stdlib.h>"
    ,"int main(){"
    ,"  void* final;"
    ,   result
    ,   output
    ,"  return 0;"
    ,"}"
    ]
  output = case infer e of
    TZ -> "  printf(\"%d\\n\", (int)final);\n"
    TS -> "  printf(\"%s\\n\", (char*)final);\n"
  (_, result) = (runWriter . flip evalStateT names) (outputCode M.empty e "final")

newReg :: Compiler String
newReg = do
  (n:ames) <- get
  put ames
  tell ("  void* "++n++";\n")
  return n

outputCode :: RCtx -> Expr -> String -> Compiler ()
outputCode c e0 ret = case e0 of
  EZ _ z -> tell ("  "++ret ++ " = (void*)" ++ show z ++ ";\n")
  ES _ s -> tell ("  "++ret ++ " = (void*)" ++ show s ++ ";\n")
  EV _ v -> case M.lookup v c of
    Just r -> tell ("  "++ret ++ " = (void*)" ++ r ++ ";\n")
    Nothing -> error "bug"
  EPlus _ e1 e2 -> outputMath c "+" e1 e2 ret
  EMinus _ e1 e2 -> outputMath c "-" e1 e2 ret
  ETimes _ e1 e2 -> outputMath c "*" e1 e2 ret
  ECat _ e1 e2 -> do
    r1 <- newReg
    r2 <- newReg
    outputCode c e1 r1
    outputCode c e2 r2
    tell ("  " ++ ret ++ " = (void*)malloc(strlen("++r1++")+strlen("++r2++")+1);\n")
    tell ("  strcpy("++ret++", "++r1++");\n")
    tell ("  strcat("++ret++", "++r2++");\n")
  ELen _ e -> do
    r <- newReg
    outputCode c e r
    tell ("  " ++ ret ++ " = (void*)strlen(" ++ r ++ ");\n")
  EDec _ e -> do
    r <- newReg
    outputCode c e r
    tell ("  " ++ ret ++ " = malloc(64);\n")
    tell ("  sprintf("++ret++", \"%d\", " ++ r ++ ");\n")
  EL _ v e1 e2 -> do
    r <- newReg
    outputCode c e1 r
    outputCode (M.insert v r c) e2 ret

outputMath :: RCtx -> String -> Expr -> Expr -> String -> Compiler ()
outputMath c op e1 e2 ret = do
  r1 <- newReg
  r2 <- newReg
  outputCode c e1 r1
  outputCode c e2 r2
  tell ("  " ++ ret ++ " = (void*)((int)" ++ r1 ++ " " ++ op ++ " (int)" ++ r2 ++ ");\n")
  
  

{-
  allocate two new registers
  output the code to compute e1 and leave the result in reg1
  output the code to computer e2 and leave the result in reg2
  compute the answer and leave the result in your return reg
  EPlus Int Expr Expr
  EMinus Int Expr Expr |
  ETimes Int Expr Expr |
  ECat Int Expr Expr |

  allocate one new register
  output the code to compute e and leave the result in that reg
  compute the answer and leave it in your assigned return reg
  ELen Int Expr |
  EDec Int Expr |

  allocate one new register using the name of the variable
  output the code to compute e1 and leave the result in that reg
  output the code to compute e2 and leave the result in your
  assigned return reg
  EL Int Name Expr Expr |

  read from the reg using the variables name and leave the result
  in your assigned return reg
  EV Int Name |

  write the value to your assigned return reg
  EZ Int Integer |
  ES Int Text |
-}
