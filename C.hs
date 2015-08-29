module C where

import Expr
import Infer
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.ByteString as BS
import Numeric
import Data.Char

type RCtx = M.Map String String
type Compiler = StateT [String] (Writer [CCode])

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
    ,   unlines (map compileC result)
    ,   output
    ,"  return 0;"
    ,"}"
    ]
  output = case infer e of
    TZ -> "  printf(\"%d\\n\", (int)final);\n"
    TS -> "  printf(\"%s\\n\", (char*)final);\n"
  result = (execWriter . flip evalStateT names) (outputCode M.empty e "final")

newReg :: Compiler String
newReg = do
  (n:ames) <- get
  put ames
  out $ CDeclare ("  void* "++n++";\n")
  return n

out :: CCode -> Compiler ()
out code = tell [code]

outputCode :: RCtx -> Expr -> String -> Compiler ()
outputCode c e0 ret = case e0 of
  EZ _ z -> out $ CCall "mpz_set_str" [CV ret, CS (T.pack (show z)), CZ 10]
  ES _ s -> out $ CAssign ret (CCall "newConstantString" [CS s, CZ (byteLenS s)])
  EV _ v -> case M.lookup v c of
    Just r -> out $ CAssign ret (CV r)
    Nothing -> error "bug"
  EPlus _ e1 e2 -> outputMath c "mpz_add" e1 e2 ret
  EMinus _ e1 e2 -> outputMath c "mpz_sub" e1 e2 ret
  ETimes _ e1 e2 -> outputMath c "mpz_mul" e1 e2 ret
  ECat _ e1 e2 -> do
    r1 <- newReg
    r2 <- newReg
    outputCode c e1 r1
    outputCode c e2 r2
    out $ CAssign ret (CCall "catString" [CV r1, CV r2])
  ELen _ e -> do
    r <- newReg
    outputCode c e r
    out $ CCall "mpz_set_si" [CV ret, CCall "utf8Len" [CCast "string*" (CV r)]]
  EDec _ e -> do
    r <- newReg
    outputCode c e r
    out $ CCall "decimal" [CV ret, CV r]
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
  out $ CCall op [CV ret, CV r1, CV r2]

data CCode =
  CZ Integer |
  CS Text |
  CInclude String |
  CCall String [CCode] |
  CStruct String [String] |
  CCast String CCode |
  COp String CCode CCode |
  CAssign String CCode |
  CDeclare String |
  CFunc String String [String] [CCode] |
  CIf CCode [CCode] [CCode] |
  CNoop |
  CV String |
  CBlock [CCode]

compileC :: CCode -> String
compileC c = case c of
  CZ z -> show z
  CInclude s -> "#include <"++s++">"
  CNoop -> ""
  CAssign v code -> "  "++v++" = "++compileC code++";"
  CV s -> s
  CCast s code -> "("++s++")"++compileC code
  CS s -> encodeS s

encodeS :: Text -> String
encodeS = quote . concatMap f . BS.unpack . encodeUtf8 where
  f w | w < 32 || w > 127 = "\\x" ++ showHex w ""
      | w == 34 = "\\\""
      | w == 92 = "\\\\"
      | otherwise = chr (fromIntegral w) : ""

byteLenS :: Text -> Integer
byteLenS = fromIntegral . BS.length . encodeUtf8

quote :: String -> String
quote s = "\"" ++ s ++ "\""
