{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use bimap" #-}
module CSharp.CodeGen where

import CSharp.AbstractSyntax
import CSharp.Algebra

import SSM

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M
import Data.Map (Map)

{-
  This file contains a starting point for the code generation.
-}

-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type C = Code                                           -- Class
type M = (GVarEnv -> (Code, GVarEnv),VarFun)            -- Member
type S = GVarEnv -> LVarEnv -> ((Code, LVarEnv), IsDecl)          -- Statement
type E = GVarEnv -> LVarEnv -> ValueOrAddress -> Code   -- Expression


type IsDecl = Bool
data VarFun = VFVar | VFFun

type Declared = [Decl]

data GVarEnv = GVarEnv {globals :: [ClassVar] }

data LVarEnv = LVarEnv {locals :: [StackVar] }

addGlobal :: Ident -> RetType -> GVarEnv -> GVarEnv
addGlobal id rt env = GVarEnv $ CV (length $ globals env) rt id : globals env

getGlobal :: Ident -> ValueOrAddress -> GVarEnv -> Maybe Int
getGlobal id va (GVarEnv cvs) = find cvs
  where
    find :: [ClassVar] -> Maybe Int
    --find _ = error $ "id: " ++ id ++ "va: " ++ show va ++ "cvs: " ++ show cvs 
    find (cv:cvs)
      | gName cv == id = Just $ relativeToMK cv
      | otherwise = find cvs
    find [] = Nothing

data StackVar = SV {
  relativeToSP :: Int
  ,lType :: RetType
  ,lName :: Ident
} deriving (Show)

addLocal :: Ident -> RetType -> LVarEnv -> Int -> LVarEnv
addLocal id rt env place = LVarEnv $ SV place rt id : locals env

getLocal :: Ident -> ValueOrAddress -> LVarEnv -> Maybe Int
getLocal id va (LVarEnv svs) = find svs
  where
    find :: [StackVar] -> Maybe Int
    find (cv:cvs)
      | lName cv == id = Just $ relativeToSP cv
      | otherwise = find cvs
    find [] = Nothing

data ClassVar = CV {
  relativeToMK :: Int
  , gType :: RetType
  , gName :: Ident
  } deriving (Show)

codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
  fClass
  fMembDecl
  fMembMeth
  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock
  fExprLit
  fExprVar
  fExprOp

-- | should merge class variables 
fClass :: ClassName -> [M] -> C
fClass c ms = [Bsr "main", HALT] ++ appEnv
  where
    appEnv :: Code
    appEnv = app (GVarEnv []) (map fst $ decl ++ fun)
    decl = filter fil ms
    fun = filter (not.fil) ms

    fil m = case m of
      (_,VFFun) -> False
      (_,VFVar) -> True

app :: a -> [a -> ([b],a)] -> [b]
app a [] = []
app a (f:fs) = fst (f a) ++ app (snd $ f a) fs

-- | returns fun from env to code and new env, and its declaration type
fMembDecl :: Decl -> M
fMembDecl (Decl rt id) = (\gvs -> ([], addGlobal id rt gvs), VFVar)

--M = (GVarEnv -> (Code, GVarEnv),VarFun)  
fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t id ps s = (news, VFFun)
  where
    news genv = ([LABEL id, LDR MP,LDRR MP SP] ++ scode ++ [AJS (-1),RET], genv)
      where
        sgenv = s genv
        slenv = sgenv locVarEnv
        isDecl = [AJS (-1)| snd slenv] 
        scode = fst (fst slenv) ++ isDecl
    locVarEnv :: LVarEnv
    locVarEnv = LVarEnv (zipWith (curry parToVar) ps beforeList)
    params :: Int
    params = length ps
    beforeList :: [Int]
    beforeList = map (\x -> x - params - 1) [1,2..]
    parToVar :: (Decl,Int) -> StackVar
    parToVar (Decl rt id,i) = SV i rt id



-- | local variable should store something about the stack or something iddunu
fStatDecl :: Decl -> S
fStatDecl (Decl rt id) gvs lvs = (([AJS 1], addLocal id rt lvs loc),True)
  where
    loc = numafter + 1
    numafter :: Int
    numafter = length $ filter (\sv -> relativeToSP sv > 0) (locals lvs)

-- S = GVarEnv -> LVarEnv -> (Code, LVarEnv) 

fStatExpr :: E -> S
fStatExpr e env lenv = ((e env lenv Value ++ [pop], lenv),False)

-- | code in if cant change env out of if
fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 genv lenv = ((expCode ++ [BRF ifCode] ++ s1code ++ [BRA elseCode] ++ s2code, lenv),False) where
  expCode = e genv lenv Value
  ifCode = codeSize s1code + 2
  elseCode = codeSize s2code
  s1genv = s1 genv
  s1lenv = s1genv lenv
  s2genv = s2 genv
  s2lenv = s2genv lenv
  is1Decl = [AJS (-1) | snd s1lenv]
  is2Decl = [AJS (-1) | snd s2lenv]
  s1code = fst (fst s1lenv) ++ is1Decl
  s2code = fst (fst s1lenv) ++ is2Decl


fStatWhile :: E -> S -> S
fStatWhile e s1 genv lenv= (([BRA (codeSize $ fst$ fst s1lenv)] ++ fst (fst s1lenv) ++ c ++ [BRT (-(codeSize (fst $ fst s1lenv) + k + 2))] ++ isDecl, lenv),False) where
  s1genv = s1 genv
  s1lenv = s1genv lenv
  isDecl :: Code
  isDecl = [AJS (-1) | snd s1lenv]
  c = e genv lenv Value
  k = codeSize c

-- TODO needs to clean pars
fStatReturn :: E -> S
fStatReturn e env lenv = ((e env lenv Value ++ [pop] ++ [RET], lenv),False)

--S = GVarEnv -> LVarEnv -> (Code, LVarEnv)
-- executes multiple statements
fStatBlock :: [S] -> S
fStatBlock ss genv lenv = ((fst sslenv ++ replicate numOfDecl (AJS (-1)),lenv),False)
  where
    ssgenv :: [LVarEnv -> ((Code, LVarEnv), IsDecl)]
    ssgenv = map (\f -> f genv) ss
    sslenv :: (Code, [Bool])
    sslenv = app1 lenv ssgenv
    numOfDecl :: Int
    numOfDecl = length $ filter id (snd sslenv)
-- app lenv ss
--app :: a -> [a -> ([b],a)] -> [b]
--app :: a -> [a -> ([b],a)] -> [b]
--app a [] = []
--app a (f:fs) = fst (f a) ++ app (snd $ f a) fs

app1 :: a -> [a -> (([b],a),c)] -> ([b],[c])
app1 a [] = ([],[])
app1 a (f:fs) = (fst (fst $ f a) ++ fst applie, snd (f a) : snd applie)
  where
    applie = app1 (snd $ fst $ f a) fs


-- puts the literal on the stack
fExprLit :: Literal -> E
fExprLit l va genv lenv = [LDC n] where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

-- not finished
fExprVar :: Ident -> E
fExprVar id genv lenv va = loc
    -- ldl    load local push value relative to mp
    -- ldla   load local adress push adress of value relative to MP
    -- str Store Register.  Pops a value from the stack and stores it in a location relative to the markpointer
    -- stl Store Local.     Pops a value from the stack and stores it in a location relative to the markpointer.
  where
    loc = case getLocal id va lenv of
      Just i -> case va of
        Value   ->  [LDL  i]
        Address ->  [LDLA i]
      Nothing -> case getGlobal id va genv of
        Just i -> case va of
          Value -> []
          Address -> []
        Nothing -> error "whoops"

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 genv lenv va = e2 genv lenv Value ++ [LDS 0] ++ e1 genv lenv Address ++ [STA 0]
fExprOp OpAnd e1 e2 genv lenv va = e1 genv lenv Value ++ [LDS 0, BRF (codeSize rightOperand)] ++ rightOperand -- TODO is LDS 0 the official of of copying the return value?
  where rightOperand = e2 genv lenv Value ++ [AND]
fExprOp OpOr  e1 e2 genv lenv va = e1 genv lenv Value ++ [LDS 0, BRT (codeSize rightOperand)] ++ rightOperand
  where rightOperand = e2 genv lenv Value ++ [OR]
fExprOp op    e1 e2 genv lenv va = e1 genv lenv Value ++ e2 genv lenv Value ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD
    ; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ]

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0
