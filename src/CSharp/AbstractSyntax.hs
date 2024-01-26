-- This module defined an abstract syntax tree type for
-- (a subset of) the C# programming language
{-# LANGUAGE InstanceSigs #-}

module CSharp.AbstractSyntax where


type ClassName = String -- Class names
type Ident = String     -- Variable names

data Class    -- Classes (top-level C# programs)
  = Class ClassName [Member]
  deriving (Eq, Ord)

data Member   -- Class members
  = MemberD Decl                      -- global variable declaration
  | MemberM RetType Ident [Decl] Stat -- function (aka "method") defintions
  deriving (Eq, Ord, Show)

data Stat     -- Statements
  = StatDecl   Decl
  | StatExpr   Expr
  | StatIf     Expr Stat Stat
  | StatWhile  Expr Stat
  | StatReturn Expr
  | StatBlock  [Stat]
  deriving (Eq, Ord, Show)

data Literal = LitInt Int | LitBool Bool
  deriving (Eq, Ord, Show)

data Expr   -- Expressions
  = ExprLit   Literal
  | ExprVar   Ident
  | ExprMeth  Ident [Expr]
  | ExprOper  Operator Expr Expr
  deriving (Eq, Ord, Show)

data Operator -- Binary operators
  = OpAdd | OpSub | OpMul | OpDiv
  | OpMod
  | OpAnd | OpOr | OpXor
  | OpLeq | OpLt
  | OpGeq | OpGt
  | OpEq  | OpNeq
  | OpAsg
  deriving (Eq, Show, Ord, Enum, Bounded)

data Decl = Decl RetType Ident  -- Variable declarations
  deriving (Eq, Ord, Show)

-- (Simplified) types of C#, not including "void"
data Type = TyBool | TyInt
  deriving (Eq, Show, Ord, Enum, Bounded)

-- (Simplified) types of C#, including "void"
data RetType
  = TyVoid      -- "void"
  | NV Type     -- "not void"
  deriving (Eq, Ord, Show)


-- pritty printing

instance Show Class where
  --show :: Class -> String 
  show :: Class -> String
  show (Class cname ms) = "class " ++ cname ++ "\n{ \n" ++ concatMap (\m -> iShow 1 m ++ "\n") ms ++ "}"

tShow :: Class -> String 
tShow (Class cname ms) = "class " ++ cname ++ "\n{ \n" ++ concatMap (\m -> tabs 1 ++ show m ++ "\n") ms ++ "}"

class IndentShow a where
  iShow :: Int -> a -> String

instance IndentShow Decl where
  iShow i (Decl rt id) = iShow i rt ++ " " ++ show id

instance IndentShow RetType where 
  iShow i TyVoid = tabs i ++ show "void"
  iShow i (NV TyBool) = tabs i ++ show "bool"
  iShow i (NV TyInt) = tabs i ++ show "int"

instance IndentShow Member where
  iShow i (MemberD d) = iShow i d
  iShow i (MemberM rtyp id ds st ) =
    iShow i rtyp ++ show id ++ "(" ++ separateWith ',' (map (iShow 0) ds) ++ ")" ++ "\n"
    ++ tabs i ++ "{ \n" 
    ++ (\s -> iShow (i+1) s ++ "\n") st
    ++ tabs i ++ "}"

tabs :: Int -> String
tabs i = replicate (i*2) ' '

instance IndentShow Stat where
  iShow i (StatDecl   d) = iShow i d
  iShow i (StatExpr   e) = iShow i e
  iShow i (StatIf     e s (StatBlock [])) = tabs i ++ "if (" ++ iShow 0 e ++ ")\n" ++ psegmenti '{' '}' i s
  iShow i (StatIf     e s1 s2) = tabs i ++ "if (" ++ iShow 0 e ++ ")\n" ++ psegmenti '{' '}' i s1 ++ "\nelse\n" ++ psegmenti '{' '}' i s2
  iShow i (StatWhile  e s) = tabs i ++ "while (" ++ iShow 0 e ++ ")\n" ++ psegmenti '{' '}' i s
  iShow i (StatReturn e) = tabs i ++ "return " ++ iShow 0 e
  iShow i (StatBlock  ss) = concatMap (\s -> iShow i s ++ "\n") ss

instance IndentShow Expr where 
  iShow i (ExprLit   lit) = tabs i ++ show lit
  iShow i (ExprVar   id) = tabs i ++ show id
  iShow i (ExprMeth  id ps) = tabs i ++ show id ++ "(" ++ separateWith ';' (map (iShow 0) ps) ++ ")"
  iShow i (ExprOper  op e1 e2) = iShow i e1 ++ show op ++ iShow 0 e2

psegmenti :: (IndentShow a) => Char -> Char -> Int -> a -> String
psegmenti bb eb i ss = tabs i ++ bb: "\n" ++ iShow (i+1) ss ++ "\n" ++ tabs i ++ [eb]

separateWith :: Char -> [String] -> String
separateWith c [] = []
separateWith c [s] = s
separateWith c (s:ss) = s ++ concatMap (c :) ss

