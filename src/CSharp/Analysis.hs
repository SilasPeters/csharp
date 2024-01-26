module CSharp.Analysis where

import CSharp.AbstractSyntax
import CSharp.Algebra
import Data.Set ( Set, union, (\\), empty, singleton, fromList, toList )
import Debug.Trace (trace)
import Data.Function (on)

data ScopeBlock = ScopeBlock { decls :: Set Ident, unrecognised :: Set Ident }
  deriving (Show) -- TODO [optimise] make this a newtype (Set Ident, Set Ident)

scopeAlgebra :: CSharpAlgebra [Ident] ScopeBlock ScopeBlock ScopeBlock
scopeAlgebra = CSharpAlgebra
    { clas = \_ scopes -> toList $ unrecognised $
        foldl orderIndependentScope emptyScopeBlock scopes

    , memberD = flip ScopeBlock empty . singleton . fromDecl
    , memberM = \_ _ args b -> completeScope $ ScopeBlock (fromList $ map fromDecl args) empty
    `orderDependentScope` completeScope b -- TODO after methods are implemented, check if undefined variables are allowed when passing them to a method

    , statDecl   = \(Decl _ ident) -> ScopeBlock (singleton ident) empty
    , statExpr   = id
    , statIf     = on eitherScope . orderDependentScope
    , statWhile  = \c s -> completeScope $ orderDependentScope c s
    , statReturn = id
    , statBlock  = completeScope . foldl orderDependentScope emptyScopeBlock

    , exprLit   = const emptyScopeBlock
    , exprVar   = ScopeBlock empty . singleton
    , exprOper  = \_ left right -> left `eitherScope` right
    }

emptyScopeBlock = ScopeBlock empty empty

fromDecl (Decl _ i) = i

-- | The scopes given must be ordered
orderDependentScope :: ScopeBlock -> ScopeBlock -> ScopeBlock
orderDependentScope (ScopeBlock decls1 unr1) (ScopeBlock decls2 unr2)
  = ScopeBlock (decls1 `union` decls2) (unr1 `union` (unr2 \\ decls1))

orderIndependentScope :: ScopeBlock -> ScopeBlock -> ScopeBlock
orderIndependentScope (ScopeBlock decls1 unr1) (ScopeBlock decls2 unr2)
  = ScopeBlock (decls1 `union` decls2) ((unr1 \\ decls2) `union` (unr2 \\ decls1))

eitherScope :: ScopeBlock -> ScopeBlock -> ScopeBlock
eitherScope (ScopeBlock decls1 unr1) (ScopeBlock decls2 unr2)
  = ScopeBlock (decls1 `union` decls2) (unr1 `union` unr2)

completeScope :: ScopeBlock -> ScopeBlock
completeScope (ScopeBlock _ unrs) = ScopeBlock empty unrs

