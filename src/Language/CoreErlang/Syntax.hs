-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CoreErlang.Syntax
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  Alex Kropivny <alex.kropivny@gmail.com>
--                Feng Lee <feng@emqx.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- Abstract Syntax Tree of CoreErlang:
-- <http://erlang.org/doc/apps/compiler/compiler.pdf>
-- <https://github.com/erlang/otp/blob/master/lib/compiler/src/cerl.erl>
-- <https://github.com/erlang/otp/blob/master/lib/compiler/src/core_parser.hrl>
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module Language.CoreErlang.Syntax
  ( -- * Modules
    Module(..)
    -- * Declarations
  , FunDef(..)
    -- * Expressions
  , Expr(..), Exprs(..), Alt(..), Guard(..), Map(..)
  , List(..), TimeOut(..), Bitstring(..), FunName(..)
    -- * Patterns
  , Pats(..), Pat(..), Key(..), Alias(..)
    -- * Literals
  , Literal(..), Const(..), Atom(..)
    -- * Variables
  , Var
    -- * Annotations
  , Ann(..)
  ) where

import Prelude
import Data.Data

-- | This type is used to represent atoms
data Atom = Atom String
  deriving (Eq, Ord, Show, Data, Typeable)

-- | This type is used to represent function names
data FunName = FunName (Atom, Integer)
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Module Name
type Name = Atom
-- | Module Exports
type Exports = [FunName]
-- | Module Attrs
type Attrs = [(Atom, Const)]
-- | Module fundefs
type FunDefs = [FunDef]
-- | A CoreErlang source module.
data Module = Module Name Exports Attrs FunDefs
  deriving (Eq, Ord, Show, Data, Typeable)

-- | This type is used to represent constants
data Const
  = CLit Literal
  | CTuple [Const]
  | CList (List Const)
  | CMap (Map Const Const)
  deriving (Eq, Ord, Show, Data, Typeable)

-- | This type is used to represent lambdas
data FunDef = FunDef (Ann FunName) (Ann Expr)
  deriving (Eq, Ord, Show, Data, Typeable)

-- | /literal/.
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used. For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data Literal
  = LChar   Char    -- ^ character literal
  | LString String  -- ^ string literal
  | LInt    Integer -- ^ integer literal
  | LFloat  Double  -- ^ floating point literal
  | LAtom   Atom    -- ^ atom literal
  | LNil            -- ^ empty list
  deriving (Eq, Ord, Show, Data, Typeable)

-- | CoreErlang expressions.
data Exprs
  = Expr (Ann Expr)        -- ^ single expression
  | Exprs (Ann [Ann Expr]) -- ^ list of expressions
  deriving (Eq, Ord, Show, Data, Typeable)

-- | CoreErlang expression.
data Expr
  = Var Var                     -- ^ variable
  | Lit Literal                 -- ^ literal constant
  | Fun FunName                 -- ^ function name
  | App Exprs [Exprs]           -- ^ application
  | ModCall (Exprs, Exprs) [Exprs]  -- ^ module call
  | Lambda [Var] Exprs          -- ^ lambda expression
  | Seq Exprs Exprs             -- ^ sequencing
  | Let ([Var], Exprs) Exprs    -- ^ local declaration
  | LetRec [FunDef] Exprs       -- ^ letrec expression
  | Case Exprs [Ann Alt]        -- ^ @case@ /exp/ @of@ /alts/ end
  | Tuple [Exprs]               -- ^ tuple expression
  | List (List Exprs)           -- ^ list expression
  | EMap (Map Exprs Exprs)      -- ^ map expression
  | Binary [Bitstring Exprs]    -- ^ binary expression
  | PrimOp Atom [Exprs]         -- ^ operator application
  | Try Exprs ([Var], Exprs) ([Var], Exprs) -- ^ try expression
  | Rec [Ann Alt] TimeOut       -- ^ receive expression
  | Catch Exprs                 -- ^ catch expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A bitstring.
data Bitstring a = Bitstring a [Exprs]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A list of expressions
data List a = L [a] | LL [a] a
  deriving (Eq, Ord, Show, Data, Typeable)

-- | An erlang map
data Map k v = Map [(k, v)]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | An /alt/ in a @case@ expression
data Alt = Alt Pats Guard Exprs
  deriving (Eq, Ord, Show, Data, Typeable)

data Pats
  = Pat Pat    -- ^ single pattern
  | Pats [Pat] -- ^ list of patterns
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A pattern, to be matched against a value.
data Pat
  = PVar Var                -- ^ variable
  | PLit Literal            -- ^ literal constant
  | PTuple [Pat]            -- ^ tuple pattern
  | PList (List Pat)        -- ^ list pattern
  | PMap (Map Key Pat)      -- ^ map pattern
  | PBinary [Bitstring Pat] -- ^ list of bitstring patterns
  | PAlias Alias            -- ^ alias pattern
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A map key
data Key = KVar Var | KLit Literal
  deriving (Eq, Ord, Show, Data, Typeable)

-- | An alias, used in patterns
data Alias = Alias Var Pat
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A guarded alternative @when@ /exp/ @->@ /exp/.
-- The first expression will be Boolean-valued.
data Guard = Guard Exprs
  deriving (Eq, Ord, Show, Data, Typeable)

-- | The timeout of a receive expression
data TimeOut = TimeOut Exprs Exprs
  deriving (Eq, Ord, Show, Data, Typeable)

-- | This type is used to represent variables
type Var = String

-- | An annotation for modules, variables, ...
data Ann a
  = Constr a      -- ^ core erlang construct
  | Ann a [Const] -- ^ core erlang annotated construct
  deriving (Eq, Ord, Show, Data, Typeable)

