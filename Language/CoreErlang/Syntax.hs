-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CoreErlang.Syntax
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Henrique Ferreiro García <hferreiro@udc.es>
--                David Castro Pérez <dcastrop@udc.es>
-- Stability   :  experimental
-- Portability :  portable
--
-- A suite of datatypes describing the abstract syntax of CoreErlang 1.0.3.
-- <http://www.it.uu.se/research/group/hipe/cerl/>

-----------------------------------------------------------------------------

module Language.CoreErlang.Syntax (
    -- * Modules
    Module(..),
    -- * Declarations
    FunDef(..),
    -- * Expressions
    Exp(..), Exps(..), Alt(..), Guard(..),
    List(..), TimeOut(..), BitString(..), Function(..),
    -- * Patterns
    Pats(..), Pat(..), Alias(..),
    -- * Literals
    Literal(..), Const(..), Atom(..),
    -- * Variables
    Var,
    -- * Annotations
    Ann(..),
  ) where

-- | This type is used to represent variables
type Var = String

-- | This type is used to represent atoms
data Atom = Atom String
 deriving (Eq,Ord,Show)

-- | This type is used to represent function names
data Function = Function (Atom,Integer)
  deriving (Eq,Ord,Show)

-- | A CoreErlang source module.
data Module
        = Module Atom [Function] [(Atom,Const)] [FunDef]
  deriving (Eq,Ord,Show)

-- | This type is used to represent constants
data Const
        = CLit Literal
        | CTuple [Const]
        | CList (List Const)
  deriving (Eq,Ord,Show)

-- | This type is used to represent lambdas
data FunDef
        = FunDef (Ann Function) (Ann Exp)
  deriving (Eq,Ord,Show)

-- | /literal/.
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used. For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data Literal
        = LChar    Char     -- ^ character literal
        | LString  String   -- ^ string literal
        | LInt     Integer  -- ^ integer literal
        | LFloat   Double   -- ^ floating point literal
        | LAtom   Atom      -- ^ atom literal
        | LNil              -- ^ empty list
  deriving (Eq,Ord,Show)

-- | CoreErlang expressions.
data Exps
        = Exp (Ann Exp)        -- ^ single expression
        | Exps (Ann [Ann Exp]) -- ^ list of expressions
  deriving (Eq,Ord,Show)

-- | CoreErlang expression.
data Exp
        = Var Var                    -- ^ variable
        | Lit Literal                -- ^ literal constant
        | Fun Function               -- ^ function name
        | App Exps [Exps]            -- ^ application
        | ModCall (Exps,Exps) [Exps] -- ^ module call
        | Lambda [Var] Exps          -- ^ lambda expression
        | Seq Exps Exps              -- ^ sequencing
        | Let ([Var],Exps) Exps      -- ^ local declaration
        | LetRec [FunDef] Exps       -- ^ letrec expression
        | Case Exps [Ann Alt]        -- ^ @case@ /exp/ @of@ /alts/ end
        | Tuple [Exps]               -- ^ tuple expression
        | List (List Exps)           -- ^ list expression
        | Binary [BitString Exps]    -- ^ binary expression
        | Op Atom [Exps]             -- ^ operator application
        | Try Exps ([Var],Exps) ([Var],Exps) -- ^ try expression
        | Rec [Ann Alt] TimeOut      -- ^ receive expression
        | Catch Exps                 -- ^ catch expression
  deriving (Eq,Ord,Show)

-- | A bitstring.
data BitString a
        = BitString a [Exps]
  deriving (Eq,Ord,Show)

-- | A list of expressions
data List a
        = L [a]
        | LL [a] a
  deriving (Eq,Ord,Show)

-- | An /alt/ in a @case@ expression
data Alt
        = Alt Pats Guard Exps
  deriving (Eq,Ord,Show)

data Pats
        = Pat Pat    -- ^ single pattern
        | Pats [Pat] -- ^ list of patterns
  deriving (Eq,Ord,Show)

-- | A pattern, to be matched against a value.
data Pat
        = PVar Var                 -- ^ variable
        | PLit Literal             -- ^ literal constant
        | PTuple [Pat]             -- ^ tuple pattern
        | PList (List Pat)         -- ^ list pattern
        | PBinary [BitString Pat]  -- ^ list of bitstring patterns
        | PAlias Alias             -- ^ alias pattern
  deriving (Eq,Ord,Show)

-- | An alias, used in patterns
data Alias
        = Alias Var Pat
  deriving (Eq,Ord,Show)

-- | A guarded alternative @when@ /exp/ @->@ /exp/.
-- The first expression will be Boolean-valued.
data Guard
        = Guard Exps
  deriving (Eq,Ord,Show)

-- | The timeout of a receive expression
data TimeOut
        = TimeOut Exps Exps
  deriving (Eq,Ord,Show)

-- | An annotation for modules, variables, ...
data Ann a
        = Constr a      -- ^ core erlang construct
        | Ann a [Const] -- ^ core erlang annotated construct
  deriving (Eq,Ord,Show)
