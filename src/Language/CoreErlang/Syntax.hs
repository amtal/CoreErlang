{-# LANGUAGE DeriveDataTypeable #-}
module Language.CoreErlang.Syntax where
import Data.Text(Text)
import Data.Data
import Prelude

--------------------------------------------------------------------------------
data Atom = Atom Text
  deriving (Eq, Ord, Show, Data, Typeable)

data List a = L a
            | LL a a
  deriving (Eq, Ord, Show, Data, Typeable)

data Map kv a =  Map [kv]
               | UMap [kv] a
               deriving (Eq, Ord, Show, Data, Typeable)

data KeyV k v = Insert k v
              | Update k v
              deriving (Eq, Ord, Show, Data, Typeable)

data Bitstring a = Bitstring a a a a a
  deriving (Eq, Ord, Show, Data, Typeable)

data FunDef = FunDef FunNameA (FunA ExprsA)
  deriving (Eq, Ord, Show, Data, Typeable)

--------------------------------------------------------------------------------

data Module = Module Atom [FunNameA] AttrsA [FunDefA]
  deriving (Eq, Ord, Show, Data, Typeable)

data FunName = FunName Atom Integer
  deriving (Eq, Ord, Show, Data, Typeable)

type Attrs = [(Atom, ConstA)]

data Const = CLit LiteralA
           | CTuple [ConstA]
           | CList (List ConstA)
           | CMap [(ConstA, ConstA)]
           | CBinary [Bitstring ConstA]
           deriving (Eq, Ord, Show, Data, Typeable)

data Literal = LChar   Char    -- ^ character literal
             | LString Text  -- ^ string literal
             | LInt    Integer -- ^ integer literal
             | LFloat  Float  -- ^ floating point literal
             | LAtom   Atom    -- ^ atom literal
             | LNil            -- ^ empty list
             deriving (Eq, Ord, Show, Data, Typeable)

data Fun a = Fun [VarA] a
           | ExtFun Atom FunName
  deriving (Eq, Ord, Show, Data, Typeable)

data Var = Var Text
  deriving (Eq, Ord, Show, Data, Typeable)

data Exprs = Expr (ExprA ExprsA)
           | Exprs [ExprA ExprsA]
           deriving (Eq, Ord, Show, Data, Typeable)

data Expr a = EVar VarA                    -- ^ variable
            | EFunN FunNameA                 -- ^ function name
            | ELit LiteralA                 -- ^ literal constant
            | EFun (FunA a)                  -- ^ lambda expression
            | EList (List (ExprA a))          -- ^ list expression
            | ETuple [a]               -- ^ tuple expression
            | EMap (Map (KeyV a a) a)
            | EBinary [BitstringA a]    -- ^ binary expression

            | ELet [VarA] a a    -- ^ local declaration
            | ECase a [ClauseA a]        -- ^ @case@ /exp/ @of@ /alts/ end
            | ELetRec [FunDefA] a       -- ^ letrec expression
            | EApp a [a]           -- ^ application
            | EModCall a a [a]  -- ^ module call
            | EPrimOp (Ann Atom) [a]         -- ^ operator application
            | EReceive [ClauseA a] a a      -- ^ receive expression
            | ETry a [VarA] a [VarA] a -- ^ try expression
            | EDo a a             -- ^ sequencing
            | ECatch a                 -- ^ catch expression
            deriving (Eq, Ord, Show, Data, Typeable)

data Clause a = Clause [PatA] a a
  deriving (Eq, Ord, Show, Data, Typeable)

data Pat = PVar VarA
         | PLiteral LiteralA
         | PList (List PatA)
         | PTuple [PatA]
         | PBinary [BitstringA PatA]
         | PMap (Map (KeyVA PatA PatA) PatA)
         | PAlias VarA PatA
         deriving (Eq, Ord, Show, Data, Typeable)

--------------------------------------------------------------------------------
data Ann a = Ann a Text
  deriving (Eq, Ord, Show, Data, Typeable)

type ModuleA   = Ann Module
type FunNameA  = Ann FunName
type AttrsA    = Ann Attrs
type ConstA    = Ann Const
type LiteralA  = Ann Literal
type FunDefA   = Ann FunDef
type FunA a    = Ann (Fun a)
type VarA      = Ann Var
type ExprsA    = Ann Exprs
type ExprA a   = Ann (Expr a)
type ClauseA a = Ann (Clause a)
type PatA      = Ann Pat

type BitstringA a = Ann (Bitstring a)
type KeyVA k v = Ann (KeyV k v)
