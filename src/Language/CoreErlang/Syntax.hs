{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.CoreErlang.Syntax where
import Data.Text(Text)
import Data.Data
import Prelude

--------------------------------------------------------------------------------
data Atom ann = Atom Text ann
              deriving (Eq, Ord, Show, Data, Typeable)

data KeyV k v ann = Insert k v ann
                  | Update k v ann
                  deriving (Eq, Ord, Show, Data, Typeable)

data Bitstring a ann = Bitstring a a a a a ann
                   deriving (Eq, Ord, Show, Data, Typeable)

data List a ann = L  a
                | LL a a
                deriving (Eq, Ord, Show, Data, Typeable)

data Map kv a ann =  Map [kv]
                  | UMap [kv] a
                  deriving (Eq, Ord, Show, Data, Typeable)

type Binary a ann = [Bitstring a ann]

data FunDef ann = FunDef (FunName ann) (Fun ann)
                deriving (Eq, Ord, Show, Data, Typeable)

--------------------------------------------------------------------------------

data Module a = Module (Atom a) [FunName a] (Attrs a) [FunDef a] a
              deriving (Eq, Ord, Show, Data, Typeable)

data FunName a = FunName (Atom a) Integer a
               deriving (Eq, Ord, Show, Data, Typeable)

data Attrs a = Attrs [(Atom a, Const a)] a
             deriving (Eq, Ord, Show, Data, Typeable)


data Const a = CLit    (Literal a)             a
             | CTuple  [Const a]               a
             | CList   (List (Const a) a)      a
             | CMap    [(Const a, Const a)]    a
             | CBinary [Bitstring (Const a) a] a
             deriving (Eq, Ord, Show, Data, Typeable)

data Literal a = LChar  Char     a     -- ^ character literal
               | LString Text    a     -- ^ string literal
               | LInt   Integer  a     -- ^ integer literal
               | LFloat Double    a     -- ^ floating point literal
               | LAtom  (Atom a) a     -- ^ atom literal
               | LNil            a     -- ^ empty list
              deriving (Eq, Ord, Show, Data, Typeable)

data Fun a = Fun    [Var a] (Exprs a)    a
           | ExtFun (Atom a) (FunName a) a
           deriving (Eq, Ord, Show, Data, Typeable)

data Var a = Var Text a
  deriving (Eq, Ord, Show, Data, Typeable)

data Exprs a = Expr  (Expr a) a
             | Exprs [Expr a] a
           deriving (Eq, Ord, Show, Data, Typeable)

data Expr a = EVar    (Var a)                                        a
            | EFunN   (FunName a)                                    a
            | ELit    (Literal a)                                    a
            | EFun    (Fun a)                                        a
            | EList   (List (Expr a) a)                              a
            | ETuple  [Exprs a]                                      a
            | EMap    (Map (KeyV (Exprs a) (Exprs a) a) (Exprs a) a) a
            | EBinary [Bitstring (Exprs a) a]                        a

            | ELet     [Var a] (Exprs a) (Exprs a)                   a
            | ECase    (Exprs a) [Clause a]                          a
            | ELetRec  [FunDef a] (Exprs a)                          a
            | EApp     (Exprs a) [Exprs a]                           a
            | EModCall (Exprs a) (Exprs a) [Exprs a]                 a
            | EPrimOp  (Atom a) [Exprs a]                            a
            | EReceive [Clause a] (Exprs a) (Exprs a)                a
            | ETry     (Exprs a) [Var a] (Exprs a) [Var a] (Exprs a) a
            | EDo      (Exprs a) (Exprs a)                           a
            | ECatch   (Exprs a)                                     a
            deriving (Eq, Ord, Show, Data, Typeable)

data Clause a = Clause [Pat a] (Exprs a) (Exprs a) a
  deriving (Eq, Ord, Show, Data, Typeable)

data Pat a = PVar     (Var a)                                  a
           | PLiteral (Literal a)                              a
           | PList    (List (Pat a) a)                         a
           | PTuple   [Pat a]                                  a
           | PBinary  [Bitstring (Pat a) a]                    a
           | PMap     (Map (KeyV (Pat a) (Pat a) a) (Pat a) a) a
           | PAlias   (Var a) (Pat a)                          a
           deriving (Eq, Ord, Show, Data, Typeable)

------------------------------------------------------------------------------

class Ann m a where 
  annText :: a -> (a -> m a) -> m a

instance Ann Atom Text where 
  annText a f = f a

instance Ann (KeyV k v) Text where 
  annText a f = f a

instance Ann (Bitstring a) Text where 
  annText a f = f a

instance Ann Module Text where 
  annText a f = f a

instance Ann FunName Text where 
  annText a f = f a

instance Ann Attrs Text where 
  annText a f = f a

instance Ann Const Text where 
  annText a f = f a

instance Ann Literal Text where 
  annText a f = f a

instance Ann Fun Text where 
  annText a f = f a

instance Ann Var Text where 
  annText a f = f a

instance Ann Exprs Text where 
  annText a f = f a

instance Ann Expr Text where 
  annText a f = f a

instance Ann Clause Text where 
  annText a f = f a

instance Ann Pat Text where 
  annText a f = f a

