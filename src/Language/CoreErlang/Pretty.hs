{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- |
-- Module      :  Language.CoreErlang.Parser
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
--                (c) Feng Lee 2020
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Feng Lee <feng@emqx.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for CoreErlang.
-- <http://erlang.org/doc/apps/compiler/compiler.pdf>
module Language.CoreErlang.Pretty (Pretty(..)) where

import Data.Text (empty)
import Language.CoreErlang.Syntax
import Data.Text.Prettyprint.Doc
import Text.Megaparsec (errorBundlePretty, ParseErrorBundle(..), Stream(..), ShowErrorComponent(..))

-- import Data.Text.Prettyprint.Doc.Internal(unsafeTextWithoutNewlines)
commaSep :: Pretty a => [a] -> Doc ann
commaSep xs = concatWith (surround ",") (map pretty xs)

tuplePretty :: Pretty a => [a] -> Doc ann
tuplePretty = braces . commaSep

binaryPretty :: Pretty a => [a] -> Doc ann
binaryPretty xs = enclose "#{" "}#" (commaSep xs)
-- mapPretty :: [Text] -> Doc ann
-- mapPretty kvs = enclose (commaSep kvs) "~{" "}~"
varsPretty :: [VarA] -> Doc ann
varsPretty xs = angles (commaSep xs)
--
instance (Stream s, ShowErrorComponent e, Pretty a) => Pretty (Either (ParseErrorBundle s e) a) where
  pretty (Right x) = pretty x
  pretty (Left  r) = error ("parseFail" ++ errorBundlePretty r)

instance Pretty Atom where
  pretty (Atom x) = squotes (pretty x)

instance Pretty a => Pretty (List a) where
  pretty (L  x)   = brackets $ pretty x
  pretty (LL x y) = brackets $ pretty x <> "|" <> pretty y

instance (Pretty kv, Pretty a) => Pretty (Map kv a) where
  pretty (Map  kvs)   = enclose "~{" "}~" (commaSep kvs)
  pretty (UMap kvs x) = enclose "~{" "}~" (commaSep kvs <> "|" <> pretty x)

instance (Pretty k, Pretty v) => Pretty (KeyV k v) where
  pretty (Insert x y) = pretty x <> "=>" <> pretty y
  pretty (Update x y) = pretty x <> ":=" <> pretty y

instance Pretty a => Pretty (Bitstring a) where
  pretty (Bitstring x y z m n) = "#" <> angles (pretty x) <> parens (commaSep [y,z,m,n])

instance Pretty FunDef where
  pretty (FunDef funname fun) = pretty funname <+> "=" <+> pretty fun

--------------------------------------------------------------------------------

instance Pretty Module where
  pretty (Module atom funnames attrs fundefs)
        = "module" <+> pretty atom
      <+> brackets (commaSep funnames)
      <+> "attributes" <+> pretty attrs
      <+> vsep (pretty <$> fundefs) <+> "end"

instance Pretty FunName where
  pretty (FunName atom n) = pretty atom <> "/" <> pretty n

instance {-# OVERLAPPING #-} Pretty ((,) Atom ConstA) where
  pretty (x, y) = pretty x <+> "=" <+> pretty y

instance Pretty Const where
  pretty (CLit    x)        = pretty x
  pretty (CTuple  consts)   = tuplePretty consts
  pretty (CList   list)     = pretty list
  pretty (CMap    kvs)      = enclose "~{" "}~" (commaSep kvs)
  pretty (CBinary bstrings) = binaryPretty bstrings

instance Pretty Literal where
  pretty (LChar   x) = pretty x
  pretty (LString x) = pretty x
  pretty (LInt    x) = pretty x
  pretty (LFloat  x) = pretty x
  pretty (LAtom   x) = pretty x
  pretty LNil        = "[]"

instance Pretty a => Pretty (Fun a) where
  pretty (Fun vars x)          = "fun" <+> parens (commaSep vars) <+> "->" <+> pretty x
  pretty (ExtFun atom funname) = "fun" <+> pretty atom <> ":" <> pretty funname

instance Pretty Var where
  pretty (Var x) = pretty x

instance Pretty Exprs where
  pretty (Expr x)   = pretty x
  pretty (Exprs xs) = angles $ commaSep xs

instance Pretty a => Pretty (Expr a) where
  pretty (EVar    var)      = pretty var
  pretty (EFunN   funname)  = pretty funname
  pretty (ELit    literal)  = pretty literal
  pretty (EFun    fun)      = pretty fun
  pretty (EList   list)     = pretty list
  pretty (ETuple  xs)       = tuplePretty xs
  pretty (EMap    x)        = pretty x
  pretty (EBinary bstrings) = binaryPretty bstrings

  pretty (ELetRec  fundefs x)   = "letrec"  <+> hsep (pretty <$> fundefs) <+> "in" <+> pretty x
  pretty (ELet     vars x y)    = "let"     <+> varsPretty vars <+> "=" <+> pretty x <+> "in" <+> pretty y
  pretty (ECase    x clauses)   = "case"    <+> pretty x <+> "of" <+> vsep (pretty <$> clauses) <+> "end"
  pretty (EApp     x y)         = "apply"   <+> pretty x <+> parens (commaSep y)
  pretty (EModCall x y xs)      = "call"    <+> pretty x <> ":" <> pretty y <+> parens (commaSep xs)
  pretty (EPrimOp  x xs)        = "primop"  <+> pretty x <+> parens (commaSep xs)
  pretty (EReceive clauses x y) = "receive" <+> vsep (pretty <$> clauses) <+> "after" <+> pretty x <+> "->" <+> pretty y
  pretty (ETry     x vs y ws z) = "try"     <+> pretty x <+> "of" <+> (angles $ hsep (map pretty vs)) <+> "->" <+> pretty y
                              <+> "catch"   <+> varsPretty ws <+> "->" <+> pretty z
  pretty (EDo      x y)         = "do"      <+> pretty x <+> pretty y
  pretty (ECatch   x)           = "catch"   <+> pretty x

instance Pretty a => Pretty (Clause a) where
  pretty (Clause pats x y) = angles (commaSep pats) <+> "when" <+> pretty x <+> "->" <+> pretty y

instance Pretty Pat where
  pretty (PVar var)         = pretty var
  pretty (PLiteral lit)     = pretty lit
  pretty (PList list)       = pretty list
  pretty (PTuple pats)      = tuplePretty pats
  pretty (PBinary bstrings) = binaryPretty bstrings
  pretty (PMap m)           = pretty m
  pretty (PAlias var pat)   = pretty var <+> "=" <+> pretty pat

--------------------------------------------------------------------------------
instance Pretty a => Pretty (Ann a) where
  pretty (Ann x y)
    | y == empty = pretty x
    | otherwise  = parens $ pretty x <> "-|" <> pretty y



--------------------
--simple test
