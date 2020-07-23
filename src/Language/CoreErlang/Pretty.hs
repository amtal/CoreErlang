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
module Language.CoreErlang.Pretty (Pretty(..), prettyText) where

import Data.Text (empty, replace, Text)
import Data.Char as C
import Language.CoreErlang.Syntax
import Data.Text.Prettyprint.Doc
import Text.Megaparsec (errorBundlePretty, ParseErrorBundle(..), Stream(..), ShowErrorComponent(..))
import Data.Text.Prettyprint.Doc.Render.Text

commaSep :: Pretty a => [a] -> Doc ann
commaSep xs = concatWith (surround ",") (map pretty xs)

hcommaSep :: Pretty a => [a] -> Doc ann
hcommaSep xs = concatWith (surround ("," <> hardline)) (map pretty xs)

tuplePretty :: Pretty a => [a] -> Doc ann
tuplePretty = braces . commaSep

binaryPretty :: Pretty a => [a] -> Doc ann
binaryPretty xs = enclose "#{" "}#" (commaSep xs)

varsPretty :: Pretty a => [Var a] -> Doc ann
varsPretty xs = angles (commaSep xs)


instance {-# OVERLAPPING #-} Pretty a => Pretty (Atom a, Const a) where
  pretty (x, y) = nest 4 $ pretty x <+> "=" <> hardline <+> pretty y
--
instance (Stream s, ShowErrorComponent e, Pretty a) => Pretty (Either (ParseErrorBundle s e) a) where
  pretty (Right x) = pretty x
  pretty (Left  r) = error ("parseFail" ++ errorBundlePretty r)

instance Pretty ann => Pretty (Atom ann) where
  pretty (Atom x ann) = prettyAnn ann . squotes . pretty . replace "'" "\\'" . replace "\\" "\\\\" $ x

instance (Pretty a, Pretty ann) => Pretty (List a ann) where
  pretty (L  x)   = brackets $ pretty x
  pretty (LL x y) = brackets $ pretty x <> "|" <> pretty y

instance (Pretty kv, Pretty a, Pretty ann) => Pretty (Map kv a ann) where
  pretty (Map  kvs)   = enclose "~{" "}~" (commaSep kvs)
  pretty (UMap kvs x) = enclose "~{" "}~" (commaSep kvs <> "|" <> pretty x)

instance (Pretty k, Pretty v, Pretty ann) => Pretty (KeyV k v ann) where
  pretty (Insert x y ann) = prettyAnn ann $ pretty x <> "=>" <> pretty y
  pretty (Update x y ann) = prettyAnn ann $ pretty x <> ":=" <> pretty y

instance (Pretty a, Pretty ann) => Pretty (Bitstring a ann) where
  pretty (Bitstring x y z m n ann) = prettyAnn ann $ "#" <> angles (pretty x) <> parens (commaSep [y,z,m,n])

instance Pretty ann => Pretty (FunDef ann) where
  pretty (FunDef funname fun) = nest 4 $ pretty funname <+> "=" <+> pretty fun

--------------------------------------------------------------------------------

instance Pretty a => Pretty (Module a) where
  pretty (Module atom funnames attrs fundefs ann)
        = prettyAnn ann $
          nest 4 ("module" <+> pretty atom
       <> (align . list . map pretty) funnames <> hardline
       <> (align "attributes" <+> pretty attrs)) <> hardline
       <> vsep (pretty <$> fundefs) <+> "end"

instance Pretty a => Pretty (FunName a) where
  pretty (FunName atom n ann) = prettyAnn ann $ pretty atom <> "/" <> pretty n

instance Pretty a => Pretty (Attrs a) where
  pretty (Attrs xs ann) = prettyAnn ann $ brackets (hcommaSep xs)

instance Pretty a => Pretty (Const a) where
  pretty (CLit    x        ann) = prettyAnn ann $ pretty x
  pretty (CTuple  consts   ann) = prettyAnn ann $ tuplePretty consts
  pretty (CList   list     ann) = prettyAnn ann $ pretty list
  pretty (CMap    kvs      ann) = prettyAnn ann $ enclose "~{" "}~" (commaSep kvs)
  pretty (CBinary bstrings ann) = prettyAnn ann $ binaryPretty bstrings

instance Pretty a => Pretty (Literal a) where
  pretty (LChar   x ann) = prettyAnn ann $ (pretty . toInteger . C.ord $ x)
  pretty (LString x ann) = prettyAnn ann $ pretty (show x)
  pretty (LInt    x ann) = prettyAnn ann $ pretty x
  pretty (LFloat  x ann) = prettyAnn ann $ pretty x
  pretty (LAtom   x ann) = prettyAnn ann $ pretty x
  pretty (LNil      ann) = prettyAnn ann $ "[]"

instance Pretty a => Pretty (Fun a) where
  pretty (Fun vars x          ann) = prettyAnn ann $ "fun" <+> parens (commaSep vars) <+> "->" <> hardline <> pretty x
  pretty (ExtFun atom funname ann) = prettyAnn ann $ "fun" <+> pretty atom <> ":" <> pretty funname

instance Pretty a => Pretty (Var a) where
  pretty (Var x ann) = prettyAnn ann $ pretty x

instance Pretty a => Pretty (Exprs a) where
  pretty (Expr x   ann) = prettyAnn ann $ pretty x
  pretty (Exprs xs ann) = prettyAnn ann $ angles $ commaSep xs

instance Pretty a => Pretty (Expr a) where
  pretty (EVar    var      ann) = prettyAnn ann $ pretty var
  pretty (EFunN   funname  ann) = prettyAnn ann $ pretty funname
  pretty (ELit    literal  ann) = prettyAnn ann $ pretty literal
  pretty (EFun    fun      ann) = prettyAnn ann $ pretty fun
  pretty (EList   list     ann) = prettyAnn ann $ pretty list
  pretty (ETuple  xs       ann) = prettyAnn ann $ tuplePretty xs
  pretty (EMap    x        ann) = prettyAnn ann $ pretty x
  pretty (EBinary bstrings ann) = prettyAnn ann $ binaryPretty bstrings

  pretty (ELetRec  fundefs x   ann) = prettyAnn ann $ "letrec"  <+> hsep (pretty <$> fundefs) <+> "in" <+> pretty x
  pretty (ELet     vars x y    ann) = prettyAnn ann $ "let"     <+> varsPretty vars <+> "=" <> hardline <> pretty x <+> "in" <+> pretty y
  pretty (ECase    x clauses   ann) = prettyAnn ann $ "case"    <+> pretty x <+> "of" <> hardline <> vsep (pretty <$> clauses) <+> "end"
  pretty (EApp     x y         ann) = prettyAnn ann $ "apply"   <+> pretty x <> hardline <> parens (commaSep y)
  pretty (EModCall x y xs      ann) = prettyAnn ann $ "call"    <+> pretty x <> ":" <> pretty y <> hardline <> parens (commaSep xs)
  pretty (EPrimOp  x xs        ann) = prettyAnn ann $ "primop"  <+> pretty x <+> parens (commaSep xs)
  pretty (EReceive clauses x y ann) = prettyAnn ann $ "receive" <+> vsep (pretty <$> clauses) <+> "after" <+> pretty x <+> "->" <+> pretty y
  pretty (ETry     x vs y ws z ann) = prettyAnn ann $ "try"     <+> pretty x <+> "of" <+> (angles $ hsep (map pretty vs)) <+> "->" <+> pretty y
                                   <+> "catch"   <+> varsPretty ws <+> "->" <+> pretty z
  pretty (EDo      x y         ann) = prettyAnn ann $ "do"      <+> pretty x <+> pretty y
  pretty (ECatch   x           ann) = prettyAnn ann $ "catch"   <+> pretty x

instance Pretty a => Pretty (Clause a) where
  pretty (Clause pats x y ann) = prettyAnn ann $ angles (commaSep pats) <+> "when" <+> pretty x <+> "->" <> hardline <> pretty y

instance Pretty a => Pretty (Pat a) where
  pretty (PVar var         ann) = prettyAnn ann $ pretty var
  pretty (PLiteral lit     ann) = prettyAnn ann $ pretty lit
  pretty (PList list       ann) = prettyAnn ann $ pretty list
  pretty (PTuple pats      ann) = prettyAnn ann $ tuplePretty pats
  pretty (PBinary bstrings ann) = prettyAnn ann $ binaryPretty bstrings
  pretty (PMap m           ann) = prettyAnn ann $ pretty m
  pretty (PAlias var pat   ann) = prettyAnn ann $ pretty var <+> "=" <+> pretty pat

--------------------------------------------------------------------------------
prettyAnn :: Pretty a => a -> Doc ann -> Doc ann
prettyAnn y doc
    | show y' == "" = nest 2 doc
    | otherwise  = nest 1 $ parens $ doc <> hardline <> "-|" <> y'
    where
      y' = pretty y

prettyText :: Pretty a => a -> Text
prettyText x = renderStrict (layoutSmart defaultLayoutOptions (pretty x))
--------------------
--simple test
