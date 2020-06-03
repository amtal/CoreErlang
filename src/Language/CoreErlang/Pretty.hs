{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Language.CoreErlang.Pretty
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
-- License     :  BSD-style (see the LICENSE file)
-- Maintainer  :  Alex Kropivny <alex.kropivny@gmail.com>
--             :  Feng Lee <feng@emqx.io>
-- Stability   :  experimental
-- Portability :  portable
--
-- Pretty printer for CoreErlang.
module Language.CoreErlang.Pretty
  ( -- * Pretty printing
    Pretty,
    prettyPrintStyleMode,
    prettyPrintWithMode,
    prettyPrint,

    -- * Pretty-printing styles (from -- "Text.PrettyPrint.HughesPJ")
    P.Style (..),
    P.style,
    P.Mode (..),

    -- * CoreErlang formatting modes
    PPMode (..),
    Indent,
    PPLayout (..),
    defaultMode,
    render,
  )
where

import Language.CoreErlang.Syntax
import qualified Text.PrettyPrint as P
import Prelude hiding ((<>))
import Data.Char as C

infixl 5 $$$

-----------------------------------------------------------------------------

-- | Varieties of layout we can use.
data PPLayout
  = -- | classical layout
    PPDefault
  | -- | everything on a single line
    PPNoLayout
  deriving (Eq)

type Indent = Int

-- | Pretty-printing parameters.
data PPMode
  = PPMode
      { -- | indentation of the alternatives
        -- in a @case@ expression
        altIndent :: Indent,
        -- | indentation of the declarations
        -- in a @case@ expression
        caseIndent :: Indent,
        -- | indentation of the declarations
        -- in a function definition
        fundefIndent :: Indent,
        -- | indentation of the declarations
        -- in a @lambda@ expression
        lambdaIndent :: Indent,
        -- | indentation of the declarations
        -- in a @let@ expression
        letIndent :: Indent,
        -- | indentation of the declarations
        -- in a @letrec@ expression
        letrecIndent :: Indent,
        -- | indentation added for continuation
        -- lines that would otherwise be offside
        onsideIndent :: Indent,
        -- | Pretty-printing style to use
        layout :: PPLayout
      }

-- | The default mode: pretty-print using sensible defaults.
defaultMode :: PPMode
defaultMode =
  PPMode
    { altIndent = 4,
      caseIndent = 4,
      fundefIndent = 4,
      lambdaIndent = 4,
      letIndent = 4,
      letrecIndent = 4,
      onsideIndent = 4,
      layout = PPDefault
    }

-- | Pretty printing monad
newtype DocM s a = DocM (s -> a)

instance Functor (DocM s) where
  fmap f xs = do x <- xs; return (f x)

instance Applicative (DocM s) where
  pure = return
  (<*>) m1 m2 = do x1 <- m1; x2 <- m2; return (x1 x2)

instance Monad (DocM s) where
  (>>=) = thenDocM
  (>>) = then_DocM
  return = retDocM

{-# INLINE thenDocM #-}

{-# INLINE then_DocM #-}

{-# INLINE retDocM #-}

{-# INLINE unDocM #-}

{-# INLINE getPPEnv #-}

thenDocM :: DocM s a -> (a -> DocM s b) -> DocM s b
thenDocM m k = DocM $ (\s -> case unDocM m $ s of a -> unDocM (k a) $ s)

then_DocM :: DocM s a -> DocM s b -> DocM s b
then_DocM m k = DocM $ (\s -> case unDocM m $ s of _ -> unDocM k $ s)

retDocM :: a -> DocM s a
retDocM a = DocM (\_s -> a)

unDocM :: DocM s a -> (s -> a)
unDocM (DocM f) = f

-- all this extra stuff, just for this one function.
getPPEnv :: DocM s s
getPPEnv = DocM id

-- | The document type produced by these pretty printers uses a 'PPMode'
-- environment.
type Doc = DocM PPMode P.Doc

-- | Things that can be pretty-printed, including all the syntactic objects
-- in "Language.CoreErlang.Syntax".
class Pretty a where
  -- | Pretty-print something in isolation.
  pretty :: a -> Doc

  -- | Pretty-print something in a precedence context.
  prettyPrec :: Int -> a -> Doc

  pretty = prettyPrec 0
  prettyPrec _ = pretty

-- The pretty printing combinators

empty :: Doc
empty = return P.empty

nest :: Int -> Doc -> Doc
nest i m = m >>= return . P.nest i

-- Literals
text :: String -> Doc
text = return . P.text

char :: Char -> Doc
char = return . P.char

integer :: Integer -> Doc
integer = return . P.integer

double :: Double -> Doc
double = return . P.double

-- Simple Combining Forms
parens, brackets, braces :: Doc -> Doc
parens d = d >>= return . P.parens
brackets d = d >>= return . P.brackets
braces d = d >>= return . P.braces

-- quotes d = d >>= return . P.quotes
-- doubleQuotes d = d >>= return . P.doubleQuotes

-- Constants
comma :: Doc
comma = return P.comma

-- semi = return P.semi
-- colon = return P.colon
-- space = return P.space
-- equals = return P.equals

-- Combinators
(<>), (<+>), ($$) :: Doc -> Doc -> Doc
aM <> bM = do a <- aM; b <- bM; return $ a P.<> b
aM <+> bM = do a <- aM; b <- bM; return $ a P.<+> b
aM $$ bM = do a <- aM; b <- bM; return $ a P.$$ b

hcat, hsep, vcat, sep, fsep :: [Doc] -> Doc
hcat dl = sequence dl >>= return . P.hcat
hsep dl = sequence dl >>= return . P.hsep
vcat dl = sequence dl >>= return . P.vcat
sep dl = sequence dl >>= return . P.sep
fsep dl = sequence dl >>= return . P.fsep

-- Yuk, had to cut-n-paste this one from Pretty.hs
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate p (d1 : ds) = go d1 ds
  where
    go d [] = [d]
    go d (e : es) = (d <> p) : go e es

-- | render the document with a given style and mode.
renderStyleMode :: P.Style -> PPMode -> Doc -> String
renderStyleMode ppStyle ppMode d = P.renderStyle ppStyle . unDocM d $ ppMode

-- | render the document with a given mode.
renderWithMode :: PPMode -> Doc -> String
renderWithMode = renderStyleMode P.style

-- | render the document with defaultMode
render :: Doc -> String
render = renderWithMode defaultMode

-- | pretty-print with a given style and mode.
prettyPrintStyleMode :: Pretty a => P.Style -> PPMode -> a -> String
prettyPrintStyleMode ppStyle ppMode = renderStyleMode ppStyle ppMode . pretty

-- | pretty-print with the default style and a given mode.
prettyPrintWithMode :: Pretty a => PPMode -> a -> String
prettyPrintWithMode = prettyPrintStyleMode P.style

-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> String
prettyPrint = prettyPrintWithMode defaultMode

-------------------------  Pretty-Print a Module  --------------------

instance Pretty Module where
  pretty (Module m exports attrs fundefs) =
    topLevel (ppModuleHeader m exports attrs) (map pretty fundefs)

--------------------------  Module Header ------------------------------

ppModuleHeader :: Atom -> [FunName] -> [(Atom, Const)] -> Doc
ppModuleHeader m exports attrs =
  myFsep
    [ text "module" <+> pretty m <+> (bracketList $ map pretty exports),
      text "attributes" <+> bracketList (map ppAssign attrs)
    ]

instance Pretty FunName where
  pretty (FunName (name, arity)) = pretty name <> char '/' <> integer arity

instance Pretty Const where
  pretty = \case
    (CLit l) -> pretty l
    (CTuple l) -> ppTuple l
    (CList l) -> pretty l
    (CMap m) -> ppMap "=>" m

-------------------------  Declarations ------------------------------
instance Pretty FunDef where
  pretty (FunDef function e) =
    (pretty function <+> char '=')
      $$$ ppBody fundefIndent [pretty e]

------------------------- Expressions -------------------------
instance Pretty Literal where
  pretty = \case
    (LChar c) -> return . P.integer . toInteger . C.ord $ c
    (LString s) -> text (show s)
    (LInt i) -> integer i
    (LFloat f) -> double f
    (LAtom a) -> pretty a
    LNil -> bracketList [empty]

instance Pretty Atom where
  pretty (Atom a) = char '\'' <> text a <> char '\''

instance Pretty Exprs where
  pretty = \case
    (Expr e) -> pretty e
    (Exprs (Constr e)) -> angleList (map pretty e)
    (Exprs (Ann e cs)) -> parens (angleList (map pretty e) $$$ ppAnn cs)

instance Pretty Expr where
  pretty = \case
    (EVar v) -> pretty v
    (Lit l) -> pretty l
    (Fun f) -> pretty f
    (ExtFun m f) -> text "fun" <+> pretty m <> char ':' <> pretty f
    (Lam vars e) ->
      sep
        [ text "fun" <> parenList (map pretty vars) <+> text "->",
          ppBody lambdaIndent [pretty e]
        ]
    (App e exps) -> text "apply" <+> pretty e <> parenList (map pretty exps)
    (ModCall (e1, e2) exps) ->
      sep
        [ text "call"
            <+> pretty e1 <> char ':' <> pretty e2,
          parenList (map pretty exps)
        ]
    (Seq e1 e2) -> sep [text "do", pretty e1, pretty e2]
    (Let (vars, e1) e2) ->
      text "let"
        <+> angleList (map pretty vars)
        <+> char '='
        <+> pretty e1
        $$$ text "in"
        <+> pretty e2
    (LetRec fundefs e) ->
      sep
        [ text "letrec" <+> ppBody letrecIndent (map pretty fundefs),
          text "in",
          pretty e
        ]
    (Case e alts) ->
      sep [text "case", pretty e, text "of"]
        $$$ ppBody caseIndent (map pretty alts)
        $$$ text "end"
    (Tuple exps) -> braceList $ map pretty exps
    (List l) -> pretty l
    (EMap m) -> ppMap "=>" m
    (VMap m) -> ppVarMap "=>" m
    (UMap m) -> ppUpdateMap ":=" m
    (PrimOp a exps) -> text "primop" <+> pretty a <> parenList (map pretty exps)
    (Binary bs) -> char '#' <> braceList (map pretty bs) <> char '#'
    (Try e (vars1, exps1) (vars2, exps2)) ->
      text "try"
        $$$ ppBody caseIndent [pretty e]
        $$$ text "of" <+> angleList (map pretty vars1) <+> text "->"
        $$$ ppBody altIndent [pretty exps1]
        $$$ text "catch" <+> angleList (map pretty vars2) <+> text "->"
        $$$ ppBody altIndent [pretty exps2]
    (Rec alts tout) ->
      text "receive"
        $$$ ppBody caseIndent (map pretty alts)
        $$$ text "after"
        $$$ ppBody caseIndent [pretty tout]
    (Catch e) -> sep [text "catch", pretty e]

instance Pretty a => Pretty (List a) where
  pretty (L l) = bracketList $ map pretty l
  pretty (LL h t) = brackets . hcat $ punctuate comma (map pretty h) ++ [char '|' <> pretty t]

instance Pretty Alt where
  pretty (Alt pats guard exps) =
    myFsep [pretty pats, pretty guard <+> text "->"]
      $$$ ppBody altIndent [pretty exps]

instance Pretty Pats where
  pretty = \case
    (Pat p) -> pretty p
    (Pats (Constr p)) -> angleList (map pretty p)
    (Pats (Ann p cs)) -> parens (angleList (map pretty p) $$$ ppAnn cs)

instance Pretty Pat where
  pretty = \case
    (PVar v) -> pretty v
    (PLit l) -> pretty l
    (PTuple p) -> braceList $ map pretty p
    (PList l) -> pretty l
    (PMap m) -> ppMap ":=" m
    (PBinary bs) -> char '#' <> braceList (map pretty bs) <> char '#'
    (PAlias a) -> pretty a

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = ppMap "=>" m

instance (Pretty k, Pretty v) => Pretty (VarMap k v) where
  pretty m = ppVarMap "=>" m

instance (Pretty k, Pretty v) => Pretty (UpdateMap k v) where
  pretty m = ppUpdateMap ":=" m




instance Pretty Key where
  pretty (KVar v) = pretty v
  pretty (KLit l) = pretty l

instance Pretty Alias where
  pretty (Alias v p) = ppAssign (v, p) -- FIXME: hack!

instance Pretty Guard where
  pretty (Guard e) = text "when" <+> pretty e

instance Pretty TimeOut where
  pretty (TimeOut e1 e2) =
    pretty e1 <+> text "->"
      $$$ ppBody altIndent [pretty e2]

instance Pretty a => Pretty (Bitstring a) where
  pretty (Bitstring e es) = text "#<" <> pretty e <> char '>' <> parenList (map pretty es)

----------------------- Var ------------------------

instance Pretty Var where
  pretty (Var v) = case v of
    (Constr s) -> text s
    (Ann s cs) -> parens (text s $$$ ppAnn cs)

----------------------- Annotations ------------------------

instance Pretty a => Pretty (Ann a) where
  pretty (Constr a) = pretty a
  pretty (Ann a cs) = parens (pretty a $$$ ppAnn cs)

------------------------- pp utils -------------------------
angles :: Doc -> Doc
angles p = char '<' <> p <> char '>'

myEmptyList :: [Doc] -> Doc
myEmptyList = myFsepSimple . punctuate comma

angleList :: [Doc] -> Doc
angleList = angles . myFsepSimple . punctuate comma

braceList :: [Doc] -> Doc
braceList = braces . myFsepSimple . punctuate comma

bracketList :: [Doc] -> Doc
bracketList = brackets . myFsepSimple . punctuate comma

parenList :: [Doc] -> Doc
parenList = parens . myFsepSimple . punctuate comma

-- | Monadic PP Combinators -- these examine the env
topLevel :: Doc -> [Doc] -> Doc
topLevel header dl = do
  e <- fmap layout getPPEnv
  let s = case e of
        PPDefault -> header $$ vcat dl
        PPNoLayout -> header <+> hsep dl
  s $$$ text "end"

ppAssign :: (Pretty a, Pretty b) => (a, b) -> Doc
ppAssign (a, b) = pretty a <+> char '=' <+> pretty b

ppTuple :: Pretty a => [a] -> Doc
ppTuple t = braceList (map pretty t)

ppMap :: (Pretty k, Pretty v) => String -> (Map k v) -> Doc
ppMap s (Map l) = char '~' <> braceList (map (ppKV s) l) <> char '~'

ppVarMap :: (Pretty k, Pretty v) => String -> (VarMap k v) -> Doc
ppVarMap s (VarMap l e) =
  char '~'
    <> char '{'
    <> myEmptyList (map (ppKV s) l)
    <> char '|'
    <> pretty e
    <> char '}'
    <> char '~'

ppUpdateMap :: (Pretty k, Pretty v) => String -> (UpdateMap k v) -> Doc
ppUpdateMap s (UpdateMap l e) =
  char '~'
    <> char '{'
    <> myEmptyList (map (ppKV s) l)
    <> char '|'
    <> pretty e
    <> char '}'
    <> char '~'


ppKV :: (Pretty k, Pretty v) => String -> (k, v) -> Doc
ppKV s (k, v) = pretty k <> text s <> pretty v

ppBody :: (PPMode -> Int) -> [Doc] -> Doc
ppBody f dl = do
  e <- fmap layout getPPEnv
  i <- fmap f getPPEnv
  case e of
    PPDefault -> nest i . vcat $ dl
    _ -> hsep dl

($$$) :: Doc -> Doc -> Doc
a $$$ b = layoutChoice (a $$) (a <+>) b

myFsepSimple :: [Doc] -> Doc
myFsepSimple = layoutChoice fsep hsep

-- same, except that continuation lines are indented,
-- which is necessary to avoid triggering the offside rule.
myFsep :: [Doc] -> Doc
myFsep = layoutChoice fsep' hsep
  where
    fsep' [] = empty
    fsep' (d : ds) = do
      e <- getPPEnv
      let n = onsideIndent e
      nest n (fsep (nest (- n) d : ds))

layoutChoice :: (a -> Doc) -> (a -> Doc) -> a -> Doc
layoutChoice a b dl = do
  e <- getPPEnv
  if layout e == PPDefault
    then a dl
    else b dl

ppAnn :: (Pretty a) => [a] -> Doc
ppAnn cs = text "-|" <+> bracketList (map pretty cs)
