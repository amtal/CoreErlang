{-# LANGUAGE OverloadedStrings #-}
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
module Language.CoreErlang.Parser (parseModuleA, Text, ParseErr) where


import Language.CoreErlang.Syntax
import Data.Text(Text, cons, pack, empty)
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void Text

escapes :: [Char]
escapes = "bdefnrstv\"\'\\"

upper :: Parser Char
upper = upperChar

lower :: Parser Char
lower = lowerChar

digit :: Parser Char
digit = digitChar

nameChar :: Parser Char
nameChar = digitChar
       <|> upper <|> lower
       <|> satisfy (=='@')
       <|> satisfy (=='_')

name :: Parser Text
name = pack <$> many nameChar

text :: Char -> Parser Text
text c = pack <$  char c
              <*> manyTill L.charLiteral (char c)
              <*  whitespace
  -- where
  --     notFormat = satisfy (`notElem` ['\r','\n','\\', c])

whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "%") M.empty

symbol :: Tokens Text -> Parser Text
symbol = L.symbol whitespace

wrappedBy :: Parser a -> Text -> Text -> Parser a
wrappedBy p s1 s2 = between (symbol s1) (symbol s2) p

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (symbol ",")

--------------------------------------------------------------------------------
--
-- escape :: Parser Char
-- escape = char '\\' *> escapeChar
--
-- escapeChar :: Parser Char
-- escapeChar = foldr ((<|>) . satisfy . (==)) M.empty escapes

--octal <|> ctrl <|>

--------------------------------------------------------------------------------

tuple, parens, square, angle :: Parser a -> Parser [a]
tuple p = (commaSep p) `wrappedBy` "{" $ "}"
parens p = (commaSep p) `wrappedBy` "(" $ ")"
square p = (commaSep p) `wrappedBy` "[" $ "]"
angle p = (commaSep p) `wrappedBy` "<" $ ">"


listAtom :: Parser [Atom]
listAtom = (:) <$ symbol "["
       <*> atom
       <*> (symbol "]" *> pure []
       <|>  symbol "|" *> listAtom <* symbol "]")
       <*  whitespace

list :: Parser a -> Parser (List a)
list p = symbol "["
      *> (p >>= \x -> option (L x)
                             (LL x <$ symbol "|" <*> p) )
     <*  symbol "]"

-- identifier :: Parser Text
-- identifier = cons <$> (upper <|> char '_')
--                   <*> name
--                   <*  whitespace

atom :: Parser Atom
atom = Atom <$> text '\''

charP :: Parser Char
charP = char '$' *> L.charLiteral

stringP :: Parser Text
stringP = text '\"'

integer :: Parser Integer
integer = L.signed space L.decimal

float :: Parser Float
float = L.signed space L.float

keyV :: Parser k -> Parser v -> Parser (KeyV k v)
keyV k v = try(Insert <$> k <* symbol "=>" <*> v <* whitespace)
       <|> Update <$> k <* symbol ":=" <*> v <* whitespace

keyV' :: Parser a -> Parser (a, a)
keyV' p = (,) <$> p <* symbol "=>" <*> p <* whitespace

mapC :: Parser a -> Parser [a]
mapC p = (commaSep p) `wrappedBy` "~{" $ "}~"

mapE :: Parser kv -> Parser a -> Parser (Map kv a)
mapE kv p = try (Map <$> (mapC kv))
         <|>  ((UMap <$> (commaSep kv)
                    <* symbol "|" <*> p)
             `wrappedBy` "~{" $ "}~")

binary :: Parser a -> Parser [Bitstring a]
binary p = (commaSep (bitstring p)) `wrappedBy` "#{" $ "}#"

binaryA :: Parser a -> Parser [BitstringA a]
binaryA p = (commaSep (bitstringA p)) `wrappedBy` "#{" $ "}#"

bitstring :: Parser a -> Parser (Bitstring a)
bitstring p = Bitstring <$> (p `wrappedBy` "#<" $ ">")
                        <*  symbol "("
                        <*> p         <* symbol ","
                        <*> p         <* symbol ","
                        <*> p     <* symbol ","
                        <*> p
                        <*  symbol ")"

attr :: Parser (Atom, ConstA)
attr = (,) <$> atom <*> (symbol "=" *> constA)

variables :: Parser [VarA]
variables = angle variableA

--------------------------------------------------------------------------------

moduleP :: Parser Module
moduleP = Module <$  symbol "module"
                 <*> atom
                 <*> square funnameA
                 <*> attrsA
                 <*> many fundefA
                 <*  symbol "end"

funname :: Parser FunName
funname = FunName <$> atom <* char '/' <*> integer <* whitespace

attrs :: Parser Attrs
attrs = symbol "attributes"
     *> square attr

constP :: Parser Const
constP = (CTuple  <$> tuple constA
     <|> try (CLit    <$> literalA)
     <|> CList   <$> list constA
     <|> CMap    <$> mapC (keyV' constA)
     <|> CBinary <$> binary constA)
     <*  whitespace

literal :: Parser Literal
literal = (LChar  <$> charP
      <|> LString <$> stringP
      <|> try (LFloat  <$> float)
      <|> LInt    <$> integer
      <|> LAtom   <$> atom
      <|> LNil    <$ symbol "[" <* symbol "]")
      <*  whitespace

fun :: Parser a -> Parser (Fun a)
fun p = symbol "fun"
     *> ((try $ Fun
        <$> parens variableA
        <*  symbol "->"
        <*> p)
    <|> ExtFun <$> atom <* symbol ":" <*> funname)

fundef :: Parser FunDef
fundef = FunDef <$> funnameA
                <*  symbol "="
                <*> funA exprsA

variable :: Parser Var
variable = Var <$> (cons <$> (char '_' <|> upperChar)
                         <*> name)
               <*  whitespace

exprs :: Parser Exprs
exprs = Exprs <$> angle (exprA exprsA)
    <|> Expr  <$> exprA exprsA

expr :: Parser a -> Parser (Expr a)
expr p = EVar     <$> variableA
     <|> EFunN    <$> (try funnameA)
     -- <|> ExtFun Atom FunName
     <|> ELit     <$> (try literalA)
     <|> EFun     <$> funA p
     <|> EList    <$> list (exprA p)
     <|> ETuple   <$> tuple p
     <|> EMap     <$> mapE (keyV p p) p
     <|> EBinary  <$> binaryA p
     <|> ELetRec  <$  symbol "letrec"  <*> many fundefA <* symbol "in" <*> p
     <|> ELet     <$  symbol "let"     <*> variables <* symbol "=" <*> p <* symbol "in" <*> p
     <|> ECase    <$  symbol "case"    <*> p <* symbol "of" <*> some (clauseA p) <* symbol "end"
     <|> EApp     <$  symbol "apply"   <*> p <*> parens p
     <|> EModCall <$  symbol "call"    <*> p <* symbol ":" <*> p <*> parens p
     <|> EPrimOp  <$  symbol "primop"  <*> annotation atom <*> parens p
     <|> EReceive <$  symbol "receive" <*> many (clauseA p) <* symbol "after" <*> p <* symbol "->" <*> p
     <|> ETry     <$  symbol "try"     <*> p <* symbol "of" <*> variables <* symbol "->" <*> p
                  <*  symbol "catch"   <*> variables <* symbol "->" <*> p
     <|> EDo      <$  symbol "do"      <*> p <*> p
     <|> ECatch   <$  symbol "catch"   <*> p

clause :: Parser a -> Parser (Clause a)
clause p = Clause <$> (angle patA
                  <|> (:[]) <$> patA)
                  <* symbol "when" <*> p <* symbol "->" <*> p

pat :: Parser Pat
pat = try (PAlias <$> variableA <* symbol "=" <*> patA)
  <|> try (PLiteral <$> literalA)
  <|> PList   <$> list patA
  <|> PTuple  <$> tuple patA
  <|> PBinary <$> binaryA patA
  <|> PMap    <$> mapE (keyVA patA patA) patA
  <|> PVar    <$> variableA

--------------------------------------------------------------------------------
moduleA :: Parser ModuleA
moduleA = annotation moduleP

funnameA :: Parser FunNameA
funnameA = annotation funname

attrsA :: Parser AttrsA
attrsA = annotation attrs

constA :: Parser ConstA
constA = annotation constP

literalA :: Parser LiteralA
literalA = annotation literal

funA :: Parser a -> Parser (FunA a)
funA = annotation . fun

fundefA :: Parser FunDefA
fundefA = annotation fundef

variableA :: Parser VarA
variableA = annotation variable

exprsA :: Parser ExprsA
exprsA = annotation exprs

exprA :: Parser a -> Parser (ExprA a)
exprA = annotation . expr

clauseA :: Parser a -> Parser (ClauseA a)
clauseA = annotation . clause

patA :: Parser PatA
patA = annotation pat

bitstringA :: Parser a -> Parser (BitstringA a)
bitstringA = annotation . bitstring

keyVA :: Parser k -> Parser v -> Parser (KeyVA k v)
keyVA = (annotation .) . keyV
--------------------------------------------------------------------------------

annotation :: Parser a -> Parser (Ann a)
annotation p = (try ((Ann <$> p <* symbol "-|" <*> takeWhileP (Just "annotation") (/= ')') ) `wrappedBy` "(" $ ")")
           <|> Ann <$> p <*> (pure Data.Text.empty)) <* whitespace

--------------------------------------------------------------------------------

type ParseErr = ParseErrorBundle Text Void

parseModuleA :: Text -> Either ParseErr (ModuleA)
parseModuleA = parse moduleA ""
