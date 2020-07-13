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

-- digit :: Parser Char
-- digit = digitChar

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


-- listAtom :: Parser [Atom]
-- listAtom = (:) <$ symbol "["
--        <*> atom
--        <*> (symbol "]" *> pure []
--        <|>  symbol "|" *> listAtom <* symbol "]")
--        <*  whitespace

list :: Parser a -> Parser (List a ann)
list p = symbol "["
      *> (p >>= \x -> option (L x)
                             (LL x <$ symbol "|" <*> p) )
     <*  symbol "]"

-- identifier :: Parser Text
-- identifier = cons <$> (upper <|> char '_')
--                   <*> name
--                   <*  whitespace

atom :: Parser (Text -> Atom Text)
atom = Atom <$> text '\''

charP :: Parser Char
charP = char '$' *> L.charLiteral

stringP :: Parser Text
stringP = text '\"'

integer :: Parser Integer
integer = L.signed space L.decimal

float :: Parser Double
float = L.signed space L.float

keyV :: Parser k -> Parser v -> Parser (Text -> KeyV k v Text)
keyV k v = try(Insert <$> k <* symbol "=>" <*> v <* whitespace)
       <|> Update <$> k <* symbol ":=" <*> v <* whitespace

keyV' :: Parser a -> Parser (a, a)
keyV' p = (,) <$> p <* symbol "=>" <*> p <* whitespace

mapC :: Parser a -> Parser [a]
mapC p = (commaSep p) `wrappedBy` "~{" $ "}~"

mapE :: Parser kv -> Parser a -> Parser (Map kv a ann)
mapE kv p = try (Map <$> (mapC kv))
         <|>  ((UMap <$> (commaSep kv)
                    <* symbol "|" <*> p)
             `wrappedBy` "~{" $ "}~")

-- binary :: Parser a -> Parser [Bitstring a]
-- binary p = (commaSep (bitstring p)) `wrappedBy` "#{" $ "}#"

binary :: Parser a -> Parser (Binary a Text)
binary p = (commaSep (bitstringA p)) `wrappedBy` "#{" $ "}#"

bitstring :: Parser a -> Parser (Text -> Bitstring a Text)
bitstring p = Bitstring <$> (p `wrappedBy` "#<" $ ">")
                        <*  symbol "("
                        <*> p         <* symbol ","
                        <*> p         <* symbol ","
                        <*> p         <* symbol ","
                        <*> p
                        <*  symbol ")"

attr :: Parser (Atom Text, Const Text)
attr = (,) <$> atomA <*> (symbol "=" *> constA)

variables :: Parser [Var Text]
variables = angle variableA

--------------------------------------------------------------------------------

moduleP :: Parser (Text -> Module Text)
moduleP = Module <$  symbol "module"
                 <*> atomA
                 <*> square funnameA
                 <*> attrsA
                 <*> many fundef
                 <*  symbol "end"

funname :: Parser (Text -> FunName Text)
funname = FunName <$> atomA <* char '/' <*> integer <* whitespace

attrs :: Parser (Text -> Attrs Text)
attrs = Attrs <$ symbol "attributes"
     <*> square attr

constP :: Parser (Text -> Const Text)
constP = (CTuple  <$> tuple constA
     <|> try (CLit    <$> literalA)
     <|> CList   <$> list constA
     <|> CMap    <$> mapC (keyV' constA)
     <|> CBinary <$> binary constA)
     <*  whitespace

literal :: Parser (Text -> Literal Text)
literal = (LChar  <$> charP
      <|> LString <$> stringP
      <|> try (LFloat  <$> float)
      <|> LInt    <$> integer
      <|> LAtom   <$> atomA
      <|> LNil    <$ symbol "[" <* symbol "]")
      <*  whitespace

fun :: Parser (Text -> Fun Text)
fun = (try $ Fun
        <$ symbol "fun"
        <*> parens variableA
        <*  symbol "->"
        <*> exprsA)
    <|> ExtFun <$ symbol "fun" <*> atomA <* symbol ":" <*> funnameA

fundef :: Parser (FunDef Text)
fundef = FunDef <$> funnameA
                <*  symbol "="
                <*> funA

variable :: Parser (Text -> Var Text)
variable = Var <$> (cons <$> (char '_' <|> upperChar)
                         <*> name)
               <*  whitespace

exprs :: Parser (Text -> Exprs Text)
exprs = Exprs <$> angle (exprA)
    <|> Expr  <$> exprA

expr :: Parser (Text -> Expr Text)
expr  = EVar     <$> variableA
     <|> EFunN    <$> (try funnameA)
     <|> ELit     <$> (try literalA)
     <|> EFun     <$> funA
     <|> EList    <$> list exprA
     <|> ETuple   <$> tuple exprsA
     <|> EMap     <$> mapE (keyVA exprsA exprsA) exprsA
     <|> EBinary  <$> binary exprsA
     <|> ELetRec  <$  symbol "letrec"  <*> many fundef <* symbol "in" <*> exprsA
     <|> ELet     <$  symbol "let"     <*> variables <* symbol "=" <*> exprsA <* symbol "in" <*> exprsA
     <|> ECase    <$  symbol "case"    <*> exprsA <* symbol "of" <*> some clauseA <* symbol "end"
     <|> EApp     <$  symbol "apply"   <*> exprsA <*> parens exprsA
     <|> EModCall <$  symbol "call"    <*> exprsA <* symbol ":" <*> exprsA <*> parens exprsA
     <|> EPrimOp  <$  symbol "primop"  <*> atomA <*> parens exprsA
     <|> EReceive <$  symbol "receive" <*> many clauseA <* symbol "after" <*> exprsA <* symbol "->" <*> exprsA
     <|> ETry     <$  symbol "try"     <*> exprsA <* symbol "of" <*> variables <* symbol "->" <*> exprsA
                  <*  symbol "catch"   <*> variables <* symbol "->" <*> exprsA
     <|> EDo      <$  symbol "do"      <*> exprsA <*> exprsA
     <|> ECatch   <$  symbol "catch"   <*> exprsA

clause :: Parser (Text -> Clause Text)
clause = Clause <$> (angle patA
                <|> (:[]) <$> patA)
                <* symbol "when" <*> exprsA <* symbol "->" <*> exprsA

pat :: Parser (Text -> Pat Text)
pat = try (PAlias <$> variableA <* symbol "=" <*> patA)
  <|> try (PLiteral <$> literalA)
  <|> PList   <$> list patA
  <|> PTuple  <$> tuple patA
  <|> PBinary <$> binary patA
  <|> PMap    <$> mapE (keyVA patA patA) patA
  <|> PVar    <$> variableA

--------------------------------------------------------------------------------
moduleA :: Parser (Module Text)
moduleA = annotation moduleP

funnameA :: Parser (FunName Text)
funnameA = annotation funname

attrsA :: Parser (Attrs Text)
attrsA = annotation attrs

constA :: Parser (Const Text)
constA = annotation constP

literalA :: Parser (Literal Text)
literalA = annotation literal

funA :: Parser (Fun Text)
funA = annotation fun

-- fundefA :: Parser (FunDef Text)
-- fundefA = annotation fundef

variableA :: Parser (Var Text)
variableA = annotation variable

exprsA :: Parser (Exprs Text)
exprsA = annotation exprs

exprA :: Parser (Expr Text)
exprA = annotation expr

clauseA :: Parser (Clause Text)
clauseA = annotation clause

patA :: Parser (Pat Text)
patA = annotation pat

atomA :: Parser (Atom Text)
atomA = annotation atom

bitstringA :: Parser a -> Parser (Bitstring a Text)
bitstringA = annotation . bitstring

keyVA :: Parser k -> Parser v -> Parser (KeyV k v Text)
keyVA = (annotation .) . keyV
--------------------------------------------------------------------------------

annotation :: Parser (Text -> a) -> Parser a
annotation p = (try ((p <* symbol "-|" <*> takeWhileP (Just "annotation") (/= ')') ) `wrappedBy` "(" $ ")")
           <|> p <*> (pure Data.Text.empty)) <* whitespace

--------------------------------------------------------------------------------

type ParseErr = ParseErrorBundle Text Void

parseModuleA :: Text -> Either ParseErr (Module Text)
parseModuleA = parse moduleA ""
