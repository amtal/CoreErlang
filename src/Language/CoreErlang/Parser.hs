-----------------------------------------------------------------------------
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
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.CoreErlang.Parser
    ( parseModule
    , ParseError
    , runLex
    ) where

import Language.CoreErlang.Syntax

import Control.Monad ( liftM )
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Token
        ( makeTokenParser, TokenParser )
import Text.ParserCombinators.Parsec.Language

-- Lexical definitions

uppercase :: Parser Char
uppercase = upper

lowercase :: Parser Char
lowercase = lower

namechar :: Parser Char
namechar = uppercase <|> lowercase <|> digit <|> oneOf "@_"

-- Terminals

integer :: Parser Integer
integer = do
  i <- positive <|> negative <|> decimal
  whiteSpace -- TODO: buff
  return $ i

positive :: Parser Integer
positive = do
  char '+'
  p <- decimal
  return p

negative :: Parser Integer
negative = do
  char '-'
  n <- decimal
  return $ negate n

atom :: Parser Atom
atom = do
  char '\''
  a <- many (noneOf "\r\n\\\'")
  char '\''
  whiteSpace -- TODO: buff
  return $ Atom a

echar :: Parser Literal
echar = do
  char '$'
  c <- noneOf "\n\r\\ "
  whiteSpace -- TODO: buff
  return $ LChar c

estring :: Parser Literal
estring = do
  char '"'
  s <- many $ noneOf "\n\r\\\""
  char '"'
  return $ LString s

variable :: Parser Var
variable = liftM Var (annotated identifier)

-- Non-terminals

emodule :: Parser (Ann Module)
emodule = annotated amodule

amodule :: Parser Module
amodule = do
  reserved "module"
  name <- atom
  funs <- exports
  attrs <- attributes
  fundefs <- many fundef
  reserved "end"
  return $ Module name funs attrs fundefs

exports :: Parser [FunName]
exports = brackets $ commaSep fname

attributes :: Parser [(Atom,Const)]
attributes = do
  reserved "attributes"
  brackets $ commaSep attribute

attribute :: Parser (Atom,Const)
attribute = do
  a <- atom
  symbol "="
  c <- constant
  return (a,c)

constant :: Parser Const
constant = liftM CLit (try literal) <|>
           liftM CTuple (tuple constant) <|>
           liftM CList (elist constant) <|>
           liftM CMap (emap "=>" constant constant)

fundef :: Parser FunDef
fundef = do
  name <- annotated fname
  symbol "="
  body <- annotated lambda
  return $ FunDef name body

fname :: Parser FunName
fname = do
  a <- atom
  char '/'
  i <- decimal
  whiteSpace -- TODO: buff
  return $ FunName (a,i)

literal :: Parser Literal
literal = try (liftM LFloat float) <|> liftM LInt integer <|>
          liftM LAtom atom <|> nil <|> echar <|> estring

nil :: Parser Literal
nil = brackets (return LNil)

expression :: Parser Exprs
expression = try (liftM Exprs (annotated $ angles $ commaSep (annotated sexpression))) <|>
             liftM Expr (annotated sexpression)

sexpression :: Parser Expr
sexpression = app <|> ecatch <|> ecase <|> elet <|>
              liftM Fun (try fname) {- because of atom -} <|>
              (try extfun) <|> lambda <|> letrec <|>
              liftM Binary (ebinary expression) <|>
              liftM List (try $ elist expression) {- because of nil -} <|>
              liftM Lit literal <|> modcall <|> op <|> receive <|>
              eseq <|> etry <|> liftM Tuple (tuple expression) <|>
              liftM EMap (emap "=>" expression expression) <|>
              liftM EVar variable

app :: Parser Expr
app = do
  reserved "apply"
  e1 <- expression
  eN <- parens $ commaSep expression
  return $ App e1 eN

ecatch :: Parser Expr
ecatch = do
  reserved "catch"
  e <- expression
  return $ Catch e

ebinary :: Parser a -> Parser [Bitstring a]
ebinary p = do
  symbol "#"
  bs <- braces (commaSep (bitstring p))
  symbol "#"
  return bs

emap :: String -> Parser k -> Parser v -> Parser (Map k v)
emap s kp vp = do
  symbol "~"
  l <- braces (commaSep (emapKV s kp vp))
  symbol "~"
  return $ Map l

emapKV :: String -> Parser k -> Parser v -> Parser (k,v)
emapKV s kp vp = do
  k <- kp
  symbol s
  v <- vp
  return (k,v)

bitstring :: Parser a -> Parser (Bitstring a)
bitstring p = do
  symbol "#"
  e0 <- angles p
  es <- parens (commaSep expression)
  return $ Bitstring e0 es

ecase :: Parser Expr
ecase = do
  reserved "case"
  e <- expression
  reserved "of"
  alts <- many1 (annotated clause)
  reserved "end"
  return $ Case e alts

clause :: Parser Alt
clause = do
  pat <- patterns
  g <- guard
  symbol "->"
  e <- expression
  return $ Alt pat g e

patterns :: Parser Pats
patterns = try (liftM Pats (annotated $ angles $ commaSep (annotated pattern))) <|>
           liftM Pat (annotated pattern)

pattern :: Parser Pat
pattern = liftM PAlias (try alias) {- because of variable -} <|> liftM PVar variable <|>
          liftM PLit (try literal) {- because of nil -} <|> liftM PTuple (tuple pattern) <|>
          liftM PList (elist pattern) <|> liftM PBinary (ebinary pattern) <|>
          liftM PMap (emap ":=" pkey pattern) -- TODO: Fixme later

pkey :: Parser Key
pkey = liftM KVar variable <|> liftM KLit literal

alias :: Parser Alias
alias = do
  v <- variable
  symbol "="
  p <- pattern
  return $ Alias v p

guard :: Parser Guard
guard = do
  reserved "when"
  e <- expression
  return $ Guard e

elet :: Parser Expr
elet = do
  reserved "let"
  vars <- variables
  symbol "="
  e1 <- expression
  symbol "in"
  e2 <- expression
  return $ Let (vars,e1) e2

variables :: Parser [Var]
variables = do { v <- variable; return [v] } <|> (angles $ commaSep variable)

lambda :: Parser Expr
lambda = do
  reserved "fun"
  vars <- parens $ commaSep variable
  symbol "->"
  expr <- expression
  return $ Lambda vars expr

extfun :: Parser Expr
extfun = do
  reserved "fun"
  m <- atom
  symbol ":"
  f <- fname
  return $ ExtFun m f

letrec :: Parser Expr
letrec = do
  reserved "letrec"
  defs <- many fundef
  reserved "in"
  e <- expression
  return $ LetRec defs e

elist :: Parser a -> Parser (List a)
elist a = brackets $ list a

list :: Parser a -> Parser (List a)
list el = do
  elems <- commaSep1 el
  option (L elems) (do symbol "|"
                       t <- el
                       return $ LL elems t)

modcall :: Parser Expr
modcall = do
  reserved "call"
  e1 <- expression
  symbol ":"
  e2 <- expression
  eN <- parens $ commaSep expression
  return $ ModCall (e1,e2) eN

op :: Parser Expr
op = do
  reserved "primop"
  a <- atom
  e <- parens $ commaSep expression
  return $ PrimOp a e

receive :: Parser Expr
receive = do
  reserved "receive"
  alts <- many $ annotated clause
  to <- timeout
  return $ Rec alts to

timeout :: Parser TimeOut
timeout = do
  reserved "after"
  e1 <- expression
  symbol "->"
  e2 <- expression
  return $ TimeOut e1 e2

eseq :: Parser Expr
eseq = do
  reserved "do"
  e1 <- expression
  e2 <- expression
  return $ Seq e1 e2

etry :: Parser Expr
etry = do
  reserved "try"
  e1 <- expression
  reserved "of"
  v1 <- variables
  symbol "->"
  e2 <- expression
  reserved "catch"
  v2 <- variables
  symbol "->"
  _ <- expression
  return $ Try e1 (v1,e1) (v2,e2)

tuple :: Parser a -> Parser [a]
tuple el = braces $ commaSep el

annotation :: Parser [Const]
annotation = do
  symbol "-|"
  cs <- brackets $ many constant
  return $ cs

annotated :: Parser a -> Parser (Ann a)
annotated p = parens (do e <- p
                         cs <- annotation
                         return $ Ann e cs)
              <|>
              do e <- p
                 return $ Constr e

lexer :: TokenParser ()
lexer = makeTokenParser
          (emptyDef {
           --    commentStart = "",
           --    commentEnd = "",
                 commentLine = "%",
           --    nestedComments = True,
                 identStart = upper <|> char '_',
                 identLetter = namechar
           --    opStart,
           --    opLetter,
           --    reservedNames,
           --    reservedOpNames,
           --    caseSensitive = True,
             })

angles, braces, brackets :: Parser a -> Parser a
angles = Token.angles lexer
braces = Token.braces lexer
brackets = Token.brackets lexer

commaSep, commaSep1 :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer

decimal :: Parser Integer
decimal = Token.decimal lexer

float :: Parser Double
float = Token.float lexer

identifier :: Parser String
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

runLex :: Show a => Parser a -> String -> IO ()
runLex p file = do
  input <- readFile file
  parseTest (do whiteSpace
                x <- p
                eof
                return x) input
  return ()

-- | Parse of a string, which should contain a complete CoreErlang module
parseModule :: String -> Either ParseError (Ann Module)
parseModule input = parse (do whiteSpace
                              x <- emodule
                              eof
                              return x) "" input

