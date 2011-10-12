-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CoreErlang.Parser
-- Copyright   :  (c) Henrique Ferreiro García 2008
--                (c) David Castro Pérez 2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Henrique Ferreiro García <hferreiro@udc.es>
--                David Castro Pérez <dcastrop@udc.es>
-- Stability   :  experimental
-- Portability :  portable
--
-- CoreErlang parser.
-- <http://www.it.uu.se/research/group/hipe/cerl/>

-----------------------------------------------------------------------------

module Language.CoreErlang.Parser
    ( parseModule
    , ParseError
    ) where

import Language.CoreErlang.Syntax

import Control.Monad ( liftM )
import Data.Char ( isControl, chr )
import Numeric ( readOct )

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Token
        ( makeTokenParser, TokenParser )
import Text.ParserCombinators.Parsec.Language

-- Lexical definitions

uppercase :: Parser Char
uppercase = upper

lowercase :: Parser Char
lowercase = lower

inputchar :: Parser Char
inputchar = noneOf "\n\r"

control :: Parser Char
control = satisfy isControl

namechar :: Parser Char
namechar = uppercase <|> lowercase <|> digit <|> oneOf "@_"

escape :: Parser Char
escape = do char '\\'
            s <- octal <|> ctrl <|> escapechar
            return s

octal :: Parser Char
octal = do chars <- tryOctal
           let [(o, _)] = readOct chars
           return (chr o)

tryOctal :: Parser [Char]
tryOctal = choice [ try (count 3 octaldigit),
                    try (count 2 octaldigit),
                    try (count 1 octaldigit) ]

octaldigit :: Parser Char
octaldigit = oneOf "01234567"

ctrl :: Parser Char
ctrl = char '^' >> ctrlchar

ctrlchar :: Parser Char
ctrlchar = satisfy (`elem` ['\x0040'..'\x005f'])

escapechar = oneOf "bdefnrstv\"\'\\"

-- Terminals

integer :: Parser Integer
integer = do i <- positive <|> negative <|> decimal
             whiteSpace -- TODO: buff
             return $ i

positive :: Parser Integer
positive = do char '+'
              p <- decimal
              return p

negative :: Parser Integer
negative = do char '-'
              n <- decimal
              return $ negate n

-- float :: Parser Double
-- float = sign?digit+.digit+((E|e)sign?digit+)?

atom :: Parser Atom
atom = do char '\''
--          ((inputchar except control and \ and ')|escape)*
--          inputchar = noneOf "\n\r"
          a <- many (noneOf "\n\r\\\'")
          char '\''
          whiteSpace -- TODO: buff
          return $ Atom a

echar :: Parser Literal
-- char = $((inputchar except control and space and \)|escape)
echar = do char '$'
           c <- noneOf "\n\r\\ "
           whiteSpace -- TODO: buff
           return $ LChar c

estring :: Parser Literal
-- string = "((inputchar except control and \\ and \"")|escape)*"
estring = do char '"'
             s <- many $ noneOf "\n\r\\\""
             char '"'
             return $ LString s

variable :: Parser Var
-- variable = (uppercase | (_ namechar)) namechar*
variable = identifier

-- Non-terminals

emodule :: Parser (Ann Module)
emodule = annotated amodule

amodule :: Parser Module
amodule = do reserved "module"
             name <- atom
             funs <- exports
             attrs <- attributes
             fundefs <- many fundef
             reserved "end"
             return $ Module name funs attrs fundefs

exports :: Parser [Function]
exports = brackets $ commaSep function

attributes :: Parser [(Atom,Const)]
attributes = do reserved "attributes"
                brackets (commaSep $ do a <- atom
                                        symbol "="
                                        c <- constant
                                        return (a,c))

constant :: Parser Const
constant = liftM CLit (try literal) <|>
           liftM CTuple (tuple constant) <|>
           liftM CList (elist constant)

fundef :: Parser FunDef
fundef = do name <- annotated function
            symbol "="
            body <- annotated lambda
            return $ FunDef name body

function :: Parser Function
function = do a <- atom
              char '/'
              i <- decimal
              whiteSpace -- TODO: buff
              return $ Function (a,i)

literal :: Parser Literal
literal = try (liftM LFloat float) <|> liftM LInt integer <|>
          liftM LAtom atom <|> nil <|> echar <|> estring

nil :: Parser Literal
nil = brackets (return LNil)

expression :: Parser Exps
expression =  try (liftM Exps (annotated $ angles $ commaSep (annotated sexpression))) <|>
              liftM Exp (annotated sexpression)

sexpression :: Parser Exp
sexpression = app <|> ecatch <|> ecase <|> elet <|>
              liftM Fun (try function) {- because of atom -} <|>
              lambda <|> letrec <|> liftM Binary (ebinary expression) <|>
              liftM List (try $ elist expression) {- because of nil -} <|>
              liftM Lit literal <|> modcall <|> op <|> receive <|>
              eseq <|> etry <|> liftM Tuple (tuple expression) <|>
              liftM Var variable

app :: Parser Exp
app = do reserved "apply"
         e1 <- expression
         eN <- parens $ commaSep expression
         return $ App e1 eN

ecatch :: Parser Exp
ecatch = do reserved "catch"
            e <- expression
            return $ Catch e

ebinary :: Parser a -> Parser [BitString a]
ebinary p = do symbol "#"
               bs <- braces (commaSep (bitstring p))
               symbol "#"
               return bs

bitstring :: Parser a -> Parser (BitString a)
bitstring p = do symbol "#"
                 e0 <- angles p
                 es <- parens (commaSep expression)
                 return $ BitString e0 es

ecase :: Parser Exp
ecase = do reserved "case"
           exp <- expression
           reserved "of"
           alts <- many1 (annotated clause)
           reserved "end"
           return $ Case exp alts

clause :: Parser Alt
clause = do pat <- patterns
            g <- guard
            symbol "->"
            exp <- expression
            return $ Alt pat g exp

patterns :: Parser Pats
patterns = liftM Pat pattern <|>
           liftM Pats (angles $ commaSep pattern)

pattern :: Parser Pat
pattern = liftM PAlias (try alias) {- because of variable -} <|> liftM PVar variable <|>
          liftM PLit (try literal) {- because of nil -} <|> liftM PTuple (tuple pattern) <|>
          liftM PList (elist pattern) <|> liftM PBinary (ebinary pattern)

alias :: Parser Alias
alias = do v <- variable
           symbol "="
           p <- pattern
           return $ Alias v p

guard :: Parser Guard
guard = do reserved "when"
           e <- expression
           return $ Guard e

elet :: Parser Exp
elet = do reserved "let"
          vars <- variables
          symbol "="
          e1 <- expression
          symbol "in"
          e2 <- expression
          return $ Let (vars,e1) e2

variables :: Parser [Var]
variables = do { v <- variable; return [v]} <|> (angles $ commaSep variable)

lambda :: Parser Exp
lambda = do reserved "fun"
            vars <- parens $ commaSep variable
            symbol "->"
            expr <- expression
            return $ Lambda vars expr

letrec :: Parser Exp
letrec = do reserved "letrec"
            defs <- many fundef
            reserved "in"
            e <- expression
            return $ LetRec defs e

elist :: Parser a -> Parser (List a)
elist a = brackets $ list a

list :: Parser a -> Parser (List a)
list elem = do elems <- commaSep1 elem
               option (L elems) (do symbol "|"
                                    t <- elem
                                    return $ LL elems t)

modcall :: Parser Exp
modcall = do reserved "call"
             e1 <- expression
             symbol ":"
             e2 <- expression
             eN <- parens $ commaSep expression
             return $ ModCall (e1, e2) eN

op :: Parser Exp
op = do reserved "primop"
        a <- atom
        e <- parens $ commaSep expression
        return $ Op a e

receive :: Parser Exp
receive = do reserved "receive"
             alts <- many $ annotated clause
             to <- timeout
             return $ Rec alts to

timeout :: Parser TimeOut
timeout = do reserved "after"
             e1 <- expression
             symbol "->"
             e2 <- expression
             return $ TimeOut  e1 e2

eseq :: Parser Exp
eseq =  do reserved "do"
           e1 <- expression
           e2 <- expression
           return $ Seq e1 e2

etry :: Parser Exp
etry = do reserved "try"
          e1 <- expression
          reserved "of"
          v1 <- variables
          symbol "->"
          e2 <- expression
          reserved "catch"
          v2 <- variables
          symbol "->"
          e3 <- expression
          return $ Try e1 (v1,e1) (v2,e2)

tuple :: Parser a -> Parser [a]
tuple elem = braces $ commaSep elem

annotation :: Parser [Const]
annotation = do symbol "-|"
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

angles = Token.angles lexer
braces = Token.braces lexer
brackets = Token.brackets lexer
commaSep = Token.commaSep lexer
commaSep1 = Token.commaSep1 lexer
decimal = Token.decimal lexer
float = Token.float lexer
identifier = Token.identifier lexer
natural = Token.natural lexer
parens = Token.parens lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer

runLex :: Show a => Parser a -> String -> IO ()
runLex p file = do input <- readFile file
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
