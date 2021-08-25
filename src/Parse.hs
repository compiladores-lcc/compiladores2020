{-|
Module      : Parse
Description : Define un parser de términos PCF0 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP,parse)
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else","in", 
                           "ifz", "print","Nat"],
         reservedOpNames = ["->",":","=","+","-"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier 

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P Ty
tyatom = (reserved "Nat" >> return NatTy)
         <|> parens typeP

typeP :: P Ty
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (FunTy x y))
      <|> tyatom
          
const :: P Const
const = CNat <$> num

printOp :: P NTerm
printOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  a <- atom
  return (Print i str a)

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity NTerm
binary s f = Ex.Infix (reservedOp s >> return (BinaryOp NoPos f))

table :: [[Operator String () Identity NTerm]]
table = [[binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft]]

expr :: P NTerm
expr = Ex.buildExpressionParser table tm

atom :: P NTerm
atom =     (flip Const <$> const <*> getPos)
       <|> flip V <$> var <*> getPos
       <|> parens expr
       <|> printOp

-- parsea un par (variable : tipo)
binding :: P (Name, Ty)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)

lam :: P NTerm
lam = do i <- getPos
         reserved "fun"
         (v,ty) <- parens binding
         reservedOp "->"
         t <- expr
         return (Lam i v ty t)

-- Nota el parser app también parsea un solo atom.
app :: P NTerm
app = (do i <- getPos
          f <- atom
          args <- many atom
          return (foldl (App i) f args))

ifz :: P NTerm
ifz = do i <- getPos
         reserved "ifz"
         c <- expr
         reserved "then"
         t <- expr
         reserved "else"
         e <- expr
         return (IfZ i c t e)

fix :: P NTerm
fix = do i <- getPos
         reserved "fix"
         (f, fty) <- parens binding
         (x, xty) <- parens binding
         reservedOp "->"
         t <- expr
         return (Fix i f fty x xty t)

letexp :: P NTerm
letexp = do
  i <- getPos
  reserved "let"
  (v,ty) <- parens binding
  reservedOp "="  
  def <- expr
  reserved "in"
  body <- expr
  return (Let i v ty def body)

-- | Parser de términos
tm :: P NTerm
tm = app <|> lam <|> ifz <|> printOp <|> fix <|> letexp

-- | Parser de declaraciones
decl :: P (Decl NTerm)
decl = do 
     i <- getPos
     reserved "let"
     v <- var
     reservedOp "="
     t <- expr
     return (Decl i v t)

-- | Parser de programas (listas de declaraciones) 
program :: P [Decl NTerm]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (Decl NTerm) NTerm)
declOrTm =  try (Left <$> decl) <|> (Right <$> expr)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> NTerm
parse s = case runP expr s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)
