{-|
Module      : Eval
Description : Evalúa un término siguiendo la semántica big-step
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo evaluá términos siguiendo la semántica big-step (estrategia CBV)
-}

module Eval where

import Common ( abort )
import Lang
import Subst ( substN, subst )
import MonadPCF ( MonadPCF, lookupDecl, failPCF, printPCF )
import PPrint ( ppName )

-- | Semántica de operadores binarios
semOp :: BinaryOp -> Int -> Int -> Int
semOp Add x y=  x + y
semOp Sub x y = max 0 (x - y)

-- | Evaluador de términos CBV
eval ::  MonadPCF m => Term -> m Term
eval (V _ (Global nm)) = do
  -- unfold and keep going
  mtm <- lookupDecl nm 
  case mtm of 
    Nothing -> failPCF $ "Error de ejecución: variable no declarada: " ++ ppName nm 
    Just t -> eval t

eval (App p l r) = do
     le <- eval l
     re <- eval r
     case (le, re) of
        (Lam _ y _ m, n) ->
           eval (subst n m)
        (ff@(Fix _ f _ _ _ t), n) ->
           eval (substN [ff, n] t)
        _ ->
           abort("Error de tipo en runtime " ++ show (le, re))
eval (Print p str t) = do
        te <- eval t
        case te of
          Const _ (CNat n) -> do printPCF (str++show n)
                                 return te
          _                -> abort "Error de tipo en runtime!"
eval (BinaryOp p op t u) = do 
        te <- eval t
        ue <- eval u
        case (te,ue) of
          (Const _ (CNat n), Const _ (CNat m)) -> return $ Const p (CNat (semOp op n m))
          _                -> abort "Error de tipo en runtime!"
eval (IfZ p c t e) = do
     ce <- eval c
     case ce of
       Const _ (CNat 0) -> eval t
       Const _ (CNat _) -> eval e
       c' -> abort ("Error de tipo en runtime!")
eval (Let _ _ _ m n) = do
    v <- eval m
    eval (subst v n)

-- nada más para reducir
eval t = return t
