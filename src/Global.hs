{-|
Module      : Global
Description : Define el estado global del compilador
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}
module Global where

import Lang

data GlEnv = GlEnv {
  inter :: Bool,        --  ^ True, si estamos en modo interactivo.
  lfile :: String,      -- ^ Último archivo cargado.
  glb :: [Decl Term],   -- ^ Entorno con declaraciones globales
  tyEnv :: [(Name,Ty)]  -- ^ Entorno de tipado de declaraciones globales
}

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv True "" [] []
