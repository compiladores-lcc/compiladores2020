{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : MonadPCF
Description : Mónada con soporte para estado, errores, e IO.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definimos la clase de mónadas 'MonadPCF' que abstrae las mónadas con soporte para estado, errores e IO,
y la mónada 'PCF' que provee una instancia de esta clase.
-}

module MonadPCF (
  PCF,
  runPCF,
  lookupDecl,
  lookupTy,
  printPCF,
  failPosPCF,
  failPCF,
  addDecl,
  addTy,
  catchErrors,
  MonadPCF,
  module Control.Monad.Except,
  module Control.Monad.State)
 where

import Common
import Lang
import Global
import Errors ( Error(..) )
import Control.Monad.State
import Control.Monad.Except
import System.IO

-- * La clase 'MonadPCFm'

{-| La clase de mónadas 'MonadPCF' clasifica a las mónadas con soporte para operaciones @IO@, estado de tipo 'Global.GlEnv', y errores de tipo 'Errors.Error'.

Las mónadas @m@ de esta clase cuentan con las operaciones:
   - @get :: m GlEnv@
   - @put :: GlEnv -> m ()@
   - @throwError :: Error -> m a@
   - @catchError :: m a -> (Error -> m a) -> m a@
   - @liftIO :: IO a -> m a@

y otras operaciones derivadas de ellas, como por ejemplo
   - @modify :: (GlEnv -> GlEnv) -> m ()

-}
class (MonadIO m, MonadState GlEnv m, MonadError Error m) => MonadPCF m where

printPCF :: MonadPCF m => String -> m ()
printPCF = liftIO . putStrLn

addDecl :: MonadPCF m => Decl Term -> m ()
addDecl d = modify (\s -> s { glb = d : glb s })
  
addTy :: MonadPCF m => Name -> Ty -> m ()
addTy n ty = modify (\s -> s { tyEnv = (n,ty) : tyEnv s })

hasName :: Name -> Decl a -> Bool
hasName nm (Decl { declName = nm' }) = nm == nm'

lookupDecl :: MonadPCF m => Name -> m (Maybe Term)
lookupDecl nm = do
     s <- get
     case filter (hasName nm) (glb s) of
       (Decl { declBody=e }):_ -> return (Just e)
       [] -> return Nothing

lookupTy :: MonadPCF m => Name -> m (Maybe Ty)
lookupTy nm = do
      s <- get
      return $ lookup nm (tyEnv s)

failPosPCF :: MonadPCF m => Pos -> String -> m a
failPosPCF p s = throwError (ErrPos p s)

failPCF :: MonadPCF m => String -> m a
failPCF = failPosPCF NoPos

catchErrors  :: MonadPCF m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c) 
                           (\e -> liftIO $ hPutStrLn stderr (show e) 
                              >> return Nothing)

----
-- Importante, no eta-expandir porque GHC no hace una
-- eta-contracción de sinónimos de tipos
-- y Main no va a compilar al escribir `InputT PCF()`

-- | El tipo @PCF@ es un sinónimo de tipo para una mónada construida usando dos transformadores de mónada sobre la mónada @IO@.
-- El transformador de mónad @ExcepT Error@ agrega a la mónada IO la posibilidad de manejar errores de tipo 'Errors.Error'.
-- El transformador de mónadas @StateT GlEnv@ agrega la mónada @ExcepT Error IO@ la posibilidad de manejar un estado de tipo 'Global.GlEnv'.
type PCF = StateT GlEnv (ExceptT Error IO)

-- | Esta es una instancia vacía, ya que 'MonadPCF' no tiene funciones miembro.
instance MonadPCF PCF

-- 'runPCF\'' corre una computación de la mónad 'PCF' en el estado inicial 'Global.initialEnv' 
runPCF' :: PCF a -> IO (Either Error (a, GlEnv))
runPCF' c =  runExceptT $ runStateT c initialEnv

runPCF:: PCF a -> IO (Either Error a)
runPCF c = fmap fst <$> runPCF' c
