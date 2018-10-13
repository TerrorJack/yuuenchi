{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StrictData #-}

module GHCInception
  ( GHCInfo(..)
  , ghcInceptionRun
  , ghcInception
  ) where

import Data.Functor
import qualified DynFlags as GHC
import qualified GHC
import Language.Haskell.TH.Syntax
import System.Environment
import System.FilePath
import System.Process

data GHCInfo a = GHCInfo
  { ghcLibDir :: FilePath
  , ghcInfo :: [(String, String)]
  , ghcArgs :: [String]
  , ghcEnvironment :: [(String, String)]
  , ghcExtraInfo :: a
  } deriving (Lift, Show)

ghcInceptionRun :: Lift a => GHC.Ghc a -> Q (TExp (GHCInfo a))
ghcInceptionRun m =
  runIO
    (do _ghc_name <- takeFileName <$> getExecutablePath
        _ghc_info <- read <$> readProcess _ghc_name ["--info"] ""
        let Just _ghc_libdir = lookup "LibDir" _ghc_info
        _ghc_args <- getArgs
        _ghc_env <- getEnvironment
        _ghc_extra_info <-
          GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
          GHC.runGhc (Just _ghc_libdir) $ do
            dflags0 <- GHC.getSessionDynFlags
            (dflags1, _, _) <-
              GHC.parseDynamicFlags dflags0 $ map GHC.noLoc _ghc_args
            void $ GHC.setSessionDynFlags dflags1
            m
        pure
          GHCInfo
            { ghcLibDir = _ghc_libdir
            , ghcInfo = _ghc_info
            , ghcArgs = _ghc_args
            , ghcEnvironment = _ghc_env
            , ghcExtraInfo = _ghc_extra_info
            }) >>=
  unsafeTExpCoerce . lift

ghcInception :: Q (TExp (GHCInfo ()))
ghcInception = ghcInceptionRun (pure ())
