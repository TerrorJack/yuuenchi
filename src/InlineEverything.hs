{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module InlineEverything
  ( plugin
  ) where

import Bag
import Data.Data (Data, gmapM)
import Data.List
import Data.Traversable
import GhcPlugins hiding ((<>))
import HsBinds
import HsDecls
import HsExtension
import Type.Reflection

bindsFromDecls :: [HsDecl GhcPs] -> [HsBind GhcPs]
bindsFromDecls =
  foldr
    (\_decl _tot ->
       case _decl of
         ValD _ _bind -> _bind : _tot
         _ -> _tot)
    []

sigsFromDecls :: [HsDecl GhcPs] -> [Sig GhcPs]
sigsFromDecls =
  foldr
    (\_decl _tot ->
       case _decl of
         SigD _ _sig -> _sig : _tot
         _ -> _tot)
    []

namesFromBinds :: [HsBind GhcPs] -> [RdrName]
namesFromBinds =
  foldr
    (\_bind _tot ->
       case _bind of
         FunBind {fun_id = (unLoc -> _name)} -> _name : _tot
         _ -> _tot)
    []

namesFromInlinePragmas :: [Sig GhcPs] -> [RdrName]
namesFromInlinePragmas =
  foldr
    (\_sig _tot ->
       case _sig of
         InlineSig _ (unLoc -> _name) _ -> _name : _tot
         _ -> _tot)
    []

extraInlinePragmas :: [HsBind GhcPs] -> [Sig GhcPs] -> [Sig GhcPs]
extraInlinePragmas _binds _prev_sigs = _extra_pragmas
  where
    _all_names = namesFromBinds _binds
    _inlined_names = namesFromInlinePragmas _prev_sigs
    _extra_names = _all_names \\ _inlined_names
    _extra_pragmas =
      [ InlineSig
        NoExt
        (noLoc _name)
        InlinePragma
          { inl_src = NoSourceText
          , inl_inline = Inlinable
          , inl_sat = Nothing
          , inl_act = AlwaysActive
          , inl_rule = FunLike
          }
      | _name <- _extra_names
      ]

inlineEverything :: (Monad m, Data t) => t -> m t
inlineEverything t =
  case eqTypeRep (typeOf t) (typeRep @[LHsDecl GhcPs]) of
    Just HRefl -> do
      t' <- for t inlineEverything
      let _decls = map unLoc t'
          _binds = bindsFromDecls _decls
          _prev_sigs = sigsFromDecls _decls
          _extra_pragmas = extraInlinePragmas _binds _prev_sigs
          _extra_decls = map (noLoc . SigD NoExt) _extra_pragmas
      pure $ _extra_decls <> t'
    _ ->
      case eqTypeRep (typeOf t) (typeRep @(ClsInstDecl GhcPs)) of
        Just HRefl -> do
          let _binds = map unLoc $ bagToList $ cid_binds t
              _prev_sigs = map unLoc $ cid_sigs t
              _extra_pragmas = extraInlinePragmas _binds _prev_sigs
              _extra_decls = map noLoc _extra_pragmas
          pure t {cid_sigs = _extra_decls <> cid_sigs t}
        _ -> gmapM inlineEverything t

plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction =
        \_ _ parsed_mod -> do
          patched_mod <- inlineEverything $ hpm_module parsed_mod
          pure parsed_mod {hpm_module = patched_mod}
    }
