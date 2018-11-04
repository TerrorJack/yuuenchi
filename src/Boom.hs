module Boom
  ( boom
  , boomM
  , boomWith
  , boomMWith
  ) where

import Language.Haskell.TH.Syntax

boom :: Q Exp
boom = boomWith $ \loc -> "BOOM! Location: " <> show loc

boomM :: Q Exp
boomM = boomMWith $ \loc -> "BOOM! Location: " <> show loc

boomWith :: (Loc -> String) -> Q Exp
boomWith f = do
  Just _error <- lookupValueName "error"
  _loc <- location
  pure $ AppE (VarE _error) (LitE (StringL (f _loc)))

boomMWith :: (Loc -> String) -> Q Exp
boomMWith f = do
  Just _fail <- lookupValueName "fail"
  _loc <- location
  pure $ AppE (VarE _fail) (LitE (StringL (f _loc)))
