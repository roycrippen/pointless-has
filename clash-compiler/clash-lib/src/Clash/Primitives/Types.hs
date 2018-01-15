{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type and instance definitions for Primitive
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Primitives.Types where

import           Control.Applicative  ((<|>))
import           Data.Aeson           (FromJSON (..), Value (..), (.:), (.:?), (.!=))
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Strict  as H
import qualified Data.Text            as S
import           Data.Text.Lazy       (Text)

-- | Primitive Definitions
type PrimMap a = HashMap S.Text (Primitive a)

-- | Externally defined primitive
data Primitive a
  -- | A primitive that has a template that can be filled out by the backend render
  = BlackBox
  { name     :: !S.Text
    -- ^ Name of the primitive
  , outputReg :: Bool
    -- ^ Verilog only: whether the result should be a /reg/(@True@) or /wire/
    -- (@False@); when not specified in the /.json/ file, the value will default
    -- to @False@ (i.e. /wire/).
  , library  :: [S.Text]
    -- ^ VHDL only: add /library/ declarations for the given names
  , imports  :: [S.Text]
    -- ^ VHDL only: add /use/ declarations for the given names
  , qsysInclude :: Maybe (S.Text,a)
    -- ^ Intel/Quartus only: create a /.qsys/ file from the given template.
    -- Defaults to @Nothing@ when not specified in the /.json/ file
  , template :: !(Either a a) -- ^ Either a /declaration/ or an /expression/ template.
  }
  -- | A primitive that carries additional information
  | Primitive
  { name     :: !S.Text -- ^ Name of the primitive
  , primType :: !Text -- ^ Additional information
  }
  deriving Show

instance FromJSON (Primitive Text) where
  parseJSON (Object v) = case H.toList v of
    [(conKey,Object conVal)] -> case conKey of
      "BlackBox"  -> BlackBox <$> conVal .: "name"
                              <*> conVal .:? "outputReg" .!= False
                              <*> conVal .:? "libraries" .!= []
                              <*> conVal .:? "imports" .!= []
                              <*> (conVal .:? "qsysInclude" >>= parseInclude)
                              <*> ((Left <$> conVal .: "templateD") <|> (Right <$> conVal .: "templateE"))
      "Primitive" -> Primitive <$> conVal .: "name" <*> conVal .: "primType"
      _ -> error "Expected: BlackBox or Primitive object"
    _ -> error "Expected: BlackBox or Primitive object"
    where
      parseInclude Nothing  = pure Nothing
      parseInclude (Just c) =
        Just <$> ((,) <$> c .: "name" <*> c .: "content")
  parseJSON _ = error "Expected: BlackBox or Primitive object"
