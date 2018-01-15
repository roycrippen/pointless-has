{-|
  Copyright  :  (C) 2013-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd,
                    2017     , QBayLogic, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type definitions used by the Driver module
-}

module Clash.Driver.Types
  (module Clash.Driver.Types
  ,SrcSpan, noSrcSpan
  )
where

import Control.Exception (Exception)
import Data.HashMap.Lazy (HashMap)
import Data.Text.Lazy    (Text)

import BasicTypes        (InlineSpec)
import SrcLoc            (SrcSpan, noSrcSpan)

import Clash.Core.Term   (Term,TmName,TmOccName)
import Clash.Core.Type   (Type)

import Clash.Netlist.BlackBox.Types (HdlSyn)

-- | Global function binders
--
-- Global functions cannot be mutually recursive, only self-recursive
type BindingMap = HashMap TmOccName (TmName,Type,SrcSpan,InlineSpec,Term)

-- | Debug Message Verbosity
data DebugLevel
  = DebugNone    -- ^ Don't show debug messages
  | DebugFinal   -- ^ Show completely normalized expressions
  | DebugName    -- ^ Names of applied transformations
  | DebugApplied -- ^ Show sub-expressions after a successful rewrite
  | DebugAll     -- ^ Show all sub-expressions on which a rewrite is attempted
  deriving (Eq,Ord,Read)

data ClashOpts = ClashOpts { opt_inlineLimit :: Int
                           , opt_specLimit   :: Int
                           , opt_inlineFunctionLimit :: Word
                           , opt_inlineConstantLimit :: Word
                           , opt_dbgLevel    :: DebugLevel
                           , opt_cachehdl    :: Bool
                           , opt_cleanhdl    :: Bool
                           , opt_intWidth    :: Int
                           , opt_hdlDir      :: Maybe String
                           , opt_hdlSyn      :: HdlSyn
                           , opt_errorExtra  :: Bool
                           , opt_floatSupport :: Bool
                           , opt_allowZero   :: Bool
                           , opt_importPaths :: [FilePath]
                           }

data ClashException = ClashException SrcSpan String (Maybe String)

instance Show ClashException where
  show (ClashException _ s eM) = s ++ "\n" ++ maybe "" id eM

instance Exception ClashException

-- | Information about the generated HDL between (sub)runs of the compiler
data Manifest
  = Manifest
  { manifestHash :: (Int,Maybe Int)
    -- ^ Hash of the TopEntity and all its dependencies
    --   + (maybe) Hash of the TestBench and all its dependencies
  , portInNames  :: [Text]
  , portInTypes  :: [Text]
    -- ^ The rendered versions of the types of the input ports of the TopEntity
    --
    -- Used when dealing with multiple @TopEntity@s who have different names
    -- for types which are structurally equal
  , portOutNames :: [Text]
  , portOutTypes :: [Text]
    -- ^ The rendered versions of the types of the output ports of the TopEntity
    --
    -- Used when dealing with multiple @TopEntity@s who have different names
    -- for types which are structurally equal
  , componentNames :: [Text]
    -- ^ Names of all the generated components for the @TopEntity@ (does not
    -- include the names of the components of the @TestBench@ accompanying
    -- the @TopEntity@).
  }
  deriving (Show,Read)
