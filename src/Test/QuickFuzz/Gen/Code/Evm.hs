{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickFuzz.Gen.Code.Evm where

import Data.Default

import Blockchain.Data.Code
import Blockchain.VM.Code
import Blockchain.VM.Opcodes

import Data.ByteString.Lazy
import Data.Hex
import Data.List

import Test.QuickCheck

import Test.QuickFuzz.Derive.Arbitrary
import Test.QuickFuzz.Derive.Fixable

import Test.QuickFuzz.Derive.Show
import Test.QuickFuzz.Gen.FormatInfo
import Test.QuickFuzz.Gen.Base.ByteString
import Test.QuickFuzz.Gen.Base.String

type EvmBytecode = [Operation]

devArbitrary ''EvmBytecode
devShow ''EvmBytecode

evmInfo :: FormatInfo EvmBytecode NoActions
evmInfo = def
  { encode = hex . fromStrict . codeBytes . compile
    -- encode = pack . formatCode . compile
  , random = arbitrary
  , value = show
  , ext = "evm"
  }
