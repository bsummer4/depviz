{-# LANGUAGE UnicodeSyntax, TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Distribution.Hackage.DB (readHackage)
import System.Environment
import Data.Functor

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive.PatriciaTree

import DepViz

main ∷ IO()
main = do
  pkg ← T.pack . (\case {[]→"warp"; (x:_)→x}) <$> getArgs
  h ← readHackage
  let gr ∷ Gr Text () = toFgr $ graphDeps $ lookupDepTree h pkg
  putStrLn $ LT.unpack $ printDotGraph $ flip graphToDot gr $ nonClusteredParams
    { isDirected = True
    , fmtNode = \(_,l) → [Label $ StrLabel $ LT.fromStrict l]
    }
