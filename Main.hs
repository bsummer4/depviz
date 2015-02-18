{-# LANGUAGE UnicodeSyntax, TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Distribution.Hackage.DB (readHackage)
import Data.Functor
import Options.Applicative
import Data.GraphViz
import Data.GraphViz.Attributes.Complete hiding (value)
import Data.Graph.Inductive.PatriciaTree

import DepViz

outputGraphviz ∷ Args → IO()
outputGraphviz args = do
  h ← readHackage
  let gr ∷ Gr Text () = toFgr $ graphDeps $ lookupDepTree h $ pkg args
  putStrLn $ LT.unpack $ printDotGraph $ flip graphToDot gr $ nonClusteredParams
    { isDirected = True
    , fmtNode = \(_,l) → [Label $ StrLabel $ LT.fromStrict l]
    }

data OutputTy = GraphViz | List | URLs
  deriving (Read,Show)

data Args = Args { outputType∷OutputTy, allDeps∷Bool, pkg∷Text }


parseArgs ∷ Parser Args
parseArgs = Args
  <$> option auto
      ( long "output-type"
      <> short 'T'
      <> value List )
  <*> switch
      ( long "full-dependencies"
      <> short 'D'
      <> help "Include benchmark and testsuite dependencies" )
  <*> (T.pack <$> argument str (metavar "package-name") )

optParser :: ParserInfo Args
optParser = info (helper <*> parseArgs)
    ( fullDesc
    <> progDesc "Show information about a package."
    <> header "hello - a test for optparse-applicative" )

main ∷ IO()
main = execParser optParser >>= outputGraphviz
