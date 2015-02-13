{-# LANGUAGE UnicodeSyntax, TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- Collects the Hackage dependency tree for a package.

module Main where

import System.Environment

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe
import Distribution.Hackage.DB (Hackage, readHackage)
import Control.Monad
import Data.Functor
import Debug.Trace

import Distribution.PackageDescription
import Distribution.Package
import Distribution.Text (display)
import Distribution.PackageDescription.Configuration

import Data.Graph.Inductive.Graph
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive.PatriciaTree

-- Should we represent edges by embeding or by storing a name? The
-- second sounds far more sane if recursive dependencies are a thing. Are
-- they? let's assume no.
--
-- Also, we ignore version information for now. We don't need 100%
-- correct results.

data DepTree = DepTree Text [DepTree] -- TODO Use ‘Data.Tree’.
type DepGraph = Set (Text,Text)

toFgr ∷ Graph gr => DepGraph → gr Text ()
toFgr dg = mkGraph (M.elems nodes) edges
  where nodeNames = S.toAscList $ S.union (S.map fst dg) (S.map snd dg)
        nodes = M.fromList $ snd $ L.foldl' loop (0,[]) nodeNames
        loop (i,acc) nm = (i+1, (nm,(i,nm)):acc)
        edges = catMaybes $ mkEdge <$> S.toList dg
        mkEdge (a,b) = do
          (aId,_) ← M.lookup a nodes
          (bId,_) ← M.lookup b nodes
          return (aId, bId, ())

graphDeps ∷ DepTree → DepGraph
graphDeps = snd . loop (S.empty,S.empty)
  where depName (DepTree n _) = n
        loop acc@(seen,gr) (DepTree n deps) =
          if alreadySeen then acc else dive
            where edges ∷ Set (Text,Text)
                  edges = S.fromList $ (n,) . depName <$> deps
                  dive = L.foldl' loop (S.insert n seen, S.union edges gr) deps
                  alreadySeen = S.member n seen

pkgDeps ∷ GenericPackageDescription → [Text]
pkgDeps gdesc = depName <$> secondTry
  where desc = flattenPackageDescription gdesc
        buildInfos = allBuildInfo desc
        firstTry = join $ targetBuildDepends <$> buildInfos
        secondTry = buildDepends desc
        depName (Dependency nm _) = T.pack $ display nm

lookupDepTree ∷ Hackage -> Text → DepTree
lookupDepTree hack nm = DepTree nm children
  where versions = fromMaybe M.empty $ M.lookup (T.unpack nm) hack
        children = join $ maybeToList $ do
          (maxVer,pkg) ← if M.null versions then Nothing else Just(M.findMax versions)
          let depNames = pkgDeps pkg
          Just $ lookupDepTree hack <$> depNames

-- I feel like it would be better to handle lookup failures core data
-- structure instead of ignoring them. However I'm going to defer that change
-- for now. This approach should work well enough for a first draft.

showGraph ∷ Show a => Set (a,a) → String
showGraph = unlines . map (\(a,b) → concat[show a," -> ",show b]) . S.toList

main = do
  pkg ← T.pack . (\case {(x:_) → x; [] → "warp"}) <$> getArgs
  h ← readHackage
  let gr ∷ Gr Text () = toFgr $ graphDeps $ lookupDepTree h pkg
  putStrLn $ LT.unpack $ printDotGraph $ flip graphToDot gr $ nonClusteredParams
    { isDirected = True
    , fmtNode = \(_,l) → [Label $ StrLabel $ LT.fromStrict l]
    }

-- printDotGraph ∷ DotRepr n → String
-- defaultVis :: (Graph gr) => gr nl el -> DotGraph Node
-- defaultVis = graphToDot nonClusteredParams
-- graphToDot :: (Ord cl, Graph gr) => GraphvizParams nl el cl l -> gr nl el -> DotGraph NodeSource
