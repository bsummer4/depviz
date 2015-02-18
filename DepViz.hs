-- Collects the Hackage dependency tree for a package.

-- I feel like it would be better to handle lookup failures core data
-- structure instead of ignoring them. However I'm going to defer that change
-- for now. This approach should work well enough for a first draft.

-- TODO Is there an alternative to classyPrelude that's suitable for
-- libraries? This is absurd.

{-# LANGUAGE UnicodeSyntax, TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module DepViz where

import Data.Text (Text)
import qualified Data.Text as T

-- import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.List as L

import Data.Set (Set)
import qualified Data.Set as S

import Data.Maybe
import Distribution.Hackage.DB (Hackage)
import Control.Monad
import Data.Functor

import Distribution.PackageDescription
import Distribution.Package
import Distribution.Text (display)
import Distribution.PackageDescription.Configuration

import Data.Graph.Inductive.Graph hiding (edges,nodes)

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
pkgDeps gdesc = depName <$> buildDepends desc
  where desc = flattenPackageDescription gdesc
        depName (Dependency nm _) = T.pack $ display nm

lookupDepTree ∷ Hackage -> Text → DepTree
lookupDepTree hack nm = DepTree nm children
  where versions = fromMaybe M.empty $ M.lookup (T.unpack nm) hack
        children = join $ maybeToList $ do
          (_,pkg) ← if M.null versions then Nothing else Just(M.findMax versions)
          let depNames = pkgDeps pkg
          Just $ lookupDepTree hack <$> depNames
