{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

-- algebraic-graphs
import Algebra.Graph.AdjacencyMap ( AdjacencyMap, edge, empty, hasVertex, overlay, overlays )
import Algebra.Graph.AdjacencyMap.Algorithm ( dfsForest, reachable )
import Algebra.Graph.Export.Dot

-- aeson
import Data.Aeson

-- attoparsec
import Data.Attoparsec.Text ( parseOnly )

-- base
import Data.Char
import Data.Foldable ( fold, for_, traverse_ )
import Data.Function ( (&) )
import Data.Functor ( (<&>) )
import Data.Maybe ( fromMaybe, listToMaybe, mapMaybe )
import Data.Traversable ( for )
import Debug.Trace
import qualified Prelude
import Prelude hiding ( getContents, lines, readFile, words )

-- containers
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Tree

-- filepath
import System.FilePath

-- nix-derivation
import Nix.Derivation

-- process
import System.Process

-- text
import Data.Text ( Text, lines, pack, unpack, words )
import Data.Text.Encoding
import Data.Text.IO ( getContents, readFile )

-- unordered-containers
import qualified Data.HashMap.Strict as HashMap

import System.Environment ( getArgs )


main :: IO ()
main = do
  jobsExpr <- fromMaybe "./jobs.nix" . listToMaybe <$> getArgs

  -- Run nix-instantiate on the jobs expression to instantiate .drvs for all
  -- things that may need to be built.
  inputDrvPaths <- Prelude.lines <$> readProcess "nix-instantiate" [ jobsExpr ] ""

  -- Build a 'Map StorePath DrvPath'. This will serve the role of
  -- 'nix-store --query --deriver', but nix-store won't work because Nix itself
  -- has no mapping from store paths to derivations yet.
  storePathToDrv <- fold <$> for inputDrvPaths \drvPath ->
    fmap (parseOnly parseDerivation) (readFile drvPath) <&> foldMap \Derivation{ outputs } ->
      outputs & foldMap \DerivationOutput{ path } ->
        Map.singleton path drvPath

  -- Re-run nix-instantiate, but with --eval --strict --json. This should give
  -- us back a JSON object that is a tree with leaves that are store paths.
  jsonString <- readProcess "nix-instantiate" [ "--eval", "--strict", "--json", jobsExpr ] ""

  json <-
    decodeStrict (encodeUtf8 (pack jsonString))
      & maybe (fail "Could not parse JSON") return

  let drvs :: [(Text, FilePath)]
      drvs = go "" json
        where
          go prefix (Object kv)        = HashMap.foldlWithKey' (\x k v -> x <> go (prefix <> "." <> k) v) [] kv
          go prefix (String storePath) = foldMap pure $ (,) <$> pure prefix <*> Map.lookup (unpack storePath) storePathToDrv
          go _ _                       = mempty

  g <- foldr (\(_, drv) m -> m >>= \g -> add g drv) (pure empty) drvs

  putStrLn "steps:"
  for_ drvs \(label, drvPath) -> do
    putStrLn $ "  - label: " <> tail (unpack label)
    putStrLn $ "    command: nix-store -r" <> drvPath
    putStrLn $ "    key: " <> stepify drvPath

    let dependencies = filter (`elem` map snd drvs) (drop 1 (reachable drvPath g))
    case dependencies of
      [] -> return ()
      _ -> do
        putStrLn $ "    depends_on:"
        traverse_ (putStrLn . ("      - " <>) . stepify) dependencies


stepify :: String -> String
stepify = map replace . takeBaseName
  where
    replace x | isAlphaNum x = x
    replace '/' = '/'
    replace '-' = '-'
    replace _ = '_'


add :: AdjacencyMap FilePath -> FilePath -> IO (AdjacencyMap FilePath)
add g drvPath =
  if hasVertex drvPath g then
    return g

  else
    fmap (parseOnly parseDerivation) (readFile drvPath) >>= \case
      Left _ ->
        return g

      Right Derivation{ inputDrvs } -> do
        deps <- foldr (\dep m -> m >>= \g' -> add g' dep) (pure g) (Map.keys inputDrvs)

        let g' = overlays (edge drvPath <$> Map.keys inputDrvs)

        return $ overlay deps g'
