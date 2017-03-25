#!/usr/bin/env stack
{- stack
  --install-ghc runghc
  --package criterion
  --package turtle
-}

{-# LANGUAGE OverloadedStrings #-}

import           Criterion.Main
import           Criterion.Types           hiding (template)
import           Filesystem.Path.CurrentOS
import Control.Monad (forM_)
import           Prelude                   hiding (FilePath)
import           Turtle                    hiding (env)

myConfig :: FilePath -> Config
myConfig r = defaultConfig { timeLimit  = 120
                           , reportFile = Just (encodeString r) }
main :: IO ()
main = do
  dir <- pwd
  let plusDir = "Plus"
      evenDir = "Even"
      subdirs = [plusDir, evenDir]

  prepare dir subdirs

  defaultMainWith (myConfig (dir <> "report" <.> "html"))
    [ mkbenchGroup dir plusDir $ plusBenchCases 5
    , mkbenchGroup dir evenDir $ evenBenchCases 5
    ]

  cleanup dir subdirs

cleanup :: FilePath -> [Text] -> IO ()
cleanup dir = mapM_ (rmtree . (dir <>) . fromText)

prepare :: FilePath -> [Text] -> IO ()
prepare dir subdirs = forM_ subdirs $ \sd -> do
  mkdir (dir <> fromText sd)
  cp (dir <> fromText (sd <> "Base.agda")) (dir <> fromText sd <> "Base" <.> "agda")

mkbenchGroup :: FilePath -> Text -> [BenchCase] -> Benchmark
mkbenchGroup dir subdir benchCases =
  bgroup (show subdir) $ map (\c ->
    env (setupEnv dir subdir c) $ \ _ -> do
      let nm = name c
      bench (show nm) $ nfIO (benchAgda nm))
    benchCases

setupEnv :: FilePath -> Text -> BenchCase -> IO ()
setupEnv dir sd bc = do
  cd dir
  setupBenchCase (dir <> fromText sd) bc
  cd (fromText sd)

benchAgda :: Text -> IO ()
benchAgda fname = do
  -- maybe a .agdai file exists already and we have to remove it
  let ifile = fromText $ fname <> "i"
  exists <- testfile ifile
  when exists $ rm ifile
  -- run agda and discard output
  sh $ inproc "agda" [fname] (return "")

data BenchCase =
  BenchCase { name :: Text
            , template :: [Line] }

setupBenchCase ::  FilePath -> BenchCase -> IO ()
setupBenchCase dir bc = do
  -- forge the filepath of the file
  let fpath = dir     <> fromText (name bc)

  -- create a .agda file with the template
  output fpath (select . template $ bc)

int :: IsString s => Int -> s
int n = fromString (show n)

--------------------------------------------------------------------------------
-- Even benchmark

evenBenchCases :: Int -> [BenchCase]
evenBenchCases n = take n $ map evenBenchCase (iterate (*2) 2)

evenBenchCase :: Int -> BenchCase
evenBenchCase n =
  BenchCase { name     = nm <> ".agda"
            , template = templ }
  where
    nm :: (Monoid s, IsString s) => s
    nm = "Even" <> int n

    templ :: [Line]
    templ =
      [ "module " <> nm <> " where"
      , ""
      , "open import Base"
      , "open import Auto"
      , ""
      , nm <> " : Even " <> int n
      , nm <> " = apply (auto " <> int n <> " rules)" ]

--------------------------------------------------------------------------------
-- Plus benchmark

plusBenchCases :: Int -> [BenchCase]
plusBenchCases n = take n $ map plusBenchCase (iterate (*2) 2)

plusBenchCase :: Int -> BenchCase
plusBenchCase n =
  BenchCase { name     = nm <> ".agda"
            , template = templ }
  where
    nm :: (Monoid s, IsString s) => s
    nm = "Plus" <> int n

    templ :: [Line]
    templ =
      [ "module " <> nm <> " where"
      , ""
      , "open import Base"
      , "open import Auto"
      , ""
      , nm <> " : Plus " <> int n  <> " " <> unsafeTextToLine (repr (0 :: Int)) <> " " <> int n
      , nm <> " = apply (auto " <> int (n+1) <> " rules)" ]
