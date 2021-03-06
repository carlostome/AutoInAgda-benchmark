#!/usr/bin/env stack
{- stack
  --install-ghc runghc
  --package criterion
  --package turtle
  --package extra
-}

{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Extra       (whenM)
import           Criterion.Main
import           Criterion.Types           hiding (template)
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
import           Turtle                    hiding (env)
import           Data.Text                 (unpack)

myConfig :: FilePath -> Config
myConfig r = defaultConfig { timeLimit  = 300
                           , reportFile = Just (encodeString r) }

main :: IO ()
main = do
  dir <- pwd
  let -- plusDir = "Plus"
      evenDir = "Even"
      subdirs = [evenDir]

  echo "Preparing testfiles"
  prepare subdirs

  defaultMainWith (myConfig (dir <> "report" <.> "html"))
    [ --mkbenchGroup dir plusDir $ plusBenchCases 7
     mkbenchGroup dir evenDir $ evenBenchCases 6
    ]

  echo "Cleaning testfiles"
  cleanup (map ((dir <>) . fromText) subdirs)

cleanup :: [FilePath] -> IO ()
cleanup = mapM_ rmtree

prepare :: [Text] -> IO ()
prepare = mapM_ $ \sd -> do
  let sd' = fromText sd
  whenM (not <$> testdir sd') (mkdir sd')
  cp (fromText (sd <> "Base.agda")) (fromText sd <> "Base" <.> "agda")

mkbenchGroup :: FilePath -> Text -> [BenchCase] -> Benchmark
mkbenchGroup dir subdir benchCases =
  bgroup (unpack subdir) $ map (\c ->
    env (setupEnv dir subdir c) $ \ _ -> do
      let nm = name c
      bench (unpack nm) $ nfIO (benchAgda nm))
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
  sh $ do result <- inprocWithErr "agda" [fname] (return "")
          case result of
            Left err   -> die (lineToText err)
            Right succ -> return ()

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
evenBenchCases n =
  let cases = take n $ iterate (*2) 8
  in  map evenBenchCase1 cases
      ++ map evenBenchCase2 cases

evenBenchCase1 :: Int -> BenchCase
evenBenchCase1 n =
  BenchCase { name     = nm <> ".agda"
            , template = templ }
  where
    nm :: (Monoid s, IsString s) => s
    nm = "EvenNum" <> int n

    templ :: [Line]
    templ =
      [ "module " <> nm <> " where"
      , ""
      , "open import Base"
      , "open import Auto"
      , ""
      , nm <> " : Even " <> int n
      , nm <> " = apply (auto " <> int n <> " rules)" ]

evenBenchCase2 :: Int -> BenchCase
evenBenchCase2 n =
  BenchCase { name     = nm <> ".agda"
            , template = templ }
  where
    nm :: (Monoid s, IsString s) => s
    nm = "EvenFun" <> int n

    templ :: [Line]
    templ =
      [ "module " <> nm <> " where"
      , ""
      , "open import Base"
      , "open import Auto"
      , ""
      , nm <> " : forall {n} -> Even n -> Even (n + " <> int n <> ")"
      , nm <> " = apply (auto " <> int (n * 2)  <> " rules)" ]

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
      , nm <> " : Plus " <> int n  <> " " <> int 0 <> " " <> int n
      , nm <> " = apply (auto " <> int (n+1) <> " rules)" ]


