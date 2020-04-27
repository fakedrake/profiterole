{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Config(emptyConfig, readConfig, Config(..),showConfig) where

import Data.Maybe
import Data.List.Extra
import qualified Data.Map as Map
import System.IO.Extra
import Data.Functor
import Prelude

data Config = Root
  | Bury
  | Omit
  | RBury
  | Fold
  | NoConfig
  deriving Eq

readConfig :: FilePath -> IO (String -> Config -> Bool)
readConfig file = do
    let f "" = Nothing
        f (stripPrefix "root:" -> Just x) = Just (x, Root)
        f (stripPrefix "bury:" -> Just x) = Just (x, Bury)
        f (stripPrefix "fold:" -> Just x) = Just (x, Fold)
        f (stripPrefix "omit:" -> Just x) = Just (x, Omit)
        f (stripPrefix "rbury:" -> Just x) = Just (x, RBury)
        f (stripPrefix "noconf:" -> Just _) = Nothing
        f x = error $ "Bad config, got " ++ show x
    mp <- mapFromList
         . mapMaybe (fmap (first $ unwords . words) . f)
         . lines
         <$> readFile' file
    pure $ \s -> \case
      NoConfig -> maybe True (const False) $ Map.lookup s mp
      c -> maybe False (elem c) $ Map.lookup s mp
  where
    first f (x,y) = (f x,y)

showConfig :: [(String,Config)] -> String
showConfig xs = unlines [sh cnf ++ ": " ++ str | (str,cnf) <- xs]
  where
    sh Root = "root"
    sh Bury = "bury"
    sh RBury = "rbury"
    sh Fold = "fold"
    sh Omit = "omit"
    sh NoConfig = "noconf"

emptyConfig :: String -> Config -> Bool
emptyConfig _ NoConfig = True
emptyConfig _ _ = False

mapFromList :: Ord a => [(a,b)] -> Map.Map a [b]
mapFromList = foldl' (\m (k,v) -> Map.alter (Just . maybe [v] (v:)) k m) mempty
