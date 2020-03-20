{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Main(main) where

import Data.Foldable
import Debug.Trace
import Data.Maybe
import GHC.Prof
import Control.Monad
import Data.List.Extra
import Data.Char
import Data.Monoid
import Data.Tree hiding (foldTree)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text.Lazy.IO as T
import System.Environment
import System.FilePath
import System.Directory
import Type
import Config
import Report
import Data.Functor
import Prelude


main :: IO ()
main = do
    [arg] <- getArgs
    Right (valsFull :: Tree Val) <-
      fmap (removeZero . valFromProfile) . decode <$> T.readFile arg
    b <- doesFileExist ".profiterole.yaml"
    config0 <- if b then readConfig ".profiterole.yaml" else pure emptyConfig
    let vals = expandTree config0 valsFull
    let arg0 = if takeExtension arg == ".prof" then dropExtension arg else arg
    let writeTo file x = do
            putStrLn $ "Writing to " ++ file ++ " ..."
            writeFile file x
    writeTo (arg0 <.> "profiterole.txt") $ unlines $ reportText vals
    writeTo (arg0 <.> "profiterole.html") $ unlines $ reportHTML vals
    putStrLn "Done"
    when False $ do
        -- Should check the time is not lost, if it is, suggest -P
        print $ sum $ map timeInd $ concatMap flatten vals
        print $ sum $ map (timeInh . rootLabel) vals

expandTree :: (String -> Config -> Bool) -> Tree Val -> [Tree Val]
expandTree config0 valsRaw =
  fmap (omitNodes config0')
  $ mergeRoots
  $ liftRoots roots vals
  where
    config0' = config0 . name . rootLabel
    roots = findRoots config vals
    config :: String -> Config -> Bool
    infBurried = inferBuried config0' valsRaw
    config n = if Set.member n infBurried
               then (== Bury)
               else config0 n
    vals = foldTree config0' valsRaw
---------------------------------------------------------------------
-- TRANSFORMATIONS

removeZero :: Tree Val -> Tree Val
removeZero (Node x xs) = Node x $ map removeZero $ filter (not . isZero . rootLabel) xs
    where isZero Val{..} = timeTot == 0


-- | A root has at least two distinct parents and isn't a local binding
findRoots :: (String -> Config -> Bool) -> Tree Val -> Set.Set String
findRoots config x =
  Map.keysSet
  $ Map.filterWithKey (\k v -> if
                          | config k Root -> True
                          | config k NoConfig -> Set.size v > 1
                          | otherwise -> False)
  $ Map.fromListWith (<>) $ f x
  where
    f :: Tree Val -> [(String,Set.Set String)]
    f (Node v xs) = [(name $ rootLabel x, Set.singleton $ name v) | x <- xs] ++
                    concatMap f xs
    isLocal (word1 -> (_, x)) =  any isAlpha x && '.' `elem` x

liftRoots :: Set.Set String -> Tree Val -> [Tree Val]
liftRoots = fs
  where
    fs set x = let (y,_,ys) = f set x in y:ys

    -- return (this tree, discount to apply up, new roots)
    f :: Set.Set String -> Tree Val -> (Tree Val, Double, [Tree Val])
    f set (Node x ys)
      | name x `Set.member` set = (Node x{timeInh=0,timeInd=0} [], timeInh x, fs (Set.delete (name x) set) $ Node x ys)
      | otherwise = (Node x{timeInh = timeInh x - disc} child, disc, root)
          where (child, sum -> disc, concat -> root) = unzip3 $ map (f set) ys

mergeRoots :: [Tree Val] -> [Tree Val]
mergeRoots xs = Map.elems $ Map.fromListWith f [(name $ rootLabel x, x) | x <- xs]
    where f (Node x xs) (Node y ys) = Node (mergeVal x y) $ mergeRoots $ xs ++ ys

foldTree :: (Tree Val -> Config -> Bool) -> Tree Val -> Tree Val
foldTree config tree = if config tree Fold
  then foldTree' config tree
  else tree{subForest=foldTree config <$> subForest tree}

inferBuried :: (Tree Val -> Config -> Bool) -> Tree Val -> Set.Set String
inferBuried config tree =
  if config tree RBury
  then inferBuried' config tree
  else mconcat $ inferBuried config <$> subForest tree

-- | The provided tree is omitted.
inferBuried' :: (Tree Val -> Config -> Bool) -> Tree Val -> Set.Set String
inferBuried' config = Set.fromList . fmap (name . rootLabel) . subForest'
  where
    subForest1' tree = [subTree | subTree <- subForest tree,not $ config subTree Root]
    subForest' tree = tree:trees ++ (subForest' =<< trees) where
      trees = subForest1' tree

-- | fold except for roots.
foldTree' :: (Tree Val -> Config -> Bool) -> Tree Val -> Tree Val
foldTree' config omitted = omitted{
  subForest=mapMaybe reduceSubtree $ subForest omitted}
  where
    reduceSubtree :: Tree Val -> Maybe (Tree Val)
    reduceSubtree tree = if config tree Root
      then Just $ foldTree' config tree
      else Nothing

-- Top node and root nodes can't be omitted
omitNodes :: (Tree Val -> Config -> Bool) -> Tree Val -> Tree Val
omitNodes config node = node{subForest=go =<< subForest node} where
  go :: Tree Val -> [Tree Val]
  go node = if | config node Root -> [node{subForest=go =<< subForest node}]
               | config node Omit -> go =<< subForest node
               | otherwise -> [node{subForest=go =<< subForest node}]

findSubTree :: (Tree a -> Bool) -> Tree a -> [Tree a]
findSubTree f tree = if f tree
  then [tree]
  else findSubTree f =<< subForest tree
