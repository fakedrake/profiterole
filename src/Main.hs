{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}

module Main(main) where

import GHC.Prof
import Data.List.Extra
import Data.Char
import Data.Monoid
import Data.Scientific
import Data.Tree
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    Right (Just tree) <- fmap costCentres . decode <$> T.readFile (head args)
    let vals = removeZero $ fmap toVal tree
    let roots = findRoots vals
    let vals2 =  sortOn (negate . valTimeInh . rootLabel) $
                 map (sortTreeOn (negate . valTimeTot)) $
                 map fixTimeInh $
                 mergeRoots $ liftRoots roots vals
    putStr $ unlines $ intercalate ["",""] $
        showVals (map rootLabel $ take 25 vals2) :
        [showVals [y{valId = replicate (i*2) ' ' ++ valId y} | (i,y) <- unwindTree x] | x <- vals2]

unwindTree :: Tree a -> [(Int,a)]
unwindTree = f 0
    where f i (Node x xs) = (i,x) : concatMap (f $! i+1) xs

sortTreeOn :: Ord b => (a -> b) -> Tree a -> Tree a
sortTreeOn f (Node x xs) = Node x $ sortOn (f . rootLabel) (map (sortTreeOn f) xs)

rootId = valId . rootLabel

data Val = Val
    {valId :: String
    ,valTimeTot :: Scientific
    ,valTimeInh :: Scientific
    ,valTimeInd :: Scientific
    ,valEntries :: Integer
    } deriving Show

toVal :: CostCentre -> Val
toVal CostCentre{..} = Val
    (T.unpack costCentreModule ++ " " ++ T.unpack costCentreName)
    costCentreInhTime costCentreInhTime costCentreIndTime
    costCentreEntries

removeZero :: Tree Val -> Tree Val
removeZero (Node x xs) = Node x $ map removeZero $ filter (not . isZero . rootLabel) xs
    where isZero Val{..} = valTimeTot == 0


-- | A root has at least two distinct parents and isn't a local binding
findRoots :: Tree Val -> Set.Set String
findRoots x = Map.keysSet $
    Map.filterWithKey (\k v -> not (isLocal k) && not (isSpecial k) && Set.size v > 1) $
    Map.fromListWith (<>) $ f x
    where
        f (Node v xs) = [(rootId x, Set.singleton $ valId v) | x <- xs] ++
                        concatMap f xs
        isLocal (word1 -> (_, x)) =  any isAlpha x && '.' `elem` x
        isSpecial (word1 -> (x, _)) = x `elem` ["Language.Haskell.Exts.ParseMonad","Language.Haskell.Exts.InternalLexer"]

liftRoots :: Set.Set String -> Tree Val -> [Tree Val]
liftRoots set x = fs set x
    where
        fs set x = uncurry (:) $ f set x
        f set (Node x ys)
            | valId x `Set.member` set = (Node x{valTimeInh=0,valTimeInd=0} [], fs (Set.delete (valId x) set) $ Node x ys)
            | otherwise = let (as, bs) = unzip $ map (f set) ys in (Node x as, concat bs)


mergeRoots :: [Tree Val] -> [Tree Val]
mergeRoots xs = Map.elems $ Map.fromListWith f [(rootId x, x) | x <- xs]
    where
        f (Node x1 xs) (Node y1 ys)
            | valId x1 /= valId y1 = error $ "mergeRoots, invariant violated"
            | otherwise = Node xy1 $ mergeRoots $ xs ++ ys
            where
                xy1 = Val {valId = valId x1
                          ,valTimeTot = valTimeTot x1 + valTimeTot y1
                          ,valTimeInh = valTimeInh x1 + valTimeInh y1
                          ,valTimeInd = valTimeInd x1 + valTimeInd y1
                          ,valEntries = valEntries x1 + valEntries y1}

fixTimeInh :: Tree Val -> Tree Val
fixTimeInh (Node x xs) = Node x{valTimeInh = sum $ valTimeInd x : map (valTimeInh . rootLabel) ys} ys
    where ys = map fixTimeInh xs


newtype Id = Id {fromId :: String} -- the ModuleName functionName pair 
    deriving (Eq,Ord,Show)

data Result2 = Result2
    {key2 :: Id
    ,timeTot :: Scientific -- spent in other trees
    ,timeInh :: Scientific -- spent in this tree
    ,timeInd :: Scientific -- spent in this node
    } deriving Show

showResults :: [Result2] -> [String]
showResults xs = [intercalate "  " [g w1 timeTot, g w2 timeInh, g w3 timeInd, fromId key2] | Result2{..} <- xs]
    where
        w1 = maximum $ map (length . f . timeTot) xs
        w2 = maximum $ map (length . f . timeInh) xs 
        w3 = maximum $ map (length . f . timeInd) xs 
        f x = case show x of
            "0.0" -> "-"
            ['0','.',x] -> ['.',x]
            x -> x
        g w x = let s = f x in replicate (w - length s) ' ' ++ s

showVals :: [Val] -> [String]
showVals xs = showResults [Result2{key2 = Id $ valId ++ " (" ++ show valEntries ++ ")", timeTot = valTimeTot, timeInh = valTimeInh, timeInd = valTimeInd} | Val{..} <- xs]
