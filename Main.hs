{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Control.Monad

import qualified Data.Map                   as M
import qualified Data.ByteString.Lazy.Char8 as B

import System.Environment

import Effusion

main :: IO ()
main = do
    (fn:c1:c2:c3:[]) <- getArgs
    (headers : cells) <- liftM (map (B.split ',') . B.lines) (B.readFile fn)
    let (c1i:c2i:c3i:[]) = let m = ((M.fromList .) . zip) headers [0..] in map (m M.!) (map B.pack (c1:c2:c3:[]))
    let sparse = (M.fromList . map (\x -> (((,) .) . (,)) (x !! c1i) (x !! c2i) (x !! c3i))) cells
    let (xs,ys) = let ks = M.keys sparse in ((fastNub . map fst) ks, (fastNub . map snd) ks)
    let mesh = chunk (length ys) [(x, y) | x <- xs, y <- ys]
    B.putStrLn $ B.intercalate "," ("*":ys)
    mapM_ B.putStrLn $ zipWith (\rn ps -> B.intercalate "," (rn:(map (flip (M.findWithDefault "0") sparse) ps))) xs mesh
