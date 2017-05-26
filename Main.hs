module Main where

import System.IO
import Graph
import Dinic

main = do
    text <- readFile "input.txt"
    let graph = graphFromString text
    putStrLn $ show $ dinic graph