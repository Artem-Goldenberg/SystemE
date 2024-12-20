module Main (main) where

import Format
import Algorithms

x = Var $ TermVar "x"
y = Var $ TermVar "y"
z = Var $ TermVar "z"
xvar = TermVar "x"
yvar = TermVar "y"
zvar = TermVar "z"

t = App (Lam xvar $ App x x) (Lam zvar $ App z y)

main :: IO ()
main = putStrLn "some"
