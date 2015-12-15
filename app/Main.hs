module Main where

import Lib
import Data.SBV

main :: IO ()
main = do
  r <- sat $ do
    di <- exists "di"
    dj <- exists "dj"


    i <- exists "i"
    j <- exists "j"
    iv <- exists "iv"
    x <- exists "x"
    y <- exists "y"
    constrain $ 0 .<= iv &&& iv .< (4::SInteger)
    constrain $ x+y.==i
    constrain $ x  .==4*j+iv
    let i'=i+di
    let j'=j+dj
    iv' <- exists "iv'"
    x' <- exists "x'"
    y' <- exists "y'"
    constrain $ 0 .<= iv' &&& iv' .< 4
    constrain $ x'+y'.==i'
    constrain $ x'  .==4*j'+iv'

    constrain $ x + 3 .== x'
    constrain $ y + 4 .== y'

    constrain $ (di,dj) ./= (7,0)
    constrain $ (di,dj) ./= (7,1)


    return $ (true :: SBool)
  print r
