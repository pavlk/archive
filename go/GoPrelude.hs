module GoPrelude 
(
 array, Array, get, put, nub, getArgs, evalState, maximumBy, elemIndex, shiftR, comparing,
 (//), (!), (\\), (.&.), (.|.), sz, v, if', mean, fi, ods, evs, rf, fmax
)
where

import System.Environment  -- getArgs
import Control.Monad.State -- evalstate, get, put
import Data.List           -- nub, \\, maximumBy, elemIndex 
import Data.Array          -- !, //, array
import Data.Bits           -- .&., .|., shiftR
import Data.Ord            -- comparing

mean l = sum l / fromIntegral (sz l); 
fmax f xs = fst $ maximumBy (comparing snd) $ reverse $ zip xs (map f xs)
v c n = array (0,n) [(i,c) | i <- [0..n] ] :: Array Int Int
sz = length ; fi x l = elemIndex x (l++[x])

if' True  x _ = x 
if' False _ y = y 

ods = fst.split'
evs = snd.split' 

split' []=([],[]); split' [x]=([x],[]); split' (x:y:xs)=(x:xp,y:yp) 
    where (xp,yp) = split' xs

rf [] f a = return a 
rf  l f a = do k <- rnd (sz l)
               let (b, (x:xs)) = splitAt k l in if' (f x) (return x) (rf (b++xs) f a)
    where rnd n = do x <- get 
                     let a = 3412421; s = a+x*a `mod` 2^20; r = shiftR ((s.&.65535)*n) 16 
                     put s >> return r