-- href, optimization via the State Monad
-- by: Pavol Vlcek, 2011-06-26

import System.Environment    -- getArgs
import Data.List             -- nub, \\, maximumBy, elemIndex 
import Data.Bits             -- .&., .|., shiftR
import Data.Ord              -- comparing
import Control.Monad.ST (ST, runST)
import Control.Monad    (filterM, foldM)
import Data.Array.ST    (STUArray, newListArray, getElems)
import Data.Array.Base  (unsafeRead, unsafeWrite)
import Data.STRef
import Data.Array            -- !, //, array

-- board --------------------------------------------------------------------------
data Game s = G {col::STRef s Int, hist::STRef s [Int], lstks::STRef s Int, moves::STRef s [Int], bx:: !(STUArray s Int Int), 
                dd:: !(STUArray s Int Int), dd'::STRef s Int,
                gr:: !(STUArray s Int Int), gr'::STRef s Int}

n = 9; k = 7; nn = (n+1)*(n+2);
ps = [1+c+(r+1)*(n+1) | r<-[0..n-1], c<-[0..n-1]] 
eps = filter ((==0).(ib!)) ps
ib = v 4 nn // [(i,0)|i<-ps]; fs c = if' (odd c) 1 2; es c = if' (odd c) 2 1 

igM = do c <- newSTRef 0
         h <- newSTRef []
         l <- newSTRef (-1)
         m <- newSTRef eps
         b <- mkSTUArray $ elems ib
         d <- mkSTUArray $ elems ib
         d' <- newSTRef 0
         g <- mkSTUArray $ elems ib
         g' <- newSTRef 0
         return $ G c h l m b d d' g g'

fillG l g = do put (col g) 0
               put (hist g) []
               put (lstks g) (-1)
               put (moves g) eps
               mapM (unmv' g) ps
               mapM (\m -> do domv' g m; move' g m) (reverse l)

move' g m = do c <- get (col g)
               t <- tksM g c m
               mapM (unmv' g) t
               put (col g) (c+1)
               put (lstks g) $ if (sz t) == 1 then (head t) else (-1)
               h <- get (hist g)
               put (hist g) (m:h)
               ms <- get (moves g)
               put (moves g) (((ms)\\[m])++t) 

unmv' g m = if (m==0) then return 0 else do wa (bx g) m 0; return 0
domv' g m = if (m==0) then return 0 else do c <- get (col g); wa (bx g) m (fs c); return 0

inb m = map (+m) [1,-1,n+1,-(n+1)]; 
inbs = listArray (0,nn) (map inb [0..nn])

cnbM g m = do nbs <- mapM (ra (bx g)) (inbs!m)
              return $ filter (<4) nbs
ndieM g m = do c <- get (col g)
               die <- mapM (ra (bx g)) $ map (+m) [n,-n,n+2,-(n+2)] 
               return (sz (filter (==es c) die))

edge = any (==4).map (ib!).(inbs!); 
edg' = v 1 nn // [(i,0)|i<-(filter edge ps)];

ls g = do h <- get (hist g)
          return (head h)

end g =  do h <- get (hist g)
            return ([0,0] `isPrefixOf` h)

addset gr gi m = go gr 0 m
  where go gr lo m = if lo >= gi 
                            then do wa gr lo m
                                    return 1
                            else do x <- ra gr lo 
                                    if x == m 
                                     then return 0
                                     else go gr (lo+1) m

ddsM g t = do wa (dd g) 1 t
              grp g 0 1
 where grp g gi di = do t' <- ra (bx g) t
                        if di == 0 
                         then do el <- getElems (gr g)
                                 return (take gi el)
                         else do m <- ra (dd g) di 
                                 x <- ra (bx g) m
                                 if x == 0 
                                  then return []
                                  else do if x /= t' 
                                            then grp g gi (di-1)         
                                            else do added <- addset (gr g) gi m
                                                    el <- getElems (gr g)
                                                    let dd2 = ((inbs!m)\\(take gi el))                                        
                                                    mapM (\(i,v) -> do wa (dd g) i v) (zip [di,di+1,di+2,di+3] dd2)
                                                    grp g (gi+added) (di-1+(sz dd2))

tksM g c m = do cnb <- filterM (\x -> do i <- ra (bx g) x; return (i==es c)) (inbs!m)
                dd <- mapM (ddsM g) cnb
                if (m==0) then return [] else return $ nub $ concat $ dd

-- rules --------------------------------------------------------------------------
legal g m =   do c <- get (col g)
                 cnbb <- cnbM g m
                 ndiee <- ndieM g m
                 d <- ddsM g m 
                 t <- tksM g c m
                 lst <- get (lstks g)
                 lsg <- ls g
                 let eye = all (==fs c) cnbb && ndiee <= edg'!m
                     scd = t == [] && d /= []
                     ko =  sz t == 1 && head t == lsg && lst == m
                 return $ not (eye || scd || ko)

sx :: Int -> Game s -> (ST s Int)
sx c g = (do ss <- s; if (ss == 0) then return 0 else (if ((ss > 0) /= odd c) then return 1 else return (0-1)))
    where s = do ww <- p 2 
                 bb <- p 1 
                 return (ww - bb - k)
          p x = do f <- filterM (\y -> (do o <- owns y; return (x==o))) ps
                   return (sz f)
          owns m = (do x <- ra (bx g) m
                       if (x > 0) then return x else (do cnbb <- cnbM g m 
                                                         return (3 .&. foldl (.|.) 0 cnbb)))
              
-- algorithm ----------------------------------------------------------------------
evl h p m = do fil <- filterM first p
               scs <- mapM (\x -> do return $ snd x) fil
               return $ mean $ scs
   where first g = do let r = drop (sz h) (reverse (fst g))
                      return $ fi m (ods r) < fi m (evs r)

play h gg g = do e <- end g     
                 if e 
                  then do hi <- get (hist g)
                          sc <- sx (sz h) g
                          return (hi,sc) 
                  else do ms <- get (moves g)
                          m <- rf g gg ms legal 0
                          domv' g m
                          move' g m
                          play h gg g

rf g gg [] _ z = return z 
rf g gg  l f z = do k <- rnd gg (sz l)
                    let (ys, x:xs) = splitAt k l 
                    domv' g x
                    lg <- f g x
                    if lg
                       then (return x)
                       else  do unmv' g x
                                (rf g gg (ys++xs) f z)
                  
mc h l = runST (do g <- igM
                   fillG h g
                   gg <- newSTRef (1,g)
                   ms <- get (moves g)
                   emp <- filterM (legal g) ms
                   let lgs = emp++[0]
                   sts <- mapM (\_ -> do fillG h g; (play h gg g)) $ replicate l 0
                   ys <- mapM (evl h sts) lgs
                   return $ fst $ maximumBy (comparing snd) $ reverse $ zip lgs ys)
   
-- main ---------------------------------------------------------------------------
main = do a <- map read `fmap` getArgs; let (l:h) = if' (a==[]) [10] a
          print $ mc (reverse h) l 

-- utils --------------------------------------------------------------------------
sz = length; if' True  x _ = x; if' False _ y = y; 
v c s = array (0,s) [(i,c) | i <- [0..s] ] :: Array Int Int                           
mkSTUArray = newListArray (0,nn) :: [Int] ->  ST s (STUArray s Int Int)

ra :: STUArray s Int Int -> Int -> ST s Int
ra = unsafeRead
wa :: STUArray s Int Int -> Int -> Int -> ST s ()
wa = unsafeWrite

mean = go 0 0 where go s l []     = realToFrac s / fromIntegral l
                    go s l (x:xs) = go (s+x) (l+1) xs

fi x l = elemIndex x (l++[x]); ods = fst.split' ; evs = snd.split' 
split' []=([],[]); split' [x]=([x],[]); split' (x:y:xs)=(x:xp,y:yp) 
    where (xp,yp) = split' xs

get = readSTRef; put = writeSTRef                

rnd gg d = do (x,g) <- get gg
              let a = 3412421; s = a+x*a `mod` 2^20; r = shiftR ((s.&.65535)*d) 16 
              put gg (s,g) >> return r

-- Notes:
-- Intel(R) Core(TM)2 Quad CPU    Q9400  @ 2.66GHz
-- ghc -O2 -ddump-simpl      --core
-- +RTS -sstderr             --stat
-- -keep-tmp-files           --asm
-- ghc -O2 --make 020.hs -prof -auto-all -caf-all -fforce-recomp     ------->    ./020 1000 +RTS -p -K400M -hy  --->  hp2ps -c 020.hp
-- {-# OPTIONS_GHC -fvia-C -optc-O3 -fexcess-precision #-}
