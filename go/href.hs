-- href - a simple Haskell reference GO bot
--  - plays the basic Monte Carlo AMAF strategy, 
--  - based on the C reference bot "cref" by Don Dayley, posted on the Computer Go google group as zipfile

-- by: Pavol Vlcek, 2011-06-29

import GoPrelude 

n = 9; k = 7; nn = (n+1)*(n+2); fs c = iff (odd c) 1 2 ; es c = iff (odd c) 2 1
ib = v 4 nn // zip ps [0,0..]; ps = [1+c+(r+1)*(n+1) | r<-[0..n-1], c<-[0..n-1]]

bd = snd.foldr (\m (c,b) -> (c+1,mv b c m)) (0,ib)
    where mv b c m = iff (m==0) b (b//[(m,fs c)]//[(i,0) | i<-tks (b//[(m,fs c)]) c m])

inb m = map (+m) [1,-1,n+1,-(n+1)] 
cnb b = filter (<4).map (b!).inb
moves l = filter ((==0).(bd l!)) ps

dds b t = gr [] [t]
    where gr g [] = g
          gr _ (m:_)  | b!m==0    = []
          gr g (m:ms) | b!m/=b!t  = gr g ms
                      | otherwise = gr (nub (m:g)) $ ms ++ (inb m\\g)

tks b c m = iff (m==0) [] $ nub.concat.map (dds b) $ filter ((==es c).(b!)) $ inb m

legal l m = not (eye || sui || ko)
    where edge = (<4).sz.cnb ib; c = sz l; b = bd l//[(m,fs c)]
          ndien l m = sz.filter (==es (sz l)) $ map ((bd l!).(+m)) [n,-n,n+2,-(n+2)]
          tks' = tks (bd ((drop 1 l))//[((head l),fs (c-1))]) (c-1) (head l)
          eye = all (==fs c) (cnb b m) && (ndien l m) <= iff (edge m) 0 1
          sui = tks b c m == [] && dds b m /= []
          ko = tks b c m == [head l] && tks' == [m]

sc c b = if s==0 then 0 else if (s>0)/=odd c then 1 else -1
    where s = (p 2)-(p 1)-k; p c = sz $ filter ((c==).(owns)) ps
          owns m = iff (b!m>0) (b!m) $ 3.&.foldl (.|.) 0 (cnb b m)

------------------------------------------------------------------------------------------

eval h p m = mean $ map ((sc (sz h)).bd) $ filter first p
    where first l = fi m (ods r) < fi m (evs r) where r = drop (sz h) (reverse l)

play g | [0,0]==(take 2 g) = return g
       | otherwise         = do m <- rf (moves g) (legal g) 0; play (m:g)

mc h l = do p <- mapM play $ replicate l h
            return $ fmax (eval h p) $ 0:filter (legal h) (moves h)

------------------------------------------------------------------------------------------

main = do a <- map read `fmap` getArgs; let (l:h) = iff (a==[]) (10:[]) a
          print $ evalState (mc (reverse h) l) 1

-- TODO: unfold h p t = map h takewhile p iterate t
