-- A simple Elm Game of Life demo.

-- Note: Be careful when setting n > 10. 
-- It is very slow and may freeze your browser.

-- TODO: optimize for speed

-- Utils
safe_plus n (a,b) (x,y) = (mod (a+x) n, mod (b+y) n)
has l x = any (\y -> y==x) l
cprod xs ys = if (xs == [] || ys == []) then []
              else (map (\y -> (head xs, y)) ys) ++ (cprod (tail xs) ys)

-- Model
n = 12 -- grid size
step _ b = let ambient c = map (safe_plus n c) $ cprod [0-1..1] [0-1..1]
               is_nextgen c = let score = length $ filter (has b) (ambient c)
                              in    score == 4 && (has b c)
                                 || score == 3
           in filter is_nextgen $ cprod [0..n-1] [0..n-1]

-- Display
cell2color b c = if (snd c==0) then gray else if has b c then black else white
cellcolors b   = split [gray] (map (cell2color b) $ cprod [0..n-1] [0..n-1])
cellshape c    = collage 10 10 [filled c (ngon 10 10)]
display b      = flow down $ map (\x -> flow right (map cellshape x)) (cellcolors b)

-- Patterns
blinker   = [(0,7), (1,7), (2,7)]
glider    = [(4,3), (5,1), (5,3), (6,2), (6,3)]
cross     = (zip [0..n-1] [0..n-1]) ++ (zip (reverse [0..n-1]) [0..n-1])
pentomino = blinker ++ [(0,8), (1,6)]

-- Run
init_board = pentomino --cross ++ blinker ++ glider
main = display <~ (foldp step init_board $ every (second/2))