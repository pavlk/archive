-- Prelude -------------------------------------------------
ith i lst = case lst of { x:xs -> if i == 0 then x else ith (i-1) xs }
safeIth i lst = ith (i `mod` length lst) lst
-- Globals -------------------------------------------------
ims = map (\x -> "kvet/" ++ x ++ ".jpg") ["01", "02", "03", "04", "05"]
-- ims2 = map (\x -> "numbers/" ++ (show x) ++ "c.jpg") [1..9]

-- Input ---------------------------------------------------
data UserInput = UserInput (Int,Int) (Int,Int) Bool Bool Bool
q x = div (3*x) 4
wdim = (,) <~ (q <~ Window.width) ~ (q <~ Window.height) 
mx = (-) <~ Mouse.x ~ ((\x -> div x 4) <~ Window.width)  
mpos = (,) <~ mx ~ Mouse.y

userInput = UserInput <~ wdim ~ mpos ~ Keyboard.space ~ Keyboard.shift ~ Keyboard.ctrl
--data Input = Input UserInput
data Lt r a = Ver r Lt Lt | Hor r Lt Lt | Seg a | FocSeg a
layout = Hor 0.3 (Hor 0.6 (Seg 0) (Ver 0.5 (FocSeg 1) (Seg 2))) (Hor 0.5 (Seg 3) (Seg 4))


-- Model ---------------------------------------------------
data GameState = GameState [{ x :: Float, y :: Float , i :: Int}] [Int] Lt
defaultGame = GameState [{x = 0.3, y = 0.2, i = 0},{x = 0.4, y = 0.5, i = 1},{x = 0.3, y = 0.3, i = 2},{x = 0.3, y = 0.3, i = 3},{x = 0.3, y = 0.3, i = 0}] [0..6] layout

clr l =
   case l of
    Seg i -> Seg i
    FocSeg i -> Seg i 
    Ver r a b -> Ver r (clr a) (clr b) 
    Hor r a b -> Hor r (clr a) (clr b) 

g_over (w,h) (w',h') l =
    --if (h'>h) || (h'<0) then 0 else 
  case l of
    Seg i -> i
    FocSeg i -> i 
    Ver r a b -> if (h'<h*r) then (g_over (w,h*r) (w',h') a) else (g_over (w,h*(1-r)) (w',h'-h*r) b)
    Hor r a b -> if (w'<w*r) then (g_over (w*r,h) (w',h') a) else (g_over (w*(1-r),h) (w'-w*r,h') b)

f_over (w,h) (w',h') l =
   case l of
    Seg i -> FocSeg i
    FocSeg i -> Seg i 
    Ver r a b -> if (h'<h*r) then (Ver r (f_over (w,h*r) (w',h') a) b) else (Ver r a (f_over (w,h*(1-r)) (w',h'-h*r) b))
    Hor r a b -> if (w'<w*r) then (Hor r (f_over (w*r,h) (w',h') a) b) else (Hor r a (f_over (w*(1-r),h) (w'-w*r,h') b))

fc (w,h) x = customOutline  [8,4] yellow (rect (w-x) (h-x) (div w 2,div h 2) )
dispL' (w,h,s) df l =
  case l of
    FocSeg i -> collage w h ([ toForm (div w 2,div h 2) (df (w,h) i)] ++ (map (fc (w,h)) [3,4,5]) )
    Seg i -> df (w,h) i
    Ver r a b -> flow down [dispL' (w,h*r-s,s) df a, spacer w (2*s), dispL' (w,h*(1-r)-s,s) df b]
    Hor r a b -> flow right [dispL' (w*r-s,h,s) df a, spacer (2*s) h, dispL' (w*(1-r)-s,h,s) df b]

-- Update --------------------------------------------------
mouse_over_fake g b = if b then 1 else 3
update i l f = (take i l) ++ [f (head (drop i l))] ++ (drop (i+1) l)
stepGame (UserInput (w,h) (w',h') spc shi ctr) (GameState g is l) =
  let l2 = (f_over (w,h) (w', h') (if (not ctr) then (clr l) else l))
      ii = g_over (500,600) (150,130) l
      --is2 = (update ii is (\x -> x+1)) 
  in GameState g is l2

-- Display -------------------------------------------------  
displayIm2 g is (w,h) i = displayIm is (w,h) (safeIth i g)
displayIm is (w,h) {x,y,i} = image w h (safeIth (safeIth i is) ims)
displayGS (w,h) (GameState g is l) (UserInput (w,h) (w',h') spc shi ctr) = 
  let ii = (g_over (w,h) (w',h') l)  in  
  flow down $ [ flow right [ container (div w 3) (div h 3) topLeft (flow down [(asText (w',h')) ])--, dispL' (60,40,1) (displayIm2 g is) l]) 
                           , dispL' (w,h,1) (displayIm2 g is) (if shi then (Seg ii) else l)]] --
               ++ [asText spc, asText (g_over (w,h) (w',h') l), asText l, asText (update ii is (\x -> x+1)), asText ii]
--   flow down [asText spc, asText g, asText l, (asText (w,h)), (asText (w',h'))]

-- Run -----------------------------------------------------
input = sampleOn Mouse.clicks userInput
gameState = foldp stepGame defaultGame input 
main = displayGS <~ Window.dimensions ~ gameState ~ userInput


-- Todo -----------------------------------------------------
-- control focused segment(s)
-- rotation + translation + zoom
-- d&d change segment border
-- fusion/overlay
-- segment text
-- separate imgs lists in segments
-- proper synchronization 
-- merge/concatenate layouts (flow down/right)
-- context menus
-- read dicom images
-- read dicom text
-- d&d image-to-segment, imdir-to-layout, db-to-dip
-- d&d --> merge/concatenate layouts (flow down/right)
-- compute and show dicom volumes/3D
-- blowup === layout switch, datarole === alias/selector mnoziny dat === synchronization, overlay === beside, dip === list of layouts
-- online vs. "general" layout editing , d&d replace segments, rotate segments, switch blowup focus -> ako xmonad
-- tangible values


-- Done -------------------------------------------------
--+ mouse_over_fake, --+ synchronization_tmp, + w,h scalable, proper 2D layout, + recursive tree (vertical/horizontal) layout
-- segment borders, -- proper mouse_over/select segment, -- blowup 