module GraphingCalculator where

import AuralCalculator as A hiding (main, å, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)

import Codec.Picture
--import Codec.Picture.Gif --(GifEncode(GifEncode))
import Codec.Picture.Types

import Data.Complex

data Color = Color Int Int Int --from haskell mooc
  deriving (Show,Eq)

black = Color 0 0 0;white = Color 255 255 255;pink = Color 255 105 180;red = Color 255 0 0;yellow = Color 255 240 0

blue = PixelRGB8 0 0 255
green = PixelRGB8 0 255 0
violet = PixelRGB8 255 0 255
orange = PixelRGB8 255 100 0

pictureName = "haskellpicture.png"

pictureWidth = floor floatWidth
pictureHeight = floor floatHeight

floatWidth :: Float
floatHeight :: Float
--floatWidth = fromIntegral pictureWidth :: Float
--floatWidth = 400
--floatWidth = 128
floatWidth = 512
--floatWidth = 1024
--floatHeight = fromIntegral pictureHeight :: Float
--floatHeight = floorf (up-down)*floatWidth/(right-left)
floatHeight = floatWidth

left :: Float
right :: Float
up :: Float
down :: Float
--(left,right,up,down) = (-2.5,1.0,1.0,-1.0)
(left,right,up,down) = (-1.0,1.0,1.0,-1.0)
--(left,right,up,down) = (-2.0,2.0,1.0,-1.0)
--left = -2.5
--right = 1.0
--up = 1.0
--down = -1.0

type ColorFunction = Int -> Int -> PixelRGB8

--scales the coordinate space to fit within the bounds and also flips y to start from the bottom
scale :: Coordinate -> Coordinate
scale (Coordinate x y) = Coordinate ((right-left)*x/floatWidth+left) (-(up-down)*y/floatHeight-down)

--scales everything to go from -width/2 to width/2, so it only centers, doesn't stretch
scale2 (Coordinate x y) = Coordinate (x+floatWidth*left/(right-left)) (-y-floatHeight*down/(up-down))

{-
unaryToPicture :: (Float -> Float) -> PictureFunction
unaryToPicture f (Coordinate x y)
--  | floor w == 0 = yellow
--  | floor z2 == 0 = yellow
--  | w < 0 = red
--  | floor (floatWidth *§ f z) == floor (floatHeight *§ w) = white
--  | floor (f z) == floor w = white
  | floor (zzz*zz*f z) == floor w = white
--  | floor (zzz*zz*f (floorf (z*zoom)/zoom)) == floor (floorf (w*zoom)/zoom) = white
  | otherwise = black
  where
    zoffset = 0
    woffset = -0
    zzoom = 1
    wzoom = 1
    --this is the zoom
    zzz = 1
    zz = floatHeight/2
    zoom = 2
    --a subresolution only makes sense on a square frame. so here we pick width even though height is equivalent as well.
    resolution = floatWidth/2
    --Coordinate z w = scale (Coordinate x y)
    --Coordinate z w = scale2 (Coordinate x y)
    Coordinate z2 w2 = scale2 (Coordinate x y); z = z2/zz/zzz/zzoom-zoffset; w = w2/wzoom-woffset*zz*zzz
    --Coordinate z2 w2 = scale2 (Coordinate (floorf (x/zoom)*zoom) (floorf (y/2)*2)); z = z2/zz/zzz/zzoom-zoffset; w = w2/wzoom-woffset*zz*zzz
    --Coordinate z2 w2 = scale2 (Coordinate x y); z3 = z2/zz/zzz/zzoom-zoffset; w = w2/wzoom-woffset*zz*zzz--; z = floorf (z3*zoom)/zoom; w = w3--floorf (w3/2)*2
-}

{-
unaryToPicture2 :: (Float -> Float) -> PictureFunction2
unaryToPicture2 f (Coordinate x y)
--  | y == -1 = blue
--  | x == -1 = blue
  | (floorf $ (*z2) . f .(/z2) $ floorf $ z2*x) == (floorf $ resolution2*y/(up-down)) = violet
--  | (floorf $ (*z) . f .(/z) $ floorf $ z*x) == (floorf $ resolution*y/(up-down)) = blue
--  | (floorf $ (*z) . f .(/z) . (+1/resolution) $ floorf $ z*x) == (floorf $ resolution*y/(up-down)) = orange
--the 1/resolution makes sure that the function is applied to the value at the middle of the square, not at the left coordinate, the middle one, so that the square is in the middle of the very function that goes through it.
--  | (floorf $ (*z3) . f . (+1/resolution3) .(/z3) $ floorf $ z3*x) == (floorf $ resolution3*y/(up-down)) = blue
  | function 4 = blue
  | function 8 = orange
--  | (floorf $ (*z) . f . (+1/resolution) .(/z) $ floorf $ z*x) == (floorf $ resolution*y/(up-down)) = orange
  | otherwise = green
  where
    function i = (floorf $ (*zz) . f . (+1/res) .(/zz) $ floorf $ zz*x) == (floorf $ res*y/(up-down))
      where
        res = floatWidth/i
        zz = res/(right-left)

    z = resolution/(right-left)
    resolution = floatWidth/8

    z2 = resolution2/(right-left)
    resolution2 = floatWidth/1

    z3 = resolution3/(right-left)
    resolution3 = floatWidth/4
-}

{-
unaryToPicture3 :: (Float -> Float) -> PictureFunction2
unaryToPicture3 f (Coordinate x y)
  | function 1 = hslToRgb2 (0,0,1)
--  | function 2 = hslToRgb2 (0,0,0.8)
--  | function 4 = hslToRgb2 (0,0,0.6)
--  | function 8 = hslToRgb2 (0,0,0.4)
--  | function 16 = hslToRgb2 (0,0,0.2)
  | y == 0 = blue
  | x == 0 = blue
  | otherwise = hslToRgb2 (0,0,0)
  where
    function i = (floorf $ (*zz) . f . (+1/resolution) .(/zz) $ floorf $ zz*x) == (floorf $ resolution*y/(up-down))
      where
        resolution = floatWidth/i
        zz = resolution/(right-left)
-}


pictureFunction :: Coordinate -> Color
--pictureFunction = pic
--pictureFunction = circular . mandelbrot 13 . scale
--pictureFunction = circular . mandelbar 13 . scale
--pictureFunction = circular . burningship 13 . scale
--pictureFunction = circular . mandelbox 5 . scale
pictureFunction = circular . julia 40 . scale
{-
oh heck it is supposed to have gaps in it.
I'm not gonna interpolate that
-}

type PictureWidth = Int
type PictureHeight = Int

--renders with the color type
render2 :: PictureFunction -> IO ()
render2 f = writePng pictureName (generateImage (\x y -> colorToPixel (f (floatCoordinate x y))) w h)
--render2 f = writePng pictureName (generateImage (\x y -> colorToPixel (f (floatCoordinate2 x y))) w h)
  where
    colorToPixel :: Color -> PixelRGB8
    colorToPixel (Color r g b) = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
    w = pictureWidth
    h = pictureHeight

data Coordinate = Coordinate Float Float

floatCoordinate :: Int -> Int -> Coordinate
floatCoordinate x y = Coordinate (fromIntegral x) (fromIntegral y)

floatCoordinate2 x y = floatCoordinate (zoom*div x zoom) (zoom*div y zoom)
  where
    zoom = 8

type Color2 = PixelRGB8

type PictureFunction = Coordinate -> Color
type PictureFunction2 = Coordinate -> PixelRGB8

--now, the thing is that my float functions for the sound stuff are bounded between one and negative one, because of stuff like sine and cosine.

--now I just need some sort of function that converts a singular value to a black and white gradient, for example

--hue, saturation, and lightness go from 0 to 1
hslToRgb :: (Float,Float,Float) -> Color
hslToRgb (h,s,l) = Color (ceil $ 255*(r'+m)) (ceil $ 255*(g'+m)) (ceil $ 255*(b'+m))
  where
    c = (1-abs (2*l-1))*s
    x = c*((1-triangle2 (6*h)) /2)
    m = l-c/2
    (r',g',b')
      | h <= 1/6 = (c,x,0)
      | h <= 2/6 = (x,c,0)
      | h <= 3/6 = (0,c,x)
      | h <= 4/6 = (0,x,c)
      | h <= 5/6 = (x,0,c)
      | otherwise = (c,0,x)
    --(r,g,b) = (255*(r'+m),255*(g'+m),255*(b'+m))

--converts straight to pixels
hslToRgb2 :: (Float,Float,Float) -> PixelRGB8
hslToRgb2 (h,s,l) = PixelRGB8 (ceil $ 255*(r'+m)) (ceil $ 255*(g'+m)) (ceil $ 255*(b'+m))
  where
    c = (1-abs (2*l-1))*s
    x = c*((1-triangle2 (6*h)) /2)
    m = l-c/2
    (r',g',b')
      | h <= 1/6 = (c,x,0)
      | h <= 2/6 = (x,c,0)
      | h <= 3/6 = (0,c,x)
      | h <= 4/6 = (0,x,c)
      | h <= 5/6 = (x,0,c)
      | otherwise = (c,0,x)

distant (Coordinate x y) = x*§x+§y*§y

--so this doesn't even check whether it's within a circle, just colors based on values, so should rename this function, we need no circular, and it's a dumb name anyway, I should rename some stuff
circular :: Coordinate -> Color
circular (Coordinate x y)
  --actually we shouldn't do any magnitude checking at the circular, that's duplicated code
--  | distant (Coordinate x y) < 4 = black
  | x < 4 = black
--  | distant c < 8 = yellow
--  | distant c < 19 = pink
--  | otherwise = hslToRgb (5/6,(x-4-y)/(1+x-4),(x-4-y/2)/(1+x-4))
  | otherwise = hslToRgb (3/6,1,y/(1+x-4))
  | otherwise = Color (div (256* floor y) 13) (div (256* floor y) 13) (div (256* floor y) 13)
  | y < 5 = yellow
  | y < 8 = pink
  | y < 11 = red
  | otherwise = black

mandelbrot iterations c = iteration c c iterations
  where
    iteration x y 0 = x
    iteration (Coordinate x y) (Coordinate z w) i
      | distant (Coordinate x y) < 4 = iteration (Coordinate (x*x-y*y+z) (2*x*y+w)) (Coordinate z w) (i-1)
      | otherwise = Coordinate 4 i

mandelbrot2 iterations c = iteration c c iterations
  where
    iteration x y 0 = x
    iteration x y i
      | isShorter x 2 = iteration (x ^^ 2 + y) y (i-1)
      | otherwise = (4 :+ i)

mandelbar iterations c = iteration c c iterations
  where
    iteration x y 0 = x
    iteration (Coordinate x y) (Coordinate z w) i
      | distant (Coordinate x y) < 4 = iteration (Coordinate (x*x-y*y+z) (w-2*x*y)) (Coordinate z w) (i-1)
      | otherwise = Coordinate 4 i

mandelbar2 iterations c = iteration c c iterations
  where
    iteration x y 0 = x
    iteration x y i
      | isShorter x 2 = iteration (conjugate (x ^^ 2) + y) y (i-1)
      | otherwise = (4 :+ i)

burningship iterations c = iteration c c iterations
  where
    iteration (Coordinate x y) (Coordinate z w) 0 = Coordinate x y
    iteration (Coordinate x y) (Coordinate z w) i
      | distant (Coordinate x y) < 4 = iteration (Coordinate (abs (x*x-y*y)+z) (abs (2*x*y)+w)) (Coordinate z w) (i-1)
      | otherwise = Coordinate 4 i

burningship2 iterations c = iteration c c iterations
  where
    iteration x y 0 = x
    iteration x y i
      | isShorter x 2 = iteration ((abs a :+ abs b) + y) y (i-1)
      | otherwise = (4 :+ i)
      where
        a :+ b = x ^^ 2

mandelbox iterations c = iteration c c iterations
  where
    iteration x y 0 = x
    iteration (Coordinate x y) (Coordinate z w) i
      | m < 99 = iteration (Coordinate (scalar*xxx+z) (scalar*yyy+w)) (Coordinate z w) (i-1)
      | otherwise = Coordinate 4 i
      where
        scalar = 2.1--1.5
        xx = if x > 1 then x-2 else if x < -1 then x+2 else x
        yy = if y > 1 then y-2 else if y < -1 then y+2 else y
        Coordinate xxx yyy = if m < 0.25 then Coordinate (4*xx) (4*yy) else if m < 1 then Coordinate (xx/m) (yy/m) else Coordinate xx yy
        m = distant (Coordinate xx yy)

julia iterations c = iteration c iterations
  where
    iteration x 0 = x
    iteration (Coordinate x y) i
--      | distant (Coordinate x y) < 4 = iteration (Coordinate (x*x-y*y+z) (2*x*y+w)) (i-1)
      | distant (Coordinate x y) < 4 = iteration (Coordinate xx yy) (i-1)
      | otherwise = Coordinate (4+iterations) i
      where
        (xx :+ yy) = (x :+ y) ^^ (2) + (z :+ w)
    --z = 0.3
    --z = -0.4
    z = -0.6
    w = 0.6

julia2 iterations c = iteration c iterations
  where
    iteration x 0 = x
    iteration x i
      | isShorter x 4 = iteration (x ^^ (2) + (z :+ w)) (i-1)
      | otherwise = (4+iterations) :+ i
    --z = 0.3
    --z = -0.4
    z = -0.6
    w = 0.6

pic :: PictureFunction
pic (Coordinate x y)
  | x < y = yellow
  | True = hslToRgb (0,0,(y/floatHeight))
  | y < 3 = pink
  | x < 1 = red
  | sin (16*x) > 1/2 = white
  | otherwise = black
  where
    w = floatWidth
    h = floatHeight


main = mainly

mainly = do
  render2 pictureFunction

x_stands_for_main :: IO ()
x_stands_for_main = mainly
å = mainly

--this tests how to do pictures from the ground up from ints and at different resolutions
mainly2 = do
  writePng pictureName (generateImage (\x y -> fun (scale4 x y)) pictureWidth pictureHeight)

--I mean it kind of has to be -1 0 1 because that's what format the bytestring needs to be saved in
--scale4 takes the int coordinates and scales them into the float coordinates
scale4 :: Int -> Int -> Coordinate
--scale4 x y = Coordinate ((right-left)*(fromIntegral x)/floatWidth+left) ((down-up)*(fromIntegral y)/floatHeight-down)
--here I flip the input y to go from pictureHeight-1 to 0 instead of the other way around
--I also make it go from -1 to almost 1 but not to 1
scale4 x y = Coordinate ((right-left)*(fromIntegral x)/floatWidth+left) (2*(fromIntegral ((pictureHeight-1)-y))/floatHeight+down)
fun :: Coordinate -> PixelRGB8
--fun (Coordinate x y) = PixelRGB8 z z z
fun (Coordinate x y) = hslToRgb2 (0,0,(1+z)/2)
  where
    --z = floor ((1+x)*127.5)
    --z = x*y
    z = sin(34*x)*sin(23*y)
    --z = sawtooth (magnitude zz); xx = (x :+ y);zz = (signum $ 999*xx) --cool
    --z = (magnitude zz); xx = (x :+ y);zz = sin (abs (pi*(3*(xx+1)+1)))*sin (abs (pi*(3*(xx-1)-1))) --wave interference pattern

--prerender takes a unary function and generates a list containing corresponding y values to the index
prerender :: UnaryF -> Ints
prerender f = map (
  (pictureHeight-1-) .
  floor .
  --(*128).
  ((*((floatHeight-1)/(up-down))) . (+(-down))) .
  f .
  ((+left) . (*((right-left)/floatWidth)))
  ) [0 .. floatWidth-1]

{-
prerender2 :: UnaryF -> Floats
prerender2 f = map (
  (*64).
  (+(-down)).
  --((*((floatHeight-1)/(up-down))) . (+(-down))) .
  f .
  ((+left) . (*((right-left)/floatWidth)))
  ) [0 .. floatWidth-1]
-}

på = main

{-
mainly3 = do
  writePng pictureName (generateImage (\x y -> unaryToPicture3 function (scale4 x y)) pictureWidth pictureHeight)
  where
    function =
      --blanc 13 . (*8) --gorgeous
-}

--this stands for the sparsed out multiresolution audio for the picture generation from it
thing' = (sparser (div constant2 pictureWidth) $ multiResolutionLine constant2)
thing x
  | 0 <= z && z <= pictureWidth-1 = thing' !! z
  | otherwise = 0
  where
    z = (floor $ ((x+1)/2)*floatWidth)

--perhaps a faster way to do unary to picture, using prerendering
unaryToPicture4 :: UnaryF -> Int -> Int -> PixelRGB8
unaryToPicture4 f i j
  | prerender f !! i == j = black2
  | i == div pictureWidth 2 = orange
  | j == div pictureHeight 2 = orange
  | otherwise = white2

{-
--prerenders everytime the function is called, so way too slow
mainly4 = do
  writePng pictureName (generateImage (unaryToPicture4 function) pictureWidth pictureHeight)
  where
    function =
      thing
-}

black2 = PixelRGB8 0 0 0
white2 = PixelRGB8 255 255 255

--here we prerender only once, I know not if mainly4 prerenders for each function application but here we see if there is a performance difference
--omg there is a difference
unaryMultiResolutionPicture = do
  writePng pictureName (generateImage f pictureWidth pictureHeight)
  where
    {-
    f :: Int -> Int -> PixelRGB8
    f i j
      | True = h 0 i j
      | g 1 = hslToRgb2 (0,1,1/2)
      | g 2 = hslToRgb2 (0.1,1,1/2)
      | g 4 = hslToRgb2 (0.2,1,1/2)
      | g 8 = hslToRgb2 (0.3,1,1/2)
      | g 16 = hslToRgb2 (0.4,1,1/2)
      | g 32 = hslToRgb2 (0.5,1,1/2)
      | g 64 = hslToRgb2 (0.6,1,1/2)
      | g 128 = hslToRgb2 (0.7,1,1/2)
      | g 256 = hslToRgb2 (0.8,1,1/2)
      | i == div pictureWidth 2 = orange
      | j == div pictureWidth 2 = orange
      | otherwise = white2
      where
        --this is the multires function, that div x 2 term centers it
        g 1 = is !! i == j
        g x = x*div (is !! (div x 2+x*div i x)) x == x*div j x

        h :: Int -> Int -> Int -> PixelRGB8
        h n i j
          | n == iterations = black2--white2
          | otherwise = if g (2^n) then hslToRgb2 (0.8,1,(floatiterations- fromIntegral n)/floatiterations) else h (n+1) i j
        iterations = floor $ logBase 2 floatWidth
        floatiterations = logBase 2 floatWidth
            
    is = prerender function
    -}
    f = unaryToMultiresolution function
    function =
      thing

unaryMultiResolutionPicture2 f = writePng pictureName (generateImage (unaryToMultiresolution f) pictureWidth pictureHeight)

unaryToMultiresolution :: UnaryF -> ColorFunction
unaryToMultiresolution f i j = h 0 i j
  where
    is = prerender f
    --this is the multires function, that div x 2 term centers it
    g 1 = is !! i == j
    g x = x*div (is !! (div x 2+x*div i x)) x == x*div j x
    h :: Int -> Int -> Int -> PixelRGB8
    h n i j
      | n == iterations = black2--white2
      | otherwise = if g (2^n) then hslToRgb2 (0.8,1,(floatiterations- fromIntegral n)/floatiterations) else h (n+1) i j
    iterations = floor $ logBase 2 floatWidth
    floatiterations = logBase 2 floatWidth


--MULTIRESOLUTION
--this just generates an image from an ordinary binary function, so just like the first rendering functions but without coordinates
picture = do
  writePng pictureName (generateImage f constant2 constant2)
    where
      f i j = hslToRgb2 (0,0,construct constant2 !! i !! j)
      --f i j = hslToRgb2 (0.8,1,construct constant2 !! i !! j)
      --f i j = hslToRgb3 (0.8,1,construct constant2 !! i !! j)

      --this is just making brightness and saturation the same, which is not actually what we did in the javascript version, we generated three independent numbers for hsl
      hslToRgb3 (x,y,z) = hslToRgb2 (x,z,z)

--unarytopicture but the unary function divides the picture into two regions, red and blue, up and down, 
unarySplitPicture = do
  writePng pictureName (generateImage f pictureWidth pictureHeight)
  where
    f = unaryToSplit function
    function =
      --thing
      sawtooth . bifurcation 9 . (*2) . (+1)

unarySplitPicture2 f = writePng pictureName (generateImage (unaryToSplit f) pictureWidth pictureHeight)

--takes a unary function and returns a function that splits the Int plane according to it
unaryToSplit :: UnaryF -> ColorFunction
unaryToSplit f = function
  where
    l = prerender f
    function i j
      | l !! i == j = hslToRgb2 (0.25,1,1/2)
      | i == div pictureWidth 2 || j == div pictureHeight 2 = hslToRgb2 (0.75,1,1/2) --axes
      | l !! i < j = hslToRgb2 (0,1,limiter2 1 0 $ (fromIntegral $ l !! i)/floatHeight)
      | otherwise = hslToRgb2 (0.5,1,limiter2 1 0 $ 1-(fromIntegral $ l !! i)/floatHeight)

--this just takes a unary function and simply plots it
unaryToPicture5 :: UnaryF -> ColorFunction
unaryToPicture5 f = function
  where
    l = prerender f
    function i j
      | l !! i == j = black2
      | i == div pictureWidth 2 || j == div pictureHeight 2 = orange --axes
      | otherwise = white2

--
mainly8 = do
  writePng pictureName (generateImage function pictureWidth pictureHeight)
  where
    --function i j = hslToRgb2 (0,1,cos x) where Coordinate x y = scale4 i j
    --function i j = hslToRgb2 (halfsawtooth z,1,halfsawtooth w) where Coordinate x y = scale4 i j; (z :+ w) = newton2 cosh sinh 7 (zoom*x :+ zoom*y); zoom = 1/2
    --function i j = hslToRgb2 ((sigmoid z+1)/2,1,1/2) where Coordinate x y = scale4 i j; (z :+ w) = newton2 cosh sinh 7 (zoom*x :+ zoom*y); zoom = 1/2
    function i j
      | w == newtoniterations2 = hslToRgb2 (z/newtoniterations2,1,1/2)
--      | w == newtoniterations2 = hslToRgb2 ((z+1)/2,1,1/2)
      | otherwise = hslToRgb2 (0.8,0,(sigmoid w/2+1)/2)
--      | otherwise = hslToRgb2 (0.8,1,(halfsawtooth (9*z)/2+1)/2)
--      where Coordinate x y = scale4 i j; (z :+ w) = newton2 cosh sinh newtoniterations (zoom*x :+ zoom*y); zoom = 1/2
      where Coordinate x y = scale4 i j; (z :+ w) = newton2 cosh sinh newtoniterations ((x :+ y)*zoom); zoom = 1/2
      --where Coordinate x y = scale4 i j; (z :+ w) = newton2 (\a -> cosh a-1) sinh newtoniterations (zoom*x :+ zoom*y); zoom = 1/2
      --where Coordinate x y = scale4 i j; (z :+ w) = newton2 (\a -> sin a-1) cos newtoniterations (zoom*x :+ zoom*y); zoom = 1/2

--const2 x y z = x
--const3 x y z w = x














