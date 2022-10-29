module AuralCalculator where

--import GraphingCalculator

import qualified Data.ByteString.Lazy as B --from tsoding, https://github.com/tsoding/haskell-music
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process

--import Data.Fixed --maybe I don't need this because it makes sense to have that information buffer that floats have in order to make more precise computations
import Debug.Trace

import System.Random
import System.Random.Stateful
--import System.Random.Shuffle
import Control.Monad
import Data.Complex
import Data.List

--import Codec.Audio.FLAC.Metadata
--import Codec.Audio.FLAC.StreamEncoder

--import Control.Concurrent.Timer

--volume = 0.03
volume = 1.0

--I should make a volume safeguard, a limiter
--it takes a list and limits it
limiterSafeguard :: Floats -> Floats
limiterSafeguard = map limiterSafeguardFunction

limiterSafeguardFunction :: UnaryF
limiterSafeguardFunction x
  | x > volume = volume
  | x < -volume = -volume
--  | x /= x = 0  --make sure there are no NaNs in there, it appears that NaNs make the audio loud
  | isNaN x = 0
  | otherwise = x

--step = 0.05
step = 0.02


frequency = 48000.0 :: Float



functionToAudioData :: UnaryF -> Floats
functionToAudioData f = (map ((volume*) . f . (step*)) [0.0 .. frequency*seconds])

functionToAudioData2 :: Float -> UnaryF -> Floats
functionToAudioData2 s f = (map ((volume*) . f . (step*)) [0.0 .. frequency*s])

unsafeAudioData :: Floats
--we expect the sound function to be bounded between -1 and 1
unsafeAudioData = functionToAudioData soundFunction
--unsafeAudioData = auralMultiresolution
seconds :: Float
--seconds = 1.0
--seconds = 8.0
--seconds = 0.68266666
seconds = 32/6 --32 beats and period is divided by 6 yields 32/6
period = frequency*step

safeAudioData = limiterSafeguard unsafeAudioData

save :: IO ()
save = B.writeFile "haskellsound.bin" $ B.toLazyByteString $ fold $ map B.floatLE safeAudioData

--floatLE means little endian. so they are floating point numbers in little endian

--ffplay -volume 11 -loop 9 -f f32le -ar 48000 /home/bruh/Code/I/haskle/aural_calculator/haskellsound.bin & in console in the folder produces that sound

{-
these produce audio files

ffmpeg -f f32le -vol 28 -ar 48000 -i haskellsound.bin -f flac haskellsound.flac


ffmpeg -f f32le -vol 28 -ar 48000 -i multiresolution.bin -f wav multiresolution.wav


ffmpeg -f f32le -vol 28 -ar 48000 -i multiresolution.bin -f mp3 multiresolution.mp3
-}
--mpv --loop ~/k/haskle/aural_calculator/haskellsound.flac --this plays sound in a loop

play :: IO ProcessHandle
play = do
  runCommand "ffplay -volume 11 -loop 1000 -f f32le -ar 48000 /home/bruh/Code/I/haskle/aural_calculator/haskellsound.bin"
--wmctrl -r "/home/bruh/Code/I/haskle/aural_calculator/sound.bin" -e 1,20,50,1880,600
saveAndPlay = do
    save
    play

saveandplay = saveAndPlay

sap = saveAndPlay

--w in the ffplay prompt toggles the visual actual wave in an oscilloscope

--a command line thing that asks a UnaryF function and a time in Floats
playFunction :: Float -> UnaryF -> IO ProcessHandle
playFunction s f = do
  B.writeFile "functionsound.bin" $ B.toLazyByteString $ fold $ map B.floatLE $ limiterSafeguard $ functionToAudioData2 s f
  runCommand "ffplay -volume 11 -loop 1000 -f f32le -ar 48000 /home/bruh/Code/I/haskle/aural_calculator/functionsound.bin"

pf = playFunction

playSavedFunction = runCommand "ffplay -volume 11 -loop 1000 -f f32le -ar 48000 /home/bruh/Code/I/haskle/aural_calculator/functionsound.bin"

playSavedFunctionOnce = runCommand "ffplay -volume 11 -f f32le -ar 48000 /home/bruh/Code/I/haskle/aural_calculator/functionsound.bin"

--play once
po s f = do
  B.writeFile "functionsound.bin" $ B.toLazyByteString $ fold $ map B.floatLE $ limiterSafeguard $ functionToAudioData2 s f
  runCommand "ffplay -volume 11 -f f32le -ar 48000 /home/bruh/Code/I/haskle/aural_calculator/functionsound.bin"

saveFunction s f = do B.writeFile "functionsound.bin" $ B.toLazyByteString $ fold $ map B.floatLE $ functionToAudioData2 s f

playList xs = do
  B.writeFile "functionsound.bin" $ B.toLazyByteString $ fold $ map B.floatLE $ limiterSafeguard $ xs
  runCommand "ffplay -volume 11 -loop 1000 -f f32le -ar 48000 /home/bruh/Code/I/haskle/aural_calculator/functionsound.bin"

playListOnce xs = do
  B.writeFile "functionsound.bin" $ B.toLazyByteString $ fold $ map B.floatLE $ limiterSafeguard $ xs
  runCommand "ffplay -volume 11 -f f32le -ar 48000 /home/bruh/Code/I/haskle/aural_calculator/functionsound.bin"




textWidth :: Int
textHeight :: Int
textWidth = 64
textHeight = 64

floatTextWidth = fromIntegral textWidth :: Float
floatTextHeight = fromIntegral textHeight :: Float

textLeft :: Int
textRight :: Int
textUp :: Int
textDown :: Int
(textLeft, textRight, textUp, textDown) = (-1,1,1,-1)
--(textLeft, textRight, textUp, textDown) = (0,floor $ step*frequency*seconds,1,-1)

textHorizontalZoom = 1.0
textVerticalZoom = 1.0


visualize = do
  -- -1 mapped to zero and 1 mapped to seconds*frequency
  putStr $ unaryToText $ soundFunction . (*(step*seconds*frequency/2)) . (+1)
  putStrLn "--------------------------------------------------------------------------------------------------------------------------------"
  putStr $ listToText $ map (/volume) unsafeAudioData
  putStrLn "--------------------------------------------------------------------------------------------------------------------------------"
  putStr $ binaryToText func



func x y = sawtooth $ (*9) $ realPart $ log $ (9*x:+9*y)

--
binaryToText :: BinaryF -> String
binaryToText = binaryToText' floatTextWidth floatTextHeight horizontalStart verticalStart
  where
    horizontalStart = fromIntegral textLeft/horizontalRatio
    horizontalEnd = fromIntegral textRight/horizontalRatio-1
    verticalStart = fromIntegral textUp/verticalRatio-1
    verticalEnd = fromIntegral textDown/verticalRatio
    inputWidth = fromIntegral $ textRight-textLeft
    inputHeight = fromIntegral $ textUp-textDown
    horizontalRatio = inputWidth/floatTextWidth
    verticalRatio = inputHeight/floatTextHeight
    --w and h are the width and height and i and j count the columns and rows
    --when we reach the end of the row, we go to a new line
    binaryToText' w h column row f
      | column == horizontalEnd+1 = '\n':binaryToText' w h horizontalStart (row-1) f
      | row == verticalEnd-1 = ""
      | otherwise = (floatToText $ f (column*horizontalRatio) (row*verticalRatio)) ++binaryToText' w h (column+1) row f

--need a function that returns "_" for values less than 0 and "#" for values at least 0
floatToText x
  | x < -3/5 = "  "
  | x < -1/5 = "░░"
  | x < 1/5 = "▒▒"
  | x < 3/5 = "▓▓"
  | otherwise = "██"

--
prerenderText :: UnaryF -> Ints
prerenderText f = map (
  floor .
  ((*((fromIntegral textHeight-1)/(fromIntegral $ textUp-textDown))) . (+(fromIntegral textUp))) .
  f .
  ((+fromIntegral textLeft) . (*((fromIntegral $ textRight-textLeft)/fromIntegral textWidth)))
  ) [0 .. fromIntegral textWidth-1]

unaryToText :: UnaryF -> String
unaryToText f = unaryToText' textWidth textHeight 0 (textHeight-1)
  where
    ints = prerenderText f
    unaryToText' w h column row
      | column == textWidth = '\n':unaryToText' w h 0 (row-1)
      | row == -1 = ""
      | ints !! column == row = "██"++unaryToText' w h (column+1) row
      | otherwise = "  "++unaryToText' w h (column+1) row

--sometimes differs by like one to unaryToText, probably because it is sampled in a different way than unaryToText, less evenly, like if there is a set [1,2,3] and we output two values, unaryToText will output [1,2] whereas listToText might output [1,3]
listToText :: Floats -> String
listToText l = listToText' textWidth textHeight 0 (textHeight-1)
  where
    ints = sparser (div (length l) textWidth) (map (floor . (*(fromIntegral textHeight/2)) . (+1)) l)
    listToText' w h column row
      | column == textWidth = '\n':listToText' w h 0 (row-1)
      | row == -1 = ""
      | ints !! column == row = "██"++listToText' w h (column+1) row
      | otherwise = "  "++listToText' w h (column+1) row



s = save
p = play
a = sap
v = visualize

å = main
main = do
  putStrLn ""

------------------------here starts the function

soundFunction :: UnaryF
--soundFunction = ((.- 1) . (*2)) . cantor 30 . halfsawtooth . (/1)
--soundFunction = sawtooth . (*2)
--soundFunction x = sawtooth $ imagPart $ cosh (sawtooth (9*x/pi) :+ sawtooth (9*x/2))
--soundFunction x = sawtooth $ imagPart $ cosh (cos (x*x/2) :+ tanh (x*x/2))
--soundFunction = sawtooth . bifurcation 100 . (*4) . halfsawtooth . (/10)
--soundFunction x = sustain 0.1 x $ triangle (x* x*(triangle $ x*(triangle $ x*99)*99)*99)
soundFunction x = sustain 0.06 x $ triangle (x* x*(sawtooth $ x)*99)
--soundFunction x = sustain 0.5 x $ sawtooth . (*99) $ phase $ newton2 cosh sinh 9 ((sin x) :+ 0)


{-
soundFunction x
  | x < 100 = 0
  | x < 200 = 0.99
  | otherwise = -1
-}

{-
funny :: UnaryF
--funny x = sin (x+cos (2*x+sin (3*x+cos (4*x))))
--funny x = sin (x+2*sin (x+3*sin (x+4*sin x)))
--funny x = sin (x+2*sin (x+4*sin (x+8*sin x)))
--funny x = (oldsawtooth (x*sin (x*1000)*1000)) *oldrhythm x [1] (period/8)
--funny x = cantor 1 (oldsawtooth ((x+1)/2))
funny x = sigmoid (3*oldsawtooth x *(oldsawtooth (exp (-2) *x))) *oldrhythm x [1,1,0,0,1,0,1,0,1,0,0,1,0,0,1,0] (period/8)
funny x = sigmoid (3*oldsawtooth x *(oldsawtooth (exp (-2) *x))) *halfsquare(x*pi/300)
funny x = sigmoid (3*oldsawtooth x *(oldsawtooth (exp (-2) *x)))
funny x = oldsawtooth (exp (sin (x)))*(oldsawtooth (x/4))
funny x = oldsawtooth (7*exp (sin x))
funny x = sigmoid (4*oldsawtooth (x/4))
-}

--I do have to make a library of functions for that, or use one
--I could use text graphics, just normalize the graphics. yeah, that word should be used in more places, "normalization"

square :: UnaryF
square x = 2*floorf (2*x) - 4*floorf x - 1

--gives 1,0,1,0,...
halfsquare :: UnaryF
halfsquare x = floorf (2*x) -2*floorf x

oldsawtooth x = 2*(x-floorf x)-1 --this is the old implementation which maps 0 to -1 and 1 to -1

--0 to 0 and 1 to -1 and 2 to 0
sawtooth :: UnaryF
sawtooth x = (x-2*floorf ((x+1)/2))

--0 to 0 and 1 to 0 and 0.5 to 0.5 and 1.5 to 1.5
halfsawtooth :: UnaryF
halfsawtooth x = x-floorf x

--0 to -1 and 1 to 0 and 2 to -1, so offset sawtooth
sawtooth2 x = x-2*floorf(x/2)-1

--0 is -1 and 1 is 0 and 2 is 1
triangle :: UnaryF
triangle x = 1 - 2* abs (sawtooth2 $ x/2)

triangle2 x = 1 - 2* abs (sawtooth x)

--0 is 0 and 1 is 1 and 2 is 0
halftriangle :: UnaryF
halftriangle x = 1 - abs (sawtooth2 $ x)

--floor for floats
floorf :: RealFrac a => a -> a
floorf = fromIntegral . floor

ceil :: (RealFrac a, Integral b) => a -> b
ceil = ceiling

roundf :: RealFrac a => a -> a
roundf = fromIntegral . round

limiter :: Float -> Float -> Float
limiter l x
  | x > l = l
  | x < -l = -l
  | otherwise = x

clipper = limiter

normal = normallimit

normallimit x = limiter 1.0 x

limiter2 a b x
  | x > a = a
  | x < b = b
  | otherwise = x

--limiter stretched horizontally
limiter3 x y = normallimit (x/y)

parabola x = sawtooth (x^2)

(.-) x y = x-y
(-.) x = -x


--I think there are problems with floating. the program keeps so many decimal points and when we convert it to float instead of double, it can't somehow convert it to zero. so we have to explicitly make everything a float everywhere. but at some point it goes to infinity, so we're gonna have to assign it to the max float. I could make my own datatype where infinity is just the max value. or maybe that exists already. or maybe I could make my own arithmetic operations for infinity. however at that point all my functions that I make would have to handle infinity
--but wouldn't they? if I combine functions using my arithmetic operators, there will be no infinite, and if I define functions from the ground up, I can't end up with infinity unless I explicitly define some output as infinite, so new arithmetic should work

--I also need it not to NaN, so NaN just becomes zero. now that the primitive operations return no NaN, need I even check for NaNs at the output? idk, tangent might return some NaNs or infinities. does the output even check for infinities? I mean it checks within the bounds, so yes

--source: the internet
maxNonInfiniteFloat :: Float
maxNonInfiniteFloat = encodeFloat m n where
  b = floatRadix (0 :: Float)
  e = floatDigits (0 :: Float)
  (_, e') = floatRange (0 :: Float)
  m = b ^ e - 1
  n = e' - e

floatOperation :: Num b => (Float -> b -> Float) -> (Float -> b -> Float)
floatOperation x y z
  | isNaN operation = 0
  | not $ isInfinite operation = operation
  | operation > 0 = maxNonInfiniteFloat
  | otherwise = -maxNonInfiniteFloat
  where
    operation = x y z

type UnaryF = Float -> Float

type BinaryF = Float -> Float -> Float

--float addition without infinity
addFloat :: BinaryF
addFloat = floatOperation (+)

subtractFloat :: BinaryF
subtractFloat = floatOperation (-)

multiplyFloat :: BinaryF
multiplyFloat = floatOperation (*)

divideFloat :: BinaryF
divideFloat = floatOperation (/)

exponentiateFloat :: BinaryF
exponentiateFloat = floatOperation (**)

exponentiateFloat2 :: Integral a => Float -> a -> Float
exponentiateFloat2 = floatOperation (^^)

-- !#$%&*+./<=>?@\^|-~
(+§) = addFloat
(-§) = subtractFloat
(*§) = multiplyFloat
(/§) = divideFloat
(**§) = exponentiateFloat
(^§) = exponentiateFloat2
infixl 6 +§,-§
infixl 7 *§,/§
infixr 8 **§, ^§

type ComplexFloat = Complex Float
type Complex0 = Complex Float
type ComplexF = Complex Float

type Complex1 = ComplexFloat -> ComplexFloat
type Complex2 = ComplexFloat -> ComplexFloat -> ComplexFloat

type UnaryC = ComplexF -> ComplexF
type BinaryC = ComplexF -> ComplexF -> ComplexF

complexOperation :: Num a => (Complex0 -> a -> Complex0) -> (Complex0 -> a -> Complex0)
complexOperation x y z
  | isNaN w || isNaN v = 0
  | otherwise = (ww :+ vv)
  where
    (w :+ v) = x y z
    ww = if isInfinite w then if w > 0 then maxNonInfiniteFloat else -maxNonInfiniteFloat else w
    vv = if isInfinite v then if v > 0 then maxNonInfiniteFloat else -maxNonInfiniteFloat else v

addComplex :: Complex2
addComplex = complexOperation (+)

subtractComplex :: Complex2
subtractComplex = complexOperation (-)

multiplyComplex :: Complex2
multiplyComplex = complexOperation (*)

divideComplex :: Complex2
divideComplex = complexOperation (/)

exponentiateComplex :: Complex2
exponentiateComplex = complexOperation (**)

exponentiateComplex2 :: Integral a => Complex0 -> a -> Complex0
exponentiateComplex2 = complexOperation (^^)

(§+) = addComplex
(§-) = subtractComplex
(§*) = multiplyComplex
(§/) = divideComplex
(§**) = exponentiateComplex
(§^) = exponentiateComplex2
infixl 6 §+,§-
infixl 7 §*,§/
infixr 8 §**, §^


summation :: (Num a, Num b, Ord a) => a -> a -> (a -> b) -> b
summation beginning end f
  | beginning <= end-1 = f beginning + summation (beginning+1) end f
  | otherwise = 0

--summation from n to zero
toZero :: (Num a, Num b, Eq a) => a -> (a -> b) -> b
toZero 0 f = f 0
toZero n f = f n + toZero (n-1) f

toOne 1 f = f 1
toOne n f = f n + toOne (n-1) f

weierstrass :: Int -> Float -> Float
weierstrass i x = weierstrass' x i a b /§ normalization
  where
    --b needs to be an odd number and a between 0 and 1
    a = 0.7 :: Float
    b = 15 :: Float
    normalization = 1/(1-a)
    --a*b > 5.71238898038469
--I need to normalize this
--what is the sum a^i?
--(1-a^i)/(1-a)
--since a^i converges to zero, the whole term converges to 1/(1-a), then we just need to divide by that
weierstrass' x y a b = case y of 0 -> cos (pi*§x)
                                 i -> a^§i *§ cos (b^§i *§ pi*§x) +§ weierstrass' x (i-1) a b
--weierstrass'' x i a b = a^§i *§ cos (b^§i *§ pi*§x)

--blancmange
--goes from 0 to like 1.4
blanc :: Int -> Float -> Float
blanc i x = toZero i (\y -> halftriangle (x *§ 2 ^§ y) /§ (w ^§ y))
  where
    w = 2
{-blanc 0 x = triangle x /§ 2
blanc i x = triangle (w^§i *§ x) /§ 2 ^§ (i+1) +§ blanc (i-1) x
  where
    w = 2/3-}
--what is the maximum height of this function? I need to normalize this
--well it is bounded by the series 1+1/2+1/4+...1/2^i which is approximately 2, so I divided all by two

--also takes a w parameter, and it's normalized, but only for positive w
blanc2 :: Int -> Float -> Float -> Float
blanc2 i w x = (.-1) . (*2) . (/((1-w^(i+1))/(1-w))) $ toZero i (\y -> halftriangle (x *§ 2 ^§ y) *§ (w ^§ y))

clausen :: Int -> Float -> Float
clausen i x = (/(pi^2/6)) $ toOne i (\y -> sin(fromIntegral y*x)/fromIntegral y^2)

type Ints = [Int]

sigmoid :: UnaryF
sigmoid x = 1-(2/(1+exp (2*x)))

--from 0 to 1
cantor :: Int -> Float -> Float
cantor 0 x = x
cantor n x
  | x <= 1/3 = (cantor (n-1) (3*x))/2
  | 1/3 <= x && x <= 2/3 = 1/2
  | otherwise = (1+cantor (n-1) (3*x-2))/2

birhythm :: Float -> Ints -> Float -> Float
birhythm x [0] d = 0
birhythm x [1] d
  | x <= d = 1
  | otherwise = 0
birhythm x (0:z:zs) d
  | x <= d = 0
  | otherwise = birhythm (x-d) (z:zs) d
birhythm x (1:z:zs) d
  | x <= d = 1
  | otherwise = birhythm (x-d) (z:zs) d

trirhythm :: Float -> Ints -> Float -> Float
trirhythm x [0] d = 0
trirhythm x [1] d
  | x <= d = bassdrum4 x
  | otherwise = 0
trirhythm x [2] d
  | x <= d = oldsawtooth (x*sin (x*exp 6 *triangle (x*6*pi))/20) *envelope (oldsawtooth (x/d))
  | otherwise = 0
trirhythm x (y:z:zs) d
  | x <= d = trirhythm x [y] d
  | otherwise = trirhythm (x-d) (z:zs) d

quadrhythm :: Float -> Ints -> Float -> Float
quadrhythm x [y] d =
  [
    0,
    bassdrum4 x,
    snare2 x,
    hat2 x
  ] !! y
quadrhythm x (y:z:zs) d
  | x <= d = quadrhythm x [y] d
  | otherwise = quadrhythm (x-d) (z:zs) d

drumFunctions d x = [
    0,  --0
    bassdrum4 x,  --1
    snare2 x, --2
    hat2 x, --3
    hat2 $ x/2, --4
    snary x,  --5
    snare9 x, --6
    hat (x*6) * (plucky $ repeatSound 1 id $ x),  --7
    snare13 x,  --8
    snare7 x, --9
    snare5 x, --10
    (sigmoid) . (*3) $ snare4 x, --11
    sawtooth $ 11*snare11 x,  --12
    bassdrum3 x,  --13
    sustain 0.02 x $ weird2 x,  --14
    sustain 0.02 x $ weird3 x,  --15
    sustain 0.02 x $ weird4 x,  --16
    sustain 0.03 x $ bellSound x, --17
    sustain 0.04 x $ weirdAlienSound x, --18
    snare15 x, --19
    belly x, --20
    sigmoid $ (/2) $ snare18 $ x/1.05, --21
    snare19 x, --22
    erans19 x, --23
    bassdrum5 x, --24
    repeatSound (d/period/4) snare19 x, --25
    repeatSound (d/period/4) (limiterSafeguardFunction . snare11) x, --26
    repeatSound (d/period/4) (sigmoid . (/2) . snare18 . (/1.05)) x, --27
    (backward 100 $ sigmoid . (/2) . snare18 . (/1.05)) x, --28
    repeatSound (d/period/4) (limiterSafeguardFunction . snare1) x/2, --29
    repeatSound (d/period/4) (limiterSafeguardFunction . snare14) x/2, --30
    downscale 3 bassdrum4 x, --31
    --(/2) . bassdrum4 . (+15) . scaledsaw (1/15) $ x, --32
    limiterSafeguardFunction . (/2) . bassdrum2 . expsaw $ x+(20), --32
    (sigmoid . snare18 . (/1.05)) x, --33
    (/1.2) $ snare2 x, --34
    (/1.2) $ hat2 x, --35
    (/1.2) $ hat2 $ x/2, --36
    (/1.4) $ marble x, --37
  error "heck"]

drumFunctionsLength = length $ drumFunctions 0 0

multirhythm :: Ints -> Float -> Float -> Float
multirhythm [y] d x =
  drumFunctions d x !! y
multirhythm (y:z:zs) d x
  | x <= d = multirhythm [y] d x
  | otherwise = multirhythm (z:zs) d (x-d)

--can repeat sounds
multirhythm2 [] d x = x
multirhythm2 ((y,r):yrs) d x
  | x <= d = (repeatSound (d/period/r) $ multirhythm [y] d) x
  | otherwise = multirhythm2 yrs d (x-d)

multirhythm3 xs d = foldr (++) [] $ map (samples !!) xs
  where
    samples = map ((($ input) . map) . ($ d) . multirhythm) (map (:[]) [0 ..])
    --samples becomes a list of functions. I need to apply those functions to the x lists
    input = map (*step) [1.0 .. frequency*(d/period)]

melody5 xs d = foldr (++) [] $ map (samples !!) xs
  where
    samples = map ((($ input) . map) . ($ d) . melody6) (map (:[]) [0..])
    --samples becomes a list of functions. I need to apply those functions to the x lists
    input = map (*step) [1.0 .. frequency*(d/period)]

melody7 m xs d = foldr (++) [] $ map ((samples !!) . (+100)) xs
  where
    samples = map ((($ input) . map) . ($ d) . m) (map (:[]) [-100..])
    --samples becomes a list of functions. I need to apply those functions to the x lists
    input = map (*step) [1.0 .. frequency*(d/period)]

--randomly adds 12 to a number in a list
randomOctave2 seed xs = zipWith adder probabilityList xs
  where
    probabilityList = randomFloat seed (length xs)
    adder x 69 = 69
    adder x y
      | x < 1/2 = y
      | otherwise = y+12

randomOctave = randomOctave2 1

melody1 :: Floats -> Float -> Float -> Float
melody1 [69] d x = 0
melody1 [y] d x = sustain 0.12 x $ sin (2*x*(2**(y/12)))
melody1 (y:z:zs) d x
  | x <= d = melody1 [y] d x
  | otherwise = melody1 (z:zs) d (x-d)

melody2 :: Floats -> Float -> Float -> Float
melody2 [69] d x = 0
melody2 [y] d x = sustain 0.08 x $ (/2) . square . (/(2*pi)) $ (4*x*(2**(y/12)))
melody2 (y:z:zs) d x
  | x <= d = melody2 [y] d x
  | otherwise = melody2 (z:zs) d (x-d)

melody3 :: Floats -> Float -> Float -> Float
melody3 [69] d x = 0
melody3 [y] d x = sustain 0.1 x $ sawtooth . (/(4*pi)) $ (2*x*(2**(y/12)))
melody3 (y:z:zs) d x
  | x <= d = melody3 [y] d x
  | otherwise = melody3 (z:zs) d (x-d)

melody4 :: Floats -> Float -> Float -> Float
melody4 [69] d x = 0
melody4 [y] d x = sustain 0.01 x $ sawtooth . (/(4*pi)) $ (4*x*(2**(y/12)))
melody4 (y:z:zs) d x
  | x <= d = melody4 [y] d x
  | otherwise = melody4 (z:zs) d (x-d)

melody6 :: Floats -> Float -> Float -> Float
melody6 [69] d x = 0
melody6 [y] d x = sin (0.702*x*(2**(y/12))) * (exp $ -x/80)
melody6 (y:z:zs) d x
  | x <= d = melody6 [y] d x
  | otherwise = melody6 (z:zs) d (x-d)

melody8 :: Floats -> Float -> Float -> Float
melody8 [69] d x = 0
--melody8 [y] d x = (square (0.7*(x-20*exp (-x/5))*(2**(y/12))/pi/2) * (exp $ -(x-60)^2/20000)) /1.8
melody8 [y] d x = (bass (0.7*(x-10*exp (-x/2))*(2**(y/12))/pi/2) * (exp $ -(x-60)^2/7000)) /1.8
melody8 (y:z:zs) d x
  | x <= d = melody8 [y] d x
  | otherwise = melody8 (z:zs) d (x-d)

--bass x = sigmoid $ 1/2 * (4*sin (x*pi*4) + square x)
--bass x = sigmoid $ 1 * (square (x*2) + 4*square x)
bass x = sigmoid $ 1 * (square (x) + 4*square (x/2))

melody :: (Float -> Float -> Float) -> Floats -> Float -> Float -> Float
melody f [69] d x = 0
melody f [y] d x = f y x
melody f (y:z:zs) d x
  | x <= d = melody f [y] d x
  | otherwise = melody f (z:zs) d (x-d)

flute y x = triangle (8*0.7*x*(2**(y/12))/pi) * sigmoid (x/100)
flute2 y x = triangle (8*0.7*x*(2**(y/12))/pi) * (exp $ -x/80) /1.4
harp y x = sin (0.702*x*(2**(y/12))) * (exp $ -x/80)
harp2 y x = sin (0.702*x*(2**((y+0.4)/12))) * (exp $ -x/80)
harp3 y x = sin (2*0.700*x*(2**(y/12))) * (exp $ -x/80)

repeatSound :: Float -> UnaryF -> UnaryF
repeatSound s f x = f $ y*sawtooth (x/y)
  where y = step*frequency*s

rhythmnotation ['\n'] = []
rhythmnotation (x:xs) = despace x:[]

despace x = 2

matrixrhythm (x:xs) = 2


pentatonic :: Int -> Int
pentatonic x = case x of
  0 -> 0
  1 -> 2
  2 -> 5
  3 -> 7
  4 -> 9
  69 -> 69
  y -> (pentatonic $ mod y 5) + 12*(div y 5)

major :: Int -> Int
major x = case x of
  0 -> 0
  1 -> 2
  2 -> 4
  3 -> 5
  4 -> 7
  5 -> 9
  6 -> 11
  69 -> 69
  y -> (major $ mod y 7) + 12*(div y 7)

minor :: Int -> Int
minor x = case x of
  0 -> 0
  1 -> 2
  2 -> 3
  3 -> 5
  4 -> 7
  5 -> 8
  6 -> 10
  69 -> 69
  y -> (minor $ mod y 7) + 12*(div y 7)

data Note = M |
            C | D | E | F | G | A | B |
            C1 | D1 | E1 | F1 | G1 | A1 | B1 |
            C2 | D2 | E2 | F2 | G2 | A2 | B2 |
            C3 | D3 | E3 | F3 | G3 | A3 | B3 |
            C0 | D0 | E0 | F0 | G0 | A0 | B0 |
            CS | DS | FS | GS | AS |
            CS1 | DS1 | FS1 | GS1 | AS1 |
            CS2 | DS2 | FS2 | GS2 | AS2 |
            CS0 | DS0 | FS0 | GS0 | AS0
            deriving (Show, Eq)

{--assignNote x = case x of
  M -> 69;C -> 1;CS -> 2;D -> 3;DS -> 4;E -> 5;F -> 6;FS -> 7;G -> 8;GS -> 9;A -> 10;AS -> 11;B -> 12;C0 -> -11;CS0 -> -10;D0 -> -9;DS0 -> -8;E0 -> -7;F0 -> -6;FS0 -> -5;G0 -> -4;GS0 -> -3;A0 -> -2;AS0 -> -1;B0 -> 0;C1 -> 13;CS1 -> 14;D1 -> 15;DS1 -> 16;E1 -> 17;F1 -> 18;FS1 -> 19;G1 -> 20;GS1 -> 21;A1 -> 22;AS1 -> 23;B1 -> 24;C2 -> 25;CS2 -> 26;D2 -> 27;DS2 -> 28;E2 -> 29;F2 -> 30;FS2 -> 31;G2 -> 32;GS2 -> 33;A2 -> 34;AS2 -> 35;B2 -> 36--}

noteAssignments = zip (M:[C0,CS0,D0,DS0,E0,F0,FS0,G0,GS0,A0,AS0,B0, C,CS,D,DS,E,F,FS,G,GS,A,AS,B, C1,CS1,D1,DS1,E1,F1,FS1,G1,GS1,A1,AS1,B1, C2,CS2,D2,DS2,E2,F2,FS2,G2,GS2,A2,AS2,B2]) (69:[(-12)..(12*3-1)])

noteAssignments2 = unzip noteAssignments
notes = fst noteAssignments2
noteNumbers = snd noteAssignments2

assignNote i = noteNumbers !! (index i notes)

assignNotes :: [Note] -> Ints
assignNotes = map assignNote

assignToNote i = notes !! (index i noteNumbers)

incrementNote M _ = M
incrementNote x i = assignToNote $ assignNote x + i

index x xs = index' x xs 0
index' _ [] _ = error "no match"
index' x (y:ys) i
  | x == y = i
  | otherwise = index' x ys (i+1)

--for filtering high and low
bandwidth f f' x = let y = f x in sin (40*x+40*(2**(y/12))/y)-- /(1+((f' x-40)/200)^2)



--rename this thing, maybe transitionlists
combinelists n xs ys = iteratelist xs2 ys2 randomlist probabilitylist
  where
    listlength = length xs
    listlength2 = n*listlength
    randomlist = generator2 1 listlength2
    probabilitylist = generator3 1 listlength2 --chooses between random list and the other ones
    probabilitylist2 = generator4 listlength2 1 listlength2 --chooses between the two non random lists
    xs2 = take listlength2 $ cycle xs
    ys2 = take listlength2 $ cycle ys
    
    chooser x y z 1 i = z
    chooser x y z p i
      | fromIntegral (probabilitylist2 !! i) > i = x
      | otherwise = y
    
    iteratelist x y z w = iteratelist' 0 x y z w
    iteratelist' i [] [] [] [] = []
    iteratelist' i (a:as) (b:bs) (c:cs) (d:ds) = (chooser a b c d i):iteratelist' (i+1) as bs cs ds

--fixing the seed
combinelists3 = combinelists4 1

combinelists4 x y = combinelists6 x y (1/2)

--actually it seems here that we are using the same random list for choosing whether a random drum plays and which one of the two normal ones play.
combinelists6 seed randomProbability normalProbability n xs ys = iteratelist xs2 ys2 randomDrums randomFloats1
  where
    listLength = length xs
    listLength2 = n*listLength
    randomDrums = generator5 33 seed listLength2
    randomFloats1 = randomFloat seed listLength2 --chooses between random drum and the other ones
    --randomFloats2 = randomFloat 2 listLength2 --chooses between normal drums
    xs2 = take listLength2 $ cycle xs
    ys2 = take listLength2 $ cycle ys
    
    --randomProbability = 1/16--1/4 --probability that it's one of the random drums

    normalChoice x y p i = if p > (fromIntegral i/fromIntegral listLength2)*normalProbability then x else y

    --randomChoice (l:ls) duration p i = (fromIntegral i/fromIntegral listLength2)

    --maxRepeatExponent = 3
    --repetitions = map (2^) $ generator4 maxRepeatExponent 1 listLength2

    chooser x y z p i
      | p < randomProbability = z
      | otherwise = normalChoice x y p i
    
    iteratelist x y z w = iteratelist' 0 x y z w
    iteratelist' i [] [] [] [] = []
    iteratelist' i (a:as) (b:bs) (c:cs) (d:ds) = (chooser a b c d i):iteratelist' (i+1) as bs cs ds

combinelists5 :: Int -> Float -> Int -> Ints -> Ints -> Ints
combinelists5 seed normalProbability n xs ys = iteratelist xs2 ys2 randomFloats
  where
    listLength = length xs
    listLength2 = n*listLength
    randomFloats = randomFloat 2 listLength2 --chooses between normal drums
    xs2 = take listLength2 $ cycle xs
    ys2 = take listLength2 $ cycle ys
    
    normalChoice x y p i = if p > (fromIntegral i/fromIntegral listLength2)*normalProbability then x else y

    chooser x y p i = normalChoice x y p i
    
    iteratelist x y p = iteratelist' 0 x y p
    iteratelist' i [] [] [] = []
    iteratelist' i (a:as) (b:bs) (p:ps) = (chooser a b p i):iteratelist' (i+1) as bs ps

combinelists2 n xs ys = iteratelist xs2 ys2 randomlist probabilitylist
  where
    listlength = length xs
    listlength2 = n*listlength
    randomlist = map (harmonize) $ generator4 5 1 listlength2
    probabilitylist = generator4 4 1 listlength2 --chooses between random list and the other ones
    probabilitylist2 = generator4 listlength2 1 listlength2 --chooses between the two non random lists
    xs2 = take listlength2 $ cycle xs
    ys2 = take listlength2 $ cycle ys
    
    harmonize 1 = 2--1
    harmonize 2 = 4--3
    harmonize 3 = 7--4
    harmonize 4 = 9--6
    harmonize 5 = 11--8
    harmonize 6 = 9
    harmonize 7 = 11
    harmonize 8 = 13

    chooser x y z 1 i = z
    chooser x y z p i
      | fromIntegral (probabilitylist2 !! i) > i = x
      | otherwise = y
    
    iteratelist x y z w = iteratelist' 0 x y z w
    iteratelist' i [] [] [] [] = []
    iteratelist' i (a:as) (b:bs) (c:cs) (d:ds) = (chooser a b c d i):iteratelist' (i+1) as bs cs ds

--cut out at the middle where both beats are intermingled equally and also it has the random beat go to double tempo over time
--but I should instead of cutting the middle just make the probability go from 0 to 3/8 and not from 0 to 6/8
drumsong n duration xs ys x0 = multirhythm (combinelists3 n xs ys) duration x0 + multirhythm (iteraterandomlist randomlist) (duration/2) x0
--drumsong n xs ys x0 = multirhythm (combinelists3 n xs ys) (period/20) x0 + multirhythm (iteraterandomlist2 randomlist2) (period/20/2) x0
  where
    combinelists3 nn xxs yys = iteratelist xs2 ys2 randomlist probabilitylist
    listlength = length xs
    listlength2 = n*listlength
    listlength3 = listlength2*2
    randomlist = map fromIntegral $ generator2 1 listlength2
    probabilitylist = randomFloat 1 listlength2 --chooses between random list and the other ones
    probabilitylist2 = generator4 listlength3 1 listlength2 --chooses between the two non random lists
    --chooses between the two random lists
    probabilitylist3 = generator4 listlength3 1 listlength2
    xs2 = take listlength2 $ cycle xs
    ys2 = take listlength2 $ cycle ys
    
    randomlistprobability = 1/4

    chooser x y z p i
      | p < randomlistprobability = if fromIntegral (probabilitylist3 !! i) > i then z else 0
      | fromIntegral (probabilitylist2 !! i) > i = x
      | otherwise = y

    iteraterandomlist l = iteraterandomlist' 0 l
    iteraterandomlist' i [] = []
    iteraterandomlist' i (l:ls) = ll:ll:iteraterandomlist' (i+1) ls
      where
        ll = (if probabilitylist !! i < randomlistprobability && fromIntegral (probabilitylist3 !! i) < i then l else 0)

    iteratelist x y z w = iteratelist' 0 x y z w
    iteratelist' i [] [] [] [] = []
    iteratelist' i (a:as) (b:bs) (c:cs) (d:ds) = (chooser a b c d i):iteratelist' (i+1) as bs cs ds

monotonicProgression :: Ints -> Ints -> Ints
monotonicProgression xs ys = changePattern xs indices
  where
    listLength = length xs
    indices = shuffledNumbers listLength 1
    --first one is xs, then xs
    --need a function that exhausts shuffledNumbers?
    --it takes the current pattern and applies one change to it according to shuffledNumbers and ys
    changePattern as [] = as
    changePattern as (n:ns) = as++changePattern as2 ns
      where
        as2 = (take n as++(ys !! n):drop (n+1) as)

--change pattern according to a list of numbers. [1,1,1,1] changes every loop by one, [0,0,0,4] changes the last loop by 4
monotonicProgression2 :: Ints -> Ints -> Ints -> Ints
monotonicProgression2 xs ys changes = changePattern xs indices changes
  where
    listLength = length xs
    indices = shuffledNumbers listLength 1
    --first one is xs, then xs
    --need a function that exhausts shuffledNumbers?
    --it takes the current pattern and applies one change to it according to shuffledNumbers and ys
    changePattern as _ [] = []
    changePattern as [] (c:cs) = as++changePattern as [] cs
    changePattern as (n:ns) (c:cs)
      | c == 0 = as++changePattern as (n:ns) cs
      | otherwise = changePattern as2 ns (c-1:cs)
      where
        as2 = (take n as++(ys !! n):drop (n+1) as)

--takes the amount that the resulting list should add up to, a seed, and the length of the resulting list
uniformIntDistribution :: Int -> Int -> Int -> Ints
uniformIntDistribution m n seed = addIndices indices startList
  where
    indices = (generator5 (n-1) seed m)
    addIndices [] xs = xs
    addIndices (i:is) xs = addIndices is $ take i xs++(1+xs !! i):(drop (i+1) xs)
    startList = (take n [0,0..])



--I should simply save functions as sounds, maybe even as lists of their values
noisy x = oldsawtooth (x*sin (x*exp 6 *triangle (x*6*pi))/20)

snare1 x = noisy x *pluck (sawtooth (x/6000))

envelope x = (1-x)^3

pluck x = (1-x)^20

oldrhythm x y d = birhythm x y d *envelope (oldsawtooth (x/d))

--l in seconds
sustain :: Float -> Float -> Float -> Float
sustain l x y
  | x < step*frequency*l = y
  | otherwise = 0

--takes a duration and a function and returns a function that lasts that long
sustain2 l y x = sustain l x $ y x

plucky x = (1-) $ sigmoid $ x/66

bellSound x = sin (7*§x) *§ sawtooth (x*§100+1) --beautiful bell sound
bellSound2 x = sin (7*§xx) *§ sawtooth (xx*§100+1) where xx = expsaw x
weirdAlienSound x = sin (x*§ sin x/§6) --weird alien sound
weirdAlienSound2 y x = sin (x*§ sin x/§(2+triangle (sin y))) --pf 11.2 $ \x -> weirdAlienSound2 x . weirdtriangle2 . (/2) $ x
snary x = sustain 0.08 x $ sawtooth . (*(99*(exp $ -x/5))) $ toZero 9 (\y -> sin $ x*y**0.5)/9
marble x = cos(x* (exp $(x+620)/100)) * (exp $ (-x)/50)
backmarble x = cos(y* (exp $(y+620)/100)) * (exp $ (-y)/200) where y = 1000-x


expfloor x = 2^^(floor $ logBase 2 x)
expsaw x = sawtooth (x/y+1) * y where y = expfloor x
exptriangle x = triangle (4*x/y) * y where y = expfloor x
expsaw2 x = (x-y)*2**y where y = floorf x
exptriangle2 x = triangle (4*x)*floorf x
exptriangle3 x = triangle (x*2**y)+y where y = floorf x
weirdtriangle x = y* triangle (x*sin y) where y = floorf x
--weirdtriangle2 x = exptriangle $ (1+boundedexptriangle (scaledtriangle x /b))*b where boundedexptriangle x = triangle $ 4*x /2^^floor(logBase 2 x); scaledtriangle x = (triangle (x/a)+1)*a; a = 2222*3; b = pi*2222/4 --222
weirdtriangle2 x = exptriangle $ (1+boundedexptriangle (scaledtriangle a x /b))*b where boundedexptriangle x = triangle $ 4*x /2^^floor(logBase 2 x); ; a = 2222; b = 222 --pf 11.2 $ weirdAlienSound . weirdtriangle2 . (/2) --pf 11.2 $ \x -> weirdAlienSound2 x . weirdtriangle2 . (/2) $ x
--weirdtriangle2 x = exptriangle $ (1+boundedexptriangle (scaledtriangle x /b))*b where boundedexptriangle x = triangle $ 4*x /2^^floor(logBase 2 x); scaledtriangle x = (triangle (x/a)+1)*a; a = 2222*2; b = 222*pi
--weirdtriangle2 x = exptriangle $ (1+boundedexptriangle((1+boundedexptriangle (scaledtriangle x /b))*b/c))*c where boundedexptriangle x = triangle $ 4*x /2^^floor(logBase 2 x); scaledtriangle x = (triangle (x/a)+1)*a; a = 22222; b = 22*pi; c = 222
scaledtriangle s x = (triangle (x/s)+1)*s
scaledtriangle2 s x = triangle (x/s)*s

weirdtriangle3 x = triangle (x*pi/3)+floorf(x/3)
weirdtriangle4 x = scaledtriangle (5*pi) x+floorf(x/13)
weirdtriangle5 x = scaledtriangle (5*pi) x+floorf(x/5*pi)/13

mountainside x = x+scaledfloor 2 x/8

--flatter
mountainside2 s = (/s) . (\x -> halfsawtooth (x+1/2)*halfsquare x + floorf x/2) . (*s)

weirdtrianglealien x =  weirdAlienSound . weirdtriangle2 . (/2) $ x
weirdtrianglealien2 x = weirdAlienSound2 x . weirdtriangle2 . (/2) $ x


iteratedtriangles 0 x = triangle x
--iteratedtriangles n x = iteratedtriangles (n-1) $ scaledtriangle2 ((1000)**(n/20)) x
iteratedtriangles n x = iteratedtriangles (n-1) $ scaledtriangle2 (exp $ n*log(1000)/20) x

floorsaw s x = (/s) . floorf . (*s) . sawtooth $ x

floorexp x = floorf $ 2^^ floor x

--infinite staircase
scaledfloor s x = (/s) . floorf . (*s) $ x

--downsamples both temporal and amplitudal
scalefloorfunction s f = scaledfloor s . f . scaledfloor (s)
--scalefloorfunction s f = scaledfloor s . f . scaledfloor (s*7)

scalefloorfunction2 s k f = scaledfloor s . f . scaledfloor k

downscale s f = scalefloorfunction s f
downscale2 s k f = scalefloorfunction2 s k f

scaledsaw s x = (/s) . sawtooth . (*s) $ x

modulation x = cos $ a*x + (sin $ b*x)*c
  where
    --w=2*p/k
    --p=2*k/(pi*k2)
    --k=28
    --k=3
    --k2=11
    --k2=3
    a = 23
    b = 0.01
    c = 200

--the spectrum shape is the derivative
modulation2 f x = cos $ a*x + (f $ b*x)*c
  where
    --w=2*p/k
    --p=2*k/(pi*k2)
    --k=28
    --k=3
    --k2=11
    --k2=3
    a = 23
    b = 0.01
    c = 200


snare15 x = (/8) $ toOne 8 $ \y -> scalefloorfunction (y*y) (hat4 $ 0.01*20/y) x
snare16 x = toOne 8 $ \y -> (/2**y) $ scalefloorfunction (y*y) (hat4 $ 0.01*8/y) x
snare17 x = (/2) $ snare16 x + snare15 x
sinthing x = toOne 8 $ \y -> (/(2**y)) $ scalefloorfunction (y*y) (\z -> sustain (0.01*20/y) z $ sin $ z*3) x  --sounds alright

snare19 x = (/2) $ belly x + snare18 x

belly x = normallimit $ (scalefloorfunction2 3 9 $ snare11) x

backward s f = f . (s-)

erans19 x = backward 100 snare19 x


noisy2 x = weirdtriangle $ x*200

snare2 x = sustain 0.05 x $ sawtooth . (*(99/((x/z)**0.8))) $ toZero 9 (\y -> sin $ x*(y)**0.8/z)/9 where z = 1000
snare3 x = sustain 0.1 x $ sawtooth . (*(99/((x/z)**0.8))) $ toZero 100 (\y -> sin $ x*(y/9)**0.8/z)/9 where z = 1000
snare4 x = sustain 0.1 x $ sin $ toZero 10 (\y -> sin $ x*(3+(y/9)**2))/10
snare5 x = sustain 0.1 x $ sawtooth y where y = (sin $ x/zoom)*zoom; zoom = 3
snare6 x = sustain 0.1 x $ sawtooth $ magnitude $ log (tanh $ x/44 :+ (sawtooth $ x*pi))
snare7 x = sustain 0.5 x $ sawtooth $ magnitude $ log (tanh $ xx*zoom/44 :+ (sawtooth $ xx*zoom*pi)) where zoom = 1; xx = x--halfsquare ((x*zoom2+1)/4)*sawtooth (x*zoom2)/zoom2; zoom2 = 1/3
snare8 x = sin . (*9) $ halfsquare ((x*zoom2+1)/4)*sawtooth (x*zoom2)/zoom2; zoom2 = 1/40
snare9 x = sustain 0.5 x $ sawtooth . (*1) $ halfsquare ((x*zoom2+1)/4)*sawtooth (x*zoom2)/zoom2 where zoom2 = 1/3
snare10 x = sustain 0.5 x $ noisy . (*1) $ halfsquare ((x*zoom2+1)/4)*sawtooth (x*zoom2)/zoom2 where zoom2 = 1/4
snare11 x = sustain 0.1 x $ sawtooth . (*(999/((xx/z)**0.8))) $ toZero 9 (\y -> (*(y/99)) . sin $ xx*(y)**0.8/z)/9 where z = 1000; xx = x*9999
snare12 x = sustain 0.1 x $ sawtooth (999*sin (sawtooth y)) where y = x/99
snare13 x = sustain 0.1 x $ sawtooth (snare2 9/y) where y = noisy $ x/9

snare14 x = sustain 0.03 x $ sawtooth $ (99*) $ (snare2 x)+snare1 x

snare18 x = normallimit $ (scalefloorfunction 3 $ sigmoid . (*5) . snare11) $ x

bassdrum x = sustain 0.1 x $ sawtooth (99*sin (x**0.5)/x)
bassdrum2 x = sustain 0.1 x $ triangle . (*(99/((x/z)**0.8))) $ toZero 9 (\y -> sin $ x*(y)**0.8/z)/9 where z = 1000
bassdrum3 x = sustain 0.1 x $ sin . (*(99/((x/z)**0.8))) $ toZero 9 (\y -> sin $ x*(y)**0.8/z)/9 where z = 1000
bassdrum4 x = sin(7*x*(exp (-x/30)))

bassdrum5 x = normallimit$ downscale 3 bassdrum3 x

weird2 x = sustain 0.5 x $ sawtooth $ imagPart $ atanh (sin $ x :+ (sawtooth $ x))
weird3 x = sustain 0.5 x $ sawtooth $ imagPart $ atan (sin $ x :+ (sawtooth $ x))
weird4 x = sustain 0.1 x $ sawtooth $ magnitude $ log (sin $ x/4 :+ (sawtooth $ x*pi))
weird5 x = sawtooth $ magnitude $ log (sin $ x/4 :+ (sawtooth $ x*pi))

hat x = sawtooth (x*sin (x*exp 6 *triangle (x*6*pi))/20) *envelope (oldsawtooth (x/8))/2 --sounds like electricity
hat2 x = sustain 0.02 x $ noisy $ x
hat3 x = sustain 0.03 x $ noisy $ x
hat4 s x = sustain s x $ noisy $ x


rainy x = weird5 . weirdtriangle $ x




rollM :: StatefulGen g m => g -> m Word
rollM = uniformRM (0, 1)
--pureGen :: StdGen
--pureGen = mkStdGen 42
--the seed is the same as the size, actually seed changes but size is 2 for 3 and such so that everything between 2^n and 2^(n+1) gets mapped to 2^n in terms of size
generator :: Int -> [Word]
generator n = runStateGen_ (mkStdGen (n+constant2)) (replicateM m . rollM)
  where
    m = (2^) $ floor $ logBase 2 (fromIntegral n)

generator2 = generator5 32

generator3 = generator4 4

--why need it be Word? oh it need not be
generator4 :: Int -> Int -> Int -> Ints
generator4 i seed n = runStateGen_ (mkStdGen seed) (replicateM n . uniformRM (1,fromIntegral i))

generator5 :: Int -> Int -> Int -> Ints
generator5 i seed n = runStateGen_ (mkStdGen seed) (replicateM n . uniformRM (0,fromIntegral i))

randomFloat :: Int -> Int -> Floats
randomFloat seed n = runStateGen_ (mkStdGen seed) (replicateM n . uniformRM (0,1.0))

--takes a size n and seed and randomly reorders the set [0..(n-1)]
shuffledNumbers :: Int -> Int -> Ints
shuffledNumbers n seed = statefulShuffle [0..(n-1)] seed
statefulShuffle ns seed = statefulShuffle' (length ns) ns seed
--uses fisher yates method
--takes a list as well, which it permutes
statefulShuffle' :: Int -> Ints -> Int -> Ints
statefulShuffle' n [] seed = []
statefulShuffle' n ns seed = (ns !! r):statefulShuffle' n2 (take r ns ++ drop (r+1) ns) seed
  where
    n2 = n-1
    r = randomInt n2 seed
randomInt n seed = runStateGen_ (mkStdGen n) (uniformRM (0,n))


--just an alternation between 0 and 1
alternating n = take m $ cycle [0,1]
  where
    m = (2^) $ floor $ logBase 2 (fromIntegral n)

--need this generator to make the hamming image, it starts from 0 for even inputs and 1 for odd
alternating2 n
  | even n = take m $ cycle [0,1]
  | otherwise = take m $ cycle [1,0]
  where
    m = (2^) $ floor $ logBase 2 (fromIntegral n)


--but there have to be n of those seeds, they can't all be the same. so we have 1 list, then we have 2 lists, 2 and 3, then we have 4 lists, 4, 5, 6, 7 and so on
komplex :: Int -> [[Float]]
komplex n = map ((map ((/layers) . fromIntegral)) . generator) [n .. 2*n-1]
--this is for 1/2 1/4 ... so on weights for the layers instead of uniform distribution
--komplex n = map ((map ((/((3/2) ^ (floor $ logBase 2 $ fromIntegral n) *3)) . fromIntegral)) . alternating2) [n .. 2*n-1]
  where
    --constant2 = map (map $ (/constant) . fromIntegral ) (map generator [n .. 2*n-1])

constant :: Float
--constant = 2^15
constant = 2^(layers2 -1)
layers :: Float
layers = 8
--layers = 25
layers2 :: Int
layers2 = floor layers
constant2 :: Int
constant2 = floor constant

--duplicates the matrix until it is big enough to match the right size
resize matrix
  | length matrix >= constant2 = matrix
  | otherwise = resize $ duplicate2 (map duplicate2 matrix)

--doubles amount of elements
duplicate2 [] = []
duplicate2 (x:xs) = x:x:duplicate2 xs

--construct constant2 is the full picture
construct 1 = resize $ komplex 1
construct n = zipWith (zipWith (+)) (resize $ komplex n) (construct (div n 2))

--used to construct the komplex explanation examples
thing2 [] = ""
thing2 (x:xs) = foldr (++) [] (map thing3 x) ++ "\n"
thing3 0.0 = "  "
thing3 0.25 = "11"--"░░"
thing3 0.5 = "22"--"▒▒"
thing3 0.75 = "33"--"▓▓"
thing3 1.0 = "44"--"██"

{-
--just a particular example
layer = generator layers2
--maybe we should add 0+1+0+1+1+... together and only then divide
-}

--multiplies amount of elements by the input
duplicate 1 x = x
duplicate i (x:xs) = (take i $ repeat x)++duplicate i xs
duplicate i [] = []

{--I tested and duplicate3 is actually slower than duplicate
duplicate3 1 xs = xs
duplicate3 i xs = duplicate3' i i xs
duplicate3' i 0 (x:xs) = duplicate3' i i xs
duplicate3' i n (x:xs) = x:duplicate3' i (n-1) (x:xs)
duplicate3' _ _ [] = []
--}

--resizes a list of values rather than a matrix
--resize2 l = duplicate (2^(iterations2-1 - (floor $ logBase 2 $ fromIntegral $ length l))) l
resize2 l = duplicate (div constant2 (length l)) l

construct2 1 = resize2 $ generator $ constant2
construct2 n = zipWith (+) (resize2 $ generator $ div constant2 n) (construct2 (div n 2))

--takes the size in n and returns the multiresolution sequence
multiResolutionLine n = map ((+(-1)) . (*(2/layers)) . fromIntegral) $ construct2 n

type Floats = [Float]

auralMultiresolution :: Floats
--auralMultiresolution = map (*volume) multiResolutionLine
--auralMultiresolution = map ((*volume) . sigmoid . (*5)) multiResolutionLine
auralMultiresolution = map ((*volume) . signum) $ multiResolutionLine constant2
--auralMultiresolution = multiResolutionLine

--this is just a random sequence of -1 and 1
--noise = map ((+(-1)) . (*2) . fromIntegral) $ generator constant2

--I need a function for shrinking a list, by cherry picking every other or so on
sparser :: Int -> [a] -> [a]
sparser n [] = []
sparser n l = head l:(sparser n $ drop n l)


logistic 0 r x = x
logistic i r x = r *§ y *§ (1-y)
  where
    y = logistic (i-1) r x

--x should range from 0 to 4
bifurcation :: Int -> Float -> Float
bifurcation n x = logistic n x (1/2)

{-
newton :: Complex1 -> Complex1 -> Int -> Complex1
newton f f' 0 z = z
newton f f' n z = newton f f' (n-1) $ z - (f z) / (f' z)
-}

--this is just a simpler magnitude, less computation, but I should actually make a function that takes two inputs, the complex number and the value to compare it to, and returns the boolean
magnitude2 (x :+ y) = x*x+y*y
isShorter x y = magnitude2 x < y*y

newtoniterations = 9 :: Int
newtoniterations2 = fromIntegral newtoniterations
newtonmax = 19 --this is the max distance allowed for points to have without being diverged
--this keeps track of how many iterations diverged it, like mandlebrot
newton2 :: Complex1 -> Complex1 -> Int -> Complex1
newton2 f f' 0 (x :+ y)
--  | x < -newtonmax || x > newtonmax || y < -newtonmax || y > newtonmax = (fromIntegral newtoniterations :+ 1)
  | x < -m || x > m || y < -m || y > m = (fromIntegral newtoniterations :+ newtoniterations2)
--  | x < -newtonmax || x > newtonmax || y < -newtonmax || y > newtonmax = (sawtooth (x*x+y*y) :+ 1)
  | otherwise = (x :+ y)
  where
    m = 1
newton2 f f' n (x :+ y)
  | x < -m || x > m || y < -m || y > m = (fromIntegral (newtoniterations-n) :+ newtoniterations2)
--  | x < -newtonmax || x > newtonmax || y < -newtonmax || y > newtonmax = (sawtooth $ x*x+y*y :+ 1)
  | otherwise = newton2 f f' (n-1) $ z - (f z) §/ (f' z)
  where
    m = 1+fromIntegral n
    z = x :+ y

--newtoniteration f f' n x y = newton f f' n (x :+ y)



--asdasdasdasdasd :: EncoderSettings -> FilePath -> FilePath -> GHC.Types.Any ()
--asdasdasdasdasd = encodeFlac encoderSettings

--encoderSettings :: EncoderSettings
--encoderSettings = EncoderSettings 5 0 False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing











{-
I could make floats of the digits of pi in such a way that I divide it into words.

now, how many digits in a float?

5.71238898038469
71 23 88 98 03 84 69
7 pairs is 14

idk what the binary one is.
but I can approximate anyway and am going to have to

so there be 10^14 floats
2^x<10^14
x<14*log_2(10)

46.50699332842308

so 2^46 is like the amount then possibly
however, those are floats, so then there are duplicates that might make up for the rest of the values.
-}


--btw, what about stereo sound? well, that can wait, I guess. Mathematical graphs don't necessarily need two outputs, although, complex functions could have such. however, stereo and more advanced functionality can come later.
