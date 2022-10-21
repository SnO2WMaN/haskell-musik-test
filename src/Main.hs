module Main where

import Data.ByteString.Builder qualified as B
import Data.List (zipWith3)
import System.Process (callProcess)

sampleRate :: Float
sampleRate = 44100.0

type Volume = Float
type Hz = Float

type BPM = Float
type Beats = Float
type Seconds = Float

beatsToSeconds :: BPM -> Beats -> Seconds
beatsToSeconds bpm = (60.0 / bpm *)

type Semitones = Float
pitchStandard :: Hz
pitchStandard = 440.0

semitonesToHz :: Semitones -> Hz
semitonesToHz n = pitchStandard * (2 ** (1 / 12)) ** n

a :: Float -> Semitones
a x = 12 * x - 48.0
b :: Float -> Semitones
b x = a x + 2.0
c :: Float -> Semitones
c x = a x - 9.0
d :: Float -> Semitones
d x = a x - 7.0
e :: Float -> Semitones
e x = a x - 5.0
f :: Float -> Semitones
f x = a x - 4.0
g :: Float -> Semitones
g x = a x - 2.0

squareWave :: Int -> Float -> Float
squareWave n t = sum $ (\k -> sin ((2 * fromIntegral k - 1) * t) / (2 * fromIntegral k - 1)) <$> [1 .. n]

sawWave :: Int -> Float -> Float
sawWave n t = sum $ (\k -> sin (fromIntegral k * t) / fromIntegral k) <$> [1 .. n]

type Pulse = Float
frequency :: Volume -> Hz -> Seconds -> [Pulse]
frequency volume hz duration = (* volume) <$> zipWith3 (\a b -> (a * b *)) attack release freq
  where
    step = (hz * 2 * pi) / sampleRate
    freq :: [Pulse]
    freq = sawWave 25 . (* step) <$> [0.0 .. sampleRate * duration]
    attack :: [Pulse]
    attack = min 1.0 <$> [0, 0.01 ..]
    release :: [Pulse]
    release = reverse $ take (length freq) attack

note :: Volume -> BPM -> Semitones -> Beats -> [Float]
note volume bpm n = frequency volume (semitonesToHz n) . beatsToSeconds bpm

chord :: [Pulse] -> [Pulse] -> [Pulse]
chord = zipWith (+)

save :: FilePath -> [Pulse] -> IO ()
save filepath wave = writeFileLBS filepath $ B.toLazyByteString $ foldMap B.floatLE wave

play :: FilePath -> IO ()
play filepath = callProcess "ffplay" ["-f", "f32le", "-nodisp", "-autoexit", "-ar", show sampleRate, filepath]

main :: IO ()
main = do
  let noteT = note 0.05 140
  save "sound" $
    concat
      [ noteT (e 5) 1.0
      , noteT (b 4) 0.5
      , noteT (c 5) 0.5
      , noteT (d 5) 1.0
      , noteT (c 5) 0.5
      , noteT (b 4) 0.5
      , noteT (a 4) 1.0
      , noteT (a 4) 0.5
      , noteT (c 5) 0.5
      , noteT (e 5) 1.0
      , noteT (d 5) 0.5
      , noteT (c 5) 0.5
      , noteT (b 4) 1.0
      , noteT (b 4) 0.5
      , noteT (c 5) 0.5
      , noteT (d 5) 1.0
      , noteT (e 5) 1.0
      , noteT (c 5) 1.0
      , noteT (a 4) 1.0
      , noteT (a 4) 2.0
      ]
  play "sound"
