import Data.Char

data Note = A | AS | B | C | CS | D | DS | E | F | FS | G | GS

instance Enum Note where
  fromEnum note = case note of
    A  -> 0
    AS -> 1
    B  -> 2
    C  -> 3
    CS -> 4
    D  -> 5
    DS -> 6
    E  -> 7
    F  -> 8
    FS -> 9
    G  -> 10
    GS -> 11

  toEnum num = case num of
    0  -> A
    1  -> AS
    2  -> B
    3  -> C
    4  -> CS
    5  -> D
    6  -> DS
    7  -> E
    8  -> F
    9  -> FS
    10 -> G
    11 -> GS

type Frequency = Double
type Octave = Int

getFrequency :: Note -> Octave -> Frequency
getFrequency A o = 440.0 * 2 ^^ (o - 4)
getFrequency n o = getFrequency A o * factor
  where factor = 2 ** (toNum n / 12)
        toNum = fromIntegral.fromEnum

correct :: Note -> Octave -> Frequency --scale actually starts at C rather than A, this corrects this
correct n = getFrequency n . check
  where check = if fromEnum n > 2 then pred else id

-------------------------------------------------------------------------------------------------------

divisions :: Num a => a
divisions = 5

base :: Frequency
base = 500.0

--as a general rule:
--generalFreq a b = generalFreq c d
--if: divisions * b + a = divisions * d + c
--    divisions * (b - d) = c - a

generalFreq :: Int -> Octave -> Frequency
generalFreq 0 o = base * 2 ^^ o
generalFreq n o = generalFreq 0 o * factor
  where factor = 2 ** (fromIntegral n / divisions)
