import Control.Monad.Reader
import Data.Maybe
import Data.Char
import Text.Show.Unicode

--------------------------------------------------------------------------------

data Consonant = ECon | K | S | T | N | H | M | Y | R | W deriving Show
data Vowel = N' | A | I | U | E | O deriving Show
data Dakuten = EDak | Dakuten | Handakuten
data Yoon = EYoon | Ya | Yu | Yo deriving Show

data Syllable = Syl Consonant Vowel Dakuten Yoon

--------------------------------------------------------------------------------

getSyl :: Consonant -> Vowel -> Dakuten -> Yoon -> Maybe Syllable
getSyl c v d y = let syl = Syl c v d y in if validSyl syl then Just syl else Nothing

validSyl :: Syllable -> Bool
validSyl = do
  b1 <- validCV
  b2 <- validD
  b3 <- validY
  return $ and [b1, b2, b3]

validCV :: Syllable -> Bool
validCV (Syl Y I _ _) = False --Yi doesn't exist
validCV (Syl Y E _ _) = False --Ye doesn't exist
validCV (Syl W U _ _) = False --Wu doesn't exist
validCV (Syl ECon N' _ _) = True  --N exists
validCV (Syl    _ N' _ _) = False --But only on its own
validCV _ = True --All others exist

validD :: Syllable -> Bool
validD (Syl _ _ EDak _) = True --No validation to be made
validD (Syl K _ Dakuten _) = True --K" becomes G
validD (Syl S _ Dakuten _) = True --S" becomes Z
validD (Syl T _ Dakuten _) = True --T" becomes D
validD (Syl H _ Dakuten _) = True --H" becomes B
validD (Syl H _ Handakuten _) = True --H0 becomes P
validD _ = False --No dakuten for any other consonants

validY :: Syllable -> Bool
validY (Syl _ _ _ EYoon) = True --No validation to be made
validY (Syl ECon _ _ _) = False --No Yoon for plain vowels
validY (Syl Y _ _ _) = False --No Yoon for Y consonants
validY (Syl W _ _ _) = False --No Yoon for W consonants
validY (Syl _ I _ _) = True
validY _ = False --Yoon is only used on I vowels

--------------------------------------------------------------------------------

cv :: Consonant -> Vowel -> String
cv S I = "SH"
cv T I = "CH"
cv T U = "TS"
cv H U = "FU"

dak :: Consonant -> Dakuten -> String
dak c EDak = pretty c
dak K Dakuten = "G"
dak S Dakuten = "Z"
dak T Dakuten = "D"
dak H Dakuten = "B"
dak H Handakuten = "P"

yoon :: Vowel -> Yoon -> String
yoon v EYoon = pretty v
yoon I y = pretty y

--------------------------------------------------------------------------------

class Pretty a where
  pretty :: a -> String

instance Pretty Vowel where
  pretty N' = "N"
  pretty v = show v

instance Pretty Consonant where
  pretty ECon = ""
  pretty c = show c

instance Pretty Yoon where
  pretty = map toUpper . show

instance Pretty Syllable where
  pretty (Syl c v d y) = head word : (map toLower $ tail word)
    where word = consonant ++ vowel
          consonant = dak c d
          vowel = yoon v y

--------------------------------------------------------------------------------

test = do
  c <- [ECon, K, S, T, N, H, M, Y, R, W]
  v <- [N', A, I, U, E, O]
  d <- [EDak, Dakuten, Handakuten]
  y <- [EYoon, Ya, Yu, Yo]
  return $ getSyl c v d y

test' = length $ filter isJust test

test'' = map (pretty.fromJust) $ filter isJust test

t = 'ぁ'

{-
char = 'ぁ' 	あ 	ぃ 	い 	ぅ 	う 	ぇ 	え 	ぉ 	お 	か 	が 	き 	ぎ 	く
U+305x 	ぐ 	け 	げ 	こ 	ご 	さ 	ざ 	し 	じ 	す 	ず 	せ 	ぜ 	そ 	ぞ 	た
U+306x 	だ 	ち 	ぢ 	っ 	つ 	づ 	て 	で 	と 	ど 	な 	に 	ぬ 	ね 	の 	は
U+307x 	ば 	ぱ 	ひ 	び 	ぴ 	ふ 	ぶ 	ぷ 	へ 	べ 	ぺ 	ほ 	ぼ 	ぽ 	ま 	み
U+308x 	む 	め 	も 	ゃ 	や 	ゅ 	ゆ 	ょ 	よ 	ら 	り 	る 	れ 	ろ 	ゎ 	わ
U+309x 	ゐ 	ゑ 	を 	ん 	ゔ 	ゕ 	ゖ 			゙ 	゚ 	゛ 	゜ 	ゝ 	ゞ 	ゟ
-}
