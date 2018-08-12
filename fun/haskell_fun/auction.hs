import Prelude
import Data.Maybe
import Data.Char
import Data.Functor
import Control.Applicative
import Control.Monad

--newtype Database = DB [Table]
newtype Table = TB [Record]

data Record = C Customer

data Customer = Customer { cusNum :: Int,
                           firstName :: String,
                           lastName :: String,
                           phoneNumber :: String,
                           selling :: [Item],
                           buying :: [Item]
                         } deriving (Show)

data Item = Item { itemNum :: Int,
                   lotNum :: Int,
                   description :: String,
                   reserve :: Float,
                   salePrice :: Maybe Float
                 } deriving (Show)

randNames :: [String] -> Random String
randNames names = random (names !!).(flip mod $ length names) --will generate names from either of the two lists

randMobiles :: Random String
randMobiles = random $ (space 5).(space 8).('0':).show where
  space n = (\tu -> fst tu ++ " " ++ snd tu).(splitAt n)

firstNames :: [String]
firstNames = ["James","John","Robert","Michael","William","David","Richard","Charles","Joseph","Thomas","Christopher","Daniel","Paul","Mark","Donald","George","Kenneth",
  "Steven","Edward","Brian","Ronald","Anthony","Kevin","Jason","Matthew","Gary","Timothy","Jose","Larry","Jeffrey","Frank","Scott","Eric","Stephen","Andrew","Raymond",
  "Gregory","Joshua","Jerry","Dennis","Walter","Patrick","Peter","Harold","Douglas","Henry","Carl","Arthur","Ryan","Roger","Joe","Juan","Jack","Albert","Jonathan","Justin",
  "Terry","Gerald","Keith","Samuel","Willie","Ralph","Lawrence","Nicholas","Roy","Benjamin","Bruce","Brandon","Adam","Harry","Fred","Wayne","Billy","Steve","Louis","Jeremy",
  "Aaron","Randy","Howard","Eugene","Carlos","Russell","Bobby","Victor","Martin","Ernest","Phillip","Todd","Jesse","Craig","Alan","Shawn","Clarence","Sean","Philip",
  "Chris","Johnny","Earl","Jimmy","Antonio",
  "Mary","Patricia","Linda","Barbara","Elizabeth","Jennifer","Maria","Susan","Margaret","Dorothy","Lisa","Nancy","Karen","Betty","Helen","Sandra","Donna",
  "Carol","Ruth","Sharon","Michelle","Laura","Sarah","Kimberly","Deborah","Jessica","Shirley","Cynthia","Angela","Melissa","Brenda","Amy","Anna","Rebecca","Virginia",
  "Kathleen","Pamela","Martha","Debra","Amanda","Stephanie","Carolyn","Christine","Marie","Janet","Catherine","Frances","Ann","Joyce","Diane","Alice","Julie","Heather",
  "Teresa","Doris","Gloria","Evelyn","Jean","Cheryl","Mildred","Katherine","Joan","Ashley","Judith","Rose","Janice","Kelly","Nicole","Judy","Christina","Kathy","Theresa",
  "Beverly","Denise","Tammy","Irene","Jane","Lori","Rachel","Marilyn","Andrea","Kathryn","Louise","Sara","Anne","Jacqueline","Wanda","Bonnie","Julia","Ruby","Lois",
  "Tina","Phyllis","Norma","Paula","Diana","Annie","Lillian","Emily","Robin"]

lastNames :: [String]
lastNames = ["Smith","Johnson","Williams","Jones","Brown","Davis","Miller","Wilson","Moore","Taylor","Anderson","Thomas","Jackson","White","Harris","Martin","Thompson",
  "Garcia","Martinez","Robinson","Clark","Rodriguez","Lewis","Lee","Walker","Hall","Allen","Young","Hernandez","King","Wright","Lopez","Hill","Scott","Green","Adams",
  "Baker","Gonzalez","Nelson","Carter","Mitchell","Perez","Roberts","Turner","Phillips","Campbell","Parker","Evans","Edwards","Collins","Stewart","Sanchez","Morris",
  "Rogers","Reed","Cook","Morgan","Bell","Murphy","Bailey","Rivera","Cooper","Richardson","Cox","Howard","Ward","Torres","Peterson","Gray","Ramirez","James","Watson",
  "Brooks","Kelly","Sanders","Price","Bennett","Wood","Barnes","Ross","Henderson","Coleman","Jenkins","Perry","Powell","Long","Patterson","Hughes","Flores","Washington",
  "Butler","Simmons","Foster","Gonzales","Bryant","Alexander","Russell","Griffin","Diaz","Hayes","Myers","Ford","Hamilton","Graham","Sullivan","Wallace","Woods","Cole",
  "West","Jordan","Owens","Reynolds","Fisher","Ellis","Harrison","Gibson","Mcdonald","Cruz","Marshall","Ortiz","Gomez","Murray","Freeman","Wells","Webb","Simpson",
  "Stevens","Tucker","Porter","Hunter","Hicks","Crawford","Henry","Boyd","Mason","Morales","Kennedy","Warren","Dixon","Ramos","Reyes","Burns","Gordon","Shaw","Holmes",
  "Rice","Robertson","Hunt","Black","Daniels","Palmer","Mills","Nichols","Grant","Knight","Ferguson","Rose","Stone","Hawkins","Dunn","Perkins","Hudson","Spencer","Gardner",
  "Stephens","Payne","Pierce","Berry","Matthews","Arnold","Wagner","Willis","Ray","Watkins","Olson","Carroll","Duncan","Snyder","Hart","Cunningham","Bradley","Lane",
  "Andrews","Ruiz","Harper","Fox","Riley","Armstrong","Carpenter","Weaver","Greene","Lawrence","Elliott","Chavez","Sims","Austin","Peters","Kelley","Franklin","Lawson"]

ran :: Int -> Int -> Int -> [Int] -> Int -> [Int]
ran a b m list (-1) = init list
ran a b m list l = ran a b m (nextrand : list) (l-1) where
  nextrand = (a * (head list) + b) `mod` m

rand :: [Int] -> Int -> [Int]
rand = ran 4893468768 2867713784 9999999999

type Random a = Int -> Int -> [a]

random :: (Int -> a) -> Random a
random f seed number = f <$> (rand [seed] number)

{--
firstWords :: String -> [String]
firstWords s = head.words <$> (lines s)

lower :: String -> String
lower (x:xs) = x : (toLower <$> xs)

exec :: FilePath -> IO [String]
exec f = do
  x <- readFile f
  return $ lower <$> (firstWords x)
--}
