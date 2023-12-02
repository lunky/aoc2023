module Day1
    (   day1
      , day1b
    ) where
import Data.Char
import Data.Either (fromRight)
import qualified  Text.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec as P

day1 :: String -> Int
day1 input = sum $ map ((\y -> read (head y : [last y])) . filter isNumber) (lines input)

day1b :: String -> Int
day1b input =  sum $ map day1'' $ lines input

day1' :: String -> Int
day1' input = fromRight 0 $ parse parseNum input

day1'' :: String -> Int
day1'' input = read $ show (day1' input)  ++  show (day1' $ reverse input)


type Parser = Parsec.Parsec String ()

parse :: Parser a -> String -> Either Parsec.ParseError a
parse p = P.parse p ""

parseNumeric :: Parser Int
parseNumeric = do
  num <- P.oneOf "0123456789"
  return $ digitToInt num

parseAlphaNumeric :: Parser Int
parseAlphaNumeric = do
  Parsec.try parseOne
        <|> Parsec.try parseTwo
        <|> Parsec.try parseThree
        <|> Parsec.try parseFour
        <|> Parsec.try parseFive
        <|> Parsec.try parseSix
        <|> Parsec.try parseSeven
        <|> Parsec.try parseEight
        <|> Parsec.try parseNine
        <|> Parsec.try parseNumeric

data WhatsThis = AChar Char | ANum Int

parseNum :: Parser Int
parseNum = do
  r <- ANum <$> parseAlphaNumeric  <|> (AChar <$> P.anyChar)
  case r of
    ANum d -> return d
    AChar _ -> parseNum

parseWordNum :: Parser Int
parseWordNum = do
    let numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    num <- Parsec.choice $ map Parsec.string $ numbers  ++ map reverse numbers
    case num of 
      "one" -> return 1
      "eno" -> return 1
      "two" -> return 2
      "owt" -> return 2
      "three" -> return 3
      "four" -> return 4
      "ruof" -> return 4
      "five" -> return 5
      "six" -> return 6
      "seven" -> return 7
      "eight" -> return 8
      "nine" -> return 9
      _ -> return 0

parseOne :: Parser Int
parseOne = do
  num <- Parsec.string "one" <|> Parsec.string "eno"
  return 1

parseTwo :: Parser Int
parseTwo = do
  num <- Parsec.string "two" <|> Parsec.string "owt"
  return 2
parseThree :: Parser Int
parseThree = do
  num <- Parsec.string "three" <|> Parsec.string "eerht"
  return 3
parseFour :: Parser Int
parseFour = do
  num <- Parsec.string "four" <|> Parsec.string "ruof"
  return 4
parseFive :: Parser Int
parseFive = do
  num <- Parsec.string "five" <|> Parsec.string "evif"
  return 5
parseSix :: Parser Int
parseSix = do
  num <- Parsec.string "six" <|> Parsec.string "xis"
  return 6
parseSeven :: Parser Int
parseSeven = do
  num <- Parsec.string "seven" <|> Parsec.string "neves"
  return 7
parseEight :: Parser Int
parseEight = do
  num <- Parsec.string "eight" <|> Parsec.string "thgie"
  return 8
parseNine :: Parser Int
parseNine = do
  num <- Parsec.string "nine" <|> Parsec.string "enin"
  return 9



_input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
_inputb = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
