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


forwardOrBackward :: String -> Parser String
forwardOrBackward numString = Parsec.string numString <|> Parsec.string (reverse numString)

parseOne :: Parser Int
parseOne = do
  _ <- forwardOrBackward "one"
  return 1

parseTwo :: Parser Int
parseTwo = do
  _ <- forwardOrBackward "two"
  return 2

parseThree :: Parser Int
parseThree = do
  _ <- forwardOrBackward "three"
  return 3

parseFour :: Parser Int
parseFour = do
  _ <- forwardOrBackward "four"
  return 4

parseFive :: Parser Int
parseFive = do
  _ <- forwardOrBackward "five"
  return 5

parseSix :: Parser Int
parseSix = do
  _ <- forwardOrBackward "six"
  return 6

parseSeven :: Parser Int
parseSeven = do
  _ <- forwardOrBackward "seven"
  return 7

parseEight :: Parser Int
parseEight = do
  _ <- forwardOrBackward "eight"
  return 8

parseNine :: Parser Int
parseNine = do
  _ <- forwardOrBackward "nine"
  return 9

_input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
_inputb = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen"
