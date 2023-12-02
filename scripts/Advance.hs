{-# LANGUAGE OverloadedStrings #-}
module Advance
    ( 
        main
    )
    where
import qualified Data.Text as T
import System.FilePath.Posix
import System.Directory
import Data.List (isPrefixOf,isSuffixOf)
import Data.Char (isDigit)


makeTemplate :: String -> String -> Int -> IO()
makeTemplate templateFile outputMask dayNumber = do
        contents <- readFile templateFile
        let output = T.replace  "%%DAY_NUMBER%%" (T.pack $ show dayNumber) (T.pack contents)
        let dayFileName = T.replace "%%DAY_NUMBER%%" (T.pack $ show dayNumber) (T.pack outputMask)
        let outPath = takeDirectory templateFile ++ [pathSeparator] ++ T.unpack dayFileName 
        writeFile outPath $ T.unpack output
        putStrLn ("file written : " ++ outPath)

makeBlank :: String -> Int -> IO()
makeBlank outputPath dayNumber = do
        let dayFileName = T.unpack $ T.replace "%%DAY_NUMBER%%" (T.pack $ show dayNumber) (T.pack outputPath)
        writeFile dayFileName $ T.unpack "" 
        putStrLn ("file written : " ++ dayFileName)


getNextNumber path = do
    files <- listDirectory path
    let days = filter (\y -> isSuffixOf ".hs" y && isPrefixOf "Day" y ) files
    let highest = maximum $ map((\y -> read y::Int ) . filter  isDigit ) days
    return (highest + 1)
    
     

main :: IO ()
main =  do  
    num <- getNextNumber "src"
    makeTemplate "src/TemplateDay.txt" "Day%%DAY_NUMBER%%.hs" num
    makeTemplate "test/TemplateDay.txt" "Day%%DAY_NUMBER%%Spec.hs" num
    makeBlank "data/day%%DAY_NUMBER%%.txt" num

