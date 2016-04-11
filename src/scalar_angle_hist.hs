module Main where

import           HEP.Analysis.Histogram1D

import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.List                        (intercalate)
import           Pipes
import           System.Environment               (getArgs)
import           System.IO

main :: IO ()
main = do
    putStrLn $ "# " ++ intercalate ", " ["bins", "cosTheta"]
    infile <- fmap head getArgs
    printHist infile histCosTheta unitNormalize

histCosTheta :: MonadIO m => Handle -> m (Hist1D Double)
histCosTheta = consHist cosTheta 100 (-1) 1
  where
    cosTheta :: Parser Double
    cosTheta = do
        skipSpace
        _ <- many' $ char '#' >> skipWhile (not . isEndOfLine) >> endOfLine
        _ <- double >> skipSpace >> char ',' >> skipSpace
        double <* skipSpace
