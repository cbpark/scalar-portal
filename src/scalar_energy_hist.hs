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
    putStrLn $ "# " ++ intercalate ", " ["bins", "energy"]
    infile <- fmap head getArgs
    printHist infile histE unitNormalize

histE :: MonadIO m => Handle -> m (Hist1D Double)
histE = consHist energy 210 0.0 420.0
  where
    energy :: Parser Double
    energy = do
        skipSpace
        _ <- many' $ char '#' >> skipWhile (not . isEndOfLine) >> endOfLine
        e <- double
        char ',' >> skipSpace
        _ <- double <* skipSpace
        return e
