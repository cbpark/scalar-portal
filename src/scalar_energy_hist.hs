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
    histograms infile

histograms :: FilePath -> IO ()
histograms infile =
    withFile infile ReadMode histE >>= putStr . showHist1D . unitNormalize

histE :: MonadIO m => Handle -> m (Hist1D Double)
histE = consHist energy 125 0.0 250.0
  where
    energy :: Parser Double
    energy = do
        skipSpace
        _ <- many' $ char '#' >> skipWhile (not . isEndOfLine) >> endOfLine
        e <- double
        char ',' >> skipSpace
        _ <- double <* skipSpace
        return e
