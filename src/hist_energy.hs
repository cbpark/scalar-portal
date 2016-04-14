module Main where

import           Data.Attoparsec.ByteString.Char8
import           Data.List                        (intercalate)
import           Pipes
import           System.Environment               (getArgs)
import           System.IO

import           HEP.Analysis.Histogram1D
import           HEP.Data.LHEF                    (skipTillEnd)

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
        _ <- many' $ char '#' >> skipTillEnd
        double <* skipTillEnd
