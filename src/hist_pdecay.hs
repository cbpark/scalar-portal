module Main where

import           Control.Monad                    (replicateM_)
import           Data.Attoparsec.ByteString.Char8
import           Data.List                        (intercalate)
import           Pipes
import           System.Environment               (getArgs)
import           System.IO

import           HEP.Analysis.Histogram1D
import           HEP.Data.LHEF                    (skipTillEnd)

main :: IO ()
main = do
    putStrLn $ "# " ++ intercalate ", " ["bins", "Pdecay"]
    infile <- fmap head getArgs
    printHist infile histPdecay unitNormalize

histPdecay :: MonadIO m => Handle -> m (Hist1D Double)
histPdecay = consHist pDecay 1000 0 1
  where
    pDecay :: Parser Double
    pDecay = do
        skipSpace
        _ <- many' $ char '#' >> skipTillEnd
        replicateM_ 3 (double >> skipSpace >> char ',' >> skipSpace)
        double <* skipTillEnd
