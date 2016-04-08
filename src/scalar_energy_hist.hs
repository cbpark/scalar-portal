module Main where

import           HEP.Analysis.Histogram1D

import           Control.Monad.Trans.State.Strict
import           Data.Attoparsec.ByteString       (skipWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile)
import           Data.ByteString.Char8            (ByteString)
import           Data.List                        (intercalate, transpose)
import           Pipes
import qualified Pipes.Attoparsec                 as PA
import           Pipes.ByteString                 (fromHandle)
import qualified Pipes.Prelude                    as P
import           System.Environment               (getArgs)
import           System.IO                        (Handle, IOMode (..),
                                                   withFile)

main :: IO ()
main = do
    putStrLn $ "# " ++ intercalate ", " ["bins", "energy"]

    infile <- fmap head getArgs
    histograms infile
      where
        histograms infile = do
            h <- withFile infile ReadMode (getEnergyHist 125 0.0 250.0)
            (mapM_ putStrLn . showHist . unitNormalize) h

getEnergyHist :: MonadIO m => Int -> Double -> Double -> Handle
              -> m (Hist1D Double)
getEnergyHist nbin lo hi hin = P.fold mappend mempty id hist
  where hist = (getEnergy . fromHandle) hin >-> P.map (histogram1 nbin lo hi)

getEnergy :: Monad m => Producer ByteString m () -> Producer Double m ()
getEnergy s = do
    (r, s') <- lift $ runStateT (PA.parse energy) s
    case r of Just (Right ev) -> yield ev >> getEnergy s'
              _               -> return ()
      where
        energy :: Parser Double
        energy = do
            skipSpace
            _ <- many' $ char '#' >> skipWhile (not . isEndOfLine) >> endOfLine
            e <- double
            char ',' >> skipSpace
            _ <- double <* skipSpace
            return e

showHist :: Hist1D Double -> [String]
showHist h = let (bs, con) = ((,) <$> bins <*> contents) h
             in map (intercalate ", " . map show) (transpose [bs, con])
