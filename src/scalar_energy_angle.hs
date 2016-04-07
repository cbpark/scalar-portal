module Main where

import           HEP.Data.LHEF
import qualified HEP.Data.LHEF.PipesUtil    as U

import           Codec.Compression.GZip     (decompress)
import           Control.Monad              (forever)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (intercalate)
import           Pipes
import qualified Pipes.Prelude              as P
import           System.Environment         (getArgs)

main :: IO ()
main = do
    let header = "# " ++ intercalate ", " [ "energy", "cosTheta"]
    putStrLn header

    infile <- fmap head getArgs
    evStr <- fmap decompress (L.readFile infile)
    runEffect $ (U.eventEntryFromBS . L.toStrict) evStr
        >-> U.finalStates >-> P.map (filter isScalar)
        >-> variables
        >-> P.mapM_ (mapM_ print)
      where
        isScalar = (== 35) . idOf

newtype Result = Result { getResult :: [(String, Double)]}

instance Show Result where
    show = intercalate ", " . map (show . snd) . getResult

variables :: Monad m => Pipe [Particle] [Result] m ()
variables = forever $ do
    ps <- await
    let calcVar = (,) <$> energyOf <*> cosThetaBeam
        results = map ((\(x, y) -> [x, y]) . calcVar) ps
    yield $ map (Result . zip ["energy", "cosTheta"]) results
