module Main where

import           Codec.Compression.GZip     (decompress)
import           Control.Monad              (forever)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (intercalate)
import           Pipes
import qualified Pipes.Prelude              as P
import           System.Environment         (getArgs)

import           HEP.Data.LHEF
import qualified HEP.Data.LHEF.PipesUtil    as U

main :: IO ()
main = do
    let header = "# " ++ intercalate ", " vars
    putStrLn header

    infile <- fmap head getArgs
    evStr <- fmap decompress (L.readFile infile)
    runEffect $ (U.eventEntryFromBS . L.toStrict) evStr
        >-> U.finalStates >-> P.map (filter isScalar)
        >-> variables
        >-> P.mapM_ (mapM_ print)
      where
        isScalar p = idOf p == 35

vars :: [String]
vars = [ "energy", "cosTheta", "beta", "Pdecay"]

newtype Result = Result { getResult :: [(String, Double)] }

instance Show Result where
    show = intercalate ", " . map (show . snd) . getResult

variables :: Monad m => Pipe [Particle] [Result] m ()
variables = forever $ do
    ps <- await
    let results = map calcVar ps
    yield $ map (Result . zip vars) results

calcVar :: Particle -> [Double]
calcVar p = let e = energyOf p
                c = cosThetaBeam p
                b = beta p
                pd = pDecay b
            in [e, c, b, pd]

pDecay :: Double -> Double
pDecay b = let g = 1 / sqrt (1 - b * b)
               bCTau = b * g * cTau
               expDecay x = exp (- x / bCTau)
           in expDecay len1 - expDecay len2

len1, len2, cTau :: Double
len1 = 50 / uGeVm
len2 = 110 / uGeVm
cTau = 1e-6 * cLight / uGeVm

uMeVfm, uGeVm, cLight :: Double
uMeVfm = 197.3269788
uGeVm = uMeVfm * 1e-3 * 1e-15
cLight = 299792458.0
