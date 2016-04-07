module Main where

import qualified HEP.Data.LHEF.PipesUtil    as U

import           Codec.Compression.GZip     (decompress)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (intercalate)
import           Pipes
import qualified Pipes.Prelude              as P
import           System.Environment         (getArgs)

main :: IO ()
main = do
    let header = "# " ++ intercalate ", " [ "energy", "cosTh"]
    putStrLn header

    infile <- fmap head getArgs
    evStr <- fmap decompress (L.readFile infile)
    runEffect $ (U.eventEntryFromBS . L.toStrict) evStr
        >-> U.finalStates
        >-> P.print
