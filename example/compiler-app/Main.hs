module Main (main) where

import System.FilePath ((</>))

import Plutarch (Config (Tracing), LogLevel (LogInfo), TracingMode (DoTracing))
import Ply.Plutarch

import Example.NftM (nftMp)

main :: IO ()
main =
  writeTypedScript
    (Tracing LogInfo DoTracing)
    "NFT Minting Policy (DoTracing)"
    ("compiled" </> "nftMp.plutus")
    nftMp
