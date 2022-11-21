module Main (main) where

import System.FilePath ((</>))

import Plutarch (Config (Config, tracingMode), TracingMode (DoTracing))
import Ply.Plutarch

import Example.NftM (nftMp)

main :: IO ()
main =
  writeTypedScript
    (Config {tracingMode = DoTracing})
    "NFT Minting Policy (DoTracing)"
    ("compiled" </> "nftMp.plutus")
    nftMp
