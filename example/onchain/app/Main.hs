module Main (main) where

import System.FilePath ((</>))

import Ply.Plutarch

import Example.NftM (nftMp)

main :: IO ()
main = writeTypedScript "NFT Minting Policy" ("../" </> "compiled" </> "nftMp.plutus") nftMp
