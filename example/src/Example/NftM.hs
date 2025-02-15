module Example.NftM (nftMp) where

import Plutarch.LedgerApi.V2
import qualified Plutarch.LedgerApi.Value as PValue
import Plutarch.Prelude

nftMp :: ClosedTerm (PTxOutRef :--> PTokenName :--> PData :--> PScriptContext :--> POpaque)
nftMp = plam $ \ref tn _ ctx' -> popaque $
  unTermCont $ do
    ctx <- pmatchC ctx'
    PMinting ownSym <- tcont $ pmatch $ pscriptContext'purpose ctx
    txInfo <- pmatchC $ pscriptContext'txInfo ctx
    pguardC "UTxO not consumed" $
      pany
        # plam (\x -> pmatch (pfromData x) $ \case x' -> ptxInInfo'outRef x' #== ref)
        #$ pfromData
        $ ptxInfo'inputs txInfo
    pguardC "Wrong NFT mint amount" $
      PValue.pvalueOf # pfromData (ptxInfo'mint txInfo) # pfromData ownSym # tn #== 1
    pure . popaque $ pconstant @PUnit ()
