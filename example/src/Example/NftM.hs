module Example.NftM (nftMp) where

import Plutarch.LedgerApi.V3
import qualified Plutarch.LedgerApi.Value as PValue
import Plutarch.Prelude

nftMp :: ClosedTerm (PTxOutRef :--> PAsData PTokenName :--> PData :--> PScriptContext :--> POpaque)
nftMp = plam $ \ref tn _ ctx' -> popaque $
  unTermCont $ do
    ctx <- pmatchC ctx'
    PMintingScript ownSym <- tcont $ pmatch $ pscriptContext'scriptInfo ctx
    txInfo <- pmatchC $ pscriptContext'txInfo ctx
    pguardC "UTxO not consumed" $
      pany
        # plam (\x -> pmatch (pfromData x) $ \case x' -> ptxInInfo'outRef x' #== ref)
        #$ pfromData
        $ ptxInfo'inputs txInfo
    pguardC "Wrong NFT mint amount" $
      PValue.pvalueOf # pfromData (ptxInfo'mint txInfo) # pfromData ownSym # pfromData tn #== 1
    pure . popaque $ pconstant @PUnit ()
