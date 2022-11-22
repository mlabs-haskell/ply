module Example.NftM (nftMp) where

import Plutarch.Api.V1
import qualified Plutarch.Api.V1.Value as PValue
import Plutarch.Prelude

nftMp :: ClosedTerm (PTxOutRef :--> PTokenName :--> PMintingPolicy)
nftMp = plam $ \ref tn _ ctx' -> popaque $
  unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PMinting mintFlds <- tcont . pmatch $ getField @"purpose" ctx
    let ownSym = pfield @"_0" # mintFlds
    txInfo <- tcont $ pletFields @'["inputs", "mint"] $ getField @"txInfo" ctx
    pguardC "UTxO not consumed" $
      pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData $
        getField @"inputs" txInfo
    pguardC "Wrong NFT mint amount" $
      PValue.pvalueOf # getField @"mint" txInfo # ownSym # tn #== 1
    pure $ pconstant ()

pguardC :: Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceError s
