module Example.NftM (nftMp) where

import Plutarch (ClosedTerm, popaque)
import Plutarch.Api.V1
import Plutarch.Prelude

nftMp :: ClosedTerm (PTxOutRef :--> PTokenName :--> PMintingPolicy)
nftMp = plam $ \ref tn _ ctx' -> popaque $
  unTermCont $ do
    ctx <- tcont $ pletFields @'["txInfo", "purpose"] ctx'
    PMinting mintFlds <- tcont . pmatch $ hrecField @"purpose" ctx
    let ownSym = pfield @"_0" # mintFlds
    txInfo <- tcont $ pletFields @'["inputs", "mint"] $ hrecField @"txInfo" ctx
    pguardC "UTxO not consumed" $
      pany # plam (\x -> pfield @"outRef" # x #== pdata ref) #$ pfromData $ hrecField @"inputs" txInfo
    pguardC "Wrong NFT mint amount" $
      pvalueOf # ownSym # pdata tn # hrecField @"mint" txInfo #== pconstantData 1
    pure $ pconstant ()

pguardC :: Term s PString -> Term s PBool -> TermCont s ()
pguardC s cond = tcont $ \f -> pif cond (f ()) $ ptraceError s

-- | Amount of an asset identified by its curr sym and tk name within a 'PValue'.
pvalueOf :: Term s (PAsData PCurrencySymbol :--> PAsData PTokenName :--> PValue :--> PAsData PInteger)
pvalueOf = phoistAcyclic $
  plam $ \cs tknm val ->
    let tokMap = pmatch (pmapLookup # cs # pto val) $ \case
          PJust x -> pfromData x
          PNothing -> pcon $ PMap pnil
     in pmatch (pmapLookup # tknm # tokMap) $ \case
          PJust x -> x
          PNothing -> pconstantData 0

-- | Look up the given key data in a 'PMap'.
pmapLookup :: Term s (PAsData k :--> PMap k v :--> PMaybe (PAsData v))
pmapLookup = phoistAcyclic $
  plam $ \key mp ->
    precList
      ( \self x xs ->
          pif
            (pfstBuiltin # x #== key)
            (pcon . PJust $ psndBuiltin # x)
            (self # xs)
      )
      (const $ pcon PNothing)
      # pto mp
