{-# LANGUAGE UndecidableInstances #-}

module Example.NftM (nftMp) where

import GHC.Generics (Generic)

import qualified Generics.SOP as SOP
import Plutarch.LedgerApi.V3
import qualified Plutarch.LedgerApi.Value as PValue
import Plutarch.Prelude
import Plutarch.Repr.Data (DeriveAsDataStruct (DeriveAsDataStruct))
import Ply.Plutarch (PlyArgOf)

import Example.Type (MyParameter)

-- | A custom parameter type acting as an extra argument to our script.
data PMyParameter (s :: S) = PMyParameter
  { pmyParameter'tn :: Term s (PAsData PTokenName)
  , pmyParameter'ref :: Term s PTxOutRef
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMyParameter)

deriving via
  DeriveDataPLiftable PMyParameter MyParameter
  instance
    PLiftable PMyParameter

-- Establish the Ply correspondence between our Plutarch and Haskell types.
type instance PlyArgOf PMyParameter = MyParameter

-- | An example minting policy.
nftMp :: ClosedTerm (PMyParameter :--> PData :--> PScriptContext :--> POpaque)
nftMp = plam $ \param' _ ctx' -> popaque $
  unTermCont $ do
    ctx <- pmatchC ctx'
    param <- pmatchC param'
    PMintingScript ownSym <- tcont $ pmatch $ pscriptContext'scriptInfo ctx
    txInfo <- pmatchC $ pscriptContext'txInfo ctx
    pguardC "UTxO not consumed" $
      pany
        # plam (\x -> pmatch (pfromData x) $ \case x' -> ptxInInfo'outRef x' #== pmyParameter'ref param)
        #$ pfromData
        $ ptxInfo'inputs txInfo
    pguardC "Wrong NFT mint amount" $
      PValue.pvalueOf # pfromData (ptxInfo'mint txInfo) # pfromData ownSym # pfromData (pmyParameter'tn param) #== 1
    pure . popaque $ pconstant @PUnit ()
