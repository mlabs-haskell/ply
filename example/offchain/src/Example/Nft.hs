module Example.Nft (MintParams (..), NFTSchema, mintNft) where

import Data.Functor (void)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Void (Void)
import Text.Printf (printf)

import Ledger (
  PaymentPubKeyHash,
  TokenName,
  TxOutRef,
  pubKeyHashAddress,
  scriptCurrencySymbol,
 )
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Value as Value
import Plutus.Contract (Contract, Endpoint)
import qualified Plutus.Contract as Contract
import Ply (
  ScriptRole (MintingPolicyRole),
  TypedScript,
  (#),
 )
import qualified Ply

type NFTSchema =
  Endpoint "mint" TokenName

data MintParams = MintParams
  { mpName :: Text
  , mpDescription :: Maybe Text
  , mpTokenName :: TokenName
  , mpPubKeyHash :: PaymentPubKeyHash
  }
  deriving stock (Show)

mintNft :: TypedScript 'MintingPolicyRole '[TxOutRef, TokenName] -> MintParams -> Contract () NFTSchema Text ()
mintNft nftMp MintParams {..} = do
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- Contract.utxosAt (pubKeyHashAddress pkh Nothing)
  case Map.keys utxos of
    [] -> Contract.logError @String "no utxo found"
    oref : _ -> do
      Contract.logInfo $ "Using oref:" <> Txt.pack (show oref)
      let policy = Ply.toMintingPolicy $ nftMp # oref # mpTokenName
      let cs = scriptCurrencySymbol policy
          val = Value.singleton cs mpTokenName 1
          lookups =
            mconcat
              [ Constraints.mintingPolicy policy
              , Constraints.unspentOutputs utxos
              ]
          tx =
            mconcat
              [ Constraints.mustMintValue val
              , Constraints.mustSpendPubKeyOutput oref
              , Constraints.mustPayToPubKey mpPubKeyHash val
              ]
      void $ Contract.submitTxConstraintsWith @Void lookups tx
      Contract.logInfo @String $ printf "forged %s" (show val)
      Contract.logInfo @String "Finished"
