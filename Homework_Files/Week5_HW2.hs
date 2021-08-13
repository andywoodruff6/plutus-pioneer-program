{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.Homework2 where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet


{-# INLINABLE tn #-}
tn :: TokenName
tn = TokenName emptyByteString
-- I missed this in my original attempt

{-# INLINABLE mkPolicy #-}
-- Minting policy for an NFT, where the minting transaction must consume the given UTxO as input
-- and where the TokenName will be the empty ByteString.

mkPolicy :: TxOutRef -> ScriptContext -> Bool
mkPolicy oref ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                    traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info
--txInInfoOutRef :: TxOutRef so i is a TxOutRef
--txInfoInputs :: [TxInInfo] and TxInInfo takes {txInInfoOutRef :: TxOutRef, txInInfoResolved :: TxOut}
--TxInfo takes { txInfoInputs :: [TxInInfo], txInfoOutputs :: [TxOut]}
-- Therefore info == txInfoInputs by type

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn'== tn && amt == 1
        _                -> False
-- TxInfo = TxInfo {txInfoForge :: Value}
--flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
--Replacing the tn' with _ is not how you solve this.

policy :: TxOutRef -> Scripts.MonetaryPolicy
policy oref = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
-- two variables requires a lamba function \Var1 -> functor 
-- one variable requires only one applyCode / liftCode 
-- Because the tn is an empytByteString it will not be included in here so this is correct

curSymbol :: TxOutRef -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
--curSymbol oref tn = scriptCurrencySymbol $ policy oref tn
-- if there are two variables then the above format is needed

type NFTSchema =
    BlockchainActions
        .\/ Endpoint "mint" ()

mint :: Contract w NFTSchema Text ()
mint = do
    pk    <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let val     = Value.singleton (curSymbol oref) tn 1
                lookups = Constraints.monetaryPolicy (policy oref) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)
--In Val and lookups, curSymbol and policy need to match the format that we used in the lines above,
--so in this problem we would drop the tn
endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >> mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 ()
    callEndpoint @"mint" h2 ()
    void $ Emulator.waitNSlots 1
