{-# LANGUAGE DataKinds           #-}
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

module Batch64FreeMintingMOD where

-- On Chain 
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
-- Ledger Types
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
-- Off-chain Contract Monad
import           Plutus.Contract        as Contract
-- Simulation - Playground
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
-- Simulation - Trace
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
-- Haskell
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)


--ON-CHAIN

{-# INLINABLE freeMintingPolicy  #-}
freeMintingPolicy :: () -> ScriptContext -> Bool
freeMintingPolicy () _ = True

{-# INLINABLE freeMintingPolicyUntyped #-}
freeMintingPolicyUntyped :: BuiltinData -> BuiltinData -> ()
freeMintingPolicyUntyped _ _ = ()

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy freeMintingPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

---- MOD part
{-# INLINABLE signedMintingPolicyMOD  #-}
signedMintingPolicyMOD :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
signedMintingPolicyMOD pkh () sContext = (txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkh) 
                                        && (traceIfFalse "You can only mint a unique NFT (1-of-1)" checkMintedAmount)
  where
    info :: TxInfo
    info = scriptContextTxInfo sContext

    checkMintedAmount :: Bool
    checkMintedAmount =  case flattenValue (txInfoMint info) of
        [(_,_, amount)]  -> amount == 1
        _                -> False
 

policyMOD :: PaymentPubKeyHash -> Scripts.MintingPolicy
policyMOD pkh = mkMintingPolicyScript $
             $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . signedMintingPolicyMOD ||])
             `PlutusTx.applyCode`
             PlutusTx.liftCode pkh

curSymbolMOD :: PaymentPubKeyHash -> CurrencySymbol
curSymbolMOD pkh = scriptCurrencySymbol $ policyMOD pkh


--OFF-CHAIN CODE

data MintParams = MintParams
                { mpTokenName :: !TokenName
                , mpAmount    :: !Integer   
                } deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton (curSymbolMOD pkh) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policyMOD pkh
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema
mkKnownCurrencies []

-- EMULATOR SIMULATION

test :: IO ()
test = runEmulatorTraceIO $ do
       h1 <- activateContractWallet (knownWallet 1) endpoints
       h2 <- activateContractWallet (knownWallet 2) endpoints
       callEndpoint @"mint" h1 $ MintParams 
                               { mpTokenName = "myNFT"
                               , mpAmount    = 1
                               }
       void $ Emulator.waitNSlots 10
       callEndpoint @"mint" h2 $ MintParams 
                               { mpTokenName = "hisNFT"
                               , mpAmount    = 1
                               }
       void $ Emulator.waitNSlots 10
       callEndpoint @"mint" h1 $ MintParams 
                               { mpTokenName = "mySecondNFT"
                               , mpAmount    = 1
                               }
       void $ Emulator.waitNSlots 10