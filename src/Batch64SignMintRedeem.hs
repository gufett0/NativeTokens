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

module Batch64SignMintRedeem where

-- On Chain 
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           PlutusTx               (Data (..))
import qualified PlutusTx.Builtins   as Builtins
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

{-# INLINABLE redeemerMintingPolicy #-}
redeemerMintingPolicy :: PaymentPubKeyHash -> ScriptContext -> Bool
redeemerMintingPolicy pkh sContext = traceIfFalse "Missing signature!" $ 
                                    txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkh

redeemerPolicy :: Scripts.MintingPolicy
redeemerPolicy = mkMintingPolicyScript $
              $$(PlutusTx.compile [|| mywrap ||])
        where 
            mywrap = Scripts.wrapMintingPolicy $ redeemerMintingPolicy 

redeemerCurSymbol :: CurrencySymbol
redeemerCurSymbol = scriptCurrencySymbol redeemerPolicy


--OFF-CHAIN

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmout     :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema) 

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton redeemerCurSymbol (mpTokenName mp) (mpAmout mp)
        lookups = Constraints.mintingPolicy $ redeemerPolicy
        tx      = Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData pkh) val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema
mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
       h1 <- activateContractWallet (knownWallet 1) endpoints
       h2 <- activateContractWallet (knownWallet 2) endpoints
       callEndpoint @"mint" h1 $ MintParams
                        { mpTokenName = "Batch64Koin" 
                        , mpAmout     = 1100
                        }
       void $ Emulator.waitNSlots 10
       callEndpoint @"mint" h2 $ MintParams
                        { mpTokenName = "Batch64coin"
                        , mpAmout     = 2200   
                        }
       void $ Emulator.waitNSlots 10