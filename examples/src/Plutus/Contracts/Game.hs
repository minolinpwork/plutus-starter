{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- | A guessing game. A simplified version of 'Plutus.Contract.GameStateMachine'
-- not using 'Plutus.Contract.StateMachine' and using `yieldUnbalancedTx' for
-- balancing, signing and submitting transactions.
--
-- Currently, remote wallets (anything other than WBE) can only handles
-- `yieldUnbalancedTx` requests, and not `balanceTx`, `signTx` and `submitTx`
-- requests.
module Plutus.Contracts.Game
    ( contract
    , GameParam(..)
    , GameSchema
    , LockArgs(..)
    , GuessArgs(..)
    -- * Scripts
    , gameScript
    , hashString
    , clearString
    , HashedString
    , ClearString
    , gameInstance
    , mkValidator
    -- * Address
    , gameAddress
    , successTrace
    , tsuccessTrace
    ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as C
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Ledger (Address, POSIXTime, PaymentPubKeyHash, ScriptContext, TxOutRef, Value)
import qualified Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Ledger.Tx (ChainIndexTxOut (..))
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract 
import Plutus.Contract (AsContractError, Contract, ContractError, Endpoint, Promise, adjustUnbalancedTx, endpoint, fundsAtAddressGeq,
                        logInfo, mkTxConstraints, selectList, type (.\/), yieldUnbalancedTx)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.V1.Ledger.Scripts (Datum (Datum), Validator)
import qualified PlutusTx 
import PlutusTx.Prelude hiding (pure, (<$>))
import qualified Prelude as Haskell

import           Plutus.Trace.Emulator (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Trace
import Data.Text (Text)
import qualified Ledger.TimeSlot  as TimeSlot
import Plutus.Contract.Test (checkPredicate, goldenPir, mockWalletPaymentPubKeyHash, reasonable', valueAtAddress, w1,
                             w2, walletFundsChange, (.&&.))
import Data.Default (Default (def))
import Control.Monad (void)

import Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

-- | Datatype for creating a parameterized validator.
data GameParam = GameParam
    { gameParamPayeePkh  :: PaymentPubKeyHash
    -- ^ Payment public key hash of the wallet locking some funds
    , gameParamStartTime :: POSIXTime
    -- ^ Starting time of the game
    } deriving (Haskell.Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''GameParam

newtype HashedString = HashedString BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

type GameSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "guess" GuessArgs

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

-- | The address of the game (the hash of its validator script)
gameAddress :: GameParam -> Address
gameAddress = mkValidatorAddress . gameValidator

-- | The validator script of the game.
gameValidator :: GameParam -> Validator
gameValidator = Scripts.validatorScript . gameInstance

gameInstance :: GameParam -> Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidatorParam @Game
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @HashedString @ClearString

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
--
-- The 'GameParam' parameter is not used in the validation. It is meant to
-- parameterize the script address depending based on the value of 'GaramParam'.
{-# INLINABLE mkValidator #-}
mkValidator :: GameParam -> HashedString -> ClearString -> ScriptContext -> Bool
mkValidator _ hs cs _ = isGoodGuess hs cs

{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Haskell.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

-- | Arguments for the @"lock"@ endpoint
data LockArgs =
    LockArgs
        { lockArgsGameParam :: GameParam
        -- ^ The parameters for parameterizing the validator.
        , lockArgsSecret    :: Haskell.String -- SecretArgument Haskell.String
        -- ^ The secret
        , lockArgsValue     :: Value
        -- ^ Value that is locked by the contract initially
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs =
    GuessArgs
        { guessArgsGameParam :: GameParam
        -- ^ The parameters for parameterizing the validator.
        , guessArgsSecret    :: Haskell.String
        -- ^ The guess
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | The "lock" contract endpoint. See note [Contract endpoints]
lock :: AsContractError e => Promise () GameSchema e ()
lock = endpoint @"lock" $ \LockArgs { lockArgsGameParam, lockArgsSecret, lockArgsValue } -> do
    logInfo @Haskell.String $ "Pay " <> Haskell.show lockArgsValue <> " to the script"
    let lookups = Constraints.plutusV1TypedValidatorLookups (gameInstance lockArgsGameParam)
        tx       = Constraints.mustPayToTheScript (hashString lockArgsSecret) lockArgsValue
    mkTxConstraints lookups tx >>= adjustUnbalancedTx >>= yieldUnbalancedTx

-- | The "guess" contract endpoint. See note [Contract endpoints]
guess :: AsContractError e => Promise () GameSchema e ()
guess = endpoint @"guess" $ \GuessArgs { guessArgsGameParam, guessArgsSecret } -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq (gameAddress guessArgsGameParam) (Ada.lovelaceValueOf 1)

    let lookups = Constraints.plutusV1TypedValidatorLookups (gameInstance guessArgsGameParam)
               Haskell.<> Constraints.unspentOutputs utxos
        redeemer = clearString guessArgsSecret
        tx       = Constraints.collectFromTheScript utxos redeemer

    unbalancedTx <- mkTxConstraints lookups tx
    yieldUnbalancedTx unbalancedTx

-- | Find the secret word in the Datum of the UTxOs
findSecretWordValue :: Map TxOutRef ChainIndexTxOut -> Maybe HashedString
findSecretWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output is possible
secretWordValue :: ChainIndexTxOut -> Maybe HashedString
secretWordValue o = do
  Datum d <- snd (_ciTxOutScriptDatum o)
  PlutusTx.fromBuiltinData d

contract :: AsContractError e => Contract () GameSchema e ()
contract = do
    logInfo @Haskell.String "Waiting for lock or guess endpoint..."
    selectList [lock, guess] >> contract


gameParam :: GameParam
gameParam = GameParam (mockWalletPaymentPubKeyHash w1) (TimeSlot.scSlotZeroTime def)

successTrace :: EmulatorTrace ()
successTrace = do
    hdl <- Trace.activateContractWallet w1 $ contract @Text
    Trace.callEndpoint @"lock" hdl LockArgs { lockArgsGameParam = gameParam
                                            , lockArgsSecret = "hello"
                                            , lockArgsValue = Ada.adaValueOf 8
                                            }
    -- One slot for sending the Ada to the script.
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 $ contract @Text
    Trace.callEndpoint @"guess" hdl2 GuessArgs { guessArgsGameParam = gameParam
                                               , guessArgsSecret = "hello"
                                               }
    void $ Trace.waitNSlots 1

tsuccessTrace :: IO ()
tsuccessTrace = Trace.runEmulatorTraceIO successTrace

script :: Plutus.Script
script = Plutus.unValidatorScript $ gameValidator gameParam

gameScriptShortBs :: SBS.ShortByteString
gameScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

gameScript :: PlutusScript PlutusScriptV1
gameScript = PlutusScriptSerialised gameScriptShortBs