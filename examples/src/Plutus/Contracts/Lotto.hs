{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


-- | A guessing game
module Plutus.Contracts.Lotto
    ( lock
    , guess
    , game
    , GameSchema
    , GuessParams(..)
    , LockParams(..)
    -- * Scripts
    , gameValidator
    , hashString
    , clearString
    -- * Address
    , gameAddress
    , validateGuess
    -- * Traces
    , guessTrace
    , lockTrace
    , correctGuessTrace
    , tlockTrace
    , tguessTrace
    , tcorrectGuessTrace
    , script
    , gameScript
    , HashedString
    , ClearString
    , tbadGuessTrace
    ) where

import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import Data.Text              (Text, unpack)
import           Ledger                (Address, Datum (Datum), ScriptContext, Validator, Value)
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Tx             (ChainIndexTxOut (..))
import qualified Ledger.Typed.Scripts  as Scripts
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import           Playground.Contract
import           Plutus.Contract
import           Plutus.Contract.Trace as X
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell
import           Plutus.Trace.Emulator (EmulatorTrace)
import qualified Plutus.Trace.Emulator as Trace

import Codec.Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Plutus.V1.Ledger.Scripts  as Plutus
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

type GameSchema =
        Endpoint "lock" LockParams
        .\/ Endpoint "guess" GuessParams

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @HashedString @ClearString

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack 

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Haskell.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
{-# INLINABLE validateGuess #-}
validateGuess :: HashedString -> ClearString -> ScriptContext -> Bool
validateGuess hs cs _ = traceIfFalse "Min Bad Guess" $ isGoodGuess hs cs

{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
--isGoodGuess (HashedString actual) (ClearString guess') = False
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = mkValidatorAddress gameValidator

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { secretWord :: Haskell.String
    , amount     :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "guess" endpoint
newtype GuessParams = GuessParams
    { guessWord :: Haskell.String
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | The "lock" contract endpoint. See note [Contract endpoints]
lock :: AsContractError e => Promise () GameSchema e ()
lock = endpoint @"lock" @LockParams $ \(LockParams secret amt) -> do
    logInfo @Haskell.String $ "Pay " <> Haskell.show amt <> " to the script"
    let tx         = Constraints.mustPayToTheScript (hashString secret) amt
    void (submitTxConstraints gameInstance tx)

-- | The "guess" contract endpoint. See note [Contract endpoints]
guess :: AsContractError e => Promise () GameSchema e ()
guess = endpoint @"guess" @GuessParams $ \(GuessParams theGuess) -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)

    let redeemer = clearString theGuess
        tx       = Constraints.collectFromTheScript utxos redeemer

    -- Log a message saying if the secret word was correctly guessed
    let hashedSecretWord = findSecretWordValue utxos
        isCorrectSecretWord = fmap (`isGoodGuess` redeemer) hashedSecretWord == Just True
    if isCorrectSecretWord
        then logWarn @Haskell.String "Correct secret word! Submitting the transaction"
        else logWarn @Haskell.String "Incorrect secret word, but still submiting the transaction"

    -- This is only for test purposes to have a possible failing transaction.
    -- In a real use-case, we would not submit the transaction if the guess is
    -- wrong.
    logInfo @Haskell.String "Submitting transaction to guess the secret word"
    handleError (\err -> logInfo $ "Min caught error: " ++ unpack err) $ void (submitTxConstraintsSpending gameInstance utxos tx)

-- | Find the secret word in the Datum of the UTxOs
findSecretWordValue :: Map TxOutRef ChainIndexTxOut -> Maybe HashedString
findSecretWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output is possible
secretWordValue :: ChainIndexTxOut -> Maybe HashedString
secretWordValue o = do
  Datum d <- snd (_ciTxOutScriptDatum o)
  PlutusTx.fromBuiltinData d

game :: AsContractError e => Contract () GameSchema e ()
game = do
    logInfo @Haskell.String "Waiting for guess or lock endpoint..."
    selectList [lock, guess] >> game

lockTrace :: Wallet -> Haskell.String -> EmulatorTrace ()
lockTrace wallet secretWord = do
    hdl <- Trace.activateContractWallet wallet (lock @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"lock" hdl (LockParams secretWord (Ada.adaValueOf 10))
    void $ Trace.waitNSlots 1

guessTrace :: Wallet -> Haskell.String -> EmulatorTrace ()
guessTrace wallet guessWord = do
    hdl <- Trace.activateContractWallet wallet (guess @ContractError)
    void $ Trace.waitNSlots 1
    Trace.callEndpoint @"guess" hdl (GuessParams guessWord)
    void $ Trace.waitNSlots 1

correctGuessTrace :: EmulatorTrace ()
correctGuessTrace = do
  let w1 = X.knownWallet 1
      w2 = X.knownWallet 2
      secret = "secret"

  h1 <- Trace.activateContractWallet w1 (lock @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"lock" h1 (LockParams secret (Ada.adaValueOf 10))

  h2 <- Trace.activateContractWallet w2 (guess @ContractError)
  void $ Trace.waitNSlots 1
  Trace.callEndpoint @"guess" h2 (GuessParams secret)
  void $ Trace.waitNSlots 1

badGuessTrace :: EmulatorTrace ()
badGuessTrace = do
  let w1 = X.knownWallet 1
      w2 = X.knownWallet 2
      w3 = X.knownWallet 3
      --secret = "F31AFF304C92BABC7F0EA09E322FDB78BAF4CA8A723FABD0EC2288B35C5C5AC2"
      secret = "{\"name\":\"S1\",\"maxNo\":26,\"maxChoices\":2,\"selected\":[16,18]}"
      --secret2 = "{\"name\":\"S1\",\"maxNo\":26,\"maxChoices\":2,\"selected\":[16,18]}"
      secret2 = "{\"name\":\"S1\",\"maxNo\":26,\"maxChoices\":2,\"selected\":[16,18]}"

  void $ Trace.waitNSlots 10
  h1 <- Trace.activateContractWallet w1 (lock @ContractError)
  void $ Trace.waitNSlots 10
  Trace.callEndpoint @"lock" h1 (LockParams secret (Ada.adaValueOf 5))
  void $ Trace.waitNSlots 10

  h2 <- Trace.activateContractWallet w2 (guess @ContractError)
  void $ Trace.waitNSlots 10
  Trace.callEndpoint @"guess" h2 (GuessParams secret2)
  void $ Trace.waitNSlots 10

  h3 <- Trace.activateContractWallet w3 (guess @ContractError)
  void $ Trace.waitNSlots 10
  Trace.callEndpoint @"guess" h3 (GuessParams secret2)
  void $ Trace.waitNSlots 10
  Trace.callEndpoint @"guess" h3 (GuessParams secret2)
  void $ Trace.waitNSlots 10

tlockTrace :: IO ()
tlockTrace = do
  let w1 = X.knownWallet 1
      secret = "secret"
  Trace.runEmulatorTraceIO (lockTrace w1 secret)

tguessTrace :: IO ()
tguessTrace = do
  let w1 = X.knownWallet 1
      secret = "secret"
  Trace.runEmulatorTraceIO (guessTrace w1 secret)

tcorrectGuessTrace :: IO ()
tcorrectGuessTrace = Trace.runEmulatorTraceIO correctGuessTrace

tbadGuessTrace :: IO ()
tbadGuessTrace = Trace.runEmulatorTraceIO badGuessTrace

script :: Plutus.Script
script = Plutus.unValidatorScript gameValidator

gameScriptShortBs :: SBS.ShortByteString
gameScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

gameScript :: PlutusScript PlutusScriptV1
gameScript = PlutusScriptSerialised gameScriptShortBs