import Prelude

import Cardano.Api
import           Cardano.Api.Shelley

import System.Directory
import System.FilePath.Posix ((</>))

import Plutus.Contracts.LottoV2 (gameScript, LockParams(..), GuessParams(..), hashString, clearString, HashedString, ClearString)

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Aeson (encode)
import qualified Ledger.Ada            as Ada

main :: IO ()
main = do
  let v1dir = "generated-plutus-scripts/v1"
      v2dir = "generated-plutus-scripts/v2"
  createDirectoryIfMissing True v1dir
  createDirectoryIfMissing True v2dir


  let hashedStringPass = hashString "HelloWorld"
      clearStringPass = clearString "HelloWorld"
      clearStringFail = clearString "Failling"
      lockParams = LockParams "HelloWorld" (Ada.adaValueOf 10)
      guessParams = GuessParams "HelloWorld"
  LBS.writeFile (v1dir </> "result.datum") $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData hashedStringPass))
  LBS.writeFile (v1dir </> "result.redeem") $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData clearStringPass))
  LBS.writeFile (v1dir </> "result.fail.redeem") $ encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (Plutus.toData clearStringFail))
  result <- writeFileTextEnvelope (v1dir </> "result.plutus") Nothing gameScript

  case result of
    Left err -> print $ displayError err
    Right () -> return ()
