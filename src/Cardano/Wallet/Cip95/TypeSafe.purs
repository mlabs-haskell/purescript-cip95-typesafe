module Cardano.Wallet.Cip95.TypeSafe
  ( module Cip30
  , module X
  , TxSignErrorTag
      ( TxSignErrorProofGeneration
      , TxSignErrorUserDeclined
      , TxSignErrorDepreciatedCertificate
      )
  , TxSignError
  , enable
  , getPubDrepKey
  , getRegisteredPubStakeKeys
  , getUnregisteredPubStakeKeys
  , signTx
  , signData
  ) where

import Prelude

import Cardano.Wallet.Cip30 (Bytes, Cbor, DataSignature)
import Cardano.Wallet.Cip30.TypeSafe
  ( APIError
  , APIErrorTag
      ( APIErrorInvalidRequest
      , APIErrorInternalError
      , APIErrorRefused
      , APIErrorAccountChange
      )
  , DataSignError
  , DataSignErrorTag
      ( DataSignErrorProofGeneration
      , DataSignErrorAddressNotPK
      , DataSignErrorUserDeclined
      )
  , _apiError
  , _dataSignError
  , _success
  , _txSignError
  ) as Cip30
import Cardano.Wallet.Cip95
  ( Api
  , PubDrepKey
  , PubStakeKey
  , WalletName
  , enable
  , getPubDrepKey
  , getRegisteredPubStakeKeys
  , getUnregisteredPubStakeKeys
  , signData
  , signTx
  ) as Cip95
import Cardano.Wallet.Cip95 (PubDrepKey, PubStakeKey, WalletName) as X
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (throwError)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.Variant (Variant, expand, inj)
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Prim.Row (class Union)

-- CIP-95 extends CIP-30 TxSignError.

data TxSignErrorTag
  = TxSignErrorProofGeneration
  | TxSignErrorUserDeclined
  | TxSignErrorDepreciatedCertificate

derive instance Eq TxSignErrorTag
derive instance Generic TxSignErrorTag _

instance Show TxSignErrorTag where
  show = genericShow

type TxSignError =
  { code :: TxSignErrorTag
  , info :: String
  }

-- | Enables wallet and reads CIP-95 API if the wallet is available.
enable
  :: Cip95.WalletName
  -> Aff (Variant (success :: Cip95.Api, apiError :: Cip30.APIError))
enable wallet = catchCode "enable" (Cip95.enable wallet)
  (inj Cip30._success)
  apiErrorMatcher

-- | Get the public DRep key associated with the connected wallet.
getPubDrepKey
  :: Cip95.Api
  -> Aff (Variant (apiError :: Cip30.APIError, success :: Cip95.PubDrepKey))
getPubDrepKey api = catchCode "getPubDrepKey" (Cip95.getPubDrepKey api)
  (inj Cip30._success)
  apiErrorMatcher

-- | Get the registered public stake keys associated with the
-- | connected wallet.
getRegisteredPubStakeKeys
  :: Cip95.Api
  -> Aff (Variant (apiError :: Cip30.APIError, success :: Array Cip95.PubStakeKey))
getRegisteredPubStakeKeys api = catchCode "getRegisteredPubStakeKeys"
  (Cip95.getRegisteredPubStakeKeys api)
  (inj Cip30._success)
  apiErrorMatcher

-- | Get the unregistered public stake keys associated with the
-- | connected wallet.
getUnregisteredPubStakeKeys
  :: Cip95.Api
  -> Aff (Variant (apiError :: Cip30.APIError, success :: Array Cip95.PubStakeKey))
getUnregisteredPubStakeKeys api = catchCode "getUnregisteredPubStakeKeys"
  (Cip95.getUnregisteredPubStakeKeys api)
  (inj Cip30._success)
  apiErrorMatcher

-- | Request the wallet to inspect and provide appropriate witnesses
-- | for the supplied transaction.
signTx
  :: Cip95.Api
  -> Cbor
  -> Boolean
  -> Aff
       ( Variant
           (apiError :: Cip30.APIError, txSignError :: TxSignError, success :: Cbor)
       )
signTx api tx isPartialSign = catchCode "signTx"
  (Cip95.signTx api tx isPartialSign)
  (inj Cip30._success)
  (apiErrorMatcher `combineErrorMatchers` txSignErrorMatcher)

-- | Request the wallet to inspect and provide a cryptographic
-- | signature for the supplied data.
signData
  :: Cip95.Api
  -> String
  -> Bytes
  -> Aff
       ( Variant
           ( apiError :: Cip30.APIError
           , dataSignError :: Cip30.DataSignError
           , success :: DataSignature
           )
       )
signData api addrOrDrepId payload = catchCode "signData"
  (Cip95.signData api addrOrDrepId payload)
  (inj Cip30._success)
  (apiErrorMatcher `combineErrorMatchers` dataSignErrorMatcher)

-- Error matching machinery -- slighted modified from `mlabs/purescript-cip30-typesafe`
-- TODO: Reutilize this code. We can potentially create a module
-- `Cardano.Wallet.Cip30.TypeSafe.ErrorHandling` and use it here.
-- (we could even just have a stand-alone `Cardano.Wallet.TypeSafe.ErrorHandling`
-- module, but we have so little code that I'm not sure if it is worth it.)

-- | A known error is either a pagination error or an error with code and
-- | message.
-- | This type represents error info extracted at runtime, that is yet
-- | uninterpreted.
-- | `ErrorMatcher` can be used to dispatch on these values, taking the
-- | current CIP-95 endpoint into account.
type ErrorData = Either { maxSize :: Int } { code :: Int, info :: String }

-- | An `ErrorMatcher` is a function that tries to match a known error with
-- | an error `Variant` based on `ErrorData`.
newtype ErrorMatcher (row :: Row Type) = ErrorMatcher
  (ErrorData -> Maybe (Variant row))

-- | `ErrorMatcher`s can be joined: e.g `Cip30.APIError` and `TxSignError` have
-- | non-intersecting error codes, so we can build an `ErrorMatcher` dispatcher
-- | that tries first and then the second.
combineErrorMatchers
  :: forall row1 row2 row3
   . Union row1 row2 row3
  => Union row2 row1 row3
  => ErrorMatcher row1
  -> ErrorMatcher row2
  -> ErrorMatcher row3
combineErrorMatchers (ErrorMatcher f1) (ErrorMatcher f2) =
  ErrorMatcher \errorData ->
    expand <$> f1 errorData <|> expand <$> f2 errorData

-- | Uses `ErrorMatcher` to transform a `purescript-cip95` function into a
-- | function 'enriched' with error type variants. Arguments:
-- |
-- | - CIP-95 method name
-- | - `Aff` action
-- | - A function that injects successful result into the row
-- | - `ErrorMatcher` that captures known errors
-- |
-- | It works like this:
-- | - call the `Aff` action
-- |   - If no exception, inject it into `success` variant.
-- |   - if there is an exception, run the error matcher
-- |     - If it is successful, return the "enriched" `(Variant row)`
-- |     - If it fails, re-throw the error
catchCode
  :: forall a errorRow row
   . Union errorRow (success :: a) row
  => String
  -> Aff a
  -> (a -> Variant row)
  -> (ErrorMatcher errorRow)
  -> Aff (Variant row)
catchCode functionName action handleSuccess (ErrorMatcher handleException) = do
  -- run the action
  (action >>= handleSuccess >>> pure)
    `catchError` \errorValue -> do
      -- extract all needed information from the thrown value
      mbErrorTagInt <- liftEffect $ _getErrorTagInt Nothing Just errorValue
      mbErrorInfoString <- liftEffect $ _getErrorInfoString Nothing Just
        errorValue
      mbPaginateErrorMaxSize <- liftEffect $ _getPaginateError Nothing Just
        errorValue
      let
        mbErrorData =
          -- Figure out which type of ErrorData is this, if any
          case mbErrorTagInt, mbErrorInfoString, mbPaginateErrorMaxSize of
            -- error with error code and info message
            Just errorTagInt, Just errorInfoString, _ ->
              Just (Right { code: errorTagInt, info: errorInfoString })
            -- pagination error
            _, _, Just maxSize ->
              Just (Left { maxSize })
            -- unknown error we can't dispatch on
            _, _, _ -> Nothing
        -- will be thrown if we can't provide a recoverable error
        myBad = error $ "CIP-95 " <> functionName
          <> ": unable to match error with specification: "
          <> message errorValue
      exception <- liftMaybe myBad mbErrorData
      -- run the matcher and see if it is able to recover the error
      case expand <$> handleException exception of
        Nothing -> liftEffect $ throwError myBad
        Just res -> pure res

-- | Tries to get `error.code` number
foreign import _getErrorTagInt
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Error
  -> Effect (Maybe Int)

-- | Tries to get `error.code` number
foreign import _getErrorInfoString
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Error
  -> Effect (Maybe String)

-- | Tries to get `error.maxSize` number
foreign import _getPaginateError
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Error
  -> Effect (Maybe Int)

-- Error matchers. They correspond to error types in CIP-95

apiErrorMatcher :: ErrorMatcher (apiError :: Cip30.APIError)
apiErrorMatcher = ErrorMatcher
  case _ of
    Left _ -> skip
    Right { info, code: (-1) } -> match Cip30.APIErrorInvalidRequest info
    Right { info, code: (-2) } -> match Cip30.APIErrorInternalError info
    Right { info, code: (-3) } -> match Cip30.APIErrorRefused info
    Right { info, code: (-4) } -> match Cip30.APIErrorAccountChange info
    _ -> skip
  where
  match err info = Just $ inj Cip30._apiError { info, code: err }
  skip = Nothing

txSignErrorMatcher :: ErrorMatcher (txSignError :: TxSignError)
txSignErrorMatcher = ErrorMatcher
  case _ of
    Left _ -> skip
    Right { info, code: 1 } -> match TxSignErrorProofGeneration info
    Right { info, code: 2 } -> match TxSignErrorUserDeclined info
    Right { info, code: 3 } -> match TxSignErrorDepreciatedCertificate info
    _ -> skip
  where
  match err info = Just $ inj Cip30._txSignError { info, code: err }
  skip = Nothing

dataSignErrorMatcher :: ErrorMatcher (dataSignError :: Cip30.DataSignError)
dataSignErrorMatcher = ErrorMatcher
  case _ of
    Left _ -> skip
    Right { info, code: 1 } -> match Cip30.DataSignErrorProofGeneration info
    Right { info, code: 2 } -> match Cip30.DataSignErrorAddressNotPK info
    Right { info, code: 3 } -> match Cip30.DataSignErrorUserDeclined info
    _ -> skip
  where
  match err info = Just $ inj Cip30._dataSignError
    { info, code: err }
  skip = Nothing

