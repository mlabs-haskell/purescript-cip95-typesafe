module Test.Main where

import Prelude

import Cardano.Wallet.Cip95.TypeSafe
  ( TxSignErrorTag
      ( TxSignErrorProofGeneration
      , TxSignErrorUserDeclined
      , TxSignErrorDepreciatedCertificate
      )
  , getPubDrepKey
  , signTx
  )
import Control.Monad.Error.Class (try)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.Variant (inj)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (message, throw)
import Test.Inject (mkApi, throwingApi)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(Proxy))

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ] do
    describe "CIP-95 TypeSafe API" do
      it "passes the exception through if it is unknown" do
        try (getPubDrepKey $ mkApi (throw "hi"))
          >>= lmap message >>> shouldEqual
            ( Left
                "CIP-95 getPubDrepKey: unable to match error with specification: hi"
            )
      it "passes the exception through if no info string is specified" do
        try (getPubDrepKey $ throwingApi { code: -1 })
          >>= lmap message >>> shouldEqual
            ( Left
                "CIP-95 getPubDrepKey: unable to match error with specification: undefined"
            )
      it "passes the exception through if error code is unknown" do
        try (getPubDrepKey $ throwingApi { code: -5, info: "hiii" })
          >>= lmap message >>> shouldEqual
            ( Left
                "CIP-95 getPubDrepKey: unable to match error with specification: undefined"
            )
      -- Tests only TxSignError since the other errors are already tested in
      -- purescript-cip30-typedef
      describe "catching TxSignError variants" do
        let errorMessage = "error message"
        it "catches TxSignErrorProofGeneration" do
          (signTx (throwingApi { info: errorMessage, code: 1 }) "" false)
            >>= shouldEqual
              ( inj (Proxy :: Proxy "txSignError")
                  { code: TxSignErrorProofGeneration, info: errorMessage }
              )
        it "catches TxSignErrorUserDeclined" do
          (signTx (throwingApi { info: errorMessage, code: 2 }) "" false)
            >>= shouldEqual
              ( inj (Proxy :: Proxy "txSignError")
                  { code: TxSignErrorUserDeclined, info: errorMessage }
              )
        it "catches TxSignErrorDepreciatedCertificate" do
          (signTx (throwingApi { info: errorMessage, code: 3 }) "" false)
            >>= shouldEqual
              ( inj (Proxy :: Proxy "txSignError")
                  { code: TxSignErrorDepreciatedCertificate, info: errorMessage }
              )
