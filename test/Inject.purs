module Test.Inject (mkApi, throwingApi) where

import Cardano.Wallet.Cip95 (Api)
import Effect (Effect)

foreign import mkApi :: forall a. Effect a -> Api
foreign import throwingApi :: forall a. a -> Api
