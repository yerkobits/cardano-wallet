{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.HttpBridge.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, Network (..) )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , TxIn (..)
    , TxOut (..)
    , decodeAddress
    , encodeAddress
    , txId
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Data.Digest.CRC32
    ( crc32 )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, property, vectorOf, (===) )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS

{-# ANN spec ("HLint: ignore Use head" :: String) #-}

spec :: Spec
spec = do
    describe "Compatibility - HttpBridge (Mainnet)" $ do
        let proxy = Proxy @(HttpBridge 'Mainnet)
        it "decodeAddress . encodeAddress = pure" $ property $ \a ->
            decodeAddress proxy (encodeAddress proxy a) === Right a

        it "decodeAddress failure \"0000\"" $ do
            let err = "Unable to decode Address: expected Base58 encoding."
            decodeAddress proxy "0000" === Left (TextDecodingError err)

        it "decodeAddress failure \"EkxDbkPo\"" $ do
            let err = "Unable to decode Address: not a valid Byron address."
            decodeAddress proxy "EkxDbkPo" === Left (TextDecodingError err)

    describe "Compatibility - HttpBridge (Testnet)" $ do
        let proxy = Proxy @(HttpBridge 'Testnet)
        it "decodeAddress . encodeAddress = pure" $ property $ \a ->
            decodeAddress proxy (encodeAddress proxy a) === Right a

        it "decodeAddress failure \"0000\"" $ do
            let err = "Unable to decode Address: expected Base58 encoding."
            decodeAddress proxy "0000" === Left (TextDecodingError err)

        it "decodeAddress failure \"EkxDbkPo\"" $ do
            let err = "Unable to decode Address: not a valid Byron address."
            decodeAddress proxy "EkxDbkPo" === Left (TextDecodingError err)

    describe "TxId (Testnet)" $ do
        it "should compute correct txId (1)" $ do
            let inputId0 = Hash $ unsafeFromHex
                    "60dbb2679ee920540c18195a3d92ee9be50aee6ed5f891d92d51db8a76b02cd2"
            let address0 = Address $ unsafeFromHex
                    "82d818584283581c797eae689c4ae43f03e35e9460311f94be94bfc3ea\
                    \5a76d2f6c04808a101581e581c1afbc57540db15381fdacad79f50a10c\
                    \ff0c465f327a4f20951348f1001ae1a9cde7"
            let address1 = Address $ unsafeFromHex
                    "82d818584283581c3e8a0407c6de369cc1ad9394394442ee4f3a9c0aa3\
                    \901f6672f668b0a101581e581c1afbc57540db15782ccbedd74b6fbd51\
                    \d04e4104bfd451e64058d9f3001a806e7f0c"
            let tx = Tx
                    { inputs = [ TxIn { inputId = inputId0, inputIx = 3 } ]
                    , outputs =
                        [ TxOut { address = address0, coin = Coin  285000000 }
                        , TxOut { address = address1, coin = Coin 1810771919 }
                        ]
                    }
            let hash = txId @(HttpBridge 'Testnet) tx
            let hash' = Hash $ unsafeFromHex
                    "c470563001e448e61ff1268c2a6eb458\
                    \ace1d04011a02cb262b6d709d66c23d0"
            hash `shouldBe` hash'

        it "should compute correct txId (2)" $ do
            let inputId0 = Hash $ unsafeFromHex
                    "6967e2b5c3ad5ae07a9bd8d888f1836195a04f7a1cb4b6d083261870068fab1b"
            let inputId1 = Hash $ unsafeFromHex
                    "7064addc0968bccd7d57d2e7aa1e9c2f666d8387042483fc1e87200cfb96c8f1"
            let address0 = Address $ unsafeFromHex
                    "82d818584983581c33935c07c07e788c7cccace299584f8fd1384fca44\
                    \9cd1bc0fe5e1a2a201581e581c25f59ec34005e181d4af50411aa31f1a\
                    \4f5b7b8c63d4bda553f0369702451a4170cb17001a71518443"
            let address1 = Address $ unsafeFromHex
                    "82d818584983581c6a49d9d611eea0a9d8fc3219ccf5ade53b337b7f1e\
                    \0b824e28eb48b0a201581e581c27b756273d10060d450d2280fee8047c\
                    \7cd69e6b162a8a381eb5719002451a4170cb17001a96ee1bde"
            let tx = Tx
                    { inputs =
                        [ TxIn { inputId = inputId0, inputIx = 1 }
                        , TxIn { inputId = inputId1, inputIx = 0 }
                        ]
                    , outputs =
                        [ TxOut { address = address0, coin = Coin 1404176490 }
                        , TxOut { address = address1, coin = Coin 1004099328 }
                        ]
                    }
            let hash = txId @(HttpBridge 'Testnet) tx
            let hash' = Hash $ unsafeFromHex
                    "d30d37f1f8674c6c33052826fdc5bc19\
                    \8e3e95c150364fd775d4bc663ae6a9e6"
            hash `shouldBe` hash'


instance Arbitrary Address where
    arbitrary = genAddress (30, 100)

-- The address format on 'Staging' is the same as 'Mainnet'.
genAddress :: (Int, Int) -> Gen Address
genAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    payload <- BS.pack <$> vectorOf n arbitrary
    let crc = CBOR.toStrictByteString (CBOR.encodeWord32 $ crc32 payload)
    return $ Address (prefix <> payload <> crc)