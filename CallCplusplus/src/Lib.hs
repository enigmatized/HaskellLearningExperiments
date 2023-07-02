{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGAUGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE BangPatterns         #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Media ((//), (/:))
import Codec.Picture
import Codec.Picture.Png
import qualified  Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (liftIO)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
      :<|> "image" :> Get '[PNG] LBS.ByteString



data PNG deriving stock (Typeable)

instance Accept PNG where
    contentType _ = "image" // "png"

instance MimeRender PNG LBS.ByteString where
    mimeRender _ = id -- LBS.toStrict

data JPEG deriving stock (Typeable)

instance Accept JPEG where
    contentType _ = "image" // "jpeg"

instance MimeRender JPEG LBS.ByteString where
    mimeRender _ = id -- LBS.toStrict

startApp :: IO ()
startApp = run 8092 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  users :<|> serveImage'

generatePixel :: Int -> Int -> PixelRGB8
generatePixel x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 0

serveImage :: Handler DynamicImage
serveImage = do
  -- Load the PNG image from file or generate it programmatically
  let image = generateImage generatePixel 256 256
  pure $ ImageRGB8 image





serveImage' :: Handler  LBS.ByteString
serveImage' = do
  -- Read the PNG file
  let imageFilePath = "files.png"
  eitherImage <- liftIO $ readImage imageFilePath

  case eitherImage of
    Left err -> throwError err400 { errBody = "Failed to read image: " <> LBS.pack (map (fromIntegral . fromEnum) err) }
    Right dynamicImage ->
      case dynamicImage of
        ImageRGBA16 image -> do
          -- Convert the image to PNG format
          --let pngImage = ImageRGB8 image
          let !imageRGBA8 = convertRGB16 dynamicImage
          let !pngBytes = encodePng imageRGBA8
          --let pngBytes = encodePng pngImage

          -- Set the response headers
          --let dispositionHeader = "attachment; filename=image.png"
          --let headers = addHeader dispositionHeader $ addHeader "image/png" $ addHeaderLenient (show $ BS.length pngBytes) $ emptyHeaders

          -- Return the image as the response body
          --pure $ headers `addHeader` pngBytes
          pure $ pngBytes
        ImageRGBA16 image -> do
          let !imageRGBA8 = convertRGB8 dynamicImage
          let !pngBytes = encodePng imageRGBA8
          pure $ pngBytes
        ImageRGBA8 image -> do
          let !imageRGBA8 = convertRGB8 dynamicImage
          let !pngBytes = encodePng imageRGBA8
          pure $ pngBytes

        ImageY8 image -> throwError err400 { errBody = "Invalid image format. Only y8 format is supported." }
        ImageY16 image -> throwError err400 { errBody = "Invalid image format. Only y16 format is supported." }
        ImageY32 image -> throwError err400 { errBody = "Invalid image format. Only y32 format is supported." }
        ImageYF image -> throwError err400 { errBody = "Invalid image format. Only yf format is supported." }
        ImageYA8 image -> throwError err400 { errBody = "Invalid image format. Only ya8 format is supported." }
        ImageYA16 image -> throwError err400 { errBody = "Invalid image format. Only ya16 format is supported." }
        ImageRGB8 image -> throwError err400 { errBody = "Invalid image format. Only ya16 format is supported." }
        --ImageRGB16 image -> throwError err400 { errBody = "Invalid image format. Only rgb8 format is supported." }
        ImageRGBF image -> throwError err400 { errBody = "Invalid image format. Only rgbf format is supported." }
        --ImageRGBA8 image -> throwError err400 { errBody = "Invalid image format. Only rgba8 format is supported." }
        --ImageRGBA16 image -> throwError err400 { errBody = "Invalid image format. Only y8 format is supported." }
        ImageYCbCr8 image -> throwError err400 { errBody = "Invalid image format. Only cr8 format is supported." }
        ImageCMYK8 image -> throwError err400 { errBody = "Invalid image format. Only cmyk8 format is supported." }
        ImageCMYK16 image -> throwError err400 { errBody = "Invalid image format. Only cmyk16 format is supported." }
      

        ImageRGB8 image -> do
          -- Convert the image to PNG format
          let pngImage = ImageRGB8 image
          let imageRGBA8 = convertRGB8 pngImage
          let pngBytes = encodePng imageRGBA8
          --let pngBytes = encodePng pngImage

          -- Set the response headers
          --let dispositionHeader = "attachment; filename=image.png"
          --let headers = addHeader dispositionHeader $ addHeader "image/png" $ addHeaderLenient (show $ BS.length pngBytes) $ emptyHeaders

          -- Return the image as the response body
          --pure $ headers `addHeader` pngBytes
          pure $ pngBytes  
        --ImageY8 _ ->
          --throwError err400 { errBody = "Invalid image format. Only RGB8 format is supported." }






users :: Handler [User]
users = pure $ [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
