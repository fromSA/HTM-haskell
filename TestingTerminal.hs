{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL

instance FromJSON EncoderType

instance FromJSON EncoderConfig

main :: IO ()
main = do
    args <- getArgs
    let maybeEncodings = map (decode . BL.pack) args :: [Maybe EncoderConfig]
    mapM_ handleEncoding maybeEncodings

handleEncoding :: Maybe EncoderConfig -> IO ()
handleEncoding Nothing = putStrLn "Error: could not parse JSON."
handleEncoding (Just p) = putStrLn $ "The parsed encoding is: " ++ show p