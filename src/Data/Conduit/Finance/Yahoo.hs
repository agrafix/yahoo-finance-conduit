{-# LANGUAGE OverloadedStrings #-}
module Data.Conduit.Finance.Yahoo
    ( Symbol (..), StockTime (..), StockQuote (..)
    , stockQuoteSource
    )
where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Lens
import Control.Monad.Trans
import Data.Conduit
import Data.Csv
import Data.Monoid
import Network.Wreq
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Text as T
import qualified Data.Vector as V

newtype Symbol =
    Symbol { unSymbol :: T.Text }
    deriving (Show, Ord, Eq)

data StockTime
   = StockTime
   { st_hour :: Int
   , st_min :: Int
   } deriving (Show, Ord, Eq)

data StockQuote
   = StockQuote
   { sq_symbol :: Symbol
   , sq_name :: T.Text
   , sq_time :: StockTime
   , sq_ask :: Double
   , sq_bid :: Double
   } deriving (Show, Ord, Eq)

instance FromRecord StockQuote where
    parseRecord v
        | V.length v == 5 =
            StockQuote <$>
            (Symbol <$> v .! 0) <*>
            v .! 1 <*>
            v .! 4 <*>
            v .! 2 <*>
            v .! 3
        | otherwise = fail "Can't parse as StockQuote"

instance FromField StockTime where
    parseField bs =
        case P.parseOnly (dateP <* P.endOfInput) bs of
          Left er -> fail er
          Right val -> return val

dateP :: P.Parser StockTime
dateP =
    do hour <- P.decimal
       _ <- P.string ":"
       minute <-  P.decimal
       amOrPm <- (P.string "am" <|>  P.string "pm")
       let hour' = if amOrPm == "pm" then hour + 12 else hour
       return (StockTime hour' minute)

stockQuoteSource :: MonadIO m => [Symbol] -> Source m StockQuote
stockQuoteSource symbols =
    loop
    where
      loop =
          do r <- liftIO $ get endPoint
             let mVals = decode NoHeader (r ^. responseBody)
             case mVals of
               Left err ->
                   liftIO $ putStrLn ("Error: Failed to parse finance api response: " ++ err)
               Right vals ->
                   mapM_ yield (V.toList vals)
             liftIO $ threadDelay (9 * 100000000) -- 15 minutes
      stockQ = T.intercalate "+" (map unSymbol symbols)
      endPoint =
          T.unpack $ "http://finance.yahoo.com/d/quotes.csv?s=" <> stockQ <> "&f=snabt1"
