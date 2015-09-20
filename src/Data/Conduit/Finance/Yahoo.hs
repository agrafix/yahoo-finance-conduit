{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Conduit.Finance.Yahoo
    ( Symbol (..), StockTime (..), StockQuote (..)
    , stockQuoteSource
    , simpleStockPrinter
    )
where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Lens
import Control.Monad.Trans
import Data.Conduit
import Data.Csv
import Data.Hashable
import Data.Monoid
import Network.Wreq
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

newtype Symbol =
    Symbol { unSymbol :: T.Text }
    deriving (Show, Ord, Eq, Hashable)

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

-- | Example stock printer, could be used as basic command line
-- stock ticker
simpleStockPrinter :: MonadIO m => Sink StockQuote m ()
simpleStockPrinter =
    do val <- await
       case val of
         Just quote ->
             do liftIO $ putStrLn $
                       T.unpack (sq_name quote) ++ ": Ask " ++ show (sq_ask quote) ++ " / Bid " ++ show (sq_bid quote)
                       ++ " (" ++ show (st_hour (sq_time quote)) ++ ":" ++ show (st_min (sq_time quote)) ++ ")"
                simpleStockPrinter
         Nothing ->
             liftIO $ putStrLn "Stock Quote Source terminated."

-- | Yield the current stock quotes for given symbols. Values will be
-- streamed every 15 minutes if they changed.
stockQuoteSource :: MonadIO m => [Symbol] -> Source m StockQuote
stockQuoteSource symbols =
    loop HM.empty
    where
      loop !st =
          do r <- liftIO $ get endPoint
             let mVals = decode NoHeader (r ^. responseBody)
             newState <-
                 case mVals of
                   Left err ->
                       fail ("Error: Failed to parse finance api response: " ++ err ++ ".\n" ++ show (r ^. responseBody))
                   Right vals ->
                       V.foldM' handleQuote st vals
             liftIO $ threadDelay (9 * 100000000) -- 15 minutes
             loop newState
      handleQuote hm quote =
          let yieldAndRemember =
                  do yield quote
                     return $ HM.insert (sq_symbol quote) quote hm
          in case HM.lookup (sq_symbol quote) hm of
               Nothing ->
                   yieldAndRemember
               Just v ->
                   if v == quote
                   then return hm
                   else yieldAndRemember
      stockQ = T.intercalate "+" (map unSymbol symbols)
      endPoint =
          T.unpack $ "http://finance.yahoo.com/d/quotes.csv?s=" <> stockQ <> "&f=snabt1"
