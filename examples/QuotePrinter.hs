{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.Finance.Yahoo

main :: IO ()
main =
    stockQuoteSource [Symbol "AAPL", Symbol "GOOG", Symbol "MSFT"] $$ simpleStockPrinter
