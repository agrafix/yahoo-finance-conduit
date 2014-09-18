yahoo-finance-conduit
=====

# Intro

Hackage: http://hackage.haskell.org/package/yahoo-finance-conduit

A very simple conduit streaming stock market data from the yahoo finance api.

# Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.Finance.Yahoo
import qualified Data.Conduit.List as CL

main :: IO ()
main =
    stockQuoteSource [Symbol "AAPL", Symbol "GOOG", Symbol "MSFT"] $$ CL.mapM_ print
```

# Install

* Using cabal: `cabal install yahoo-finance-conduit`
* From Source: `git clone https://github.com/agrafix/yahoo-finance-conduit.git && cd yahoo-finance-conduit && cabal install`
