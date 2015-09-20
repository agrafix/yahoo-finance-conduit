yahoo-finance-conduit
=====

[![Build Status](https://travis-ci.org/agrafix/yahoo-finance-conduit.svg)](https://travis-ci.org/agrafix/yahoo-finance-conduit)
[![Hackage](https://img.shields.io/hackage/v/yahoo-finance-conduit.svg)](http://hackage.haskell.org/package/yahoo-finance-conduit)

## Intro

Hackage: [yahoo-finance-conduit](http://hackage.haskell.org/package/yahoo-finance-conduit)
Stackage: [yahoo-finance-conduit](https://www.stackage.org/package/yahoo-finance-conduit)

Streaming aproach to the yahoo finance api


## Library Usage Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.Finance.Yahoo

main :: IO ()
main =
    stockQuoteSource [Symbol "AAPL", Symbol "GOOG", Symbol "MSFT"] $$ simpleStockPrinter

```

## Install

* Using cabal: `cabal install yahoo-finance-conduit`
* Using Stack: `stack install yahoo-finance-conduit`
* From Source (cabal): `git clone https://github.com/agrafix/yahoo-finance-conduit.git && cd yahoo-finance-conduit && cabal install`
* From Source (stack): `git clone https://github.com/agrafix/yahoo-finance-conduit.git && cd yahoo-finance-conduit && stack build`


## Misc

### Supported GHC Versions

* 7.8.4
* 7.10.2

### License

Released under the MIT license.
(c) 2014 - 2015 Alexander Thiemann <mail@agrafix.net>
