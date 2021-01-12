module Main (main) where

import Criterion.Main
import Criterion.Types
import Control.Exception (evaluate)
import Control.DeepSeq (rnf)
import Data.Word (Word64)

import qualified Data.Vector as V
import qualified Data.Text as T

import Witherable

main :: IO ()
main = do
  evaluate (rnf strings)
  evaluate (rnf word64s)
  evaluate (rnf texts)

  evaluate (rnf stringsV)
  evaluate (rnf word64sV)
  evaluate (rnf textsV)

  defaultMainWith (defaultConfig { timeLimit = 2 })
    [ bgroup "word64s"
      [ bgroup "list"
        [ bench "ordNub"  $ nf ordNub  word64s
        , bench "hashNub" $ nf hashNub word64s
        ]
      , bgroup "vector"
        [ bench "ordNub"  $ nf ordNub  word64sV
        , bench "hashNub" $ nf hashNub word64sV
        ]
      ]
    , bgroup "string"
      [ bgroup "list"
        [ bench "ordNub"  $ nf ordNub  strings
        , bench "hashNub" $ nf hashNub strings
        ]
      , bgroup "vector"
        [ bench "ordNub"  $ nf ordNub  stringsV
        , bench "hashNub" $ nf hashNub stringsV
        ]
      ]
    , bgroup "fewstring"
      [ bgroup "list"
        [ bench "ordNub"  $ nf ordNub  fewstrings
        , bench "hashNub" $ nf hashNub fewstrings
        ]
      , bgroup "vector"
        [ bench "ordNub"  $ nf ordNub  fewstringsV
        , bench "hashNub" $ nf hashNub fewstringsV
        ]
      ]
    , bgroup "text"
      [ bgroup "list"
        [ bench "ordNub"  $ nf ordNub  texts
        , bench "hashNub" $ nf hashNub texts
        ]
      , bgroup "vector"
        [ bench "ordNub"  $ nf ordNub  textsV
        , bench "hashNub" $ nf hashNub textsV
        ]
      ]
    ]

word64s :: [Word64]
word64s = [1.. fromIntegral (length strings) ]

word64sV :: V.Vector Word64
word64sV = V.fromList word64s

fewstrings :: [String]
fewstrings = take 2000 $ cycle
  [ [x0,x1]
  | x0 <- chars
  , x1 <- chars
  ] where chars = ['A' .. 'Z']

fewstringsV :: V.Vector String
fewstringsV = V.fromList fewstrings

strings :: [String]
strings =
  [ [x0,x1,x2]
  | x0 <- chars
  , x1 <- chars
  , x2 <- chars
  ] where chars = ['A' .. 'Z'] ++ ['a' .. 'z' ]

stringsV :: V.Vector String
stringsV = V.fromList strings

texts :: [T.Text]
texts = fmap T.pack strings

textsV :: V.Vector T.Text
textsV = fmap T.pack stringsV
