hadron
======

`hadron` is a library for dealing with HTTP at the protocol level. It
is the home of `HTTPRequest` and `HTTPResponse` types as well as
parsers and `QuickCheck` instances for same.

Motivation
==========

`hadron` is a response to a lack of exposed or general-purpose
functionality in other HTTP libraries in two primary areas. The first
is HTTP message types and parsers; the second is `QuickCheck` generators
for HTTP messages which achieve good coverage of what's permitted by
the protocol, driven by a need for a high level of assurance of
correctness of software which directly manipulates requests and
responses.

Example usage
=============

With raw bytestrings
--------------------

```haskell
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Hadron.Core.Request (parseHTTPRequest, renderHTTPRequest)

main = do
  BS.putStrLn "enter an HTTP request:"
  bs <- BS.getContents
  case parseHTTPRequest bs of
    Left e ->
      T.putStrLn $ "nope, try again: " <> renderRequestError e
        >> main
    Right r ->
      BS.putStrLn $ renderHTTPRequest r
```

From airship
------------

```haskell
import Hadron.Wai (toHTTPRequest)
import Hadron.Core.Data.Request (renderHTTPRequest)

hadronReq <- firstEitherT MyErrorType $ toHTTPRequest request
liftIO . BS.putStr $ renderHTTPRequest hadronReq
```

From the CLI
------------

```sh
nc -l 8080 | hadron validate
curl -XPOST -H "foo: bar" localhost:8080/blah -d 42
```

In tests
--------

```haskell
import           Test.Hadron.Core.Gen

instance Arbitrary MyRequest where
  arbitrary = fmap MyRequest $ genHTTPRequest'
                                 genHTTPMethod
                                 genRequestTarget
                                 genHTTPRequestHeaders
                                 (elements ["{\"foo\":1}", "{\"bar\":2}"])
```

Conceptual warriors
===================

 - Sharif
