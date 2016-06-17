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

```haskell
parseHTTPRequest :: ByteString -> Either HTTPError HTTPRequest

renderHTTPRequest :: HTTPRequest -> ByteString

genValidHTTPRequest :: Gen ByteString

genInvalidHTTPRequest :: Gen ByteString
```
