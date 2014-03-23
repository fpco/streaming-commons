streaming-commons
=================

Common lower-level functions needed by various streaming data libraries.
Intended to be shared by libraries like conduit and pipes.

[![Build Status](https://travis-ci.org/fpco/streaming-commons.svg)](https://travis-ci.org/fpco/streaming-commons)

Dependencies
------------

One of the requirements of this package is to restrict ourselves to "core"
dependencies. The definition of core is still to be decided, but here's a
working start:

* *No* dependency on system libraries, beyond that which is required by other
  dependencies.
* Anything which ships with GHC. *However*, we must retain compatibility with
  versions of those packages going back to at least GHC 7.4, and preferably
  earlier.
* text, once again with backwards compatibility for versions included with
  legacy Haskell Platform. In other words, 0.11.2 support is required.
* network, support back to 2.3. We do *not* need to support the
  network/network-bytestring split.
* stm, preferably all the way back to 2.1.
* transformers

For debate:

* Other Haskell Platform packages, especially vector and attoparsec.
