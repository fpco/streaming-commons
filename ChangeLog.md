# ChangeLog for streaming-commons

## 0.2.2.4

* Fix docstrings for text 2.0

## 0.2.2.3

* Support text 2.0 [#65](https://github.com/fpco/streaming-commons/pull/65)

## 0.2.2.2

* Support GHC 9.2 [#62](https://github.com/fpco/streaming-commons/pull/62)

## 0.2.2.1

* Fix test suite compilation issue [stackage#5528](https://github.com/commercialhaskell/stackage/issues/5528)

## 0.2.2.0

* Remove `AI_ADDRCONFIG` [#58](https://github.com/fpco/streaming-commons/issues/58)

## 0.2.1.2

* Update `defaultReadBufferSize` to use system default instead of hardcoded value [#54](https://github.com/fpco/streaming-commons/issues/54)

## 0.2.1.1

* Fix a failing test case (invalid `ByteString` copying), does not affect library itself

## 0.2.1.0

* Change `bindRandomPortGen` to use binding to port 0

## 0.2.0

* Drop `blaze-builder` dependency

## 0.1.19

* Update `getAddrInfo` hints to allow hostnames and portnames [#46](https://github.com/fpco/streaming-commons/issues/46)

## 0.1.18

* Add `isCompleteInflate`

## 0.1.17

* Add `bindPortGenEx`

## 0.1.16

* Add `closeStreamingProcessHandle`

## 0.1.15.5

* Make getSocket{Family}TCP try all addr candidates [#32](https://github.com/fpco/streaming-commons/pull/32)

## 0.1.15.3

* Fix benchmarks

## 0.1.15.2

* Document child process behavior in `waitForProcess`

## 0.1.15.1

* Catch exceptions thrown by `waitForProcess`

## 0.1.15

* Use `NO_DELAY1 for TCP client connections [#27](https://github.com/fpco/streaming-commons/issues/27)

## 0.1.14.2

* Fix bug in process exception display of args with spaces/quotes

## 0.1.14

* Exporting HasReadBufferSize; instance for ClientSettingsUnix [#24](https://github.com/fpco/streaming-commons/pull/24)

## 0.1.13

* Make size of read buffer configurable, change default size to 32 kiB [#23](https://github.com/fpco/streaming-commons/pull/23)

## 0.1.12.1

* Fix up `acceptSafe` as [mentioned by Kazu](https://github.com/yesodweb/wai/issues/361#issuecomment-102204803)

## 0.1.12

* `appRawSocket`

## 0.1.11

* `getUnusedInflated`: Return uncompressed data following compressed data [#20](https://github.com/fpco/streaming-commons/issues/20)

## 0.1.10

Support blaze-builder >= 0.4.  Add `newByteStringBuilderRecv` to Data.Streaming.ByteString.Builder; add modules Data.Streaming.ByteString.Builder.Buffer and  Data.Streaming.ByteString.Builder.Class.

## 0.1.9

Add Data.Streaming.ByteString.Builder

## 0.1.8

Generalise types of run\*Server which never cleanly return [#13](https://github.com/fpco/streaming-commons/pull/13)

## 0.1.7.1

Fix `streamingProcess` so that it doesn't close `Handle`s passed in with
`UseProvidedHandle`.

## 0.1.7

`withCheckedProcess` added.

## 0.1.6

Provide `appCloseConnection` to get the underlying connection from an `AppData`.
