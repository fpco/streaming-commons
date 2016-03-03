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
