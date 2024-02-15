# UTxO RPC Service SDK

An SDK for services of the [UTxO RPC](https://utxorpc.org/) specification.

The goal of this SDK is to reduce boilerplate and increase ease of use of the UTxO RPC spec. This SDK provides a function for creating a UTxO RPC service from a set of method handlers and support for automated logging of each event in a gRPC call.

## How to Use
> [!NOTE]
> This SDK depends on GitHub repositories. Consult [`stack.yaml`](https://github.com/utxorpc/haskell-sdk/) or [`cabal.project`](https://github.com/utxorpc/haskell-sdk/) for help configuring your project's dependencies.

Using the `Utxorpc.Server` module:
1. Create a `UtxorpcHandlers` record, containing a handler for each method in the specification.
1. Create a `ServiceConfig` record, containing server settings (e.g., TLS settings), the handlers, and (optionally), a logger.
1. Call `runUtxorpc` with the `ServiceConfig`.

## Logging

This SDK supports automated logging through the `UtxorpcServerLogger` type. It is a record of one user-defined logging function for each of the following events:
1. Request received.
1. Unary reply sent.
1. Server stream data sent.
1. Server stream ended.

For more information, see [`Utxorpc.Logged`](./src/Utxorpc/Logged.hs) and [`/example`](./example/Main.hs).

## Examples

`/example` shows how to use the SDK by creating a u5c service with simple handlers that execute a log function and return empty replies. It demonstrates how to use the SDK without dealing with implementation details of the handlers. It uses one of the following two loggers:
    1. `/example/SimpleLogger.hs` is a simple logger implementation that prints human-readable output.
    1. `/example/KatipLogger.hs` is a more involved logger that demonstrates how to use logging functions that run in a transformer stack. Run the example with `--katip` to use this logger.