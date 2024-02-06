# UTxO RPC Client SDK

An SDK for clients of the [UTxO RPC](https://utxorpc.org/) specification.

This SDK provides convenience methods for creating a connection to a UTxO RPC service and for calling each each method in the UTxO RPC specification. Automated logging of each event in a gRPC call is also supported.

## How to Use

Use the SDK through one of the factory functions in `Utxorpc.Client`.
1. `simpleUtxorpcService` -- connect to a service using the bare minimum required information.
    1. See `/quick-start/Main.hs`.
1. `utxorpcService` -- connect to a service using the provided `ServiceInfo`.
    1. See `/example/Main.hs`
1. `utxorpcServiceWith` -- for fine grained control, provide a `GrpcClientConfig` (from `http2-client-grpc`)

Each of these functions provides a record type hosting functions for each method in UTxO RPC, as well as a method to close the connection.

## Logging

This SDK supports automated logging through the `UtxorpcClientLogger` type. It is a record hosting user-defined logging functions for each of the following events:
1. Request sent.
1. Reply received.
1. Server stream data received.
1. Server stream ended.

For more information, see `Utxorpc.Logged` and `/example`.

## Examples

There are two provided examples:
1. `/quick-start` shows the bare minimum required to make a single unary request.
1. `/example` shows a more involved example. `/example/SimpleLogger.hs` is a simple logger implementation that prints human-readable output, and `/example/KatipLogger.hs` is a more involved logger that demonstrates how to use logging functions that run in a transformer stack.