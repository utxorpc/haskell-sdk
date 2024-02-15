# UTxO RPC Client SDK

An SDK for clients of the [UTxO RPC](https://utxorpc.org/) specification.

The goal of this SDK is to reduce boilerplate and increase ease of use of the UTxO RPC spec. This SDK provides functions for creating a connected UTxO RPC client and for calling each each RPC in the UTxO RPC specification. Automated logging of each event in a gRPC call is also supported.

## How to Use
> [!NOTE]
> This SDK depends on package versions that are not on Hackage. Consult [`stack.yaml`](../stack.yaml) or [`cabal.project`](../cabal.project) for help configuring your project's dependencies.

Use the SDK through one of the client-creating functions in `Utxorpc.Client`.
1. `simpleUtxorpcClient` -- connect to a service using the bare minimum required information.
    1. See `/quick-start/Main.hs`.
1. `utxorpcClient` -- connect to a service using a `UtxorpcInfo` record.
    1. See `/example/Main.hs`
1. `utxorpcClientWith` -- for fine grained control, provide a `GrpcClientConfig` (from `http2-client-grpc`)

Each of these functions provides a record of functions for each method in UTxO specification, as well as a function to close the connection.

## Logging

This SDK supports automated logging through the `UtxorpcClientLogger` type. It is a record of one user-defined logging function for each of the following events:
1. Request sent.
1. Reply received.
1. Server stream data received.
1. Server stream ended.

For more information, see [`Utxorpc.Logged`](./src/Utxorpc/Logged.hs) and [`/example`](./example/Main.hs).

## Examples

There are two provided examples:
1. `/quick-start` shows the bare minimum required to make a single unary request.
1. `/example` shows a more involved example that uses one of the following two logger implementations:
    1. `/example/SimpleLogger.hs` is a simple logger implementation that prints human-readable output.
    1. `/example/KatipLogger.hs` is a more involved logger that demonstrates how to use logging functions that run in a transformer stack. Run the example with `--katip` to use this logger.