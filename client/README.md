# UTxO RPC Client SDK

> [!IMPORTANT]
> This package is currently pre-release. Until this package reaches v0.1.0.0 it is subject to breaking changes without change in major version.

An SDK for clients of the [UTxO RPC](https://utxorpc.org/) specification.

The goal of this SDK is to reduce boilerplate and increase ease of use of the UTxO RPC spec. This SDK provides functions for creating a connected UTxO RPC client and for calling each each RPC in the UTxO RPC specification. Automated logging of each event in a gRPC call is also supported.

To get started, please see the documentation for [`Utxorpc.Client`](https://hackage.haskell.org/package/utxorpc-client).

> [!NOTE]
> This SDK depends on package versions that are not on Hackage. Consult [`stack.yaml`](https://github.com/utxorpc/haskell-sdk/) or [`cabal.project`](https://github.com/utxorpc/haskell-sdk/) for help configuring your project's dependencies.