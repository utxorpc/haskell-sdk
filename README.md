# UTxO RPC SDK

SDKs for clients and services of the [UTxO RPC](https://utxorpc.org/) specification.

The goal of this SDK is to reduce boilerplate and increase ease of use of the UTxO RPC (u5c) spec. This project contains two packages: an SDK for clients and an SDK for services. Both build on code generated by `proto-lens-protoc` from the u5c specification and provide more convenient interfaces than the raw generated code. In addition, both SDKs provide support for automated logging. Please pay close attention to the documentation and types if you are implementing service and client logging, as they differ slightly.

## How to Use

> [!NOTE]
> These SDKs depend on package versions that are not on Hackage. Consult [`stack.yaml`](../stack.yaml) or [`cabal.project`](../cabal.project) for help configuring your project's dependencies.

Please consult the `README` for the [client](./client/README.md) and the [server](./server/README.md)