// Code generated by protoc-gen-connect-go. DO NOT EDIT.
//
// Source: utxorpc/state/v1/state.proto

package statev1connect

import (
	context "context"
	errors "errors"
	v1 "github.com/bufbuild/buf-tour/gen/utxorpc/state/v1"
	connect_go "github.com/bufbuild/connect-go"
	http "net/http"
	strings "strings"
)

// This is a compile-time assertion to ensure that this generated file and the connect package are
// compatible. If you get a compiler error that this constant is not defined, this code was
// generated with a version of connect newer than the one compiled into your binary. You can fix the
// problem by either regenerating this code with an older version of connect or updating the connect
// version compiled into your binary.
const _ = connect_go.IsAtLeastVersion0_1_0

const (
	// LedgerStateName is the fully-qualified name of the LedgerState service.
	LedgerStateName = "utxorpc.state.v1.LedgerState"
)

// LedgerStateClient is a client for the utxorpc.state.v1.LedgerState service.
type LedgerStateClient interface {
	GetChainTip(context.Context, *connect_go.Request[v1.Empty]) (*connect_go.Response[v1.ChainPoint], error)
	GetUtxoByAddress(context.Context, *connect_go.Request[v1.UtxoByAddressRequest]) (*connect_go.Response[v1.UtxoSet], error)
	GetUtxoByTx(context.Context, *connect_go.Request[v1.UtxoByTxRequest]) (*connect_go.Response[v1.UtxoSet], error)
}

// NewLedgerStateClient constructs a client for the utxorpc.state.v1.LedgerState service. By
// default, it uses the Connect protocol with the binary Protobuf Codec, asks for gzipped responses,
// and sends uncompressed requests. To use the gRPC or gRPC-Web protocols, supply the
// connect.WithGRPC() or connect.WithGRPCWeb() options.
//
// The URL supplied here should be the base URL for the Connect or gRPC server (for example,
// http://api.acme.com or https://acme.com/grpc).
func NewLedgerStateClient(httpClient connect_go.HTTPClient, baseURL string, opts ...connect_go.ClientOption) LedgerStateClient {
	baseURL = strings.TrimRight(baseURL, "/")
	return &ledgerStateClient{
		getChainTip: connect_go.NewClient[v1.Empty, v1.ChainPoint](
			httpClient,
			baseURL+"/utxorpc.state.v1.LedgerState/GetChainTip",
			opts...,
		),
		getUtxoByAddress: connect_go.NewClient[v1.UtxoByAddressRequest, v1.UtxoSet](
			httpClient,
			baseURL+"/utxorpc.state.v1.LedgerState/GetUtxoByAddress",
			opts...,
		),
		getUtxoByTx: connect_go.NewClient[v1.UtxoByTxRequest, v1.UtxoSet](
			httpClient,
			baseURL+"/utxorpc.state.v1.LedgerState/GetUtxoByTx",
			opts...,
		),
	}
}

// ledgerStateClient implements LedgerStateClient.
type ledgerStateClient struct {
	getChainTip      *connect_go.Client[v1.Empty, v1.ChainPoint]
	getUtxoByAddress *connect_go.Client[v1.UtxoByAddressRequest, v1.UtxoSet]
	getUtxoByTx      *connect_go.Client[v1.UtxoByTxRequest, v1.UtxoSet]
}

// GetChainTip calls utxorpc.state.v1.LedgerState.GetChainTip.
func (c *ledgerStateClient) GetChainTip(ctx context.Context, req *connect_go.Request[v1.Empty]) (*connect_go.Response[v1.ChainPoint], error) {
	return c.getChainTip.CallUnary(ctx, req)
}

// GetUtxoByAddress calls utxorpc.state.v1.LedgerState.GetUtxoByAddress.
func (c *ledgerStateClient) GetUtxoByAddress(ctx context.Context, req *connect_go.Request[v1.UtxoByAddressRequest]) (*connect_go.Response[v1.UtxoSet], error) {
	return c.getUtxoByAddress.CallUnary(ctx, req)
}

// GetUtxoByTx calls utxorpc.state.v1.LedgerState.GetUtxoByTx.
func (c *ledgerStateClient) GetUtxoByTx(ctx context.Context, req *connect_go.Request[v1.UtxoByTxRequest]) (*connect_go.Response[v1.UtxoSet], error) {
	return c.getUtxoByTx.CallUnary(ctx, req)
}

// LedgerStateHandler is an implementation of the utxorpc.state.v1.LedgerState service.
type LedgerStateHandler interface {
	GetChainTip(context.Context, *connect_go.Request[v1.Empty]) (*connect_go.Response[v1.ChainPoint], error)
	GetUtxoByAddress(context.Context, *connect_go.Request[v1.UtxoByAddressRequest]) (*connect_go.Response[v1.UtxoSet], error)
	GetUtxoByTx(context.Context, *connect_go.Request[v1.UtxoByTxRequest]) (*connect_go.Response[v1.UtxoSet], error)
}

// NewLedgerStateHandler builds an HTTP handler from the service implementation. It returns the path
// on which to mount the handler and the handler itself.
//
// By default, handlers support the Connect, gRPC, and gRPC-Web protocols with the binary Protobuf
// and JSON codecs. They also support gzip compression.
func NewLedgerStateHandler(svc LedgerStateHandler, opts ...connect_go.HandlerOption) (string, http.Handler) {
	mux := http.NewServeMux()
	mux.Handle("/utxorpc.state.v1.LedgerState/GetChainTip", connect_go.NewUnaryHandler(
		"/utxorpc.state.v1.LedgerState/GetChainTip",
		svc.GetChainTip,
		opts...,
	))
	mux.Handle("/utxorpc.state.v1.LedgerState/GetUtxoByAddress", connect_go.NewUnaryHandler(
		"/utxorpc.state.v1.LedgerState/GetUtxoByAddress",
		svc.GetUtxoByAddress,
		opts...,
	))
	mux.Handle("/utxorpc.state.v1.LedgerState/GetUtxoByTx", connect_go.NewUnaryHandler(
		"/utxorpc.state.v1.LedgerState/GetUtxoByTx",
		svc.GetUtxoByTx,
		opts...,
	))
	return "/utxorpc.state.v1.LedgerState/", mux
}

// UnimplementedLedgerStateHandler returns CodeUnimplemented from all methods.
type UnimplementedLedgerStateHandler struct{}

func (UnimplementedLedgerStateHandler) GetChainTip(context.Context, *connect_go.Request[v1.Empty]) (*connect_go.Response[v1.ChainPoint], error) {
	return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("utxorpc.state.v1.LedgerState.GetChainTip is not implemented"))
}

func (UnimplementedLedgerStateHandler) GetUtxoByAddress(context.Context, *connect_go.Request[v1.UtxoByAddressRequest]) (*connect_go.Response[v1.UtxoSet], error) {
	return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("utxorpc.state.v1.LedgerState.GetUtxoByAddress is not implemented"))
}

func (UnimplementedLedgerStateHandler) GetUtxoByTx(context.Context, *connect_go.Request[v1.UtxoByTxRequest]) (*connect_go.Response[v1.UtxoSet], error) {
	return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("utxorpc.state.v1.LedgerState.GetUtxoByTx is not implemented"))
}