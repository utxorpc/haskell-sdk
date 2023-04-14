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
	// LedgerStateServiceName is the fully-qualified name of the LedgerStateService service.
	LedgerStateServiceName = "utxorpc.state.v1.LedgerStateService"
)

// These constants are the fully-qualified names of the RPCs defined in this package. They're
// exposed at runtime as Spec.Procedure and as the final two segments of the HTTP route.
//
// Note that these are different from the fully-qualified method names used by
// google.golang.org/protobuf/reflect/protoreflect. To convert from these constants to
// reflection-formatted method names, remove the leading slash and convert the remaining slash to a
// period.
const (
	// LedgerStateServiceGetChainTipProcedure is the fully-qualified name of the LedgerStateService's
	// GetChainTip RPC.
	LedgerStateServiceGetChainTipProcedure = "/utxorpc.state.v1.LedgerStateService/GetChainTip"
	// LedgerStateServiceGetUtxoByAddressProcedure is the fully-qualified name of the
	// LedgerStateService's GetUtxoByAddress RPC.
	LedgerStateServiceGetUtxoByAddressProcedure = "/utxorpc.state.v1.LedgerStateService/GetUtxoByAddress"
	// LedgerStateServiceGetUtxoByTxProcedure is the fully-qualified name of the LedgerStateService's
	// GetUtxoByTx RPC.
	LedgerStateServiceGetUtxoByTxProcedure = "/utxorpc.state.v1.LedgerStateService/GetUtxoByTx"
	// LedgerStateServiceGetTxProcedure is the fully-qualified name of the LedgerStateService's GetTx
	// RPC.
	LedgerStateServiceGetTxProcedure = "/utxorpc.state.v1.LedgerStateService/GetTx"
)

// LedgerStateServiceClient is a client for the utxorpc.state.v1.LedgerStateService service.
type LedgerStateServiceClient interface {
	GetChainTip(context.Context, *connect_go.Request[v1.Empty]) (*connect_go.Response[v1.ChainPoint], error)
	GetUtxoByAddress(context.Context, *connect_go.Request[v1.UtxoByAddressRequest]) (*connect_go.Response[v1.UtxoSet], error)
	GetUtxoByTx(context.Context, *connect_go.Request[v1.UtxoByTxRequest]) (*connect_go.Response[v1.UtxoSet], error)
	GetTx(context.Context, *connect_go.Request[v1.GetTxRequest]) (*connect_go.Response[v1.GetTxResponse], error)
}

// NewLedgerStateServiceClient constructs a client for the utxorpc.state.v1.LedgerStateService
// service. By default, it uses the Connect protocol with the binary Protobuf Codec, asks for
// gzipped responses, and sends uncompressed requests. To use the gRPC or gRPC-Web protocols, supply
// the connect.WithGRPC() or connect.WithGRPCWeb() options.
//
// The URL supplied here should be the base URL for the Connect or gRPC server (for example,
// http://api.acme.com or https://acme.com/grpc).
func NewLedgerStateServiceClient(httpClient connect_go.HTTPClient, baseURL string, opts ...connect_go.ClientOption) LedgerStateServiceClient {
	baseURL = strings.TrimRight(baseURL, "/")
	return &ledgerStateServiceClient{
		getChainTip: connect_go.NewClient[v1.Empty, v1.ChainPoint](
			httpClient,
			baseURL+LedgerStateServiceGetChainTipProcedure,
			opts...,
		),
		getUtxoByAddress: connect_go.NewClient[v1.UtxoByAddressRequest, v1.UtxoSet](
			httpClient,
			baseURL+LedgerStateServiceGetUtxoByAddressProcedure,
			opts...,
		),
		getUtxoByTx: connect_go.NewClient[v1.UtxoByTxRequest, v1.UtxoSet](
			httpClient,
			baseURL+LedgerStateServiceGetUtxoByTxProcedure,
			opts...,
		),
		getTx: connect_go.NewClient[v1.GetTxRequest, v1.GetTxResponse](
			httpClient,
			baseURL+LedgerStateServiceGetTxProcedure,
			opts...,
		),
	}
}

// ledgerStateServiceClient implements LedgerStateServiceClient.
type ledgerStateServiceClient struct {
	getChainTip      *connect_go.Client[v1.Empty, v1.ChainPoint]
	getUtxoByAddress *connect_go.Client[v1.UtxoByAddressRequest, v1.UtxoSet]
	getUtxoByTx      *connect_go.Client[v1.UtxoByTxRequest, v1.UtxoSet]
	getTx            *connect_go.Client[v1.GetTxRequest, v1.GetTxResponse]
}

// GetChainTip calls utxorpc.state.v1.LedgerStateService.GetChainTip.
func (c *ledgerStateServiceClient) GetChainTip(ctx context.Context, req *connect_go.Request[v1.Empty]) (*connect_go.Response[v1.ChainPoint], error) {
	return c.getChainTip.CallUnary(ctx, req)
}

// GetUtxoByAddress calls utxorpc.state.v1.LedgerStateService.GetUtxoByAddress.
func (c *ledgerStateServiceClient) GetUtxoByAddress(ctx context.Context, req *connect_go.Request[v1.UtxoByAddressRequest]) (*connect_go.Response[v1.UtxoSet], error) {
	return c.getUtxoByAddress.CallUnary(ctx, req)
}

// GetUtxoByTx calls utxorpc.state.v1.LedgerStateService.GetUtxoByTx.
func (c *ledgerStateServiceClient) GetUtxoByTx(ctx context.Context, req *connect_go.Request[v1.UtxoByTxRequest]) (*connect_go.Response[v1.UtxoSet], error) {
	return c.getUtxoByTx.CallUnary(ctx, req)
}

// GetTx calls utxorpc.state.v1.LedgerStateService.GetTx.
func (c *ledgerStateServiceClient) GetTx(ctx context.Context, req *connect_go.Request[v1.GetTxRequest]) (*connect_go.Response[v1.GetTxResponse], error) {
	return c.getTx.CallUnary(ctx, req)
}

// LedgerStateServiceHandler is an implementation of the utxorpc.state.v1.LedgerStateService
// service.
type LedgerStateServiceHandler interface {
	GetChainTip(context.Context, *connect_go.Request[v1.Empty]) (*connect_go.Response[v1.ChainPoint], error)
	GetUtxoByAddress(context.Context, *connect_go.Request[v1.UtxoByAddressRequest]) (*connect_go.Response[v1.UtxoSet], error)
	GetUtxoByTx(context.Context, *connect_go.Request[v1.UtxoByTxRequest]) (*connect_go.Response[v1.UtxoSet], error)
	GetTx(context.Context, *connect_go.Request[v1.GetTxRequest]) (*connect_go.Response[v1.GetTxResponse], error)
}

// NewLedgerStateServiceHandler builds an HTTP handler from the service implementation. It returns
// the path on which to mount the handler and the handler itself.
//
// By default, handlers support the Connect, gRPC, and gRPC-Web protocols with the binary Protobuf
// and JSON codecs. They also support gzip compression.
func NewLedgerStateServiceHandler(svc LedgerStateServiceHandler, opts ...connect_go.HandlerOption) (string, http.Handler) {
	mux := http.NewServeMux()
	mux.Handle(LedgerStateServiceGetChainTipProcedure, connect_go.NewUnaryHandler(
		LedgerStateServiceGetChainTipProcedure,
		svc.GetChainTip,
		opts...,
	))
	mux.Handle(LedgerStateServiceGetUtxoByAddressProcedure, connect_go.NewUnaryHandler(
		LedgerStateServiceGetUtxoByAddressProcedure,
		svc.GetUtxoByAddress,
		opts...,
	))
	mux.Handle(LedgerStateServiceGetUtxoByTxProcedure, connect_go.NewUnaryHandler(
		LedgerStateServiceGetUtxoByTxProcedure,
		svc.GetUtxoByTx,
		opts...,
	))
	mux.Handle(LedgerStateServiceGetTxProcedure, connect_go.NewUnaryHandler(
		LedgerStateServiceGetTxProcedure,
		svc.GetTx,
		opts...,
	))
	return "/utxorpc.state.v1.LedgerStateService/", mux
}

// UnimplementedLedgerStateServiceHandler returns CodeUnimplemented from all methods.
type UnimplementedLedgerStateServiceHandler struct{}

func (UnimplementedLedgerStateServiceHandler) GetChainTip(context.Context, *connect_go.Request[v1.Empty]) (*connect_go.Response[v1.ChainPoint], error) {
	return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("utxorpc.state.v1.LedgerStateService.GetChainTip is not implemented"))
}

func (UnimplementedLedgerStateServiceHandler) GetUtxoByAddress(context.Context, *connect_go.Request[v1.UtxoByAddressRequest]) (*connect_go.Response[v1.UtxoSet], error) {
	return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("utxorpc.state.v1.LedgerStateService.GetUtxoByAddress is not implemented"))
}

func (UnimplementedLedgerStateServiceHandler) GetUtxoByTx(context.Context, *connect_go.Request[v1.UtxoByTxRequest]) (*connect_go.Response[v1.UtxoSet], error) {
	return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("utxorpc.state.v1.LedgerStateService.GetUtxoByTx is not implemented"))
}

func (UnimplementedLedgerStateServiceHandler) GetTx(context.Context, *connect_go.Request[v1.GetTxRequest]) (*connect_go.Response[v1.GetTxResponse], error) {
	return nil, connect_go.NewError(connect_go.CodeUnimplemented, errors.New("utxorpc.state.v1.LedgerStateService.GetTx is not implemented"))
}