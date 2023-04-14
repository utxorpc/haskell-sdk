// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.30.0
// 	protoc        (unknown)
// source: utxorpc/sync/v1/sync.proto

package syncv1

import (
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

type Intersection struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Era uint32 `protobuf:"varint,1,opt,name=era,proto3" json:"era,omitempty"`
}

func (x *Intersection) Reset() {
	*x = Intersection{}
	if protoimpl.UnsafeEnabled {
		mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[0]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *Intersection) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Intersection) ProtoMessage() {}

func (x *Intersection) ProtoReflect() protoreflect.Message {
	mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[0]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Intersection.ProtoReflect.Descriptor instead.
func (*Intersection) Descriptor() ([]byte, []int) {
	return file_utxorpc_sync_v1_sync_proto_rawDescGZIP(), []int{0}
}

func (x *Intersection) GetEra() uint32 {
	if x != nil {
		return x.Era
	}
	return 0
}

type BlockHeader struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Slot   uint64 `protobuf:"varint,1,opt,name=slot,proto3" json:"slot,omitempty"`
	Height uint64 `protobuf:"varint,2,opt,name=height,proto3" json:"height,omitempty"`
	Hash   string `protobuf:"bytes,3,opt,name=hash,proto3" json:"hash,omitempty"`
}

func (x *BlockHeader) Reset() {
	*x = BlockHeader{}
	if protoimpl.UnsafeEnabled {
		mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[1]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *BlockHeader) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*BlockHeader) ProtoMessage() {}

func (x *BlockHeader) ProtoReflect() protoreflect.Message {
	mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[1]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use BlockHeader.ProtoReflect.Descriptor instead.
func (*BlockHeader) Descriptor() ([]byte, []int) {
	return file_utxorpc_sync_v1_sync_proto_rawDescGZIP(), []int{1}
}

func (x *BlockHeader) GetSlot() uint64 {
	if x != nil {
		return x.Slot
	}
	return 0
}

func (x *BlockHeader) GetHeight() uint64 {
	if x != nil {
		return x.Height
	}
	return 0
}

func (x *BlockHeader) GetHash() string {
	if x != nil {
		return x.Hash
	}
	return ""
}

type BlockBody struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields
}

func (x *BlockBody) Reset() {
	*x = BlockBody{}
	if protoimpl.UnsafeEnabled {
		mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[2]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *BlockBody) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*BlockBody) ProtoMessage() {}

func (x *BlockBody) ProtoReflect() protoreflect.Message {
	mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[2]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use BlockBody.ProtoReflect.Descriptor instead.
func (*BlockBody) Descriptor() ([]byte, []int) {
	return file_utxorpc_sync_v1_sync_proto_rawDescGZIP(), []int{2}
}

type Block struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Header *BlockHeader `protobuf:"bytes,1,opt,name=header,proto3" json:"header,omitempty"`
	Body   *BlockBody   `protobuf:"bytes,2,opt,name=body,proto3" json:"body,omitempty"`
	Cbor   []byte       `protobuf:"bytes,3,opt,name=cbor,proto3" json:"cbor,omitempty"`
}

func (x *Block) Reset() {
	*x = Block{}
	if protoimpl.UnsafeEnabled {
		mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[3]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *Block) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Block) ProtoMessage() {}

func (x *Block) ProtoReflect() protoreflect.Message {
	mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[3]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Block.ProtoReflect.Descriptor instead.
func (*Block) Descriptor() ([]byte, []int) {
	return file_utxorpc_sync_v1_sync_proto_rawDescGZIP(), []int{3}
}

func (x *Block) GetHeader() *BlockHeader {
	if x != nil {
		return x.Header
	}
	return nil
}

func (x *Block) GetBody() *BlockBody {
	if x != nil {
		return x.Body
	}
	return nil
}

func (x *Block) GetCbor() []byte {
	if x != nil {
		return x.Cbor
	}
	return nil
}

type StreamBlocksRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Intersect *Intersection `protobuf:"bytes,1,opt,name=intersect,proto3" json:"intersect,omitempty"`
}

func (x *StreamBlocksRequest) Reset() {
	*x = StreamBlocksRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[4]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *StreamBlocksRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*StreamBlocksRequest) ProtoMessage() {}

func (x *StreamBlocksRequest) ProtoReflect() protoreflect.Message {
	mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[4]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use StreamBlocksRequest.ProtoReflect.Descriptor instead.
func (*StreamBlocksRequest) Descriptor() ([]byte, []int) {
	return file_utxorpc_sync_v1_sync_proto_rawDescGZIP(), []int{4}
}

func (x *StreamBlocksRequest) GetIntersect() *Intersection {
	if x != nil {
		return x.Intersect
	}
	return nil
}

type StreamBlocksResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields
}

func (x *StreamBlocksResponse) Reset() {
	*x = StreamBlocksResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[5]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *StreamBlocksResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*StreamBlocksResponse) ProtoMessage() {}

func (x *StreamBlocksResponse) ProtoReflect() protoreflect.Message {
	mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[5]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use StreamBlocksResponse.ProtoReflect.Descriptor instead.
func (*StreamBlocksResponse) Descriptor() ([]byte, []int) {
	return file_utxorpc_sync_v1_sync_proto_rawDescGZIP(), []int{5}
}

type FetchBlocksRequest struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Slot   uint64 `protobuf:"varint,1,opt,name=slot,proto3" json:"slot,omitempty"`
	Height uint64 `protobuf:"varint,2,opt,name=height,proto3" json:"height,omitempty"`
	Hash   string `protobuf:"bytes,3,opt,name=hash,proto3" json:"hash,omitempty"`
}

func (x *FetchBlocksRequest) Reset() {
	*x = FetchBlocksRequest{}
	if protoimpl.UnsafeEnabled {
		mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[6]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *FetchBlocksRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*FetchBlocksRequest) ProtoMessage() {}

func (x *FetchBlocksRequest) ProtoReflect() protoreflect.Message {
	mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[6]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use FetchBlocksRequest.ProtoReflect.Descriptor instead.
func (*FetchBlocksRequest) Descriptor() ([]byte, []int) {
	return file_utxorpc_sync_v1_sync_proto_rawDescGZIP(), []int{6}
}

func (x *FetchBlocksRequest) GetSlot() uint64 {
	if x != nil {
		return x.Slot
	}
	return 0
}

func (x *FetchBlocksRequest) GetHeight() uint64 {
	if x != nil {
		return x.Height
	}
	return 0
}

func (x *FetchBlocksRequest) GetHash() string {
	if x != nil {
		return x.Hash
	}
	return ""
}

type FetchBlocksResponse struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields
}

func (x *FetchBlocksResponse) Reset() {
	*x = FetchBlocksResponse{}
	if protoimpl.UnsafeEnabled {
		mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[7]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *FetchBlocksResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*FetchBlocksResponse) ProtoMessage() {}

func (x *FetchBlocksResponse) ProtoReflect() protoreflect.Message {
	mi := &file_utxorpc_sync_v1_sync_proto_msgTypes[7]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use FetchBlocksResponse.ProtoReflect.Descriptor instead.
func (*FetchBlocksResponse) Descriptor() ([]byte, []int) {
	return file_utxorpc_sync_v1_sync_proto_rawDescGZIP(), []int{7}
}

var File_utxorpc_sync_v1_sync_proto protoreflect.FileDescriptor

var file_utxorpc_sync_v1_sync_proto_rawDesc = []byte{
	0x0a, 0x1a, 0x75, 0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x2f, 0x73, 0x79, 0x6e, 0x63, 0x2f, 0x76,
	0x31, 0x2f, 0x73, 0x79, 0x6e, 0x63, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x0f, 0x75, 0x74,
	0x78, 0x6f, 0x72, 0x70, 0x63, 0x2e, 0x73, 0x79, 0x6e, 0x63, 0x2e, 0x76, 0x31, 0x22, 0x20, 0x0a,
	0x0c, 0x49, 0x6e, 0x74, 0x65, 0x72, 0x73, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x12, 0x10, 0x0a,
	0x03, 0x65, 0x72, 0x61, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0d, 0x52, 0x03, 0x65, 0x72, 0x61, 0x22,
	0x4d, 0x0a, 0x0b, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x48, 0x65, 0x61, 0x64, 0x65, 0x72, 0x12, 0x12,
	0x0a, 0x04, 0x73, 0x6c, 0x6f, 0x74, 0x18, 0x01, 0x20, 0x01, 0x28, 0x04, 0x52, 0x04, 0x73, 0x6c,
	0x6f, 0x74, 0x12, 0x16, 0x0a, 0x06, 0x68, 0x65, 0x69, 0x67, 0x68, 0x74, 0x18, 0x02, 0x20, 0x01,
	0x28, 0x04, 0x52, 0x06, 0x68, 0x65, 0x69, 0x67, 0x68, 0x74, 0x12, 0x12, 0x0a, 0x04, 0x68, 0x61,
	0x73, 0x68, 0x18, 0x03, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x68, 0x61, 0x73, 0x68, 0x22, 0x0b,
	0x0a, 0x09, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x42, 0x6f, 0x64, 0x79, 0x22, 0x81, 0x01, 0x0a, 0x05,
	0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x12, 0x34, 0x0a, 0x06, 0x68, 0x65, 0x61, 0x64, 0x65, 0x72, 0x18,
	0x01, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x1c, 0x2e, 0x75, 0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x2e,
	0x73, 0x79, 0x6e, 0x63, 0x2e, 0x76, 0x31, 0x2e, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x48, 0x65, 0x61,
	0x64, 0x65, 0x72, 0x52, 0x06, 0x68, 0x65, 0x61, 0x64, 0x65, 0x72, 0x12, 0x2e, 0x0a, 0x04, 0x62,
	0x6f, 0x64, 0x79, 0x18, 0x02, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x1a, 0x2e, 0x75, 0x74, 0x78, 0x6f,
	0x72, 0x70, 0x63, 0x2e, 0x73, 0x79, 0x6e, 0x63, 0x2e, 0x76, 0x31, 0x2e, 0x42, 0x6c, 0x6f, 0x63,
	0x6b, 0x42, 0x6f, 0x64, 0x79, 0x52, 0x04, 0x62, 0x6f, 0x64, 0x79, 0x12, 0x12, 0x0a, 0x04, 0x63,
	0x62, 0x6f, 0x72, 0x18, 0x03, 0x20, 0x01, 0x28, 0x0c, 0x52, 0x04, 0x63, 0x62, 0x6f, 0x72, 0x22,
	0x52, 0x0a, 0x13, 0x53, 0x74, 0x72, 0x65, 0x61, 0x6d, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x52,
	0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x12, 0x3b, 0x0a, 0x09, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x73,
	0x65, 0x63, 0x74, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x1d, 0x2e, 0x75, 0x74, 0x78, 0x6f,
	0x72, 0x70, 0x63, 0x2e, 0x73, 0x79, 0x6e, 0x63, 0x2e, 0x76, 0x31, 0x2e, 0x49, 0x6e, 0x74, 0x65,
	0x72, 0x73, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x52, 0x09, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x73,
	0x65, 0x63, 0x74, 0x22, 0x16, 0x0a, 0x14, 0x53, 0x74, 0x72, 0x65, 0x61, 0x6d, 0x42, 0x6c, 0x6f,
	0x63, 0x6b, 0x73, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x22, 0x54, 0x0a, 0x12, 0x46,
	0x65, 0x74, 0x63, 0x68, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73,
	0x74, 0x12, 0x12, 0x0a, 0x04, 0x73, 0x6c, 0x6f, 0x74, 0x18, 0x01, 0x20, 0x01, 0x28, 0x04, 0x52,
	0x04, 0x73, 0x6c, 0x6f, 0x74, 0x12, 0x16, 0x0a, 0x06, 0x68, 0x65, 0x69, 0x67, 0x68, 0x74, 0x18,
	0x02, 0x20, 0x01, 0x28, 0x04, 0x52, 0x06, 0x68, 0x65, 0x69, 0x67, 0x68, 0x74, 0x12, 0x12, 0x0a,
	0x04, 0x68, 0x61, 0x73, 0x68, 0x18, 0x03, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x68, 0x61, 0x73,
	0x68, 0x22, 0x15, 0x0a, 0x13, 0x46, 0x65, 0x74, 0x63, 0x68, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x73,
	0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x32, 0xcb, 0x01, 0x0a, 0x10, 0x43, 0x68, 0x61,
	0x69, 0x6e, 0x53, 0x79, 0x6e, 0x63, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x12, 0x5d, 0x0a,
	0x0c, 0x53, 0x74, 0x72, 0x65, 0x61, 0x6d, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x12, 0x24, 0x2e,
	0x75, 0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x2e, 0x73, 0x79, 0x6e, 0x63, 0x2e, 0x76, 0x31, 0x2e,
	0x53, 0x74, 0x72, 0x65, 0x61, 0x6d, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x52, 0x65, 0x71, 0x75,
	0x65, 0x73, 0x74, 0x1a, 0x25, 0x2e, 0x75, 0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x2e, 0x73, 0x79,
	0x6e, 0x63, 0x2e, 0x76, 0x31, 0x2e, 0x53, 0x74, 0x72, 0x65, 0x61, 0x6d, 0x42, 0x6c, 0x6f, 0x63,
	0x6b, 0x73, 0x52, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x30, 0x01, 0x12, 0x58, 0x0a, 0x0b,
	0x46, 0x65, 0x74, 0x63, 0x68, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x12, 0x23, 0x2e, 0x75, 0x74,
	0x78, 0x6f, 0x72, 0x70, 0x63, 0x2e, 0x73, 0x79, 0x6e, 0x63, 0x2e, 0x76, 0x31, 0x2e, 0x46, 0x65,
	0x74, 0x63, 0x68, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74,
	0x1a, 0x24, 0x2e, 0x75, 0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x2e, 0x73, 0x79, 0x6e, 0x63, 0x2e,
	0x76, 0x31, 0x2e, 0x46, 0x65, 0x74, 0x63, 0x68, 0x42, 0x6c, 0x6f, 0x63, 0x6b, 0x73, 0x52, 0x65,
	0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x42, 0xb7, 0x01, 0x0a, 0x13, 0x63, 0x6f, 0x6d, 0x2e, 0x75,
	0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x2e, 0x73, 0x79, 0x6e, 0x63, 0x2e, 0x76, 0x31, 0x42, 0x09,
	0x53, 0x79, 0x6e, 0x63, 0x50, 0x72, 0x6f, 0x74, 0x6f, 0x50, 0x01, 0x5a, 0x37, 0x67, 0x69, 0x74,
	0x68, 0x75, 0x62, 0x2e, 0x63, 0x6f, 0x6d, 0x2f, 0x62, 0x75, 0x66, 0x62, 0x75, 0x69, 0x6c, 0x64,
	0x2f, 0x62, 0x75, 0x66, 0x2d, 0x74, 0x6f, 0x75, 0x72, 0x2f, 0x67, 0x65, 0x6e, 0x2f, 0x75, 0x74,
	0x78, 0x6f, 0x72, 0x70, 0x63, 0x2f, 0x73, 0x79, 0x6e, 0x63, 0x2f, 0x76, 0x31, 0x3b, 0x73, 0x79,
	0x6e, 0x63, 0x76, 0x31, 0xa2, 0x02, 0x03, 0x55, 0x53, 0x58, 0xaa, 0x02, 0x0f, 0x55, 0x74, 0x78,
	0x6f, 0x72, 0x70, 0x63, 0x2e, 0x53, 0x79, 0x6e, 0x63, 0x2e, 0x56, 0x31, 0xca, 0x02, 0x0f, 0x55,
	0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x5c, 0x53, 0x79, 0x6e, 0x63, 0x5c, 0x56, 0x31, 0xe2, 0x02,
	0x1b, 0x55, 0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x5c, 0x53, 0x79, 0x6e, 0x63, 0x5c, 0x56, 0x31,
	0x5c, 0x47, 0x50, 0x42, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0xea, 0x02, 0x11, 0x55,
	0x74, 0x78, 0x6f, 0x72, 0x70, 0x63, 0x3a, 0x3a, 0x53, 0x79, 0x6e, 0x63, 0x3a, 0x3a, 0x56, 0x31,
	0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x33,
}

var (
	file_utxorpc_sync_v1_sync_proto_rawDescOnce sync.Once
	file_utxorpc_sync_v1_sync_proto_rawDescData = file_utxorpc_sync_v1_sync_proto_rawDesc
)

func file_utxorpc_sync_v1_sync_proto_rawDescGZIP() []byte {
	file_utxorpc_sync_v1_sync_proto_rawDescOnce.Do(func() {
		file_utxorpc_sync_v1_sync_proto_rawDescData = protoimpl.X.CompressGZIP(file_utxorpc_sync_v1_sync_proto_rawDescData)
	})
	return file_utxorpc_sync_v1_sync_proto_rawDescData
}

var file_utxorpc_sync_v1_sync_proto_msgTypes = make([]protoimpl.MessageInfo, 8)
var file_utxorpc_sync_v1_sync_proto_goTypes = []interface{}{
	(*Intersection)(nil),         // 0: utxorpc.sync.v1.Intersection
	(*BlockHeader)(nil),          // 1: utxorpc.sync.v1.BlockHeader
	(*BlockBody)(nil),            // 2: utxorpc.sync.v1.BlockBody
	(*Block)(nil),                // 3: utxorpc.sync.v1.Block
	(*StreamBlocksRequest)(nil),  // 4: utxorpc.sync.v1.StreamBlocksRequest
	(*StreamBlocksResponse)(nil), // 5: utxorpc.sync.v1.StreamBlocksResponse
	(*FetchBlocksRequest)(nil),   // 6: utxorpc.sync.v1.FetchBlocksRequest
	(*FetchBlocksResponse)(nil),  // 7: utxorpc.sync.v1.FetchBlocksResponse
}
var file_utxorpc_sync_v1_sync_proto_depIdxs = []int32{
	1, // 0: utxorpc.sync.v1.Block.header:type_name -> utxorpc.sync.v1.BlockHeader
	2, // 1: utxorpc.sync.v1.Block.body:type_name -> utxorpc.sync.v1.BlockBody
	0, // 2: utxorpc.sync.v1.StreamBlocksRequest.intersect:type_name -> utxorpc.sync.v1.Intersection
	4, // 3: utxorpc.sync.v1.ChainSyncService.StreamBlocks:input_type -> utxorpc.sync.v1.StreamBlocksRequest
	6, // 4: utxorpc.sync.v1.ChainSyncService.FetchBlocks:input_type -> utxorpc.sync.v1.FetchBlocksRequest
	5, // 5: utxorpc.sync.v1.ChainSyncService.StreamBlocks:output_type -> utxorpc.sync.v1.StreamBlocksResponse
	7, // 6: utxorpc.sync.v1.ChainSyncService.FetchBlocks:output_type -> utxorpc.sync.v1.FetchBlocksResponse
	5, // [5:7] is the sub-list for method output_type
	3, // [3:5] is the sub-list for method input_type
	3, // [3:3] is the sub-list for extension type_name
	3, // [3:3] is the sub-list for extension extendee
	0, // [0:3] is the sub-list for field type_name
}

func init() { file_utxorpc_sync_v1_sync_proto_init() }
func file_utxorpc_sync_v1_sync_proto_init() {
	if File_utxorpc_sync_v1_sync_proto != nil {
		return
	}
	if !protoimpl.UnsafeEnabled {
		file_utxorpc_sync_v1_sync_proto_msgTypes[0].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*Intersection); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_utxorpc_sync_v1_sync_proto_msgTypes[1].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*BlockHeader); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_utxorpc_sync_v1_sync_proto_msgTypes[2].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*BlockBody); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_utxorpc_sync_v1_sync_proto_msgTypes[3].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*Block); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_utxorpc_sync_v1_sync_proto_msgTypes[4].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*StreamBlocksRequest); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_utxorpc_sync_v1_sync_proto_msgTypes[5].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*StreamBlocksResponse); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_utxorpc_sync_v1_sync_proto_msgTypes[6].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*FetchBlocksRequest); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_utxorpc_sync_v1_sync_proto_msgTypes[7].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*FetchBlocksResponse); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_utxorpc_sync_v1_sync_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   8,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_utxorpc_sync_v1_sync_proto_goTypes,
		DependencyIndexes: file_utxorpc_sync_v1_sync_proto_depIdxs,
		MessageInfos:      file_utxorpc_sync_v1_sync_proto_msgTypes,
	}.Build()
	File_utxorpc_sync_v1_sync_proto = out.File
	file_utxorpc_sync_v1_sync_proto_rawDesc = nil
	file_utxorpc_sync_v1_sync_proto_goTypes = nil
	file_utxorpc_sync_v1_sync_proto_depIdxs = nil
}