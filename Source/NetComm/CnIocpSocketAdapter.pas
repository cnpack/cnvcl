{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnIocpSocketAdapter;
{* |<PRE>
================================================================================
* 软件名称：Windows完成端口封装单元
* 单元名称：Windows完成端口封装实现单元
* 单元作者：cnwinds
*           菩提(cxmld@126.com)、Childe Ng、Liu Xiao 移植修改
* 备    注：
* 开发平台：PWin2000Pro + Delphi 7.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.11.04 V1.0
*                创建单元
================================================================================
|</PRE>}

{
  Windows完成端口封装单元。
  如果不指定并发线程数则使用(CPU个数*2+2)作为默认并发线程个数。

  create by cnwinds, 2007-3-1

  Modify by cnwinds 2007-3-20
  + 增加面向非连接的套接口收发支持

  Modify by cnwinds 2007-4-10
  * 修正了Udp的10054错误

  Modify by cnwinds 2007-4-18
  + 为ISocketIocpEvent接口增加GUID，使得接口之间可以导航

  Modify by cnwinds 2007-4-19
  + 将接口分离成面向流和面向包的两种类型。用户使用更方便。

  本类似乎还有些小问题,不知如何解决
  1.在测试程序中,关闭测试程序时, MEMO中会快速闪过一些数据
  这些好像是错误信息,不知是怎么来的.
  2.在测试程序中,发送和接收是0..100有101次,并应有202条信息,
  实际没有这么多, 没有找到原因
  3.以上二个问题是原类就有的,所以移植后的组件也有此问题
}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, WinSock, CnIocpSimpleMemPool;

const
  CN_MAX_WSABUF_COUNT = 8;

  SCnErrorCompletePortError = 'Error in Completion IO. Errro code %d';
  SCnErrorCallbackException = 'Exception in CallBacl: %s';
  SCnErrorSendBufferOverflow = 'Send Buffer Overflow. Max %d WSABUF.';

//  SCnErrorCompletePortError = '完成端口发生错误，错误代码(%d)';
//  SCnErrorCallbackException = '回调用户事件中发生异常(%s)';
//  SCnErrorSendBufferOverflow = '发送缓冲区超过大小！目前程序定义最多只能发送 %d 个WSABUF！';

type
  WSABUF = packed record
    len: U_LONG; { the length of the buffer }
    buf: PChar; { the pointer to the buffer }
  end {WSABUF};
  PWSABUF = ^WSABUF;
  LPWSABUF = PWSABUF;

  WSAOVERLAPPED = TOverlapped;
  TWSAOverlapped = WSAOverlapped;
  PWSAOverlapped = ^WSAOverlapped;
  LPWSAOVERLAPPED = PWSAOverlapped;
  TServiceType = LongInt;

  TFlowSpec = packed record
    TokenRate, // In Bytes/sec
    TokenBucketSize, // In Bytes
    PeakBandwidth, // In Bytes/sec
    Latency, // In microseconds
    DelayVariation: LongInt; // In microseconds
    ServiceType: TServiceType;
    MaxSduSize, MinimumPolicedSize: LongInt; // In Bytes
  end;
  PFlowSpec = ^TFLOWSPEC;

  QOS = packed record
    SendingFlowspec: TFlowSpec; { the flow spec for data sending }
    ReceivingFlowspec: TFlowSpec; { the flow spec for data receiving }
    ProviderSpecific: WSABUF; { additional provider specific stuff }
  end;
  TQualityOfService = QOS;
  PQOS = ^QOS;
  LPQOS = PQOS;

  LPCONDITIONPROC = function(lpCallerId: LPWSABUF; lpCallerData: LPWSABUF;
    lpSQOS, lpGQOS: LPQOS; lpCalleeId, lpCalleeData: LPWSABUF;
    g: DWORD; dwCallbackData: DWORD): Integer; stdcall;
  LPWSAOVERLAPPED_COMPLETION_ROUTINE = procedure(const dwError, cbTransferred:
    DWORD; const lpOverlapped: LPWSAOVERLAPPED; const dwFlags: DWORD); stdcall;

  // Peer 地址信息
  PPeerAddress = ^TPeerAddress;
  TPeerAddress = packed record
    Ip: Integer;          // IP (主机字节顺序)
    Port: Integer;        // 端口
  end;

  TCnIocpSocketAdapter = class;

  TSocketIocpThread = class;

  TSocketOverlappedType = (sotUnknow, sotSend, sotRecv, sotSendTo, sotRecvFrom);

  TSocketOverlapped = record
    Overlapped: TOverlapped;
    SocketOverlappedType: TSocketOverlappedType;
    Iocp: TCnIocpSocketAdapter;
    Param: Pointer;
    Buffer: array[0..CN_MAX_WSABUF_COUNT - 1] of WSABUF;
// 用于TCP接收
    TransfferBuffer: array[0..CN_MAX_WSABUF_COUNT - 1] of WSABUF;
    BufCount: Cardinal;
    WantBytesCount: Cardinal;
    TransferredBytesCount: Cardinal;

    SocketHandle: TSocket;
    SocketType: Cardinal;
    ToAddr: TSockAddr;
    FromAddr: TSockAddr;
    FromLen: Cardinal;
  end;
  PSocketOverlapped = ^TSocketOverlapped;

  TCnIocpSendEvent = procedure (Sender: TObject; Error, Transferred: Cardinal;
    Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer) of object;

  TCnIocpRecvEvent = procedure (Sender: TObject; Error, Transferred: Cardinal;
    Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer) of object;

  TCnIocpSendToEvent = procedure (Sender: TObject; Error, Transferred: Cardinal;
    Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer; ToAddr: PPeerAddress) of object;

  TCnIocpRecvFromEvent = procedure (Sender: TObject; Error, Transferred: Cardinal;
    Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer; FromAddr: PPeerAddress) of object;


{ TCnIocpSocketAdapter }

  TCnIocpSocketAdapter = class(TComponent)
  private
    FIocpHandle: THandle;
    FSocketIocpThreadArray: array of TSocketIocpThread;

    FOnSendEvent : TCnIocpSendEvent;
    FOnRecvEvent : TCnIocpRecvEvent;
    FOnSendToEvent : TCnIocpSendToEvent;
    FOnRecvFromEvent: TCnIocpRecvFromEvent;

//  FMemeryPoolType: Integer;
    FMemoryPool: TCnIocpSimpleMemPool;

    function SolveConnectResetBug(SocketHandle: TSocket): DWord;
    {* 修复UDP的BUG}

    function GetThreadCount: Integer;
    {* 获取线程总数}

    procedure CreateCompletionIo(var ConcurrentThreads: Cardinal;
      var NumberOfThreads: Cardinal);
    {* 创建完成端口,创建多线程,在构造函数中调用}
    procedure DestroyCompletionIo(var IocpHandle: THandle);
    {* 释放完成端口,和多线程}

    procedure ThrowException;

    function CreateOverlapped(Buffer: PWSABUF; BufCount: Cardinal;
      Param: Pointer): PSocketOverlapped; overload;
    {* 申请TSocketOverlapped变量}
    procedure DestroyOverlapped(SocketOverlapped: PSocketOverlapped);
    {* 释放TSocketOverlapped变量}
    procedure QueuedCompletionStatus(Milliseconds: Cardinal = INFINITE);
    {* 完成端口完成之后的处理, 被线程调用}
    procedure SetMemoryPool(const Value: TCnIocpSimpleMemPool);
  protected
    procedure DoSendEvent(Sender: TObject; Error, Transferred: Cardinal;
      Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer);
    procedure DoRecvEvent(Sender: TObject; Error, Transferred: Cardinal;
      Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer);

    procedure DoSendToEvent(Sender: TObject; Error, Transferred: Cardinal;
      Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer; ToAddr: PPeerAddress);
    procedure DoRecvFromEvent(Sender: TObject; Error, Transferred: Cardinal;
      Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer; FromAddr: PPeerAddress);            

    procedure InternalRentMemory(var MemoryPtr: Pointer);
    procedure InternalReturnMemory(MemoryPtr: Pointer);
  public
    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;

    procedure AssicoateSocket(SocketHandle: TSocket);

    procedure Recv(SocketHandle: TSocket; Memory: PChar; MemLength: Cardinal;
      Param: Pointer); overload;
    procedure Recv(SocketHandle: TSocket; Buffer: PWSABUF; BufCount: Cardinal;
      Param: Pointer); overload;

    procedure Send(SocketHandle: TSocket; Memory: PChar; MemLength: Cardinal;
      Param: Pointer); overload;
    procedure Send(SocketHandle: TSocket; Buffer: PWSABUF; BufCount: Cardinal;
      Param: Pointer); overload;

    procedure RecvFrom(SocketHandle: TSocket; Memory: PChar; MemLength: Cardinal;
      Param: Pointer); overload;
    procedure RecvFrom(SocketHandle: TSocket; Buffer: PWSABUF; BufCount: Cardinal;
      Param: Pointer); overload;

    procedure SendTo(SocketHandle: TSocket; Memory: PChar; MemLength: Cardinal;
      ToAddr: PPeerAddress; Param: Pointer); overload;
    procedure SendTo(SocketHandle: TSocket; Buffer: PWSABUF; BufCount: Cardinal;
      ToAddr: PPeerAddress; Param: Pointer); overload;

  published
    property ThreadCount: Integer read GetThreadCount;

    property MemoryPool: TCnIocpSimpleMemPool read FMemoryPool write SetMemoryPool;
    property OnSendEvent: TCnIocpSendEvent read FOnSendEvent write FOnSendEvent;
    property OnRecvEvent: TCnIocpRecvEvent read FOnRecvEvent write FOnRecvEvent;
    property OnSendToEvent: TCnIocpSendToEvent read FOnSendToEvent write FOnSendToEvent;
    property OnRecvFromEvent: TCnIocpRecvFromEvent read FOnRecvFromEvent write FOnRecvFromEvent; 
  end;

{ TSocketIocpThread }

  TSocketIocpThread = class(TThread)
  protected
    FSocketIocp: TCnIocpSocketAdapter;
    FIsRunning: Boolean;

    procedure Execute; override;
  public
    constructor Create(SocketIocp: TCnIocpSocketAdapter);
    procedure Stop;
  end;

function WSARecv(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; var
  lpNumberOfBytesRecvd: DWORD; var lpFlags: DWORD;
  lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine:
    LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;

function WSASend(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; var
  lpNumberOfBytesSent: DWORD; dwFlags: DWORD;
  lpOverlapped: LPWSAOVERLAPPED; lpCompletionRoutine:
    LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;

function WSAIoctl(s: TSocket; dwIoControlCode: DWORD; lpvInBuffer: Pointer;
  cbInBuffer: DWORD; lpvOutBuffer: Pointer; cbOutBuffer: DWORD;
  lpcbBytesReturned: LPDWORD; lpOverlapped: LPWSAOVERLAPPED;
    lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;

function WSARecvFrom(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; var
  lpNumberOfBytesRecvd: DWORD; var lpFlags: DWORD;
  lpFrom: PSockAddr; lpFromlen: PInteger; lpOverlapped: LPWSAOVERLAPPED;
    lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;

function WSASendTo(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; var
  lpNumberOfBytesSent: DWORD; dwFlags: DWORD;
  lpTo: PSockAddr; iTolen: Integer; lpOverlapped: LPWSAOVERLAPPED;
    lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;
    
implementation

const
  WINSOCK2_DLL = 'ws2_32.dll';

function WSAIoctl; external WINSOCK2_DLL name 'WSAIoctl';

function WSARecv; external WINSOCK2_DLL name 'WSARecv';

function WSARecvFrom; external WINSOCK2_DLL name 'WSARecvFrom';

function WSASend; external WINSOCK2_DLL name 'WSASend';

function WSASendTo; external WINSOCK2_DLL name 'WSASendTo';

function PeerAddress2SockAddr(PeerAddr: PPeerAddress): TSockAddr;
begin
  Result.sin_family := AF_INET;
  Result.sin_addr.s_addr := htonl(PeerAddr.Ip);
  Result.sin_port := htons(PeerAddr.Port);
end;

function SockAddr2PeerAddress(SockAddr: PSockAddr): TPeerAddress;
begin
  Result.Ip := ntohl(SockAddr.sin_addr.s_addr);
  Result.Port := ntohs(SockAddr.sin_port);
end;

destructor TCnIocpSocketAdapter.Destroy;
begin
  if not (csDesigning in ComponentState) then
    DestroyCompletionIo(FIocpHandle);
  inherited;
end;

constructor TCnIocpSocketAdapter.Create(AOwner: TComponent);
var k, l: Cardinal;
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then
  begin
    k := 0;
    l := 0;
    CreateCompletionIo(k, l);

//    FMemeryPoolType := CnSimpleMemoryPool.GetFreeMemoryType();
//    CnSimpleMemoryPool.RegisterMemoryType(FMemeryPoolType, nil, nil);
//    CnMemoryPool.SetParam(FMemeryPoolType,  0 * 2 + 5);
//    这一句话为什么有错误,请调试
  end;
end;

procedure TCnIocpSocketAdapter.CreateCompletionIo(var ConcurrentThreads: Cardinal;
  var NumberOfThreads: Cardinal);
var
  I: Integer;
  SystemInfo: TSystemInfo;
begin
  if ConcurrentThreads = 0 then
  begin
    GetSystemInfo(SystemInfo);
    ConcurrentThreads := SystemInfo.dwNumberOfProcessors * 2 + 2;
  end;
  if NumberOfThreads < ConcurrentThreads then
    NumberOfThreads := ConcurrentThreads;

  FIocpHandle := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0,
    ConcurrentThreads);
  if FIocpHandle = 0 then ThrowException;

  SetLength(FSocketIocpThreadArray, NumberOfThreads);
  for I := Low(FSocketIocpThreadArray) to High(FSocketIocpThreadArray) do
  begin
    FSocketIocpThreadArray[I] := TSocketIocpThread.Create(Self);
    FSocketIocpThreadArray[I].Priority := tpHigher;
  end;
end;

procedure TCnIocpSocketAdapter.DestroyCompletionIo(var IocpHandle: THandle);
var
  I: Integer;
begin
  // 通知线程停止，并发出通知消息。
  for I := Low(FSocketIocpThreadArray) to High(FSocketIocpThreadArray) do
    FSocketIocpThreadArray[I].Stop;
  for I := Low(FSocketIocpThreadArray) to High(FSocketIocpThreadArray) do
    PostQueuedCompletionStatus(FIocpHandle, 0, 0, nil);

  // 等待线程停止并释放
  for I := Low(FSocketIocpThreadArray) to High(FSocketIocpThreadArray) do
  begin
    FSocketIocpThreadArray[I].WaitFor;
    FreeAndNil(FSocketIocpThreadArray[I]);
  end;
  SetLength(FSocketIocpThreadArray, 0);

  CloseHandle(IocpHandle);
  IocpHandle := INVALID_HANDLE_VALUE;
end;

procedure TCnIocpSocketAdapter.ThrowException;
begin
  raise Exception.Create(Format(SCnErrorCompletePortError, [GetLastError]));
end;

procedure TCnIocpSocketAdapter.AssicoateSocket(SocketHandle: TSocket);
var
  Handle: THandle;
  Val: Integer;
  Len: Integer;
begin
  // 注意：UDP在.net 2003以前的开发库中有产生10054的bug。需要修复。
  Len := SizeOf(Val);
  if getsockopt(SocketHandle, SOL_SOCKET, SO_TYPE, @Val, Len) = 0 then
  begin
    if Val = SOCK_DGRAM then
    begin
      if SolveConnectResetBug(SocketHandle) <> 0 then
        raise Exception.Create('Can NOT Fix Udp 10054 Error.');
    end;
  end;

  Handle := CreateIoCompletionPort(SocketHandle, FIocpHandle, 0, 0);
  if Handle = 0 then
    ThrowException;
end;

function TCnIocpSocketAdapter.SolveConnectResetBug(SocketHandle: TSocket): DWord;
const
  SIO_UDP_CONNRESET = $80000000 or $18000000 or 12;
var
  NewBehavior: Boolean;
  BytesReturned: DWord;
  Status: DWord;
begin
  NewBehavior := False;
  BytesReturned := 0;
  Status := WSAIoctl(SocketHandle, SIO_UDP_CONNRESET,
                     @NewBehavior, SizeOf(NewBehavior),
                     nil, 0, @BytesReturned, nil, nil);
  Result := Status;
end;

function TCnIocpSocketAdapter.CreateOverlapped(Buffer: PWSABUF; BufCount: Cardinal;
 Param: Pointer): PSocketOverlapped;
begin
  // 建立重叠IO内存块
  InternalRentMemory(Pointer(Result));
  
  Result.SocketOverlappedType := sotUnknow;
  if BufCount > SizeOf(Result.Buffer) then
    raise Exception.CreateFmt(SCnErrorSendBufferOverflow, [CN_MAX_WSABUF_COUNT]);

  Move(Buffer^, Result.Buffer, SizeOf(WSABUF) * BufCount);
  Move(Buffer^, Result.TransfferBuffer, SizeOf(WSABUF) * BufCount);

  Result.BufCount := BufCount;
  Result.Iocp := Self;

  Result.Param := Param;
  Result.WantBytesCount := 0;
  Result.TransferredBytesCount := 0;
  Result.SocketType := 0;
end;

procedure TCnIocpSocketAdapter.DestroyOverlapped(SocketOverlapped: PSocketOverlapped);
begin
  // 释放重叠IO内存块
  SocketOverlapped.Iocp := nil;
  InternalReturnMemory(SocketOverlapped);
end;

procedure TCnIocpSocketAdapter.DoRecvEvent(Sender: TObject; Error, Transferred: Cardinal; Buffer: PWSABUF;
  BufCount: Cardinal; Param: Pointer);
begin
  if Assigned(FOnSendEvent) then
    FOnRecvEvent(Sender, Error, Transferred, Buffer, BufCount, Param);
end;

procedure TCnIocpSocketAdapter.DoRecvFromEvent(Sender: TObject; Error,
  Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer;
  FromAddr: PPeerAddress);
begin
  if Assigned(FOnRecvFromEvent) then
    FOnRecvFromEvent(Sender, Error, Transferred, Buffer, BufCount, Param,FromAddr);
end;

procedure TCnIocpSocketAdapter.DoSendEvent(Sender: TObject; Error, Transferred: Cardinal;
  Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer);
begin
  if Assigned(FOnSendEvent) then
    FOnSendEvent(Sender, Error, Transferred, Buffer, BufCount, Param);
end;

procedure TCnIocpSocketAdapter.DoSendToEvent(Sender: TObject; Error,
  Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer;
  ToAddr: PPeerAddress);
begin
  if Assigned(FOnSendToEvent) then
    FOnSendToEvent(Sender, Error, Transferred, Buffer, BufCount, Param, ToAddr);
end;

function TCnIocpSocketAdapter.GetThreadCount: Integer;
begin
  Result := Length(FSocketIocpThreadArray);
end;

procedure TCnIocpSocketAdapter.QueuedCompletionStatus(Milliseconds: Cardinal);
var
  NumberOfBytesTransferred: Cardinal;
  SocketOverlapped: PSocketOverlapped;
  Error: Cardinal;
  Tmp: Cardinal;
  IsFreeOverlapped: Boolean;

  procedure CallbackEvent;
  var
    PeerAddr: TPeerAddress;
  begin
    try
      // 产生回调事件
      case SocketOverlapped.SocketOverlappedType of
        sotSend:
        begin
          DoSendEvent(Self, Error,
              NumberOfBytesTransferred, @SocketOverlapped.Buffer,
              SocketOverlapped.BufCount, SocketOverlapped.Param);
        end;
        sotRecv:
        begin
          DoRecvEvent(Self, Error,
              NumberOfBytesTransferred, @SocketOverlapped.Buffer,
              SocketOverlapped.BufCount, SocketOverlapped.Param);
        end;
        sotSendTo:
        begin
          PeerAddr := SockAddr2PeerAddress(@SocketOverlapped.ToAddr);
          DoSendToEvent(Self, Error,
              NumberOfBytesTransferred, @SocketOverlapped.Buffer,
              SocketOverlapped.BufCount, SocketOverlapped.Param,
              @PeerAddr);
        end;
        sotRecvFrom:
        begin
          PeerAddr := SockAddr2PeerAddress(@SocketOverlapped.FromAddr);
          DoRecvFromEvent(Self, Error,
              NumberOfBytesTransferred, @SocketOverlapped.Buffer,
              SocketOverlapped.BufCount, SocketOverlapped.Param,
              @PeerAddr);
        end;
      end;
    except
      On E: Exception do
        ;
    end;
  end;

  procedure ProcessEvent(var IsFreeOverlapped: Boolean);
  var
    I: Integer;
    WsaBuf: PWSABUF;
    Flags: Cardinal;
    Count: Cardinal;
  begin
    IsFreeOverlapped := True;

    // 发生了错误，直接调用回调事件让用户处理
    if (Error <> 0) then
    begin
      CallbackEvent;
      Exit;
    end;

    // 非TCP接收，直接调用回调事件让用户处理
    if not ((SocketOverlapped.SocketType = SOCK_STREAM) and
       (SocketOverlapped.SocketOverlappedType = sotRecv)) then
    begin
      CallbackEvent;
      Exit;
    end;

  {
    TCP在接收大块的数据时，返回的事件可能告知你接收了一部分数据，
    你需要继续请求后续的数据。这在用户端会造成一些麻烦。
    下面的代码让用户端只在收到了期望的所有数据后才得到事件回调。
  }
    if (NumberOfBytesTransferred + SocketOverlapped.TransferredBytesCount =
          SocketOverlapped.WantBytesCount) then
    begin
      // 已经收到了用户期望的数据个数
      NumberOfBytesTransferred := SocketOverlapped.WantBytesCount;
      CallbackEvent;
      Exit;
    end;

    // 继续接收后续的数据
    Inc(SocketOverlapped.TransferredBytesCount, NumberOfBytesTransferred);
    for I := 0 to SocketOverlapped.BufCount - 1 do
    begin
      WsaBuf := PWSABUF(@SocketOverlapped.TransfferBuffer[I]);
      if WsaBuf.len <> 0 then
      begin
        if WsaBuf.len >= Integer(NumberOfBytesTransferred) then
        begin
          Dec(WsaBuf.len, NumberOfBytesTransferred);
          Inc(WsaBuf.buf, NumberOfBytesTransferred);
          Break;
        end else
        begin
          Dec(NumberOfBytesTransferred, WsaBuf.len);
          WsaBuf.len := 0;
        end;
      end;
    end;
    Flags := 0;
    Count := SocketOverlapped.WantBytesCount -
      SocketOverlapped.TransferredBytesCount;
    if WSARecv(SocketOverlapped.SocketHandle,
               @SocketOverlapped.TransfferBuffer,
               SocketOverlapped.BufCount,
               Count, Flags,
               PWSAOverlapped(SocketOverlapped), nil) = SOCKET_ERROR then
    begin
      if GetLastError <> ERROR_IO_PENDING then
        // 发送失败，压入一个接收失败的事件（当做连接断开处理）
        PostQueuedCompletionStatus(FIocpHandle, 0, 0,
          POverlapped(SocketOverlapped));
    end;
    IsFreeOverlapped := False;
  end;

begin
  Error := 0;
  IsFreeOverlapped := False;
  try
    if GetQueuedCompletionStatus(FIocpHandle, NumberOfBytesTransferred,
      Tmp, POverlapped(SocketOverlapped), Milliseconds) then
    begin
      if SocketOverlapped <> nil then
      begin
        if NumberOfBytesTransferred = 0 then Error := WSAECONNRESET;
        ProcessEvent(IsFreeOverlapped);
      end;
    end else
    begin
      if SocketOverlapped <> nil then
      begin
        Error := GetLastError;
        ProcessEvent(IsFreeOverlapped);
      end else
        if GetLastError <> WAIT_TIMEOUT then ThrowException;
    end;
  finally
    if (SocketOverlapped <> nil) and IsFreeOverlapped then
      DestroyOverlapped(SocketOverlapped);
  end;
end;

procedure TCnIocpSocketAdapter.Recv(SocketHandle: TSocket; Buffer: PWSABUF;
  BufCount: Cardinal; Param: Pointer);
var
  SocketOverlapped: PSocketOverlapped;
  NumberOfBytesRecvd: Cardinal;
  Flags: Cardinal;
  I: Integer;
  Len: Integer;
begin
  SocketOverlapped := CreateOverlapped(Buffer, BufCount, Param);
  SocketOverlapped.SocketOverlappedType := sotRecv;
  SocketOverlapped.SocketHandle := SocketHandle;
  Len := SizeOf(SocketOverlapped.SocketHandle);
  if 0 <> getsockopt(SocketHandle, SOL_SOCKET, SO_TYPE,
    @SocketOverlapped.SocketType, Len) then ThrowException;
  for I := 0 to BufCount - 1 do
    Inc(SocketOverlapped.WantBytesCount,
      PWSABUF(Integer(Buffer) + I * SizeOf(WSABUF)).len);

  Flags := 0;
  if WSARecv(SocketOverlapped.SocketHandle,
      @SocketOverlapped.TransfferBuffer,
      SocketOverlapped.BufCount, NumberOfBytesRecvd, Flags,
      PWSAOverlapped(SocketOverlapped), nil) = SOCKET_ERROR then
  begin
    if GetLastError <> ERROR_IO_PENDING then ThrowException;
  end;
end;

procedure TCnIocpSocketAdapter.Recv(SocketHandle: TSocket; Memory: PChar;
  MemLength: Cardinal; Param: Pointer);
var
  Buffer: WSABUF;
begin
  Buffer.len := MemLength;
  Buffer.buf := Memory;
  Recv(SocketHandle, PWSABUF(@Buffer), 1, Param);
end;

procedure TCnIocpSocketAdapter.Send(SocketHandle: TSocket; Buffer: PWSABUF;
  BufCount: Cardinal; Param: Pointer);
var
  SocketOverlapped: PSocketOverlapped;
  NumberOfBytesSent: Cardinal;
  I: Integer;
  Len: Integer;
begin
  SocketOverlapped := CreateOverlapped(Buffer, BufCount, Param);
  SocketOverlapped.SocketOverlappedType := sotSend;
  SocketOverlapped.SocketHandle := SocketHandle;
  Len := SizeOf(SocketOverlapped.SocketHandle);

  if 0 <> getsockopt(SocketHandle, SOL_SOCKET, SO_TYPE,
    @SocketOverlapped.SocketType, Len) then ThrowException;

  for I := 0 to BufCount - 1 do
    Inc(SocketOverlapped.WantBytesCount,
      PWSABUF(Integer(Buffer) + I * SizeOf(WSABUF)).len);
      
  if WSASend(SocketOverlapped.SocketHandle,
      @SocketOverlapped.TransfferBuffer,
      SocketOverlapped.BufCount, NumberOfBytesSent, 0,
      PWSAOverlapped(SocketOverlapped), nil) = SOCKET_ERROR then
  begin
    if GetLastError <> ERROR_IO_PENDING then ThrowException;
  end;
end;

procedure TCnIocpSocketAdapter.Send(SocketHandle: TSocket; Memory: PChar;
  MemLength: Cardinal; Param: Pointer);
var
  Buffer: WSABUF;
begin
  Buffer.len := MemLength;
  Buffer.buf := Memory;
  Send(SocketHandle, PWSABUF(@Buffer), 1, Param);
end;

procedure TCnIocpSocketAdapter.RecvFrom(SocketHandle: TSocket; Buffer: PWSABUF;
  BufCount: Cardinal; Param: Pointer);
var
  SocketOverlapped: PSocketOverlapped;
  NumberOfBytesRecvd: Cardinal;
  Flags: Cardinal;
  I: Integer;
  Len: Integer;
begin
  SocketOverlapped := CreateOverlapped(Buffer, BufCount, Param);
  SocketOverlapped.SocketOverlappedType := sotRecvFrom;
  SocketOverlapped.SocketHandle := SocketHandle;
  Len := SizeOf(SocketOverlapped.SocketHandle);
  if 0 <> getsockopt(SocketHandle, SOL_SOCKET, SO_TYPE,
    @SocketOverlapped.SocketType, Len) then ThrowException;
  SocketOverlapped.FromLen := SizeOf(TSockAddr);

  for I := 0 to BufCount - 1 do
    Inc(SocketOverlapped.WantBytesCount,
      PWSABUF(Integer(Buffer) + I * SizeOf(WSABUF)).len);
  Flags := 0;
  if WSARecvFrom(SocketOverlapped.SocketHandle,
      @SocketOverlapped.TransfferBuffer,
      SocketOverlapped.BufCount, NumberOfBytesRecvd, Flags,
      @SocketOverlapped.FromAddr, @SocketOverlapped.FromLen,
      PWSAOverlapped(SocketOverlapped), nil) = SOCKET_ERROR then
  begin
    if GetLastError <> ERROR_IO_PENDING then ThrowException;
  end;
end;

procedure TCnIocpSocketAdapter.RecvFrom(SocketHandle: TSocket; Memory: PChar;
  MemLength: Cardinal; Param: Pointer);
var
  Buffer: WSABUF;
begin
  Buffer.len := MemLength;
  Buffer.buf := Memory;
  RecvFrom(SocketHandle, PWSABUF(@Buffer), 1, Param);
end;

procedure TCnIocpSocketAdapter.SendTo(SocketHandle: TSocket; Buffer: PWSABUF;
  BufCount: Cardinal; ToAddr: PPeerAddress; Param: Pointer);
var
  SocketOverlapped: PSocketOverlapped;
  NumberOfBytesSent: Cardinal;
  I: Integer;
  Len: Integer;
begin
  SocketOverlapped := CreateOverlapped(Buffer, BufCount, Param);
  SocketOverlapped.SocketOverlappedType := sotSendTo;
  SocketOverlapped.SocketHandle := SocketHandle;
  Len := SizeOf(SocketOverlapped.SocketHandle);

  if 0 <> getsockopt(SocketHandle, SOL_SOCKET, SO_TYPE,
    @SocketOverlapped.SocketType, Len) then ThrowException;

  SocketOverlapped.ToAddr := PeerAddress2SockAddr(ToAddr);

  for I := 0 to BufCount - 1 do
    Inc(SocketOverlapped.WantBytesCount,
      PWSABUF(Integer(Buffer) + I * SizeOf(WSABUF)).len);

  if WSASendTo(SocketOverlapped.SocketHandle,
      @SocketOverlapped.TransfferBuffer,
      SocketOverlapped.BufCount, NumberOfBytesSent, 0,
      @SocketOverlapped.ToAddr, SizeOf(TSockAddr),
      PWSAOverlapped(SocketOverlapped), nil) = SOCKET_ERROR then
  begin
    if GetLastError <> ERROR_IO_PENDING then ThrowException;
  end;
end;

procedure TCnIocpSocketAdapter.SetMemoryPool(const Value: TCnIocpSimpleMemPool);
begin
  if Value <> nil then
  begin
    //修改分配内存的大小
    Value.MemorySize := SizeOf(TSocketOverlapped);
  end;
  FMemoryPool := Value;
end;

procedure TCnIocpSocketAdapter.SendTo(SocketHandle: TSocket; Memory: PChar;
  MemLength: Cardinal; ToAddr: PPeerAddress; Param: Pointer);
var
  Buffer: WSABUF;
begin
  Buffer.len := MemLength;
  Buffer.buf := Memory;
  SendTo(SocketHandle, PWSABUF(@Buffer), 1, ToAddr, Param);
end;

{ TSocketIocpThread }

constructor TSocketIocpThread.Create(SocketIocp: TCnIocpSocketAdapter);
begin
  FSocketIocp := SocketIocp;
  FIsRunning := True;
  inherited Create(False);
end;

procedure TSocketIocpThread.Execute;
begin
  try
    while FIsRunning do
    begin
      FSocketIocp.QueuedCompletionStatus;
    end;
  except
    // nothing
  end;
end;

procedure TSocketIocpThread.Stop;
begin
  FIsRunning := False;
end;

procedure TCnIocpSocketAdapter.InternalRentMemory(var MemoryPtr: Pointer);
begin
  if FMemoryPool = nil then
    MemoryPtr := Pointer(GlobalAlloc(GPTR, SizeOf(TSocketOverlapped)))
  else
    FMemoryPool.RentMemory(MemoryPtr);
end;

procedure TCnIocpSocketAdapter.InternalReturnMemory(MemoryPtr: Pointer);
begin
  if FMemoryPool = nil then
    GlobalFree(Cardinal(MemoryPtr))
  else
    FMemoryPool.ReturnMemory(MemoryPtr);
end;

initialization
  

finalization
  // nothing

end.
