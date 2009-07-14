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

unit CnUDP;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：UDP 通讯单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：定义了 TCnUDP，使用非阻塞方式进行 UDP 通讯，支持广播
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.11.28 V1.1
*                加入一控制接收缓冲区大小的属性
*           2003.11.21 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, Classes, SysUtils, WinSock, Forms, contnrs;

type

//==============================================================================
// UDP 通讯类
//==============================================================================

{ TCnUDP }

  TOnReceive = procedure(Sender: TComponent; Buffer: Pointer; Len: Integer;
    FromIP: string; Port: Integer) of object;
  {* 接收到数据事件
   |<PRE>
     Sender     - TCnUDP 对象
     Buffer     - 数据缓冲区
     Len        - 数据缓冲区长度
     FromIP     - 数据来源 IP
     Port       - 数据来源端口号
   |</PRE>}

  TCnUDP = class(TComponent)
  {* 使用非阻塞方式进行 UDP 通讯的类。支持广播、数据队列等。}
  private
    FRemoteHost: string;
    FRemotePort: Integer;
    FLocalPort: Integer;
    FSocketWindow: HWND;
    FOnDataReceived: TOnReceive;
    FListening: Boolean;
    Wait_Flag: Boolean;
    RemoteAddress, RemoteAddress2: TSockAddr;
    RemoteHostS: PHostEnt;
    Succeed: Boolean;
    Procing: Boolean;
    MyWSAData: TWSAData;
    EventHandle: THandle;
    ThisSocket: TSocket;
    Queue: TQueue;
    FLastError: Integer;
    FRecvBufSize: Cardinal;
    FRecvBuf: Pointer;
    procedure WndProc(var Message: TMessage);
    function ResolveRemoteHost(ARemoteHost: string): Boolean;
    procedure SetLocalPort(NewLocalPort: Integer);
    procedure ProcessIncomingdata;
    procedure ProcessQueue;
    procedure FreeQueueItem(P: Pointer);
    function GetQueueCount: Integer;
    procedure SetupLastError;
    function GetLocalHost: string;
    procedure UpdateBinding;
    procedure SetRecvBufSize(const Value: Cardinal);
  protected
    procedure Wait;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SendStream(DataStream: TStream; BroadCast: Boolean = False): Boolean;
    {* 发送一个数据流。如果 BroadCase 为真，执行 UDP 广播，否则发送数据到
       RomoteHost 的机器上的 RemotePort 端口}
    function SendBuffer(Buff: Pointer; Length: Integer; BroadCast:
      Boolean = False): Boolean;
    {* 发送一个数据块。如果 BroadCase 为真，执行 UDP 广播，否则发送数据到
       RomoteHost 的机器上的 RemotePort 端口}
    procedure ClearQueue;
    {* 清空数据队列。如果用户来不及处理接收到的数据，组件会把新数据包放到数据
       队列中，调用该方法可清空数据队列}

    property LastError: Integer read FLastError;
    {* 最后一次错误的错误号，只读属性}
    property Listening: Boolean read FListening;
    {* 表示当前是否正在监听本地端口，只读属性}
    property QueueCount: Integer read GetQueueCount;
    {* 当前数据队列的长度，只读属性}
  published
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    {* 要发送 UDP 数据的目标主机地址}
    property RemotePort: Integer read FRemotePort write FRemotePort;
    {* 要发送 UDP 数据的目标主机端口号}
    property LocalHost: string read GetLocalHost;
    {* 返回本机 IP 地址，只读属性}
    property LocalPort: Integer read FLocalPort write SetLocalPort;
    {* 本地监听的端口号}
    property RecvBufSize: Cardinal read FRecvBufSize write SetRecvBufSize default 2048;
    {* 接收的数据缓冲区大小，默认 2048}
    property OnDataReceived: TOnReceive read FOnDataReceived write
      FOnDataReceived;
    {* 接收到 UDP 数据包事件}
  end;

// 取广播地址
procedure GetBroadCastAddress(sInt: TStrings);

// 取本机IP地址
procedure GetLocalIPAddress(sInt: TStrings);

implementation

{$R-}

//==============================================================================
// 辅助过程
//==============================================================================

// 从Winsock 2.0导入函数WSAIOCtl
function WSAIoctl(s: TSocket; cmd: DWORD; lpInBuffer: PCHAR; dwInBufferLen:
  DWORD;
  lpOutBuffer: PCHAR; dwOutBufferLen: DWORD;
  lpdwOutBytesReturned: LPDWORD;
  lpOverLapped: POINTER;
  lpOverLappedRoutine: POINTER): Integer; stdcall; external 'WS2_32.DLL';

const
  SIO_GET_INTERFACE_LIST = $4004747F;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;

type
  sockaddr_gen = packed record
    AddressIn: sockaddr_in;
    filler: packed array[0..7] of AnsiChar;
  end;

  INTERFACE_INFO = packed record
    iiFlags: u_long;                    // Interface flags
    iiAddress: sockaddr_gen;            // Interface address
    iiBroadcastAddress: sockaddr_gen;   // Broadcast address
    iiNetmask: sockaddr_gen;            // Network mask
  end;

// 取广播地址
procedure DoGetIPAddress(sInt: TStrings; IsBroadCast: Boolean);
var
  s: TSocket;
  wsaD: WSADATA;
  NumInterfaces: Integer;
  BytesReturned, SetFlags: u_long;
  pAddr, pMask, pCast: TInAddr;
  pAddrStr: string;
  PtrA: pointer;
  Buffer: array[0..20] of INTERFACE_INFO;
  i: Integer;
begin
  WSAStartup($0101, wsaD);              // Start WinSock
  s := Socket(AF_INET, SOCK_STREAM, 0); // Open a socket
  if (s = INVALID_SOCKET) then
    exit;

  try                                   // Call WSAIoCtl
    PtrA := @bytesReturned;
    if (WSAIoCtl(s, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA, nil,
      nil) <> SOCKET_ERROR) then
    begin                               // If ok, find out how
      // many interfaces exist
      NumInterfaces := BytesReturned div SizeOf(INTERFACE_INFO);
      sInt.Clear;
      for i := 0 to NumInterfaces - 1 do // For every interface
      begin
        SetFlags := Buffer[i].iiFlags;
        if (SetFlags and IFF_BROADCAST = IFF_BROADCAST) and not
          (SetFlags and IFF_LOOPBACK = IFF_LOOPBACK) then
        begin
          pAddr := Buffer[i].iiAddress.AddressIn.sin_addr;
          pMask := Buffer[i].iiNetmask.AddressIn.sin_addr;
          if IsBroadCast then
          begin
            pCast.S_addr := pAddr.S_addr or not pMask.S_addr;
            pAddrStr := string(inet_ntoa(pCast));
          end
          else
          begin
            pAddrStr := string(inet_ntoa(pAddr));
          end;
            
          if sInt.IndexOf(pAddrStr) < 0 then
            sInt.Add(pAddrStr);
        end;
      end;
    end;
  except
    ;
  end;
  CloseSocket(s);
  WSACleanUp;
end;

// 取本机IP地址
procedure GetLocalIPAddress(sInt: TStrings);
begin
  DoGetIPAddress(sInt, False);
end;  

// 取广播地址
procedure GetBroadCastAddress(sInt: TStrings);
begin
  DoGetIPAddress(sInt, True);
end;

//==============================================================================
// UDP 通讯类
//==============================================================================

{ TCnUDP }

const
  WM_ASYNCHRONOUSPROCESS = WM_USER + 101;
  Const_cmd_true = 'TRUE';

type
  PRecvDataRec = ^TRecvDataRec;
  TRecvDataRec = record
    FromIP: string[128];
    FromPort: u_short;
    Buff: Pointer;
    BuffSize: Integer;
  end;

constructor TCnUDP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Queue := TQueue.Create;
  FListening := False;
  Procing := False;
  FRecvBufSize := 2048;

  GetMem(RemoteHostS, MAXGETHOSTSTRUCT);
  FSocketWindow := AllocateHWND(WndProc);
  EventHandle := CreateEvent(nil, True, False, '');
  if WSAStartup($0101, MyWSADATA) = 0 then
  begin
    ThisSocket := Socket(AF_INET, SOCK_DGRAM, 0);
    if ThisSocket = TSocket(INVALID_SOCKET) then
    begin
      SetupLastError;
      WSACleanup;
      Exit;
    end;
    setsockopt(ThisSocket, SOL_SOCKET, SO_DONTLINGER, Const_cmd_true, 4);
    setsockopt(ThisSocket, SOL_SOCKET, SO_BROADCAST, Const_cmd_true, 4);
    FListening := True;
  end;
end;

destructor TCnUDP.Destroy;
begin
  if FRecvBuf <> nil then
  begin
    FreeMem(FRecvBuf);
    FRecvBuf := nil;
  end;

  ClearQueue;
  Queue.Free;
  FreeMem(RemoteHostS, MAXGETHOSTSTRUCT);
  DeallocateHWND(FSocketWindow);
  CloseHandle(EventHandle);
  if ThisSocket <> 0 then
    closesocket(ThisSocket);
  if FListening then
    WSACleanup;
  inherited Destroy;
end;

procedure TCnUDP.UpdateBinding;
var
  Flag: DWORD;
begin
  if not (csDesigning in ComponentState) then
  begin
    FListening := False;
    RemoteAddress2.sin_addr.S_addr := Inet_Addr('0.0.0.0');
    RemoteAddress2.sin_family := AF_INET;
    RemoteAddress2.sin_port := htons(FLocalPort);
    Wait_Flag := False;
    if WinSock.Bind(ThisSocket, RemoteAddress2, SizeOf(RemoteAddress2)) =
      SOCKET_ERROR then
    begin
      SetupLastError;
      WSACleanup;
      Exit;
    end;
    // Allow to send to 255.255.255.255
    WinSock.setsockopt(ThisSocket, SOL_SOCKET, SO_BROADCAST,
      PAnsiChar(@Flag), SizeOf(Flag));
    WSAAsyncSelect(ThisSocket, FSocketWindow, WM_ASYNCHRONOUSPROCESS, FD_READ);
    FListening := True;
  end;
end;

procedure TCnUDP.Loaded;
begin
  inherited;
  UpdateBinding;
end;

procedure TCnUDP.SetLocalPort(NewLocalPort: Integer);
begin
  if NewLocalPort <> FLocalPort then
  begin
    FListening := False;
    if ThisSocket <> 0 then
      closesocket(ThisSocket);
    WSACleanup;
    if WSAStartup($0101, MyWSADATA) = 0 then
    begin
      ThisSocket := Socket(AF_INET, SOCK_DGRAM, 0);
      if ThisSocket = TSocket(INVALID_SOCKET) then
      begin
        WSACleanup;
        SetupLastError;
        Exit;
      end;
    end;
    FLocalPort := NewLocalPort;
    if not (csLoading in ComponentState) then
      UpdateBinding;
  end;
end;

function TCnUDP.ResolveRemoteHost(ARemoteHost: string): Boolean;
var
  Buf: array[0..127] of AnsiChar;
begin
  Result := False;
  if not FListening then Exit;
  try
    RemoteAddress.sin_addr.S_addr := Inet_Addr(PAnsiChar(StrPCopy(Buf, ARemoteHost)));
    if RemoteAddress.sin_addr.S_addr = SOCKET_ERROR then
    begin
      Wait_Flag := False;
      WSAAsyncGetHostByName(FSocketWindow, WM_ASYNCHRONOUSPROCESS, Buf,
        PAnsiChar(RemoteHostS), MAXGETHOSTSTRUCT);
      repeat
        Wait;
      until Wait_Flag;
      if Succeed then
      begin
        with RemoteAddress.sin_addr.S_un_b do
        begin
          s_b1 := remotehostS.h_addr_list^[0];
          s_b2 := remotehostS.h_addr_list^[1];
          s_b3 := remotehostS.h_addr_list^[2];
          s_b4 := remotehostS.h_addr_list^[3];
        end;
      end;
    end;
  except
    ;
  end;
  if RemoteAddress.sin_addr.S_addr <> 0 then
    Result := True;
  if not Result then
    SetupLastError;
end;

function TCnUDP.SendStream(DataStream: TStream; BroadCast: Boolean): Boolean;
var
  Buff: Pointer;
begin
  GetMem(Buff, DataStream.Size);
  try
    DataStream.Position := 0;
    DataStream.Read(Buff^, DataStream.Size);
    Result := SendBuffer(Buff, DataStream.Size, BroadCast);
  finally
    FreeMem(Buff);
  end;
end;

function TCnUDP.SendBuffer(Buff: Pointer; Length: Integer;
  BroadCast: Boolean): Boolean;
var
  Hosts: TStrings;
  i: Integer;
  
  function DoSendBuffer(Buff: Pointer; Length: Integer; Host: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    try
      if not ResolveRemoteHost(Host) then
        Exit;
      RemoteAddress.sin_family := AF_INET;
      RemoteAddress.sin_port := htons(FRemotePort);
      i := SizeOf(RemoteAddress);
      if WinSock.sendto(ThisSocket, Buff^, Length, 0, RemoteAddress, i)
        <> SOCKET_ERROR then
        Result := True
      else
        SetupLastError;
    except
      SetupLastError;
    end;
  end;
begin
  if BroadCast then
  begin
    Result := False;
    Hosts := TStringList.Create;
    try
      GetBroadCastAddress(Hosts);
      for i := 0 to Hosts.Count - 1 do
        if DoSendBuffer(Buff, Length, Hosts[i]) then
          Result := True;
    finally
      Hosts.Free;
    end;
  end
  else
    Result := DoSendBuffer(Buff, Length, FRemoteHost);
end;

function TCnUDP.GetQueueCount: Integer;
begin
  Result := Queue.Count;
end;

procedure TCnUDP.FreeQueueItem(P: Pointer);
var
  Rec: PRecvDataRec;
begin
  Rec := PRecvDataRec(P);
  Rec.FromIP := '';
  FreeMem(Rec.Buff);
  FreeMem(Rec);
end;

procedure TCnUDP.ClearQueue;
var
  Rec: PRecvDataRec;
begin
  while Queue.Count > 0 do
  begin
    Rec := Queue.Pop;
    FreeQueueItem(Rec);
  end;
end;

procedure TCnUDP.ProcessQueue;
var
  Rec: PRecvDataRec;
begin
  if Procing then Exit;
  Procing := True;
  try
    while Queue.Count > 0 do
    begin
      Rec := Queue.Pop;
      if Assigned(FOnDataReceived) then
        FOnDataReceived(Self, Rec.Buff, Rec.BuffSize, string(Rec.FromIP), Rec.FromPort);
      FreeQueueItem(Rec);
    end;
  finally
    Procing := False;
  end;
end;

procedure TCnUDP.WndProc(var Message: TMessage);
begin
  if FListening then
  begin
    with Message do
    begin
      if Msg = WM_ASYNCHRONOUSPROCESS then
      begin
        if LParamLo = FD_READ then
        begin
          ProcessIncomingdata;
          if not Procing then
            ProcessQueue;
        end
        else
        begin
          Wait_Flag := True;
          if LParamHi > 0 then
            Succeed := False
          else
            Succeed := True;
        end;
        SetEvent(EventHandle);
      end
      else
        Result := DefWindowProc(FSocketWindow, Msg, WParam, LParam);
    end;
  end;
end;

procedure TCnUDP.ProcessIncomingdata;
var
  from: TSockAddr;
  i: Integer;
  Rec: PRecvDataRec;
  IBuffSize: Integer;
begin
  i := SizeOf(from);
  if FRecvBuf = nil then
    GetMem(FRecvBuf, FRecvBufSize);

  IBuffSize := WinSock.recvfrom(ThisSocket, FRecvBuf^, FRecvBufSize, 0, from, i);
  if (IBuffSize > 0) and Assigned(FOnDataReceived) then
  begin
    GetMem(Rec, SizeOf(TRecvDataRec));
    ZeroMemory(Rec, SizeOf(TRecvDataRec));
    Rec.FromIP := ShortString(Format('%d.%d.%d.%d', [Ord(from.sin_addr.S_un_b.S_b1),
      Ord(from.sin_addr.S_un_b.S_b2), Ord(from.sin_addr.S_un_b.S_b3),
        Ord(from.sin_addr.S_un_b.S_b4)]));
    Rec.FromPort := ntohs(from.sin_port);
    GetMem(Rec.Buff, IBuffSize);
    Rec.BuffSize := IBuffSize;
    CopyMemory(Rec.Buff, FRecvBuf, IBuffSize);
    Queue.Push(Rec);
  end;
end;

procedure WaitforSync(Handle: THandle);
begin
  repeat
    if MsgWaitForMultipleObjects(1, Handle, False, INFINITE, QS_ALLINPUT)
      = WAIT_OBJECT_0 + 1 then
      Application.ProcessMessages
    else
      Break;
  until False;
end;

procedure TCnUDP.Wait;
begin
  WaitforSync(EventHandle);
  ResetEvent(EventHandle);
end;

procedure TCnUDP.SetupLastError;
begin
  FLastError := WSAGetLastError;
end;

function TCnUDP.GetLocalHost: string;
var
  wVersionRequested: WORD;
  wsaData: TWSAData;
  p: PHostEnt;
  s: array[0..256] of AnsiChar;
begin
  wVersionRequested := MAKEWORD(1, 1);
  WSAStartup(wVersionRequested, wsaData);
  try
    GetHostName(@s, 256);
    p := GetHostByName(@s);
    Result := string(inet_ntoa(PInAddr(p^.h_addr_list^)^));
  finally
    WSACleanup;
  end;
end;

procedure TCnUDP.SetRecvBufSize(const Value: Cardinal);
begin
  if FRecvBufSize <> Value then
  begin
    FRecvBufSize := Value;
    if FRecvBuf <> nil then
    begin
      // 释放，等待下次需要时重新分配
      FreeMem(FRecvBuf);
      FRecvBuf := nil;
    end;  
  end;
end;

end.

