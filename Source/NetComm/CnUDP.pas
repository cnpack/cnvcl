{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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
* 备    注：定义了 TCnUDP，在 Windows下使用非阻塞方式进行 UDP 通讯，支持广播
*           MACOS 下用线程阻塞方式
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP/10+ Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.12.15 V1.2
*                支持 MACOS，使用阻塞式线程
*           2008.11.28 V1.1
*                加入一控制接收缓冲区大小的属性
*           2003.11.21 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs,
  {$IFDEF MSWINDOWS}
  Windows, Messages, WinSock, Forms, // 需要设置工程的单元前缀 Vcl 或 FMX
  {$ELSE}
  Posix.Base, Posix.NetIf, Posix.SysSocket, Posix.ArpaInet, Posix.NetinetIn,
  Posix.Unistd, System.Net.Socket,
  {$ENDIF}
  CnClasses, CnConsts, CnNetConsts, CnSocket;

const
  csDefRecvBuffSize = 4096;
  csDefUDPSendBuffSize = 256 * 1024;
  csDefUDPRecvBuffSize = 256 * 1024;

type

//==============================================================================
// UDP 通讯类
//==============================================================================

{ TCnUDP }

  TCnOnDataReceived = procedure(Sender: TComponent; Buffer: Pointer; Len: Integer;
    const FromIP: string; Port: Integer) of object;
  {* 接收到数据事件
   |<PRE>
     Sender     - TCnUDP 对象
     Buffer     - 数据缓冲区
     Len        - 数据缓冲区长度
     FromIP     - 数据来源 IP
     Port       - 数据来源端口号
   |</PRE>}

  TCnUDP = class(TCnComponent)
  {* 使用非阻塞方式进行 UDP 通讯的类。支持广播、数据队列等。}
  private
    FRemoteHost: string;
    FRemotePort: Integer;
    FLocalPort: Integer;
{$IFDEF MSWINDOWS}
    FSockCount: Integer;
    FSocketWindow: HWND;
    RemoteHostS: PHostEnt;
    Succeed: Boolean;
    EventHandle: THandle;
{$ELSE}
    FThread: TThread;
    FLock: TObject;
{$ENDIF}
    Wait_Flag: Boolean;
    FProcing: Boolean;
    FRemoteAddress: TSockAddr;
    FOnDataReceived: TCnOnDataReceived;
    FListening: Boolean;
    FThisSocket: TSocket;
    FQueue: TQueue;
    FLastError: Integer;
    FRecvBufSize: Cardinal;
    FRecvBuf: Pointer;
    FBindAddr: string;
    FUDPSendBufSize: Cardinal;
    FUDPRecvBufSize: Cardinal;
{$IFDEF MSWINDOWS}
    procedure WndProc(var Message: TMessage);
    procedure ProcessIncomingdata;
{$ENDIF}
    procedure ProcessQueue;
    function ResolveRemoteHost(ARemoteHost: string): Boolean;
    procedure SetLocalPort(NewLocalPort: Integer);

    procedure FreeQueueItem(P: Pointer);
    function GetQueueCount: Integer;
    procedure SetupLastError;
    function GetLocalHost: string;
    procedure SetRecvBufSize(const Value: Cardinal);
    procedure SetBindAddr(const Value: string);
    function SockStartup: Boolean;
    procedure SockCleanup;
    procedure SetUDPRecvBufSize(const Value: Cardinal);
    procedure SetUDPSendBufSize(const Value: Cardinal);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure DoDataReceived(Buffer: Pointer; Len: Integer; const FromIP: string; Port: Integer);
{$IFDEF MSWINDOWS}
    procedure Wait;
{$ELSE}
    procedure StartThread;
    procedure StopThread;
{$ENDIF}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateBinding;
    {* 实施具体绑定端口并监听}

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

{$IFDEF MSWINDOWS}
    function ProcessRecv: Boolean;
    {* 处理该 UDP 接口的接收内容。由于 CnUDP 组件的 OnDataReceived 是在主线程
       消息处理中调用的，如果主线程代码需要等待 UDP 接收而不希望处理所有消息，
       可以调用该函数。}
{$ENDIF}

    property LastError: Integer read FLastError;
    {* 最后一次错误的错误号，只读属性}
    property Listening: Boolean read FListening;
    {* 表示当前是否正在监听本地端口，只读属性}
    property QueueCount: Integer read GetQueueCount;
    {* 当前数据队列的长度，只读属性}
    property BindAddr: string read FBindAddr write SetBindAddr;
    {* 绑定本地地址}
  published
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    {* 要发送 UDP 数据的目标主机地址}
    property RemotePort: Integer read FRemotePort write FRemotePort;
    {* 要发送 UDP 数据的目标主机端口号}
    property LocalHost: string read GetLocalHost;
    {* 返回本机 IP 地址，只读属性}
    property LocalPort: Integer read FLocalPort write SetLocalPort;
    {* 本地监听的端口号}
    property RecvBufSize: Cardinal read FRecvBufSize write SetRecvBufSize default csDefRecvBuffSize;
    {* 接收的数据缓冲区大小}
    property UDPSendBufSize: Cardinal read FUDPSendBufSize write SetUDPSendBufSize default csDefUDPSendBuffSize;
    {* UDP 发送的数据缓冲区大小}
    property UDPRecvBufSize: Cardinal read FUDPRecvBufSize write SetUDPRecvBufSize default csDefUDPRecvBuffSize;
    {* UDP 接收的数据缓冲区大小}
    property OnDataReceived: TCnOnDataReceived read FOnDataReceived write
      FOnDataReceived;
    {* 接收到 UDP 数据包事件。Windows 平台在主线程中执行，其余平台在线程中执行}
  end;

{$IFNDEF MSWINDOWS}

  TCnUDPReadThread = class(TThread)
  private
    FUDP: TCnUDP;
  protected
    procedure ProcessData;
    procedure Execute; override;
  public
    property UDP: TCnUDP read FUDP write FUDP;
  end;

{$ENDIF}

// 取广播地址
procedure GetBroadCastAddress(sInt: TStrings);

// 取本机 IP 地址
procedure GetLocalIPAddress(sInt: TStrings);

implementation

{$R-}

//==============================================================================
// 辅助过程
//==============================================================================

{$IFDEF MSWINDOWS}

// 从 Winsock 2.0导入函数 WSAIOCtl
function WSAIoctl(S: TSocket; cmd: DWORD; lpInBuffer: PCHAR; dwInBufferLen:
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

{$ENDIF}

// 取本机所有地址或广播地址
procedure DoGetIPAddress(sInt: TStrings; IsBroadCast: Boolean);
var
  pAddrStr: string;
{$IFDEF MSWINDOWS}
  S: TSocket;
  wsaD: WSADATA;
  NumInterfaces: Integer;
  BytesReturned, SetFlags: u_long;
  pAddr, pMask, pCast: TInAddr;
  PtrA: pointer;
  Buffer: array[0..20] of INTERFACE_INFO;
  I: Integer;
{$ELSE}
  Pif: Pifaddrs;
  InAddr: in_addr;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  WSAStartup($0101, wsaD);              // Start WinSock
  S := Socket(AF_INET, SOCK_STREAM, 0); // Open a socket
  if (S = INVALID_SOCKET) then
    Exit;

  try                                   // Call WSAIoCtl
    PtrA := @bytesReturned;
    if (WSAIoCtl(S, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA, nil,
      nil) <> SOCKET_ERROR) then
    begin                               // If ok, find out how
      // many interfaces exist
      NumInterfaces := BytesReturned div SizeOf(INTERFACE_INFO);
      sInt.Clear;
      for I := 0 to NumInterfaces - 1 do // For every interface
      begin
        SetFlags := Buffer[I].iiFlags;
        if (SetFlags and IFF_BROADCAST = IFF_BROADCAST) and not
          (SetFlags and IFF_LOOPBACK = IFF_LOOPBACK) then
        begin
          pAddr := Buffer[I].iiAddress.AddressIn.sin_addr;
          pMask := Buffer[I].iiNetmask.AddressIn.sin_addr;
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
  CloseSocket(S);
  WSACleanUp;
{$ELSE}
  getifaddrs(Pif);
  while Pif <> nil do
  begin
    // 先不处理 BROADCAST 标记
    if (Pif^.ifa_addr.sa_family = AF_INET) and ((Pif^.ifa_flags and IFF_LOOPBACK) = 0) then
    begin
      InAddr := Psockaddr_in(Pif^.ifa_addr)^.sin_addr;
      if IsBroadCast then
        InAddr.s_addr := InAddr.s_addr or not
          Psockaddr_in(Pif^.ifa_netmask)^.sin_addr.s_addr;

      pAddrStr := string(inet_ntoa(InAddr));

      if sInt.IndexOf(pAddrStr) < 0 then
        sInt.Add(pAddrStr);
    end;
    Pif := Pif^.ifa_next;
  end;
  freeifaddrs(Pif);
{$ENDIF}
end;

// 取本机 IP 地址
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
{$IFDEF MSWINDOWS}
  WM_ASYNCHRONOUSPROCESS = WM_USER + 101;
{$ENDIF}
  CONST_CMD_TRUE: AnsiString = 'TRUE';

type
  PRecvDataRec = ^TRecvDataRec;
  TRecvDataRec = record
    FromIP: string[128];
    FromPort: Word;
    Buff: Pointer;
    BuffSize: Integer;
  end;

constructor TCnUDP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQueue := TQueue.Create;
  FListening := False;
  FProcing := False;
  FRecvBufSize := csDefRecvBuffSize;
  FUDPSendBufSize := csDefUDPSendBuffSize;
  FUDPRecvBufSize := csDefUDPRecvBuffSize;

{$IFDEF MSWINDOWS}
  GetMem(RemoteHostS, MAXGETHOSTSTRUCT);
  FSocketWindow := AllocateHWND(WndProc);
  EventHandle := CreateEvent(nil, True, False, '');
{$ELSE}
  FLock := TObject.Create;
{$ENDIF}

  FBindAddr := '0.0.0.0';
  if SockStartup then
  begin
    FThisSocket := CnNewSocket(AF_INET, SOCK_DGRAM, 0);
    if FThisSocket = INVALID_SOCKET then
    begin
      SetupLastError;
      SockCleanup;
      Exit;
    end;

    CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_DONTLINGER, PAnsiChar(CONST_CMD_TRUE), 4);
    CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_BROADCAST, PAnsiChar(CONST_CMD_TRUE), 4);
    FListening := True;
  end;
end;

destructor TCnUDP.Destroy;
begin
{$IFNDEF MSWINDOWS}
  StopThread;
{$ENDIF}

  if FRecvBuf <> nil then
  begin
    FreeMem(FRecvBuf);
    FRecvBuf := nil;
  end;

  ClearQueue;
  FQueue.Free;

{$IFDEF MSWINDOWS}
  FreeMem(RemoteHostS, MAXGETHOSTSTRUCT);
  DeallocateHWND(FSocketWindow);
  CloseHandle(EventHandle);
{$ELSE}
  FLock.Free;
{$ENDIF}

  if FThisSocket <> 0 then
    CnCloseSocket(FThisSocket);
  if FListening then
    SockCleanup;
  inherited Destroy;
end;

procedure TCnUDP.DoDataReceived(Buffer: Pointer; Len: Integer;
  const FromIP: string; Port: Integer);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(Self, Buffer, Len, FromIP, Port);
end;

procedure TCnUDP.UpdateBinding;
var
  Data: Cardinal;
  Address: TSockAddr;
begin
  if not (csDesigning in ComponentState) then
  begin
    FListening := False;

    if FThisSocket <> 0 then
    begin
      CnCloseSocket(FThisSocket);
      SockCleanup;
    end;

    if SockStartup then
    begin
      FThisSocket := CnNewSocket(AF_INET, SOCK_DGRAM, 0);
      if FThisSocket = INVALID_SOCKET then
      begin
        SockCleanup;
        SetupLastError;
        Exit;
      end;
      CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_DONTLINGER, PAnsiChar(CONST_CMD_TRUE), 4);
      CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_BROADCAST, PAnsiChar(CONST_CMD_TRUE), 4);
    end;

    FillChar(Address, SizeOf(Address), 0);
    if FBindAddr <> '' then
      Address.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(FBindAddr)))
    else
      Address.sin_addr.S_addr := INADDR_ANY;

    Address.sin_family := AF_INET;
    Address.sin_port := htons(FLocalPort);

    Wait_Flag := False;
    if CnBind(FThisSocket, Address, SizeOf(Address)) = SOCKET_ERROR then
    begin
      SetupLastError;
      SockCleanup;
      Exit;
    end;

    // Allow to send to 255.255.255.255
    Data := 1;
    CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_BROADCAST,
      PAnsiChar(@Data), SizeOf(Data));
    Data := FUDPSendBufSize;
    CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_SNDBUF,
      PAnsiChar(@Data), SizeOf(Data));
    Data := FUDPRecvBufSize;
    CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_RCVBUF,
      PAnsiChar(@Data), SizeOf(Data));

{$IFDEF MSWINDOWS}
    WSAAsyncSelect(FThisSocket, FSocketWindow, WM_ASYNCHRONOUSPROCESS, FD_READ);
{$ELSE}
    // 起监听线程
    StopThread;
    StartThread;
{$ENDIF}

    FListening := True;
  end;
end;

procedure TCnUDP.Loaded;
begin
  inherited;
  UpdateBinding;
end;

procedure TCnUDP.SetBindAddr(const Value: string);
begin
  if Value <> FBindAddr then
  begin
    FBindAddr := Value;
    UpdateBinding;
  end;
end;

procedure TCnUDP.SetLocalPort(NewLocalPort: Integer);
begin
  if NewLocalPort <> FLocalPort then
  begin
    FLocalPort := NewLocalPort;
    UpdateBinding;
  end;
end;

function TCnUDP.ResolveRemoteHost(ARemoteHost: string): Boolean;
var
{$IFDEF MSWINDOWS}
  Buf: array[0..127] of AnsiChar;
{$ELSE}
  IP: TIPAddress;
{$ENDIF}
begin
  Result := False;
  if not FListening then
    Exit;

{$IFDEF MSWINDOWS}
  try
    FRemoteAddress.sin_addr.S_addr := Inet_Addr(PAnsiChar(StrPCopy(Buf, {$IFDEF UNICODE}AnsiString{$ENDIF}(ARemoteHost))));
    if FRemoteAddress.sin_addr.S_addr = SOCKET_ERROR then
    begin
      Wait_Flag := False;
      WSAAsyncGetHostByName(FSocketWindow, WM_ASYNCHRONOUSPROCESS, Buf,
        PAnsiChar(RemoteHostS), MAXGETHOSTSTRUCT);
      repeat
        Wait;
      until Wait_Flag;

      if Succeed then
      begin
        with FRemoteAddress.sin_addr.S_un_b do
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
{$ELSE}
  // POSIX 下如何异步解析？只能先写同步
  IP := TIPAddress.LookupName(ARemoteHost);
  FRemoteAddress.sin_addr := IP.Addr;
{$ENDIF}

  if FRemoteAddress.sin_addr.S_addr <> 0 then
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
  I: Integer;

  function DoSendBuffer(Buff: Pointer; Length: Integer; Host: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    try
      if not ResolveRemoteHost(Host) then
        Exit;
      FRemoteAddress.sin_family := AF_INET;
      FRemoteAddress.sin_port := htons(FRemotePort);
      I := SizeOf(FRemoteAddress);

      if CnSendTo(FThisSocket, Buff^, Length, 0, FRemoteAddress, I) <> SOCKET_ERROR then
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
      for I := 0 to Hosts.Count - 1 do
        if DoSendBuffer(Buff, Length, Hosts[I]) then
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
  Result := FQueue.Count;
end;

procedure TCnUDP.FreeQueueItem(P: Pointer);
var
  Rec: PRecvDataRec;
begin
  Rec := PRecvDataRec(P);
  Rec.FromIP := '';
  FreeMem(Rec^.Buff);
  FreeMem(Rec);
end;

procedure TCnUDP.ClearQueue;
var
  Rec: PRecvDataRec;
begin
  while FQueue.Count > 0 do
  begin
    Rec := FQueue.Pop;
    FreeQueueItem(Rec);
  end;
end;

procedure TCnUDP.ProcessQueue;
var
  Rec: PRecvDataRec;
begin
  if FProcing then Exit;
  FProcing := True;
  try
{$IFNDEF MSWINDOWS}
    TMonitor.Enter(FLock);
{$ENDIF}
    while FQueue.Count > 0 do
    begin
      Rec := FQueue.Pop;
      DoDataReceived(Rec^.Buff, Rec^.BuffSize, string(Rec^.FromIP), Rec^.FromPort);
      FreeQueueItem(Rec);
    end;
  finally
{$IFNDEF MSWINDOWS}
    TMonitor.Exit(FLock);
{$ENDIF}
    FProcing := False;
  end;
end;

{$IFDEF MSWINDOWS}

function TCnUDP.ProcessRecv: Boolean;
var
  Unicode: Boolean;
  MsgExists: Boolean;
  Msg: TMsg;
begin
  Unicode := IsWindowUnicode(FSocketWindow);
  if Unicode then
    MsgExists := PeekMessageW(Msg, FSocketWindow, 0, 0, PM_REMOVE)
  else
    MsgExists := PeekMessageA(Msg, FSocketWindow, 0, 0, PM_REMOVE);

  if MsgExists then
  begin
    if Msg.Message <> WM_QUIT then
    begin
      TranslateMessage(Msg);
      if Unicode then
        DispatchMessageW(Msg)
      else
        DispatchMessageA(Msg);
    end;
  end;
  Result := MsgExists;
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
          if not FProcing then
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
  From: TSockAddr;
  I: Integer;
  Rec: PRecvDataRec;
  IBuffSize: Integer;
begin
  I := SizeOf(From);
  if FRecvBuf = nil then
    GetMem(FRecvBuf, FRecvBufSize);

  IBuffSize := CnRecvFrom(FThisSocket, FRecvBuf^, FRecvBufSize, 0, From, I);
  if (IBuffSize > 0) and Assigned(FOnDataReceived) then
  begin
    GetMem(Rec, SizeOf(TRecvDataRec));
    FillChar(Rec^, SizeOf(TRecvDataRec), 0);

    Rec.FromIP := ShortString(Format('%d.%d.%d.%d', [Ord(From.sin_addr.S_un_b.S_b1),
      Ord(From.sin_addr.S_un_b.S_b2), Ord(From.sin_addr.S_un_b.S_b3),
      Ord(From.sin_addr.S_un_b.S_b4)]));
    Rec.FromPort := ntohs(From.sin_port);

    GetMem(Rec.Buff, IBuffSize);
    Rec.BuffSize := IBuffSize;
    Move(FRecvBuf^, Rec.Buff^, IBuffSize);

    FQueue.Push(Rec);
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

{$ELSE}

procedure TCnUDP.StartThread;
begin
  if FThread = nil then
  begin
    FThread := TCnUDPReadThread.Create(True);
    FThread.FreeOnTerminate := True;
  end;

  if FRecvBuf = nil then
    GetMem(FRecvBuf, FRecvBufSize);

  TCnUDPReadThread(FThread).UDP := Self;
  FThread.Resume;
end;

procedure TCnUDP.StopThread;
begin
  if FThread = nil then
    Exit;

  FThread.Terminate;
  try
    FThread.WaitFor;
  except
    ;  // WaitFor 时可能已经 Terminated，导致出句柄无效的错
  end;
  FThread := nil;
end;

{$ENDIF}

procedure TCnUDP.SetupLastError;
begin
{$IFDEF MSWINDOWS}
  FLastError := WSAGetLastError;
{$ELSE}
  FLastError := GetLastError;
{$ENDIF}
end;

procedure TCnUDP.SockCleanup;
begin
{$IFDEF MSWINDOWS}
  if FSockCount > 0 then
  begin
    Dec(FSockCount);
    if FSockCount = 0 then
      WSACleanup;
  end;
{$ENDIF}
end;

function TCnUDP.SockStartup: Boolean;
{$IFDEF MSWINDOWS}
var
  wsaData: TWSAData;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if FSockCount = 0 then
  begin
    Result := WSAStartup($0101, wsaData) = 0;
    if not Result then
      Exit;
  end;
  Inc(FSockCount);
{$ENDIF}
  Result := True;
end;

procedure TCnUDP.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnUDPName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnUDPComment;
end;

function TCnUDP.GetLocalHost: string;
var
  S: array[0..256] of AnsiChar;
{$IFDEF MSWINDOWS}
  P: PHostEnt;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  SockStartup;
  try
    GetHostName(@S, 256);
    P := GetHostByName(@S);
    Result := string(inet_ntoa(PInAddr(P^.h_addr_list^)^));
  finally
    SockCleanup;
  end;
{$ELSE}
  // 拿本机名称与 IP
  Posix.Unistd.gethostname(@S, 256);
  Result := TIPAddress.LookupName(string(S)).Address;
{$ENDIF}
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

procedure TCnUDP.SetUDPRecvBufSize(const Value: Cardinal);
var
  Data: Cardinal;
begin
  FUDPRecvBufSize := Value;
  if FListening then
  begin
    Data := FUDPRecvBufSize;
    CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_RCVBUF,
      PAnsiChar(@Data), SizeOf(Data));
  end;
end;

procedure TCnUDP.SetUDPSendBufSize(const Value: Cardinal);
var
  Data: Cardinal;
begin
  FUDPSendBufSize := Value;
  if FListening then
  begin
    Data := FUDPSendBufSize;
    CnSetSockOpt(FThisSocket, SOL_SOCKET, SO_SNDBUF,
      PAnsiChar(@Data), SizeOf(Data));
  end;
end;

{$IFNDEF MSWINDOWS}

{ TCnUDPReadThread }

procedure TCnUDPReadThread.Execute;
var
  Res, I, FromPort: Integer;
  From: TSockAddr;
  Rec: PRecvDataRec;
begin
  if (UDP = nil) or (UDP.FRecvBuf = nil) then
    Exit;

  I := SizeOf(From);
  while not Terminated do
  begin
    Res := CnRecvFrom(UDP.FThisSocket, UDP.FRecvBuf^, UDP.FRecvBufSize, 0, From, I);

    if Res <> SOCKET_ERROR then
    begin
      if Res = 0 then
        Continue;

      GetMem(Rec, SizeOf(TRecvDataRec));
      FillChar(Rec^, SizeOf(TRecvDataRec), 0);

      GetMem(Rec^.Buff, Res);
      Rec^.FromIP := TIPAddress.Create(From.sin_addr).Address;
      Rec^.FromPort := ntohs(From.sin_port);
      Rec^.BuffSize := Res;
      Move(UDP.FRecvBuf^, Rec^.Buff^, Res);

{$IFNDEF MSWINDOWS}
      TMonitor.Enter(UDP.FLock);
{$ENDIF}
      UDP.FQueue.Push(Rec);
{$IFNDEF MSWINDOWS}
      TMonitor.Exit(UDP.FLock);
{$ENDIF}

      Synchronize(ProcessData);
    end;
  end;
end;

procedure TCnUDPReadThread.ProcessData;
begin
  if not UDP.FProcing then
    UDP.ProcessQueue
  else if UDP.FQueue.Count > 0 then
  begin
    Sleep(0);
    Synchronize(ProcessData);
  end;
end;

{$ENDIF}

end.

