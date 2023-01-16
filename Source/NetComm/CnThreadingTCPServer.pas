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

unit CnThreadingTCPServer;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯组件包多线程阻塞 TCP Server 实现单元
* 单元作者：CnPack 开发组 Liu Xiao
* 备    注：一个简易的多线程阻塞式 TCP Server，新客户端连接时起新线程，调用者
*           在 OnAccept 事件中循环 Recv/Send 缓冲区即可，退出事件则断开连接，
*           无线程池机制
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2020.02.21 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs, SyncObjs,
{$IFDEF MSWINDOWS}
  Windows,  WinSock,
{$ELSE}
  System.Net.Socket, Posix.NetinetIn, Posix.SysSocket, Posix.Unistd, Posix.ArpaInet,
{$ENDIF}
  CnConsts, CnNetConsts, CnClasses, CnSocket;

type
  ECnServerSocketError = class(Exception);

  TCnThreadingTCPServer = class;

  TCnClientSocket = class
  {* 和每一个单独客户端通信的 Socket 封装}
  private
    FSocket: TSocket;
    FRemoteIP: string;
    FRemotePort: Word;
    FServer: TCnThreadingTCPServer;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    FLocalIP: string;
    FLocalPort: Word;
    FTag: TObject;
  protected
    procedure DoShutdown; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Shutdown; virtual;
    {* 封装的关闭 Socket 的操作}

    // send/recv 收发数据封装
    function Send(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    function Recv(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    // 注意 Recv 返回 0 时说明当前网络已断开，调用者需要根据返回值做断开处理

    property Server: TCnThreadingTCPServer read FServer write FServer;
    {* 所属的 TCnThreadingTCPServer 实例引用}
    property Socket: TSocket read FSocket write FSocket;
    {* 和客户端通讯的实际 Socket}
    property LocalIP: string read FLocalIP write FLocalIP;
    {* 客户端连接上时的本地 IP}
    property LocalPort: Word read FLocalPort write FLocalPort;
    {* 客户端连接上时的本地端口}
    property RemoteIP: string read FRemoteIP write FRemoteIP;
    {* 远程客户端的 IP}
    property RemotePort: Word read FRemotePort write FRemotePort;
    {* 远程客户端的端口}
    property Tag: TObject read FTag write FTag;
    {* Tag 用来存点儿别的东西}

    property BytesSent: Cardinal read FBytesSent;
    {* 本客户端的发送字节数，用 Send 才会被统计}
    property BytesReceived: Cardinal read FBytesReceived;
    {* 本客户端的收取字节数，用 Recv 才会被统计}
  end;

  TCnServerSocketErrorEvent = procedure (Sender: TObject; SocketError: Integer) of object;

  TCnSocketAcceptEvent = procedure (Sender: TObject; ClientSocket: TCnClientSocket) of object;

  TCnTCPAcceptThread = class(TThread)
  {* 监听线程，一个 TCPServer 只有一个，用于 Accept}
  private
    FServer: TCnThreadingTCPServer;
    FServerSocket: TSocket;
  protected
    procedure Execute; override;
  public
    property ServerSocket: TSocket read FServerSocket write FServerSocket;
    {* 监听线程所用的 Socket 引用}
    property Server: TCnThreadingTCPServer read FServer write FServer;
    {* 监听线程所属的 TCPServer 引用}
  end;

  TCnTCPClientThread = class(TThread)
  {* Accept 成功后针对每个新客户开启的处理线程，可以有多个，每个对应一个 ClientSocket 封装}
  private
    FClientSocket: TCnClientSocket;
  protected
    procedure Execute; override;
    procedure DoAccept; virtual;

    function DoGetClientSocket: TCnClientSocket; virtual;
    {* 子类可重载使用扩展内容的 ClientSocket}
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    property ClientSocket: TCnClientSocket read FClientSocket;
    {* 封装的供处理客户端使用的网络对象}
  end;

  TCnThreadingTCPServer = class(TCnComponent)
  {* 简单的多线程 TCP Server}
  private
    FSocket: TSocket;            // 监听的 Socket
    FAcceptThread: TCnTCPAcceptThread;
    FListLock: TCriticalSection;
    FClientThreads: TObjectList; // 存储 Accept 出的每个和 Client 通讯的线程
    FActive: Boolean;
    FListening: Boolean;
    FActualLocalPort: Word;
    FLocalPort: Word;
    FLocalIP: string;
    FOnError: TCnServerSocketErrorEvent;
    FOnAccept: TCnSocketAcceptEvent;
    FCountLock: TCriticalSection;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    FOnShutdownClient: TNotifyEvent;
    FMaxConnections: Cardinal;
    procedure SetActive(const Value: Boolean);
    procedure SetLocalIP(const Value: string);
    procedure SetLocalPort(const Value: Word);
    function GetClientCount: Integer;
    function GetClient(Index: Integer): TCnClientSocket;
    function GetActualLocalPort: Word;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    function CheckSocketError(ResultCode: Integer): Integer;
    function DoGetClientThread: TCnTCPClientThread; virtual;
    {* 子类可重载使用其他行为的 ClientThread}

    procedure DoShutdownClient(Client: TCnClientSocket); virtual;

    procedure ClientThreadTerminate(Sender: TObject);
    procedure IncRecv(C: Integer);
    procedure IncSent(C: Integer);

    function Bind: Boolean;
    function Listen: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    {* 开始监听，等同于 Active := True}
    procedure Close; virtual;
    {* 关闭所有客户端连接并停止监听，等同于 Active := False}
    function KickAll: Integer; virtual;
    {* 关闭所有客户端连接}

    property ClientCount: Integer read GetClientCount;
    {* 活动的客户端数量}
    property Clients[Index: Integer]: TCnClientSocket read GetClient;
    {* 活动的客户端封装对象}

    property BytesSent: Cardinal read FBytesSent;
    {* 发送给各客户端的总字节数}
    property BytesReceived: Cardinal read FBytesReceived;
    {* 从各客户端收取的总字节数}
    property Listening: Boolean read FListening;
    {* 是否正在监听}
    property ActualLocalPort: Word read GetActualLocalPort;
    {* LocalPort 为 0 时随机选择一个端口监听，返回该端口值}
  published
    property Active: Boolean read FActive write SetActive;
    {* 是否开始运行}
    property LocalIP: string read FLocalIP write SetLocalIP;
    {* 监听的本地 IP}
    property LocalPort: Word read FLocalPort write SetLocalPort;
    {* 监听的本地端口}
    property MaxConnections: Cardinal read FMaxConnections write FMaxConnections;
    {* 能够接入的最大连接数，超过则 Accept 时直接关闭新接入连接}

    property OnError: TCnServerSocketErrorEvent read FOnError write FOnError;
    {* 出错事件}
    property OnAccept: TCnSocketAcceptEvent read FOnAccept write FOnAccept;
    {* 新客户端连接上时触发的事件，处理函数可循环接收、输出，退出事件则断开连接}
    property OnShutdownClient: TNotifyEvent read FOnShutdownClient write FOnShutdownClient;
    {* 调用 ClientSocket.Shutdown 关闭某客户端时触发，供使用者额外关闭客户端连接相关的另外资源}
  end;

implementation

{$IFDEF MSWINDOWS}
var
  WSAData: TWSAData;
{$ENDIF}

{ TCnThreadingTCPServer }

function TCnThreadingTCPServer.Bind: Boolean;
var
  SockAddress, ConnAddr: TSockAddr;
  Len, Ret: Integer;
begin
  Result := False;
  if FActive then
  begin
    SockAddress.sin_family := AF_INET;
    if FLocalIP <> '' then
      SockAddress.sin_addr.S_addr := inet_addr(PAnsiChar(AnsiString(FLocalIP)))
    else
      SockAddress.sin_addr.S_addr := INADDR_ANY;

    SockAddress.sin_port := ntohs(FLocalPort);
    Result := CheckSocketError(CnBind(FSocket, SockAddress, SizeOf(SockAddress))) = 0;

    FActualLocalPort := FLocalPort;
    if FActualLocalPort = 0 then
    begin
      Len := SizeOf(ConnAddr);
      Ret := CheckSocketError(CnGetSockname(FSocket, ConnAddr, Len));

      if Ret = 0 then
        FActualLocalPort := ntohs(ConnAddr.sin_port);
    end;
  end;
end;

function TCnThreadingTCPServer.CheckSocketError(ResultCode: Integer): Integer;
begin
  Result := ResultCode;
  if ResultCode = SOCKET_ERROR then
  begin
    if Assigned(FOnError) then
    begin
{$IFDEF MSWINDOWS}
      FOnError(Self, WSAGetLastError);
{$ELSE}
      FOnError(Self, GetLastError);
{$ENDIF};
    end;
  end;
end;

procedure TCnThreadingTCPServer.ClientThreadTerminate(Sender: TObject);
begin
  // 客户端线程结束，在主线程中删除无效的 Socket 与线程引用。Sender 是 Thread 实例
  FListLock.Enter;
  try
    FClientThreads.Remove(Sender);
  finally
    FListLock.Leave;
  end;
end;

procedure TCnThreadingTCPServer.Close;
begin
  if not FActive then
    Exit;

  if FActive then
  begin
    // 通知停止 Accept 线程，防止还有新 Client 进来
    CnShutdown(FSocket, SD_BOTH); // 忽略未连接时的出错
    CheckSocketError(CnCloseSocket(FSocket));

    FSocket := INVALID_SOCKET;
    FAcceptThread.Terminate;
    try
      FAcceptThread.WaitFor;
    except
      ;  // WaitFor 时可能已经 Terminated，导致出句柄无效的错
    end;
    FAcceptThread := nil;

    // 踢掉所有客户端
    KickAll;

    FActualLocalPort := 0;
    FListening := False;
    FActive := False;
  end;
end;

constructor TCnThreadingTCPServer.Create(AOwner: TComponent);
begin
  inherited;
  FListLock := TCriticalSection.Create;
  FCountLock := TCriticalSection.Create;
  FClientThreads := TObjectList.Create(False);
end;

destructor TCnThreadingTCPServer.Destroy;
begin
  Close;
  FClientThreads.Free;
  FCountLock.Free;
  FListLock.Free;
  inherited;
end;

function TCnThreadingTCPServer.DoGetClientThread: TCnTCPClientThread;
begin
  Result := TCnTCPClientThread.Create(True);
end;

procedure TCnThreadingTCPServer.DoShutdownClient(Client: TCnClientSocket);
begin
  if Assigned(FOnShutdownClient) then
    FOnShutdownClient(Client);
end;

function TCnThreadingTCPServer.GetActualLocalPort: Word;
begin
  Result := FActualLocalPort;
end;

function TCnThreadingTCPServer.GetClient(Index: Integer): TCnClientSocket;
begin
  if (Index >= 0) and (Index < FClientThreads.Count) then
    Result := TCnTCPClientThread(FClientThreads[Index]).ClientSocket
  else
    Result := nil;
end;

function TCnThreadingTCPServer.GetClientCount: Integer;
begin
  Result := FClientThreads.Count;
end;

procedure TCnThreadingTCPServer.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnThreadingTCPServerName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnThreadingTCPServerComment;
end;

procedure TCnThreadingTCPServer.IncRecv(C: Integer);
begin
  FCountLock.Enter;
  Inc(FBytesReceived, C);
  FCountLock.Leave;
end;

procedure TCnThreadingTCPServer.IncSent(C: Integer);
begin
  FCountLock.Enter;
  Inc(FBytesSent, C);
  FCountLock.Leave;
end;

function TCnThreadingTCPServer.KickAll: Integer;
var
  CT: TCnTCPClientThread;
begin
  Result := 0;

  // 关闭所有客户端连接
  while FClientThreads.Count > 0 do
  begin
    FListLock.Enter;
    try
      if FClientThreads.Count = 0 then
        Exit;

      CT := TCnTCPClientThread(FClientThreads[0]);
      CT.ClientSocket.ShutDown;
      CT.Terminate;

      try
        CT.WaitFor;
        // 等待线程结束，注意并不等待跑在主线程里的 OnTerminate 结束
      except
        ; // WaitFor 时可能句柄无效，因为已经结束了
      end;
    finally
      FListLock.Leave;
    end;

    // 线程结束时线程实例已经从 FClientThreads 中剔除了
    Inc(Result);
  end;
  FClientThreads.Clear;
end;

function TCnThreadingTCPServer.Listen: Boolean;
begin
  if FActive and not FListening then
    FListening := CheckSocketError(CnListen(FSocket, SOMAXCONN)) = 0;

  Result := FListening;
end;

procedure TCnThreadingTCPServer.Open;
begin
  if FActive then
    Exit;

  FSocket := CnNewSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  FActive := FSocket <> INVALID_SOCKET;
  if FActive then
  begin
    if Bind then
    begin
      if Listen then
      begin
        // 创建 Accept 线程，等新的客户来连接
        if FAcceptThread = nil then
        begin
          FAcceptThread := TCnTCPAcceptThread.Create(True);
          FAcceptThread.FreeOnTerminate := True;
        end;

        FAcceptThread.Server := Self;
        FAcceptThread.ServerSocket := FSocket;
        FAcceptThread.Resume;
      end;
    end;
  end;
end;

procedure TCnThreadingTCPServer.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    begin
      if Value then
        Open
      else
        Close;
    end
    else
      FActive := Value;
  end;
end;

procedure TCnThreadingTCPServer.SetLocalIP(const Value: string);
begin
  FLocalIP := Value;
end;

procedure TCnThreadingTCPServer.SetLocalPort(const Value: Word);
begin
  FLocalPort := Value;
end;

{ TCnAcceptThread }

procedure TCnTCPAcceptThread.Execute;
var
  Sock: TSocket;
  SockAddress, ConnAddr: TSockAddr;
  Len, Ret: Integer;
  ClientThread: TCnTCPClientThread;
begin
  FServer.FBytesReceived := 0;
  FServer.FBytesSent := 0;

  while not Terminated do
  begin
    Len := SizeOf(SockAddress);
    FillChar(SockAddress, SizeOf(SockAddress), 0);
    try
      Sock := CnAccept(FServerSocket, @SockAddress, @Len);
    except
      Sock := INVALID_SOCKET;
    end;

    // 新的客户连接上了
    if Sock <> INVALID_SOCKET then
    begin
      // 超出最大连接数，直接断掉
      if (FServer.MaxConnections > 0) and (FServer.ClientCount >= FServer.MaxConnections) then
      begin
        CnCloseSocket(Sock);
        Continue;
      end;

      // 起新的客户线程（或者整一个客户线程池，存不活动的线程）
      ClientThread := FServer.DoGetClientThread;
      ClientThread.FreeOnTerminate := True;
      ClientThread.OnTerminate := FServer.ClientThreadTerminate;

      ClientThread.ClientSocket.Socket := Sock;
      ClientThread.ClientSocket.Server := FServer;

      Len := SizeOf(ConnAddr);
      Ret := FServer.CheckSocketError(CnGetSockName(Sock, ConnAddr, Len));

      if Ret = 0 then
      begin
        // 拿该 Socket 的本地信息
        ClientThread.ClientSocket.LocalIP := inet_ntoa(ConnAddr.sin_addr);
        ClientThread.ClientSocket.LocalPort := ntohs(ConnAddr.sin_port);
      end
      else // 如果没拿到，姑且拿监听的 Socket 的本地信息，注意 IP 可能是空
      begin
        ClientThread.ClientSocket.LocalIP := FServer.LocalIP;
        ClientThread.ClientSocket.LocalPort := FServer.ActualLocalPort;
      end;

      // 拿该 Socket 的对端客户端信息
      ClientThread.ClientSocket.RemoteIP := inet_ntoa(SockAddress.sin_addr);
      ClientThread.ClientSocket.RemotePort := ntohs(SockAddress.sin_port);

      FServer.FListLock.Enter;
      try
        FServer.FClientThreads.Add(ClientThread);
      finally
        FServer.FListLock.Leave;
      end;
      ClientThread.Resume;
    end;
  end;
end;

{ TCnTCPClientThread }

constructor TCnTCPClientThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FClientSocket := DoGetClientSocket;
end;

destructor TCnTCPClientThread.Destroy;
begin
  FClientSocket.Free;
  inherited;
end;

procedure TCnTCPClientThread.DoAccept;
begin
  if Assigned(FClientSocket.Server.OnAccept) then
    FClientSocket.Server.OnAccept(FClientSocket.Server, FClientSocket);
end;

function TCnTCPClientThread.DoGetClientSocket: TCnClientSocket;
begin
  Result := TCnClientSocket.Create;
end;

procedure TCnTCPClientThread.Execute;
begin
  // 客户端已连接上，事件里有参数可被用
  DoAccept;

  // 客户处理完毕了，可以断开连接了，如果事件里头没主动断开的话
  FClientSocket.Shutdown;
end;

{ TCnClientSocket }

constructor TCnClientSocket.Create;
begin

end;

destructor TCnClientSocket.Destroy;
begin
  inherited;

end;

procedure TCnClientSocket.DoShutdown;
begin
  FServer.DoShutdownClient(Self);
end;

function TCnClientSocket.Recv(var Buf; Len: Integer; Flags: Integer): Integer;
begin
  Result := FServer.CheckSocketError(CnRecv(FSocket, Buf, Len, Flags));

  if Result <> SOCKET_ERROR then
  begin
    Inc(FBytesReceived, Result);
    FServer.IncRecv(Result);
  end;
end;

function TCnClientSocket.Send(var Buf; Len: Integer; Flags: Integer): Integer;
begin
  Result := FServer.CheckSocketError(CnSend(FSocket, Buf, Len, Flags));

  if Result <> SOCKET_ERROR then
  begin
    Inc(FBytesSent, Result);
    FServer.IncSent(Result);
  end;
end;

{$IFDEF MSWINDOWS}

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ECnServerSocketError.Create('WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise ECnServerSocketError.Create('WSACleanup');
end;

{$ENDIF}

procedure TCnClientSocket.Shutdown;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    FServer.CheckSocketError(CnShutdown(FSocket, SD_BOTH));
    FServer.CheckSocketError(CnCloseSocket(FSocket));

    FSocket := INVALID_SOCKET;

    DoShutdown;
  end;
end;

{$IFDEF MSWINDOWS}

initialization
  Startup;

finalization
  Cleanup;

{$ENDIF}

end.
