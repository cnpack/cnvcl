{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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
  Windows, SysUtils, Classes, Contnrs, WinSock, CnConsts, CnNetConsts, CnClasses;

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
  public
    // send/recv 收发数据封装
    function Send(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    function Recv(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    // 注意 Recv 返回 0 时说明当前网络已断开，调用者需要根据返回值做断开处理

    property Server: TCnThreadingTCPServer read FServer write FServer;
    {* 所属的 TCnThreadingTCPServer 实例引用}
    property Socket: TSocket read FSocket write FSocket;
    {* 和客户端通讯的实际 Socket}
    property RemoteIP: string read FRemoteIP write FRemoteIP;
    {* 远程客户端的 IP}
    property RemotePort: Word read FRemotePort write FRemotePort;
    {* 远程客户端的端口}

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
    FListLock: TRTLCriticalSection;
    FClientThreads: TObjectList; // 存储 Accept 出的每个和 Client 通讯的线程
    FActive: Boolean;
    FListening: Boolean;
    FLocalPort: Word;
    FLocalIP: string;
    FOnError: TCnServerSocketErrorEvent;
    FOnAccept: TCnSocketAcceptEvent;
    FCountLock: TRTLCriticalSection;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    procedure SetActive(const Value: Boolean);
    procedure SetLocalIP(const Value: string);
    procedure SetLocalPort(const Value: Word);
    function CheckSocketError(ResultCode: Integer): Integer;
    function GetClientCount: Integer;
    function GetClient(Index: Integer): TCnClientSocket;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    function DoGetClientThread: TCnTCPClientThread; virtual;
    {* 子类可重载使用其他行为的 ClientThread}

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
    procedure Close;
    {* 关闭所有客户端连接并停止监听，等同于 Active := False}
    function KickAll: Integer;
    {* 关闭所有客户端连接}

    property ClientCount: Integer read GetClientCount;
    {* 活动的客户端数量}
    property Clients[Index: Integer]: TCnClientSocket read GetClient;
    {* 活动的客户端封装对象}

    property BytesSent: Cardinal read FBytesSent;
    {* 发送给各客户端的总字节数}
    property BytesReceived: Cardinal read FBytesReceived;
    {* 从各客户端收取的总字节数}
  published
    property Active: Boolean read FActive write SetActive;
    {* 是否开始监听}
    property LocalIP: string read FLocalIP write SetLocalIP;
    {* 监听的本地 IP}
    property LocalPort: Word read FLocalPort write SetLocalPort;
    {* 监听的本地端口}

    property OnError: TCnServerSocketErrorEvent read FOnError write FOnError;
    {* 出错事件}
    property OnAccept: TCnSocketAcceptEvent read FOnAccept write FOnAccept;
    {* 新客户端连接上时触发的事件，处理函数可循环接收、输出，退出事件则断开连接}
  end;

implementation

var
  WSAData: TWSAData;

{ TCnThreadingTCPServer }

function TCnThreadingTCPServer.Bind: Boolean;
var
  Addr: TSockAddr;
begin
  Result := False;
  if FActive then
  begin
    Addr.sin_family := AF_INET;
    Addr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(FLocalIP)));
    Addr.sin_port := ntohs(FLocalPort);
    Result := CheckSocketError(WinSock.bind(FSocket, Addr, sizeof(Addr))) = 0;
  end;
end;

function TCnThreadingTCPServer.CheckSocketError(ResultCode: Integer): Integer;
begin
  Result := ResultCode;
  if ResultCode = SOCKET_ERROR then
  begin
    if Assigned(FOnError) then
      FOnError(Self, WSAGetLastError);
  end;
end;

procedure TCnThreadingTCPServer.ClientThreadTerminate(Sender: TObject);
begin
  // 客户端线程结束，在主线程中删除无效的 Socket 与线程引用。Sender 是 Thread 实例
  EnterCriticalSection(FListLock);
  try
    FClientThreads.Remove(Sender);
  finally
    LeaveCriticalSection(FListLock);
  end;
end;

procedure TCnThreadingTCPServer.Close;
begin
  if not FActive then
    Exit;

  if FActive then
  begin
    // 通知停止 Accept 线程
    FAcceptThread.Terminate;
    KickAll;

    CheckSocketError(closesocket(FSocket)); // intterupt accept call
    try
      FAcceptThread.WaitFor;
    except
      ;  // WaitFor 时可能已经 Terminated，导致出句柄无效的错
    end;
    FAcceptThread := nil;

    FSocket := INVALID_SOCKET;
    FListening := False;
    FActive := False;
  end;
end;

constructor TCnThreadingTCPServer.Create(AOwner: TComponent);
begin
  inherited;
  InitializeCriticalSection(FListLock);
  InitializeCriticalSection(FCountLock);
  FClientThreads := TObjectList.Create(False);
end;

destructor TCnThreadingTCPServer.Destroy;
begin
  Close;
  FClientThreads.Free;
  DeleteCriticalSection(FCountLock);
  DeleteCriticalSection(FListLock);
  inherited;
end;

function TCnThreadingTCPServer.DoGetClientThread: TCnTCPClientThread;
begin
  Result := TCnTCPClientThread.Create(True);
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
  EnterCriticalSection(FCountLock);
  Inc(FBytesReceived, C);
  LeaveCriticalSection(FCountLock);
end;

procedure TCnThreadingTCPServer.IncSent(C: Integer);
begin
  EnterCriticalSection(FCountLock);
  Inc(FBytesSent, C);
  LeaveCriticalSection(FCountLock);
end;

function TCnThreadingTCPServer.KickAll: Integer;
var
  I: Integer;
begin
  Result := 0;

  // 关闭所有客户端连接
  for I := FClientThreads.Count - 1 downto 0 do
  begin
    CheckSocketError(closesocket((TCnTCPClientThread(FClientThreads[I]).ClientSocket.Socket)));
    TCnTCPClientThread(FClientThreads[I]).ClientSocket.Socket := INVALID_SOCKET;
    TCnTCPClientThread(FClientThreads[I]).Terminate;

    try
      TCnTCPClientThread(FClientThreads[I]).WaitFor;
    except
      ; // WaitFor 时可能句柄无效
    end;

    // 线程结束时线程实例已经从 FClientThreads 中剔除了
    Inc(Result);
  end;
  FClientThreads.Clear;
end;

function TCnThreadingTCPServer.Listen: Boolean;
begin
  if FActive and not FListening then
    FListening := CheckSocketError(WinSock.listen(FSocket, SOMAXCONN)) = 0;

  Result := FListening;
end;

procedure TCnThreadingTCPServer.Open;
begin
  if FActive then
    Exit;

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
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
  Addr: TSockAddr;
  Len: Integer;
  ClientThread: TCnTCPClientThread;
begin
  FServer.FBytesReceived := 0;
  FServer.FBytesSent := 0;

  while not Terminated do
  begin
    Len := SizeOf(Addr);
    FillChar(Addr, SizeOf(Addr), 0);
    try
      Sock := WinSock.accept(FServerSocket, @Addr, @Len);
    except
      Sock := INVALID_SOCKET;
    end;

    // 新的客户连接上了
    if Sock <> INVALID_SOCKET then
    begin
      // 起新的客户线程（或者整一个客户线程池，存不活动的线程）
      ClientThread := FServer.DoGetClientThread;
      ClientThread.FreeOnTerminate := True;
      ClientThread.OnTerminate := FServer.ClientThreadTerminate;

      ClientThread.ClientSocket.Socket := Sock;
      ClientThread.ClientSocket.Server := FServer;
      ClientThread.ClientSocket.RemoteIP := inet_ntoa(Addr.sin_addr);
      ClientThread.ClientSocket.RemotePort := ntohs(Addr.sin_port);

      EnterCriticalSection(FServer.FListLock);
      try
        FServer.FClientThreads.Add(ClientThread);
      finally
        LeaveCriticalSection(FServer.FListLock);
      end;
      ClientThread.Resume;
    end;
  end;
end;

{ TCnTCPClientThread }

constructor TCnTCPClientThread.Create(CreateSuspended: Boolean);
begin
  inherited;
  FClientSocket := TCnClientSocket.Create;
end;

destructor TCnTCPClientThread.Destroy;
begin
  FClientSocket.Free;
  inherited;
end;

procedure TCnTCPClientThread.Execute;
begin
  // 客户端已连接上，事件里有参数可被用
  if Assigned(FClientSocket.Server.OnAccept) then
    FClientSocket.Server.OnAccept(FClientSocket.Server, FClientSocket);

  // 客户处理完毕了，可以断开连接了
  FClientSocket.Server.CheckSocketError(closesocket(FClientSocket.Socket));
  FClientSocket.Socket := INVALID_SOCKET;
end;

{ TCnClientSocket }

function TCnClientSocket.Recv(var Buf; Len: Integer; Flags: Integer): Integer;
begin
  Result := FServer.CheckSocketError(WinSock.recv(FSocket, Buf, Len, Flags));
  if Result <> SOCKET_ERROR then
  begin
    Inc(FBytesReceived, Result);
    FServer.IncRecv(Result);
  end;
end;

function TCnClientSocket.Send(var Buf; Len: Integer; Flags: Integer): Integer;
begin
  Result := FServer.CheckSocketError(WinSock.send(FSocket, Buf, Len, Flags));
  if Result <> SOCKET_ERROR then
  begin
    Inc(FBytesSent, Result);
    FServer.IncSent(Result);
  end;
end;

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

initialization
  Startup;

finalization
  Cleanup;

end.
