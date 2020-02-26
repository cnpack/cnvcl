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

unit CnTCPForwarder;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯组件包 TCP 端口转发实现单元
* 单元作者：CnPack 开发组 Liu Xiao
* 备    注：一个使用 ThreadingTCPServer 的多线程端口转发组件，无线程池机制
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2020.02.25 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Contnrs, WinSock, CnConsts, CnNetConsts, CnClasses,
  CnThreadingTCPServer, CnTCPClient;

type
  TCnTCPForwarder = class(TCnThreadingTCPServer)
  {* TCP 端口转发组件，对每个客户端连接起两个线程}
  private
    FRemoteHost: string;
    FRemotePort: Word;
    FOnRemoteConnected: TNotifyEvent;
    procedure SetRemoteHost(const Value: string);
    procedure SetRemotePort(const Value: Word);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    function DoGetClientThread: TCnTCPClientThread; override;
    {* 子类重载使用 TCnTCPForwardThread}

    procedure DoRemoteConnected; virtual;
  published
    property RemoteHost: string read FRemoteHost write SetRemoteHost;
    {* 转发的远程主机}
    property RemotePort: Word read FRemotePort write SetRemotePort;
    {* 转发的远程端口}

    property OnRemoteConnected: TNotifyEvent read FOnRemoteConnected write FOnRemoteConnected;
    {* 连接上远程服务器时触发}
  end;

implementation

const
  FORWARDER_BUF_SIZE = 32 * 1024;

type
  TCnForwarderClientSocket = class(TCnClientSocket)
  {* 封装的代表一客户端连接转发的对象，包括前向后向两个线程实例与通讯的 Socket}
  private
    FLock: TRTLCriticalSection;
    FTCPClient: TCnTCPClient;
    FBackwardThread: TThread;
    FForwardThread: TThread;
    procedure BackwardThreadTerminate(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Shutdown; override;
    {* 关闭前向后向两个 Socket}

    property TCPClient: TCnTCPClient read FTCPClient write FTCPClient;
    {* 与服务端通讯的 Socket 封装（与客户端通讯的 Socket 封装在父类中）}

    property ForwardThread: TThread read FForwardThread write FForwardThread;
    {* 从客户端读，写到服务端的线程}
    property BackwardThread: TThread read FBackwardThread write FBackwardThread;
    {* 从服务端读，写到客户端的线程}
  end;

  TCnTCPForwardThread = class(TCnTCPClientThread)
  {* 有客户端连接上时的处理线程，从客户端读，写到服务端}
  protected
    function DoGetClientSocket: TCnClientSocket; override;
    procedure Execute; override;
  end;

  TCnTCPBackwardThread = class(TThread)
  {* 从服务端读，写到客户端的线程，被作为 Accept 后的新线程使用}
  private
    FClientSocket: TCnForwarderClientSocket;
  protected
    procedure Execute; override;
  public
    property ClientSocket: TCnForwarderClientSocket read FClientSocket write FClientSocket;
  end;

{ TCnTCPForwarder }

function TCnTCPForwarder.DoGetClientThread: TCnTCPClientThread;
begin
  Result := TCnTCPForwardThread.Create(True);
end;

procedure TCnTCPForwarder.DoRemoteConnected;
begin
  if Assigned(FOnRemoteConnected) then
    FOnRemoteConnected(Self);
end;

procedure TCnTCPForwarder.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnTCPForwarderName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnTCPForwarderComment;
end;

procedure TCnTCPForwarder.SetRemoteHost(const Value: string);
begin
  FRemoteHost := Value;
end;

procedure TCnTCPForwarder.SetRemotePort(const Value: Word);
begin
  FRemotePort := Value;
end;

{ TCnTCPForwardThread }

function TCnTCPForwardThread.DoGetClientSocket: TCnClientSocket;
begin
  Result := TCnForwarderClientSocket.Create;
end;

procedure TCnTCPForwardThread.Execute;
var
  Client: TCnForwarderClientSocket;
  Forwarder: TCnTCPForwarder;
  Buf: array[0..FORWARDER_BUF_SIZE - 1] of Byte;
  Ret: Integer;
begin
  // 客户端已连接上，事件里有参数可被用
  DoAccept;
  Forwarder := TCnTCPForwarder(ClientSocket.Server);

  Client := TCnForwarderClientSocket(ClientSocket);
  Client.TCPClient := TCnTCPClient.Create(nil);
  Client.TCPClient.RemoteHost := Forwarder.RemoteHost;
  Client.TCPClient.RemotePort := Forwarder.RemotePort;
  Client.ForwardThread := Self;

  // 连接远程主机
  Client.TCPClient.Active := True;
  if not Client.TCPClient.Connected then
  begin
    Client.Shutdown;
    Exit;
  end
  else
    Forwarder.DoRemoteConnected;

  // 连接成功后，本线程从客户端 ClientSocket 读，写到服务端 OutClient
  // 另起一个线程从服务端读，写到客户端（因为没有规定必须是客户端先发起请求）

  Client.BackwardThread := TCnTCPBackwardThread.Create(True);
  (Client.BackwardThread as TCnTCPBackwardThread).ClientSocket := Client;
  Client.BackwardThread.FreeOnTerminate := True;
  Client.BackwardThread.OnTerminate := Client.BackwardThreadTerminate;
  Client.BackwardThread.Resume;

  try
    while not Terminated do
    begin
      Ret := ClientSocket.Recv(Buf, SizeOf(Buf));
      if Ret <= 0 then
      begin
        // Recv 出错说明客户端已断开，自行断开远程连接，通知另一线程停止，退出
        Client.Shutdown;
        if Client.BackwardThread <> nil then
          Client.BackwardThread.Terminate;

        Break;
      end;

      Ret := Forwarder.CheckSocketError(Client.TCPClient.Send(Buf, Ret));
      if Ret <= 0 then
      begin
        // Send 出错说明服务端已断开，也自行断开远程连接，通知另一线程停止，退出（会断开客户端连接）
        Client.Shutdown;
        if Client.BackwardThread <> nil then
          Client.BackwardThread.Terminate;

        Break;
      end;
    end;
  finally
    Client.ForwardThread := nil; // 自己准备退出，把 ForwardThread 塞为 nil
  end;
end;

{ TCnTCPBackwardThread }

procedure TCnTCPBackwardThread.Execute;
var
  FForwarder: TCnTCPForwarder;
  Buf: array[0..FORWARDER_BUF_SIZE - 1] of Byte;
  Ret: Integer;
begin
  FForwarder := TCnTCPForwarder(ClientSocket.Server);

  // 从服务端读，写进客户端
  try
    while not Terminated do
    begin
      Ret := FForwarder.CheckSocketError(ClientSocket.TCPClient.Recv(Buf, SizeOf(Buf)));
      if Ret <= 0 then
      begin
        // Recv 出错说明服务端已断开，自行断开远程连接，通知另一线程停止，退出
        ClientSocket.Shutdown;
        if ClientSocket.ForwardThread <> nil then
          ClientSocket.ForwardThread.Terminate;

        Break;
      end;

      Ret := ClientSocket.Send(Buf, Ret);
      if Ret <= 0 then
      begin
        // Send 出错说明服务端已断开，也自行断开远程连接，通知另一线程停止，退出
        ClientSocket.Shutdown;
        if ClientSocket.ForwardThread <> nil then
          ClientSocket.ForwardThread.Terminate;

        Break;
      end;
    end;
  finally
    ClientSocket.BackwardThread := nil;
  end;
end;

{ TCnForwarderClientSocket }

procedure TCnForwarderClientSocket.BackwardThreadTerminate(
  Sender: TObject);
begin
  FBackwardThread := nil;
end;

procedure TCnForwarderClientSocket.Shutdown;
begin
  // 由于可能被前向后向俩线程交叉调用，因此需要加锁
  EnterCriticalSection(FLock);
  try
    inherited;

    if FTCPClient <> nil then
      FreeAndNil(FTCPClient);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TCnForwarderClientSocket.Create;
begin
  inherited;
  InitializeCriticalSection(FLock);
end;

destructor TCnForwarderClientSocket.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

end.
