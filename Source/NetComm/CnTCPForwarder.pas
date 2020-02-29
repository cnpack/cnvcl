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
  {* 封装的代表一客户端连接转发的对象，包括双端通讯的另一个 Socket}
  private
    FRemoteSocket: TSocket;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Shutdown; override;
    {* 关闭前向后向两个 Socket}

    // send/recv 收发数据封装
    function RemoteSend(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    function RemoteRecv(var Buf; Len: Integer; Flags: Integer = 0): Integer;

    property RemoteSocket: TSocket read FRemoteSocket write FRemoteSocket;
    {* 连接远程服务器的 Socket}
  end;

  TCnTCPForwardThread = class(TCnTCPClientThread)
  {* 有客户端连接上时的处理线程，从客户端服务端双向读写}
  protected
    function DoGetClientSocket: TCnClientSocket; override;
    procedure Execute; override;
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
  Addr: TSockAddr;
  ReadFds: TFDSet;
begin
  // 客户端已连接上，事件里有参数可被用
  DoAccept;
  Forwarder := TCnTCPForwarder(ClientSocket.Server);

  Client := TCnForwarderClientSocket(ClientSocket);
  Client.RemoteSocket := Forwarder.CheckSocketError(socket(AF_INET, SOCK_STREAM, IPPROTO_TCP));
  if Client.RemoteSocket = INVALID_SOCKET then
    Exit;

  Addr.sin_family := AF_INET;
  Addr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(TCnTCPClient.LookupHostAddr(Forwarder.RemoteHost))));
  Addr.sin_port := ntohs(Forwarder.RemotePort);

  if Forwarder.CheckSocketError(WinSock.connect(Client.RemoteSocket, Addr, SizeOf(Addr))) <> 0 then
  begin
    // 连接远程服务器失败，出错退出
    Forwarder.CheckSocketError(closesocket(Client.RemoteSocket));
    Client.RemoteSocket := INVALID_SOCKET;
    Exit;
  end;

  Forwarder.DoRemoteConnected;

  // 连接成功后，本线程开始循环转发，出错则退出
  while not Terminated do
  begin
    // SELECT 等俩 Socket 上的消息，准备读来写去
    FD_ZERO(ReadFds);
    FD_SET(Client.Socket, ReadFds);
    FD_SET(Client.RemoteSocket, ReadFds);

    Ret := Forwarder.CheckSocketError(WinSock.select(0, @ReadFds, nil, nil, nil));
    if Ret > 0 then
    begin
      if FD_ISSET(Client.Socket, ReadFds) then // 客户端有数据来
      begin
        Ret := Client.Recv(Buf, SizeOf(Buf));
        if Ret <= 0 then
        begin
          Client.Shutdown;
          Exit;
        end;
        Ret := Client.RemoteSend(Buf, Ret); // 发到服务端
        if Ret <= 0 then
        begin
          Client.Shutdown;
          Exit;
        end;
      end;

      if FD_ISSET(Client.RemoteSocket, ReadFds) then // 服务端有数据来
      begin
        Ret := Client.RemoteRecv(Buf, SizeOf(Buf));
        if Ret <= 0 then
        begin
          Client.Shutdown;
          Exit;
        end;
        Ret := Client.Send(Buf, Ret); // 发到客户端
        if Ret <= 0 then
        begin
          Client.Shutdown;
          Exit;
        end;
      end;
    end;
    Sleep(0);
  end;
end;

{ TCnForwarderClientSocket }

procedure TCnForwarderClientSocket.Shutdown;
begin
  inherited;
  if FRemoteSocket <> INVALID_SOCKET then
  begin
    (Server as TCnTCPForwarder).CheckSocketError(WinSock.shutdown(FRemoteSocket, 2)); // SD_BOTH
    (Server as TCnTCPForwarder).CheckSocketError(closesocket(FRemoteSocket));
    FRemoteSocket := INVALID_SOCKET;
  end;
end;

constructor TCnForwarderClientSocket.Create;
begin
  inherited;

end;

destructor TCnForwarderClientSocket.Destroy;
begin

  inherited;
end;

function TCnForwarderClientSocket.RemoteRecv(var Buf; Len,
  Flags: Integer): Integer;
begin
  Result := (Server as TCnTCPForwarder).CheckSocketError(
    WinSock.recv(FRemoteSocket, Buf, Len, Flags));
end;

function TCnForwarderClientSocket.RemoteSend(var Buf; Len,
  Flags: Integer): Integer;
begin
  Result := (Server as TCnTCPForwarder).CheckSocketError(
    WinSock.send(FRemoteSocket, Buf, Len, Flags));
end;

end.
