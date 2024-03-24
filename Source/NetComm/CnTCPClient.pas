{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnTCPClient;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯组件包 TCP Client 实现单元
* 单元作者：CnPack 开发组 Liu Xiao
* 备    注：一个简易的多线程阻塞式 TCP Server，新客户端连接时起新线程，调用者
*           在 OnAccept 事件中循环 recv/send 即可，无线程池机制
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWin7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.02.22 V1.1
*                加入跨平台的支持，待测试
*           2020.02.22 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs,
{$IFDEF MSWINDOWS}
  Windows,  WinSock,
{$ELSE}
  System.Net.Socket, Posix.NetinetIn, Posix.SysSocket, Posix.Unistd, Posix.ArpaInet,
{$ENDIF}
  CnConsts, CnNetConsts, CnSocket, CnClasses;

type
  ECnClientSocketError = class(Exception);

  TCnClientSocketErrorEvent = procedure (Sender: TObject; SocketError: Integer) of object;

  TCnTCPClient = class(TCnComponent)
  private
    FSocket: TSocket;
    FActive: Boolean;
    FConnected: Boolean;
    FBytesReceived: Cardinal;
    FBytesSent: Cardinal;
    FRemoteHost: string;
    FOnError: TCnClientSocketErrorEvent;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FRemotePort: Word;
    procedure SetActive(const Value: Boolean);
    procedure SetRemoteHost(const Value: string);
    procedure SetRemotePort(const Value: Word);
    function CheckSocketError(ResultCode: Integer): Integer;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Open;
    {* 开始连接，等同于 Active := True}
    procedure Close;
    {* 关闭连接，等同于 Active := False}

    class function LookupHostAddr(const HostName: string): string;

    // send/recv 收发数据封装
    function Send(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    function Recv(var Buf; Len: Integer; Flags: Integer = 0): Integer;
    // 注意 Recv 返回 0 时说明当前网络对方已断开，本 Client 会自动 Close。
    // 调用者也需要根据返回值做断开处理。

    property BytesSent: Cardinal read FBytesSent;
    {* 发送给各客户端的总字节数}
    property BytesReceived: Cardinal read FBytesReceived;
    {* 从各客户端收取的总字节数}
    property Connected: Boolean read FConnected;
    {* 是否已连接}
  published
    property Active: Boolean read FActive write SetActive;
    {* 是否开始监听}
    property RemoteHost: string read FRemoteHost write SetRemoteHost;
    {* 要连接的远程主机}
    property RemotePort: Word read FRemotePort write SetRemotePort;
    {* 要连接的远程端口}

    property OnError: TCnClientSocketErrorEvent read FOnError write FOnError;
    {* 出错事件}
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    {* 连接成功的事件}
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    {* 连接断开的事件}
  end;

implementation

{$IFDEF MSWINDOWS}
var
  WSAData: TWSAData;
{$ENDIF}

{ TCnTCPClient }

function TCnTCPClient.CheckSocketError(ResultCode: Integer): Integer;
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

procedure TCnTCPClient.Close;
begin
  if FActive then
  begin
    if FConnected then
    begin
      FConnected := False;
      CnShutdown(FSocket, SD_BOTH);

      DoDisconnect;
    end;

    FActive := False;
    CheckSocketError(CnCloseSocket(FSocket));

    FSocket := INVALID_SOCKET;
  end;
end;

constructor TCnTCPClient.Create(AOwner: TComponent);
begin
  inherited;
  FSocket := INVALID_SOCKET;
end;

destructor TCnTCPClient.Destroy;
begin
  Close;
  inherited;
end;

procedure TCnTCPClient.DoConnect;
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TCnTCPClient.DoDisconnect;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TCnTCPClient.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnTCPClientName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnTCPClientComment;
end;

class function TCnTCPClient.LookupHostAddr(const HostName: string): string;
{$IFDEF MSWINDOWS}
var
  H: PHostEnt;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := '';
  if HostName <> '' then
  begin
    if HostName[1] in ['0'..'9'] then // IP 地址
    begin
      if inet_addr(PAnsiChar(AnsiString(HostName))) <> INADDR_NONE then
        Result := HostName;
    end
    else
    begin
      H := gethostbyname(PAnsiChar(AnsiString(HostName)));
      if H <> nil then
        with H^ do
        Result := Format('%d.%d.%d.%d', [Ord(h_addr^[0]), Ord(h_addr^[1]),
          Ord(h_addr^[2]), Ord(h_addr^[3])]);
    end;
  end
  else
    Result := '0.0.0.0';
{$ELSE}
  Result := TIPAddress.LookupName(HostName).Address;
{$ENDIF}
end;

procedure TCnTCPClient.Open;
var
  SockAddress: TSockAddr;
begin
  if not FActive then
  begin
    FSocket := CnNewSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    FActive := FSocket <> INVALID_SOCKET;

    if FActive and not FConnected then
    begin
      FBytesReceived := 0;
      FBytesSent := 0;

      SockAddress.sin_family := AF_INET;
      SockAddress.sin_port := ntohs(FRemotePort);

{$IFDEF MSWINDOWS}
      SockAddress.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(LookupHostAddr(FRemoteHost))));
{$ELSE}
      SockAddress.sin_addr := TIPAddress.LookupName(FRemoteHost);
{$ENDIF}

      FConnected := CheckSocketError(CnConnect(FSocket, SockAddress, SizeOf(SockAddress))) = 0;
      if FConnected then
        DoConnect;
    end;
  end;
end;

function TCnTCPClient.Recv(var Buf; Len, Flags: Integer): Integer;
begin
  Result := CheckSocketError(CnRecv(FSocket, Buf, Len, Flags));

  if Result <> SOCKET_ERROR then
  begin
    if Result = 0 then
      Close
    else
      Inc(FBytesReceived, Result);
  end
end;

function TCnTCPClient.Send(var Buf; Len, Flags: Integer): Integer;
begin
  Result := CheckSocketError(CnSend(FSocket, Buf, Len, Flags));

  if Result <> SOCKET_ERROR then
    Inc(FBytesSent, Result);
end;

procedure TCnTCPClient.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
      if Value then
        Open
      else
        Close
    else
      FActive := Value;
  end;
end;

procedure TCnTCPClient.SetRemoteHost(const Value: string);
begin
  FRemoteHost := Value;
end;

procedure TCnTCPClient.SetRemotePort(const Value: Word);
begin
  FRemotePort := Value;
end;

{$IFDEF MSWINDOWS}

procedure Startup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSAStartup($0101, WSAData);
  if ErrorCode <> 0 then
    raise ECnClientSocketError.Create('WSAStartup');
end;

procedure Cleanup;
var
  ErrorCode: Integer;
begin
  ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise ECnClientSocketError.Create('WSACleanup');
end;

initialization
  Startup;

finalization
  Cleanup;

{$ENDIF}

end.
