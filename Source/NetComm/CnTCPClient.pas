{******************************************************************************}
{                                                                              }
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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
* 单元作者：CnPack 开发组
* 备    注：一个简易的阻塞式 TCP 客户端，需要支持 Delphi 及 FPC 编译器以及 Windows、Mac、Linux 平台
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
  SysUtils, Classes, Contnrs, {$IFDEF MSWINDOWS} Windows,  WinSock, {$ELSE}
  {$IFDEF FPC} Sockets, {$ELSE}
  System.Net.Socket, Posix.NetinetIn, Posix.SysSocket, Posix.Unistd, Posix.ArpaInet,
  {$ENDIF} {$ENDIF}
  CnConsts, CnNetConsts, CnSocket, CnClasses, CnIP;

type
  ECnClientSocketError = class(Exception);

  TCnClientSocketErrorEvent = procedure (Sender: TObject; SocketError: Integer) of object;

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnTCPClient = class(TCnComponent)
  private
    FSocket: TSocket;
    FActive: Boolean;
    FConnected: Boolean;
    FBytesReceived: Int64;
    FBytesSent: Int64;
    FRemoteHost: string;
    FOnError: TCnClientSocketErrorEvent;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FRemotePort: Word;
    procedure SetActive(const Value: Boolean);
    procedure SetRemoteHost(const Value: string);
    procedure SetRemotePort(const Value: Word);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    function CheckSocketError(ResultCode: Integer): Integer;

    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;

    property Socket: TSocket read FSocket;
    {* 真正用于通讯的套接字}
  public
    constructor Create(AOwner: TComponent); override;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}

    procedure Open;
    {* 开始连接，等同于 Active := True}
    procedure Close;
    {* 关闭连接，等同于 Active := False}

    class function LookupHostAddr(const HostName: string): string;
    {* 把主机名解析为规范化的 IPv4 点分十进制串。以下情况一律返回空串：HostName 为空、
       字面量不符合严格格式（必须 4 段十进制、每段 0..255、不允许前导零与十六进制）、
       按名字查询失败、或结果不是 IPv4。也就是说空串是唯一的失败值，而 0.0.0.0 与
       255.255.255.255 都是合法结果，不会被当成失败。
       以数字开头的主机名只按 IPv4 字面量对待（沿用本组件既有约定，并避开
       getaddrinfo 的 inet_aton 数字缩写会把 1.2.3 之类补成另一个地址）。
       因此形如 3com.example 这种以数字开头的域名不会被解析。本函数不支持 IPv6。}

    // send/recv 收发数据封装
    function Send(var Buf; Len: Integer; Flags: Integer = 0): Integer; virtual;
    function Recv(var Buf; Len: Integer; Flags: Integer = 0): Integer; virtual;
    // 注意 Recv 返回 0 时说明当前网络对方已断开，本 Client 会自动 Close。
    // 调用者也需要根据返回值做断开处理。

    property BytesSent: Int64 read FBytesSent;
    {* 发送给各客户端的总字节数}
    property BytesReceived: Int64 read FBytesReceived;
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

// 单元内部的地址解析辅助函数：避免“主机名 -> 字符串 -> inet_addr”的往返。
// 往返会让解析失败值与合法地址混淆（实测 inet_addr("") 返回 $FFFFFFFF，那是链路层广播地址），
// 而且 Windows 的 inet_addr 与 FPC 的 StrToNetAddr 对八进制等宽松形式结论相反。
function CnStrictIPv4ToCardinal(const S: string; out Addr: Cardinal): Boolean;
var
  I, PartIndex, Value, Digits: Integer;
  C: Char;
  B: PByte;
  Octets: array[0..3] of Integer;
begin
  Result := False;
  if S = '' then
    Exit;
  PartIndex := 0;
  Value := 0;
  Digits := 0;
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if C = '.' then
    begin
      if (Digits = 0) or (Digits > 3) or (PartIndex >= 3) then
        Exit;
      Octets[PartIndex] := Value;
      Inc(PartIndex);
      Value := 0;
      Digits := 0;
    end
    else if (C >= '0') and (C <= '9') then
    begin
      if (Digits = 1) and (Value = 0) then
        Exit; // 前导零：部分平台按八进制解释，取值与十进制不同，一律拒绝
      Inc(Digits);
      Value := Value * 10 + (Ord(C) - Ord('0'));
      if Value > 255 then
        Exit;
    end
    else
      Exit;
  end;
  if (Digits = 0) or (Digits > 3) or (PartIndex <> 3) then
    Exit;
  Octets[3] := Value;
  // 按地址顺序逐字节写入，得到的就是网络字节序的 32 位值，不依赖本机大小端
  B := PByte(@Addr);
  B^ := Byte(Octets[0]);
  Inc(B);
  B^ := Byte(Octets[1]);
  Inc(B);
  B^ := Byte(Octets[2]);
  Inc(B);
  B^ := Byte(Octets[3]);
  Result := True;
end;

{ 把网络字节序的地址值还原成规范点分串，读取顺序与 CnStrictIPv4ToCardinal 的写入顺序对称 }
function CnCardinalToDotQuad(Addr: Cardinal): string;
var
  O0, O1, O2, O3: Integer;
  B: PByte;
begin
  B := PByte(@Addr);
  O0 := B^;
  Inc(B);
  O1 := B^;
  Inc(B);
  O2 := B^;
  Inc(B);
  O3 := B^;
  Result := Format('%d.%d.%d.%d', [O0, O1, O2, O3]);
end;

{ 只按名字查询，并且只接受 IPv4 结果；查不到、或结果不是 IPv4 时返回假 }
function CnLookupHostByName(const HostName: string; out Addr: Cardinal): Boolean;
var
{$IFDEF MSWINDOWS}
  H: PHostEnt;
  B: PByte;
{$ELSE}
  Resolved: string;
{$ENDIF}
begin
  Result := False;
  if HostName = '' then
    Exit;
{$IFDEF MSWINDOWS}
  H := gethostbyname(PAnsiChar(AnsiString(HostName)));
  if (H = nil) or (H^.h_addrtype <> AF_INET) or (H^.h_length <> 4) then
    Exit; // 只有 AAAA 记录时不能拿 16 字节的前 4 字节拼成假 IPv4
  B := PByte(@Addr);
  with H^ do
  begin
    B^ := Byte(Ord(h_addr^[0]));
    Inc(B);
    B^ := Byte(Ord(h_addr^[1]));
    Inc(B);
    B^ := Byte(Ord(h_addr^[2]));
    Inc(B);
    B^ := Byte(Ord(h_addr^[3]));
  end;
  Result := True;
{$ELSE}
{$IFDEF FPC}
  if not TCnIp.GetIPByName(Resolved, HostName) then
    Exit;
{$ELSE}
  try
    Resolved := TIPAddress.LookupName(HostName).Address;
  except
    Exit; // 该分支的失败形式未经本机验证，统一折叠成失败值
  end;
{$ENDIF}
  // 所有平台共用同一个严格解析器验收查询结果，IPv6 文本与异常格式都会被判为失败
  Result := CnStrictIPv4ToCardinal(Resolved, Addr);
{$ENDIF}
end;

{ 解析入口 }
function CnResolveHostToCardinal(const HostName: string; out Addr: Cardinal): Boolean;
begin
  if (HostName <> '') and (HostName[1] in ['0'..'9']) then
  begin
    // 数字开头一律只按严格字面量处理，不再交给名字解析：实测 getaddrinfo 会先按
    // inet_aton 的遗留文法做数字缩写，1.2.3 被补成 1.2.0.3、0177 被当八进制、
    // 0x04 被当十六进制，静默改变调用者写明的地址
    Result := CnStrictIPv4ToCardinal(HostName, Addr);
    Exit;
  end;
  Result := CnLookupHostByName(HostName, Addr);
end;

function TCnTCPClient.CheckSocketError(ResultCode: Integer): Integer;
begin
  Result := ResultCode;
  if ResultCode = SOCKET_ERROR then
  begin
    if Assigned(FOnError) then
      FOnError(Self, CnGetNetErrorNo);
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
var
  Addr: Cardinal;
begin
  // 三个平台分支收敛到同一份实现：成功一律是规范化点分串，失败一律是空串，
  // 不再把调用者的原始输入透传回去，也不用 0.0.0.0 兼任失败值
  if CnResolveHostToCardinal(HostName, Addr) then
    Result := CnCardinalToDotQuad(Addr)
  else
    Result := '';
end;

procedure TCnTCPClient.Open;
var
  SockAddress: TSockAddr;
  RemoteAddr: Cardinal;
begin
  if not FActive then
  begin
    // 先解析主机名再建套接字：解析失败绝不能拿失败值去拨号
    if not CnResolveHostToCardinal(FRemoteHost, RemoteAddr) then
      Exit;

    FSocket := CnNewSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    FActive := FSocket <> INVALID_SOCKET;

    if FActive and not FConnected then
    begin
      FBytesReceived := 0;
      FBytesSent := 0;

      FillChar(SockAddress, SizeOf(SockAddress), 0);
      SockAddress.sin_family := AF_INET;
      SockAddress.sin_port := htons(FRemotePort);

{$IFDEF MSWINDOWS}
      SockAddress.sin_addr.s_addr := RemoteAddr;
{$ELSE}
  {$IFDEF FPC}
      SockAddress.sin_addr.s_addr := RemoteAddr;
  {$ELSE}
      SockAddress.sin_addr := TIPAddress.LookupName(FRemoteHost);
  {$ENDIF}
{$ENDIF}

      FConnected := CheckSocketError(CnConnect(FSocket, SockAddress, SizeOf(SockAddress))) = 0;
      if FConnected then
        DoConnect
      else
      begin
        // 连接失败不能留下 Active=True 而套接字仍打开的半开状态：
        // Open 以 not FActive 为前提，残留会让调用方不先 Close 就无法重试
        CnCloseSocket(FSocket);
        FSocket := INVALID_SOCKET;
        FActive := False;
      end;
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
