{******************************************************************************}
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

unit CntServer;
{
  CNT - CnPack NetTool
  TCP/UDP 服务端实现单元

  跨平台支持：Delphi 5+, FPC 3+
  操作系统：Windows / Linux / macOS / Unix

  使用 CnSocket 单元提供跨平台 socket 封装，双向通信通过 CntDualComm 单元实现
}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, WinSock,
{$ENDIF}
{$IFDEF FPC}
  Sockets, {$IFNDEF MSWINDOWS} BaseUnix, {$ENDIF}
{$ENDIF}
  CntCmdLine, CntUtils, CntConsts, CntDualComm,
  CnSocket, CntTunnel;

procedure RunServer(const Options: TCntOptions; Port: Word);

implementation

{$IFDEF FPC}
{$IFDEF DARWIN}
function inet_ntoa(inaddr: in_addr): PAnsiChar; cdecl; external 'libc' name 'inet_ntoa';
function inet_addr(cp: PAnsiChar): LongInt; cdecl; external 'libc' name 'inet_addr';
{$ENDIF}
{$ENDIF}

type
  { Simple TCP server for CNT }
  TCnSimpleTCPServer = class
  private
    FSocket: TSocket;
    FPort: Word;
    FLocalIP: string;
    FListening: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Listen(APort: Word; const ALocalIP: string);
    procedure Stop;
    function AcceptClient(var ClientSocket: TSocket; var RemoteIP: string; var RemotePort: Word): Boolean;
    property Socket: TSocket read FSocket;
    property Listening: Boolean read FListening;
  end;

  TCntServer = class
  private
    FOptions: TCntOptions;
    FPort: Word;
    FServerSocket: TSocket;
    FClientSocket: TSocket;
    FOutputFile: TFileStream;
    FTotalSent: Int64;
    FTotalReceived: Int64;
    FRunning: Boolean;

    procedure SetupKeepAlive(Sock: TSocket);
    procedure OutputData(const Data: Pointer; Len: Integer; const Direction: string);
    procedure CloseSockets;
    function IsRunning: Boolean;
    procedure OnRecvData(const Data: Pointer; Len: Integer);
  public
    constructor Create(const AOptions: TCntOptions; APort: Word);
    destructor Destroy; override;
    procedure Run;
  end;

procedure RunServer(const Options: TCntOptions; Port: Word);
begin
  if Port = 0 then
  begin
    WriteLn('Error: Invalid port number.');
    Exit;
  end;

  // 如果有 exec/shell-exec 命令，使用 tunnel 模式
  if (Options.ExecCmd <> '') or (Options.ShellExecCmd <> '') then
  begin
    RunTunnelServer(Options, Port);
    Exit;
  end;

  if Options.Verbose then
  begin
    WriteLn('CNT Server listening on port ', Port);
    if Options.Protocol = cpUDP then
      WriteLn('Protocol: UDP');
    if Options.HexDump then
      WriteLn('Hex dump: enabled');
    if Options.KeepAlive then
      WriteLn('Keep-alive: enabled');
    if Options.KeepListen then
      WriteLn('Keep listen: enabled');
  end;

  with TCntServer.Create(Options, Port) do
  try
    Run;
  finally
    Free;
  end;
end;

{ TCnSimpleTCPServer }

constructor TCnSimpleTCPServer.Create;
begin
  inherited Create;
  FSocket := INVALID_SOCKET;
  FListening := False;
end;

destructor TCnSimpleTCPServer.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TCnSimpleTCPServer.Listen(APort: Word; const ALocalIP: string);
var
  SockAddr: TSockAddr;
  Data: Integer;
begin
  Stop;

  FPort := APort;
  FLocalIP := ALocalIP;

  // Create socket
  FSocket := CnNewSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    raise Exception.Create('Failed to create server socket');

  // Set SO_REUSEADDR
  Data := 1;
  CnSetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@Data), SizeOf(Data));

  // Bind
  FillChar(SockAddr, SizeOf(SockAddr), 0);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := htons(APort);

  if ALocalIP <> '' then
    SockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(ALocalIP)))
  else
    SockAddr.sin_addr.s_addr := INADDR_ANY;

  if CnBind(FSocket, SockAddr, SizeOf(SockAddr)) < 0 then
  begin
    CnCloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
    raise Exception.Create('Failed to bind to port ' + IntToStr(APort));
  end;

  if CnListen(FSocket, 5) < 0 then
  begin
    CnCloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
    raise Exception.Create('Failed to listen on port ' + IntToStr(APort));
  end;

  FListening := True;
end;

procedure TCnSimpleTCPServer.Stop;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    CnShutdown(FSocket, SHUT_RDWR);
    CnCloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
  FListening := False;
end;

function TCnSimpleTCPServer.AcceptClient(var ClientSocket: TSocket; var RemoteIP: string; var RemotePort: Word): Boolean;
var
  SockAddr: TSockAddr;
  Len: Integer;
begin
  Result := False;
  ClientSocket := INVALID_SOCKET;

  if not FListening then
    Exit;

  Len := SizeOf(SockAddr);
  FillChar(SockAddr, SizeOf(SockAddr), 0);

  ClientSocket := CnAccept(FSocket, @SockAddr, @Len);
  if ClientSocket = INVALID_SOCKET then
    Exit;

  RemoteIP := string(inet_ntoa(SockAddr.sin_addr));
  RemotePort := ntohs(SockAddr.sin_port);
  Result := True;
end;

{ TCntServer }

constructor TCntServer.Create(const AOptions: TCntOptions; APort: Word);
begin
  inherited Create;
  FOptions := AOptions;
  FPort := APort;
  FServerSocket := INVALID_SOCKET;
  FClientSocket := INVALID_SOCKET;
  FOutputFile := nil;
  FTotalSent := 0;
  FTotalReceived := 0;
  FRunning := True;

  if FOptions.OutputFile <> '' then
  begin
    try
      FOutputFile := TFileStream.Create(FOptions.OutputFile, fmCreate or fmShareDenyWrite);
    except
      on E: Exception do
        WriteLn('Warning: Could not open output file: ', E.Message);
    end;
  end;
end;

destructor TCntServer.Destroy;
begin
  CloseSockets;
  if FOutputFile <> nil then
    FreeAndNil(FOutputFile);
  inherited;
end;

procedure TCntServer.CloseSockets;
begin
  if FClientSocket <> INVALID_SOCKET then
  begin
    CnShutdown(FClientSocket, SHUT_RDWR);
    CnCloseSocket(FClientSocket);
    FClientSocket := INVALID_SOCKET;
  end;

  if FServerSocket <> INVALID_SOCKET then
  begin
    CnShutdown(FServerSocket, SHUT_RDWR);
    CnCloseSocket(FServerSocket);
    FServerSocket := INVALID_SOCKET;
  end;
end;

function TCntServer.IsRunning: Boolean;
begin
  Result := FRunning and (FClientSocket <> INVALID_SOCKET);
end;

procedure TCntServer.SetupKeepAlive(Sock: TSocket);
var
  Data: Integer;
begin
  if FOptions.KeepAlive then
  begin
    Data := 1;
    CnSetSockOpt(Sock, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@Data), SizeOf(Data));
  end;
end;

procedure TCntServer.OutputData(const Data: Pointer; Len: Integer; const Direction: string);
var
  Line: string;
begin
  if FOptions.HexDump then
  begin
    Line := HexDump(Data, Len, 0, Direction + ' ');
    Write(Line);
    Flush(Output);
  end
  else
  begin
    if FOptions.OutputMode = omQuiet then
      Exit;

    SetString(Line, PAnsiChar(Data), Len);
    Write(Line);
    Flush(Output);  // 立即刷新输出
  end;

  if (FOutputFile <> nil) and (Len > 0) then
  begin
    try
      FOutputFile.WriteBuffer(Data^, Len);
    except
      on E: Exception do
        WriteLn('Warning: Write to file failed: ', E.Message);
    end;
  end;
end;

// 收到客户端数据的回调 - 显示到屏幕并记录
procedure TCntServer.OnRecvData(const Data: Pointer; Len: Integer);
begin
  OutputData(Data, Len, '<');
end;

{==================== 主运行方法 ====================}

procedure TCntServer.Run;
var
  ServerSock: TSocket;
  ClientSock: TSocket;
  RemoteIP: string;
  RemotePort: Word;
  ServerAddr: TSockAddr;
  Data: Integer;
  AddrLen: Integer;
begin
  ServerSock := INVALID_SOCKET;
  ClientSock := INVALID_SOCKET;

  try
    // Create server socket
    ServerSock := CnNewSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
    if ServerSock = INVALID_SOCKET then
    begin
      WriteLn('Error: Failed to create server socket');
      Exit;
    end;

    // Set SO_REUSEADDR
    Data := 1;
    CnSetSockOpt(ServerSock, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@Data), SizeOf(Data));

    // Bind
    FillChar(ServerAddr, SizeOf(ServerAddr), 0);
    ServerAddr.sin_family := AF_INET;
    ServerAddr.sin_port := htons(FPort);
    ServerAddr.sin_addr.s_addr := INADDR_ANY;

    if CnBind(ServerSock, ServerAddr, SizeOf(ServerAddr)) < 0 then
    begin
      CnCloseSocket(ServerSock);
      WriteLn('Error: Failed to bind to port ', FPort);
      Exit;
    end;

    if CnListen(ServerSock, 5) < 0 then
    begin
      CnCloseSocket(ServerSock);
      WriteLn('Error: Failed to listen on port ', FPort);
      Exit;
    end;

    if FOptions.Verbose then
      WriteLn('Listening on port ', FPort, '...');

    FServerSocket := ServerSock;

    if FOptions.Protocol = cpTCP then
    begin
      // ===== 接受连接循环 =====
      while FRunning do
      begin
        if FOptions.Verbose then
          WriteLn('Waiting for connection...');

        FillChar(ServerAddr, SizeOf(ServerAddr), 0);
        AddrLen := SizeOf(ServerAddr);
        ClientSock := CnAccept(ServerSock, @ServerAddr, @AddrLen);

        if ClientSock = INVALID_SOCKET then
        begin
          Sleep(100);
          Continue;
        end;

        RemoteIP := string(inet_ntoa(ServerAddr.sin_addr));
        RemotePort := ntohs(ServerAddr.sin_port);

        if FOptions.Verbose then
          WriteLn('Connection from ', RemoteIP, ':', RemotePort);

        SetupKeepAlive(ClientSock);
        FClientSocket := ClientSock;

        // ===== 进入双向通信（使用统一的双向通信单元）=====
        CnDualCommRun(
          ClientSock,
          FOptions,
          FTotalSent,
          FTotalReceived,
          IsRunning,
          OnRecvData
        );

        // 客户端断开后关闭
        if ClientSock <> INVALID_SOCKET then
        begin
          CnShutdown(ClientSock, SHUT_RDWR);
          CnCloseSocket(ClientSock);
          ClientSock := INVALID_SOCKET;
          FClientSocket := INVALID_SOCKET;
        end;

        if not FOptions.KeepListen then
        begin
          if FOptions.Verbose then
            WriteLn('Server exiting...');
          Break;
        end;
      end;  // accept loop
    end
    else
    begin
      // UDP server mode - simplified
      WriteLn('UDP server mode not yet implemented');
    end;

  finally
    CloseSockets;
  end;

  if FOptions.Verbose then
  begin
    WriteLn('Total sent: ', FTotalSent, ' bytes');
    WriteLn('Total received: ', FTotalReceived, ' bytes');
    WriteLn('CNT Server finished.');
  end;
end;

end.
