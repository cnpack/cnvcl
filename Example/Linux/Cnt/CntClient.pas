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

unit CntClient;
{
  CNT - CnPack NetTool
  TCP/UDP 客户端实现单元

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
{$IFDEF POSIX}
  Posix.SysSocket, Posix.ArpaInet, Posix.NetinetIn,
{$ENDIF}
  CntCmdLine, CntUtils, CntConsts, CntDualComm,
  CnSocket;

type
  { Simple TCP client using CnSocket for cross-platform }
  TCnSimpleTCPClient = class
  private
    FSocket: TSocket;
    FHost: string;
    FPort: Word;
    FConnected: Boolean;
    FBytesSent: Int64;
    FBytesReceived: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const AHost: string; APort: Word);
    procedure Disconnect;
    function Send(var Buf; Len: Integer): Integer;
    function Recv(var Buf; Len: Integer): Integer;
    property Socket: TSocket read FSocket;
    property Connected: Boolean read FConnected;
    property BytesSent: Int64 read FBytesSent;
    property BytesReceived: Int64 read FBytesReceived;
  end;

  TCntClient = class
  private
    FOptions: TCntOptions;
    FHost: string;
    FPort: Word;
    FTCPClient: TCnSimpleTCPClient;
    FOutputFile: TFileStream;
    FTotalSent: Int64;
    FTotalReceived: Int64;
    FRunning: Boolean;
    FConnected: Boolean;

    procedure TCPConnect;
    procedure OutputData(const Data: Pointer; Len: Integer; const Direction: string);
    procedure OnRecvData(const Data: Pointer; Len: Integer);
    function IsRunning: Boolean;
    procedure CloseConnections;
  public
    constructor Create(const AOptions: TCntOptions; const AHost: string; const APort: Word);
    destructor Destroy; override;
    procedure Run;
  end;

procedure RunClient(const Options: TCntOptions; const Host: string; const Port: string);

implementation

{$IFDEF FPC}
{$IFDEF DARWIN}
function inet_addr(cp: PAnsiChar): LongInt; cdecl; external 'libc' name 'inet_addr';
{$ENDIF}
{$ENDIF}

procedure RunClient(const Options: TCntOptions; const Host: string; const Port: string);
var
  PortNum: Word;
  Code: Integer;
begin
  if Host = '' then
  begin
    WriteLn('Error: No host specified.');
    Exit;
  end;

  if Port = '' then
  begin
    WriteLn('Error: No port specified.');
    Exit;
  end;

  Val(Port, PortNum, Code);

  if Options.Verbose then
  begin
    WriteLn('CNT Client connecting to ', Host, ':', PortNum);
    if Options.Protocol = cpUDP then
      WriteLn('Protocol: UDP');
    if Options.HexDump then
      WriteLn('Hex dump: enabled');
    if Options.KeepAlive then
      WriteLn('Keep-alive: enabled');
    if Options.WaitTimeout > 0 then
      WriteLn('Timeout: ', Options.WaitTimeout, ' seconds');
  end;

  with TCntClient.Create(Options, Host, PortNum) do
  try
    Run;
  finally
    Free;
  end;
end;

{ TCnSimpleTCPClient }

constructor TCnSimpleTCPClient.Create;
begin
  inherited Create;
  FSocket := INVALID_SOCKET;
  FConnected := False;
  FBytesSent := 0;
  FBytesReceived := 0;
end;

destructor TCnSimpleTCPClient.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TCnSimpleTCPClient.Connect(const AHost: string; APort: Word);
var
  SockAddr: TSockAddr;
  Data: Integer;
  ErrorCode: Integer;
begin
  Disconnect;

  FHost := AHost;
  FPort := APort;

  // Create socket
  FSocket := CnNewSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    raise Exception.Create('Failed to create socket');

  // Set SO_REUSEADDR if needed
  Data := 1;
  CnSetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@Data), SizeOf(Data));

  // Resolve host using CnSocket's built-in resolution
  FillChar(SockAddr, SizeOf(SockAddr), 0);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := htons(APort);
  SockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(AHost)));

  if SockAddr.sin_addr.s_addr = INADDR_NONE then
  begin
    // Try DNS lookup using system resolver
    // For cross-platform, we'll try using gethostbyname equivalent
    // This is simplified - in production use a proper DNS lookup
    raise Exception.Create('Failed to resolve host: ' + AHost);
  end;

  // Connect
  if CnConnect(FSocket, SockAddr, SizeOf(SockAddr)) < 0 then
  begin
    ErrorCode := CnGetNetErrorNo;
    CnCloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
    raise Exception.Create('Failed to connect. Error: ' + IntToStr(ErrorCode));
  end;

  FConnected := True;
end;

procedure TCnSimpleTCPClient.Disconnect;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    CnShutdown(FSocket, SHUT_RDWR);
    CnCloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
  FConnected := False;
end;

function TCnSimpleTCPClient.Send(var Buf; Len: Integer): Integer;
begin
  if not FConnected then
  begin
    Result := -1;
    Exit;
  end;

  Result := CnSend(FSocket, Buf, Len, 0);
  if Result >= 0 then
    Inc(FBytesSent, Result);
end;

function TCnSimpleTCPClient.Recv(var Buf; Len: Integer): Integer;
begin
  if not FConnected then
  begin
    Result := -1;
    Exit;
  end;

  Result := CnRecv(FSocket, Buf, Len, 0);
  if Result > 0 then
    Inc(FBytesReceived, Result)
  else if Result = 0 then
  begin
    // Connection closed
    Disconnect;
  end;
end;

{ TCntClient }

constructor TCntClient.Create(const AOptions: TCntOptions; const AHost: string; const APort: Word);
begin
  inherited Create;
  FOptions := AOptions;
  FHost := AHost;
  FPort := APort;
  FOutputFile := nil;
  FTotalSent := 0;
  FTotalReceived := 0;
  FRunning := True;
  FConnected := False;

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

destructor TCntClient.Destroy;
begin
  CloseConnections;
  if FOutputFile <> nil then
    FreeAndNil(FOutputFile);
  inherited;
end;

function TCntClient.IsRunning: Boolean;
begin
  Result := FRunning and FConnected;
end;

procedure TCntClient.CloseConnections;
begin
  FRunning := False;
  FConnected := False;

  if FTCPClient <> nil then
  begin
    FTCPClient.Disconnect;
    FreeAndNil(FTCPClient);
  end;
end;

procedure TCntClient.TCPConnect;
var
  Data: Integer;
begin
  FTCPClient := TCnSimpleTCPClient.Create;
  try
    if FOptions.Verbose then
      WriteLn('Resolving host: ', FHost);

    FTCPClient.Connect(FHost, FPort);

    FConnected := True;

    if FOptions.Verbose then
      WriteLn('Connected to ', FHost, ':', FPort);

    // Set keepalive if requested
    if FOptions.KeepAlive then
    begin
      Data := 1;
      CnSetSockOpt(FTCPClient.Socket, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@Data), SizeOf(Data));
    end;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      if FTCPClient <> nil then
        FreeAndNil(FTCPClient);
      Exit;
    end;
  end;
end;

procedure TCntClient.OutputData(const Data: Pointer; Len: Integer; const Direction: string);
var
  Line: string;
begin
  if FOptions.HexDump then
  begin
    Line := HexDump(Data, Len, 0, Direction + ' ');
    Write(Line);
    Flush(Output);  // 立即刷新，双向实时显示
  end
  else
  begin
    if FOptions.OutputMode = omQuiet then
      Exit;

    SetString(Line, PAnsiChar(Data), Len);
    Write(Line);
    Flush(Output);  // 立即刷新
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

// 收到远程数据的回调 - 显示到屏幕并记录
procedure TCntClient.OnRecvData(const Data: Pointer; Len: Integer);
begin
  OutputData(Data, Len, '<');
end;

procedure TCntClient.Run;
begin
  TCPConnect;

  if not FConnected then
    Exit;

  if FOptions.Verbose then
    WriteLn('Starting dual-comm (press Ctrl+C or close to exit)...');

  // 调用统一的双向通信循环（stdin <-> socket）
  CnDualCommRun(
    FTCPClient.Socket,
    FOptions,
    FTotalSent,
    FTotalReceived,
    IsRunning,
    OnRecvData
  );

  CloseConnections;

  if FOptions.Verbose then
  begin
    WriteLn('Total sent: ', FTotalSent, ' bytes');
    WriteLn('Total received: ', FTotalReceived, ' bytes');
    WriteLn('CNT Client finished.');
  end;
end;

end.
