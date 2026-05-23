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
  { Simple TCP/UDP client using CnSocket for cross-platform }
  TCnNetClient = class
  private
    FSocket: TSocket;
    FHost: string;
    FPort: Word;
    FIsUDP: Boolean;
    FConnected: Boolean;
    FBytesSent: Int64;
    FBytesReceived: Int64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connect(const AHost: string; APort: Word; UseUDP: Boolean = False);
    procedure Disconnect;
    function Send(var Buf; Len: Integer): Integer;
    function Recv(var Buf; Len: Integer): Integer;
    property Socket: TSocket read FSocket;
    property Connected: Boolean read FConnected;
    property IsUDP: Boolean read FIsUDP;
    property BytesSent: Int64 read FBytesSent;
    property BytesReceived: Int64 read FBytesReceived;
  end;

  TCntClient = class
  private
    FOptions: TCntOptions;
    FHost: string;
    FPort: Word;
    FNetClient: TCnNetClient;
    FOutputFile: TFileStream;
    FTotalSent: Int64;
    FTotalReceived: Int64;
    FRunning: Boolean;
    FConnected: Boolean;

    procedure NetConnect;
    procedure CloseConnections;
    function IsRunning: Boolean;
    procedure OutputData(const Data: Pointer; Len: Integer; const Direction: string);
    procedure OnRecvData(const Data: Pointer; Len: Integer);
    procedure OnSentData(const Data: Pointer; Len: Integer);
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

type
  THostEntRec = record
    h_name: PAnsiChar;
    h_aliases: PPAnsiChar;
    h_addrtype: Integer;
    h_length: Integer;
    h_addr_list: PPAnsiChar;
  end;
  PHostEntRec = ^THostEntRec;

function gethostbyname(name: PAnsiChar): PHostEntRec; cdecl; external 'libc' name 'gethostbyname';
{$ENDIF}

{$IFDEF LINUX}
function gethostbyname(name: PAnsiChar): PHostEnt; cdecl; external 'libc' name 'gethostbyname';
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

constructor TCnNetClient.Create;
begin
  inherited Create;
  FSocket := INVALID_SOCKET;
  FConnected := False;
  FBytesSent := 0;
  FBytesReceived := 0;
end;

destructor TCnNetClient.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TCnNetClient.Connect(const AHost: string; APort: Word; UseUDP: Boolean);
var
  SockAddr: TSockAddr;
  Data: Integer;
  ErrorCode: Integer;
{$IFDEF MSWINDOWS}
  WinHostEnt: PHostEnt;
{$ELSE}
  {$IFDEF DARWIN}
  HostEnt: PHostEntRec;
  AddrPtr: PLongWord;
  {$ELSE}
  HostEnt: PHostEnt;
  {$ENDIF}
{$ENDIF}
begin
  Disconnect;

  FIsUDP := UseUDP;
  FHost := AHost;
  FPort := APort;

  // Create socket (UDP or TCP)
  if UseUDP then
    FSocket := CnNewSocket(PF_INET, SOCK_DGRAM, IPPROTO_UDP)
  else
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
  SockAddr.sin_addr.s_addr := LongWord(inet_addr(PAnsiChar(AnsiString(AHost))));

  if LongWord(SockAddr.sin_addr.s_addr) = LongWord(INADDR_NONE) then
  begin
{$IFDEF MSWINDOWS}
    WinHostEnt := WinSock.gethostbyname(PAnsiChar(AnsiString(AHost)));
    if WinHostEnt <> nil then
      SockAddr.sin_addr.s_addr := LongWord(PInAddr(WinHostEnt^.h_addr_list^)^.S_addr)
    else
      raise Exception.Create('Failed to resolve host: ' + AHost);
{$ELSE}
    HostEnt := gethostbyname(PAnsiChar(AnsiString(AHost)));
    if HostEnt <> nil then
    begin
      AddrPtr := PLongWord(HostEnt^.h_addr_list^);
      SockAddr.sin_addr.s_addr := AddrPtr^;
    end
    else
      raise Exception.Create('Failed to resolve host: ' + AHost);
{$ENDIF}
  end;

  // Connect
  // For TCP: connect failure means connection refused -> error
  // For UDP: connect() sets default destination; WSAECONNREFUSED
  //   (ICMP Port Unreachable) is expected if no service listens,
  //   but the socket can still send data.
  if CnConnect(FSocket, SockAddr, SizeOf(SockAddr)) < 0 then
  begin
    if not UseUDP then
    begin
      ErrorCode := CnGetNetErrorNo;
      CnCloseSocket(FSocket);
      FSocket := INVALID_SOCKET;
      raise Exception.Create('Failed to connect. Error: ' + IntToStr(ErrorCode));
    end;
  end;

  FConnected := True;
end;

procedure TCnNetClient.Disconnect;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    CnShutdown(FSocket, SHUT_RDWR);
    CnCloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
  FConnected := False;
end;

function TCnNetClient.Send(var Buf; Len: Integer): Integer;
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

function TCnNetClient.Recv(var Buf; Len: Integer): Integer;
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
  Result := FRunning and FConnected and GCntRunning;
end;

procedure TCntClient.CloseConnections;
begin
  FRunning := False;
  FConnected := False;

  if FNetClient <> nil then
  begin
    FNetClient.Disconnect;
    FreeAndNil(FNetClient);
  end;
end;

procedure TCntClient.NetConnect;
var
  Data: Integer;
  UseUDP: Boolean;
begin
  UseUDP := (FOptions.Protocol = cpUDP);
  FNetClient := TCnNetClient.Create;
  try
    if FOptions.Verbose then
      WriteLn('Resolving host: ', FHost);

    FNetClient.Connect(FHost, FPort, UseUDP);

    FConnected := True;

    if FOptions.Verbose then
    begin
      if UseUDP then
        WriteLn('UDP connected to ', FHost, ':', FPort)
      else
        WriteLn('Connected to ', FHost, ':', FPort);
    end;

    // Set keepalive if requested (TCP only)
    if not UseUDP and FOptions.KeepAlive then
    begin
      Data := 1;
      CnSetSockOpt(FNetClient.Socket, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@Data), SizeOf(Data));
    end;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      if FNetClient <> nil then
        FreeAndNil(FNetClient);
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

// 发送本地数据
procedure TCntClient.OnSentData(const Data: Pointer; Len: Integer);
begin
  OutputData(Data, Len, '>');
end;

procedure TCntClient.Run;
begin
  NetConnect;

  if not FConnected then
    Exit;

  if FOptions.Verbose then
    WriteLn('Starting dual-comm (press Ctrl+C or close to exit)...');

  // 调用统一的双向通信循环（stdin <-> socket）
  CnDualCommRun(
    FNetClient.Socket,
    FOptions,
    FTotalSent,
    FTotalReceived,
    IsRunning,
    OnRecvData,
    OnSentData
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
