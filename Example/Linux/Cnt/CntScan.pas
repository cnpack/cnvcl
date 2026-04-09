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

unit CntScan;
{
  CNT - CnPack NetTool
  端口扫描实现单元

  跨平台支持：Delphi 5+, FPC 3+
  操作系统：Windows / Linux / macOS / Unix

  使用 CnSocket 单元提供跨平台 socket 封装
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
  CntCmdLine, CntUtils, CntConsts,
  CnSocket;

type
  { 单个端口的扫描结果 }
  TPortResult = record
    Port: Word;
    State: Byte;  // 0=unknown, 1=open, 2=closed, 3=filtered
    Protocol: string;
    Service: string;
  end;

  { 端口扫描器 }
  TCnPortScanner = class
  private
    FOptions: TCntOptions;
    FHost: string;
    FStartPort: Word;
    FEndPort: Word;
    FIpAddr: string;
    FResults: array of TPortResult;
    FResultCount: Integer;
    FTotalScanned: Integer;
    FOpenCount: Integer;
    FConcurrent: Integer;
    FTimeout: Integer;

    procedure ResolveHost;
    function ScanPortTcp(Port: Word): Byte;
    function ScanPortUdp(Port: Word): Byte;
    procedure AddResult(Port: Word; State: Byte; const Protocol, Service: string);
    procedure OutputResult(const Result: TPortResult);
  public
    constructor Create(const AOptions: TCntOptions; const AHost: string;
      AStartPort, AEndPort: Word);
    destructor Destroy; override;
    procedure Run;
    property OpenCount: Integer read FOpenCount;
    property TotalScanned: Integer read FTotalScanned;
  end;

{ 常用端口对应的服务名 }
function GetServiceName(Port: Word): string;

procedure RunScan(const Options: TCntOptions; const Host: string; StartPort, EndPort: Word);
procedure RunScanMode(const Options: TCntOptions; const Host, PortStr: string);

implementation

{$IFDEF FPC}
{$IFDEF DARWIN}
function inet_ntoa(inaddr: in_addr): PAnsiChar; cdecl; external 'libc' name 'inet_ntoa';
function inet_addr(cp: PAnsiChar): LongInt; cdecl; external 'libc' name 'inet_addr';
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  PWinInAddr = ^WinSock.in_addr;
{$ENDIF}

{$IFDEF UNIX}
const
  EINPROGRESS = 115;
  EISCONN = 106;
{$ENDIF}
{$ENDIF}

{ 常用端口对应的服务名 }
function GetServiceName(Port: Word): string;
const
  Services: array[0..41] of record Port: Word; Name: string; end = (
    (Port: 21; Name: 'ftp'),
    (Port: 22; Name: 'ssh'),
    (Port: 23; Name: 'telnet'),
    (Port: 25; Name: 'smtp'),
    (Port: 53; Name: 'dns'),
    (Port: 80; Name: 'http'),
    (Port: 110; Name: 'pop3'),
    (Port: 111; Name: 'rpcbind'),
    (Port: 135; Name: 'msrpc'),
    (Port: 139; Name: 'netbios'),
    (Port: 143; Name: 'imap'),
    (Port: 443; Name: 'https'),
    (Port: 445; Name: 'microsoft-ds'),
    (Port: 465; Name: 'smtps'),
    (Port: 514; Name: 'syslog'),
    (Port: 587; Name: 'submission'),
    (Port: 636; Name: 'ldaps'),
    (Port: 993; Name: 'imaps'),
    (Port: 995; Name: 'pop3s'),
    (Port: 1080; Name: 'socks'),
    (Port: 1433; Name: 'mssql'),
    (Port: 1434; Name: 'mssql-m'),
    (Port: 1521; Name: 'oracle'),
    (Port: 1723; Name: 'pptp'),
    (Port: 2049; Name: 'nfs'),
    (Port: 3306; Name: 'mysql'),
    (Port: 3389; Name: 'rdp'),
    (Port: 5432; Name: 'postgresql'),
    (Port: 5900; Name: 'vnc'),
    (Port: 5901; Name: 'vnc-1'),
    (Port: 6379; Name: 'redis'),
    (Port: 8080; Name: 'http-proxy'),
    (Port: 8443; Name: 'https-alt'),
    (Port: 8888; Name: 'http-alt'),
    (Port: 9000; Name: 'cslistener'),
    (Port: 9090; Name: 'http-po'),
    (Port: 9200; Name: 'elasticsearch'),
    (Port: 9300; Name: 'es-transport'),
    (Port: 11211; Name: 'memcached'),
    (Port: 27017; Name: 'mongodb'),
    (Port: 27018; Name: 'mongos'),
    (Port: 50000; Name: 'db2')
  );
var
  I: Integer;
begin
  Result := '';
  for I := Low(Services) to High(Services) do
  begin
    if Services[I].Port = Port then
    begin
      Result := Services[I].Name;
      Exit;
    end;
  end;
end;

constructor TCnPortScanner.Create(const AOptions: TCntOptions; const AHost: string;
  AStartPort, AEndPort: Word);
begin
  inherited Create;
  FOptions := AOptions;
  FHost := AHost;
  FStartPort := AStartPort;
  FEndPort := AEndPort;
  FIpAddr := '';
  SetLength(FResults, AEndPort - AStartPort + 1);
  FResultCount := 0;
  FTotalScanned := 0;
  FOpenCount := 0;
  FConcurrent := 50;  // 默认并发数
  FTimeout := 2000;   // 2秒超时
end;

destructor TCnPortScanner.Destroy;
begin
  SetLength(FResults, 0);
  inherited;
end;

procedure TCnPortScanner.ResolveHost;
{$IFDEF MSWINDOWS}
var
  HostEnt: PHostEnt;
  SockAddrIn: TSockAddrIn;
{$ENDIF}
begin
  // 尝试将主机名转换为 IP 地址
  FIpAddr := FHost;

  // 检查是否是 IP 地址格式
  if inet_addr(PAnsiChar(AnsiString(FIpAddr))) <> INADDR_NONE then
    Exit;  // 已经是 IP 地址

  // 尝试 DNS 解析
{$IFDEF MSWINDOWS}
{$IFDEF FPC}
  // FPC on Windows: 使用 WinSock 的 gethostbyname
  HostEnt := gethostbyname(PAnsiChar(AnsiString(FHost)));
  if HostEnt <> nil then
  begin
    // FPC Windows: 需要使用 WinSock 版本的 in_addr
    FIpAddr := string(WinSock.inet_ntoa(PWinInAddr(HostEnt^.h_addr_list^)^));
    Exit;
  end;
{$ELSE}
  // Delphi on Windows: 使用 h_addr_list 获取地址
  HostEnt := gethostbyname(PAnsiChar(AnsiString(FHost)));
  if HostEnt <> nil then
  begin
    FIpAddr := string(inet_ntoa(PInAddr(HostEnt^.h_addr_list^)^));
    Exit;
  end;
{$ENDIF}
{$ELSE}
{$IFDEF FPC}
  // FPC/Linux: 使用 fpGetHostByName
  // 简化处理，直接使用原主机名
  FIpAddr := FHost;
{$ENDIF}
{$ENDIF}
  raise Exception.Create('Unable to resolve host: ' + FHost);
end;

function TCnPortScanner.ScanPortTcp(Port: Word): Byte;
var
  Sock: TSocket;
  SockAddr: TSockAddr;
  Ret: Integer;
  Readfds: TCnFDSet;
  Writefds: TCnFDSet;
  Tv: TTimeVal;
{$IFDEF FPC}
  Flags: LongInt;
{$ENDIF}
begin
  Result := 3;  // 默认 filtered/unknown

  Sock := CnNewSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Sock = INVALID_SOCKET then
  begin
    Result := 2;
    Exit;  // closed
  end;

  try
{$IFDEF FPC}
{$IFNDEF MSWINDOWS}
    // 设置为非阻塞
    Flags := fpFcntl(Sock, F_GetFl, 0);
    fpFcntl(Sock, F_SetFl, Flags or O_NONBLOCK);
{$ENDIF}
{$ENDIF}

    // 填充地址结构
    FillChar(SockAddr, SizeOf(SockAddr), 0);
    SockAddr.sin_family := AF_INET;
    SockAddr.sin_port := htons(Port);
    SockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(FIpAddr)));

    // 尝试连接（非阻塞）
    Ret := CnConnect(Sock, SockAddr, SizeOf(SockAddr));

    if Ret = 0 then
    begin
      // 连接立即成功
      Result := 1;
    end
    else if Ret < 0 then
    begin
{$IFDEF FPC}
      // 检查 errno - EINPROGRESS
      if CnGetNetErrorNo = EINPROGRESS then
      begin
        // 连接正在进行中，使用 select 等待
        CnFDZero(Readfds);
        CnFDZero(Writefds);
        CnFDSet(Sock, Writefds);

        Tv.tv_sec := FTimeout div 1000;
        Tv.tv_usec := (FTimeout mod 1000) * 1000;

        Ret := CnSelect(Sock + 1, @Readfds, @Writefds, nil, @Tv);

        if Ret > 0 then
        begin
          // 连接完成，检查是否成功
          if CnFDIsSet(Sock, Writefds) then
          begin
            // 可写，检查连接是否真的成功了
            Ret := CnConnect(Sock, SockAddr, SizeOf(SockAddr));
            if Ret = 0 then
              Result := 1  // open
            else if CnGetNetErrorNo = EISCONN then
              Result := 1  // already connected
            else
              Result := 2; // closed
          end;
        end
        else
        begin
          // 超时 - 端口可能被过滤
          Result := 3;  // filtered
        end;
      end
      else
{$ENDIF}
      begin
        // 其他错误，通常表示连接被拒绝
        Result := 2;  // closed
      end;
    end;

  finally
    CnCloseSocket(Sock);
  end;
end;

function TCnPortScanner.ScanPortUdp(Port: Word): Byte;
var
  Sock: TSocket;
  SockAddr: TSockAddr;
  Data: Byte;
  Readfds: TCnFDSet;
  Tv: TTimeVal;
  Ret: Integer;
begin
  Result := 3;  // filtered/unknown

  Sock := CnNewSocket(PF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if Sock = INVALID_SOCKET then
  begin
    Result := 2;
    Exit;
  end;

  try
{$IFDEF FPC}
    // 设置超时
    CnFDZero(Readfds);
    CnFDSet(Sock, Readfds);
    Tv.tv_sec := FTimeout div 1000;
    Tv.tv_usec := (FTimeout mod 1000) * 1000;
{$ENDIF}

    // 填充地址结构
    FillChar(SockAddr, SizeOf(SockAddr), 0);
    SockAddr.sin_family := AF_INET;
    SockAddr.sin_port := htons(Port);
    SockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(FIpAddr)));

    // 发送空数据
    Data := 0;
    CnSendTo(Sock, Data, 1, 0, SockAddr, SizeOf(SockAddr));

{$IFDEF FPC}
    // 等待响应
    Ret := CnSelect(Sock + 1, @Readfds, nil, nil, @Tv);

    if Ret > 0 then
    begin
      // 有响应，端口可能开放
      Result := 1;  // open
    end
    else
    begin
      // 超时，对于 UDP 通常意味着端口开放但无响应
      Result := 1;  // open|filtered
    end;
{$ELSE}
    Result := 1;
{$ENDIF}

  finally
    CnCloseSocket(Sock);
  end;
end;

procedure TCnPortScanner.AddResult(Port: Word; State: Byte; const Protocol, Service: string);
begin
  if FResultCount >= Length(FResults) then
    Exit;

  FResults[FResultCount].Port := Port;
  FResults[FResultCount].State := State;
  FResults[FResultCount].Protocol := Protocol;
  FResults[FResultCount].Service := Service;
  Inc(FResultCount);

  if State = 1 then
    Inc(FOpenCount);

  Inc(FTotalScanned);
end;

procedure TCnPortScanner.OutputResult(const Result: TPortResult);
var
  StateStr: string;
begin
  case Result.State of
    1: StateStr := 'OPEN';
    2: StateStr := 'CLOSED';
    3: StateStr := 'FILTERED';
  else
    StateStr := 'UNKNOWN';
  end;

  if FOptions.Verbose then
  begin
    // 详细模式：显示所有端口
    if Result.Service <> '' then
      WriteLn(FIpAddr:16, '/', Result.Protocol, Result.Port:6, '  ', StateStr:8, '  ', Result.Service)
    else
      WriteLn(FIpAddr:16, '/', Result.Protocol, Result.Port:6, '  ', StateStr:8);
  end
  else
  begin
    // 简洁模式：只显示开放端口
    if Result.State = 1 then
    begin
      if Result.Service <> '' then
        WriteLn(Result.Port:6, '/', Result.Protocol, '  ', StateStr:8, '  ', Result.Service)
      else
        WriteLn(Result.Port:6, '/', Result.Protocol, '  ', StateStr:8);
    end;
  end;
end;

procedure TCnPortScanner.Run;
var
  Port: Word;
  State: Byte;
  Service: string;
  Protocol: string;
begin
  // 解析主机名
  try
    ResolveHost;
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      Exit;
    end;
  end;

  if FOptions.Protocol = cpUDP then
    Protocol := 'udp'
  else
    Protocol := 'tcp';

  if FOptions.Verbose then
  begin
    WriteLn;
    WriteLn('Starting CNT port scanner for ', FHost, ' (', FIpAddr, ')');
    WriteLn('Port range: ', FStartPort, ' - ', FEndPort);
    WriteLn('Protocol: ', UpperCase(Protocol));
    WriteLn('Timeout: ', FTimeout, 'ms');
    WriteLn;
    WriteLn('PORT', '      ', 'STATE', '      ', 'SERVICE');
    WriteLn('----', '      ', '-----', '      ', '-------');
  end
  else
  begin
    WriteLn;
    WriteLn('PORT', '      ', 'STATE', '      ', 'SERVICE');
    WriteLn('----', '      ', '-----', '      ', '-------');
  end;

  // 扫描每个端口
  for Port := FStartPort to FEndPort do
  begin
    Service := GetServiceName(Port);

    if FOptions.Protocol = cpUDP then
      State := ScanPortUdp(Port)
    else
      State := ScanPortTcp(Port);

    AddResult(Port, State, Protocol, Service);
    OutputResult(FResults[FResultCount - 1]);

    // 显示进度
    if FOptions.Verbose and ((FTotalScanned mod 100) = 0) then
    begin
      WriteLn('Scanned: ', FTotalScanned, '/', FEndPort - FStartPort + 1);
    end;

    // 如果设置了间隔，每扫描一个端口后等待
    if FOptions.IdleInterval > 0 then
      Sleep(FOptions.IdleInterval * 1000);
  end;

  // 输出统计信息
  WriteLn;
  WriteLn('Scanned ', FTotalScanned, ' ports.');
  WriteLn('Open ports: ', FOpenCount);
end;

procedure RunScan(const Options: TCntOptions; const Host: string; StartPort, EndPort: Word);
begin
  if Host = '' then
  begin
    WriteLn('Error: No host specified for scan.');
    Exit;
  end;

  if StartPort = 0 then
    StartPort := 1;

  if EndPort = 0 then
    EndPort := 1024;

  if StartPort > EndPort then
  begin
    WriteLn('Error: Invalid port range.');
    Exit;
  end;

  with TCnPortScanner.Create(Options, Host, StartPort, EndPort) do
  try
    Run;
  finally
    Free;
  end;
end;

{ 运行扫描模式 - 支持类似 nc -z host port 的语法 }
procedure RunScanMode(const Options: TCntOptions; const Host, PortStr: string);
var
  StartPort, EndPort: Word;
  DashPos: Integer;
  PortStrCopy: string;
begin
  if Host = '' then
  begin
    WriteLn('Error: No host specified for scan.');
    Exit;
  end;

  if PortStr = '' then
  begin
    WriteLn('Error: No port specified for scan.');
    Exit;
  end;

  PortStrCopy := PortStr;

  // 解析端口范围
  DashPos := Pos('-', PortStrCopy);
  if DashPos > 0 then
  begin
    // 端口范围，如 1-1024
    StartPort := StrToIntDef(Copy(PortStrCopy, 1, DashPos - 1), 1);
    EndPort := StrToIntDef(Copy(PortStrCopy, DashPos + 1, Length(PortStrCopy)), 1024);
  end
  else
  begin
    // 单个端口
    StartPort := StrToIntDef(PortStrCopy, 0);
    EndPort := StartPort;
  end;

  RunScan(Options, Host, StartPort, EndPort);
end;

end.
