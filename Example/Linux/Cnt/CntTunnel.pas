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

unit CntTunnel;
{
  CNT - CnPack NetTool
  隧道/执行模式单元

  实现功能：
  - TCP 服务端监听端口
  - 客户端连接后 fork 子进程执行指定命令
  - 通过管道将命令的 stdin/stdout/stderr 与 socket 双向连接
  - 支持 -e (exec) 和 -c (shell-exec) 模式

  跨平台支持：Delphi 5+, FPC 3+
}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows, WinSock,
{$ELSE}
  BaseUnix, Unix, Sockets,
{$ENDIF}
  CntCmdLine, CntUtils, CntConsts,
  CnSocket;

procedure RunTunnelServer(const Options: TCntOptions; Port: Word);

implementation

{$IFDEF FPC}
{$IFDEF DARWIN}
function inet_ntoa(inaddr: in_addr): PAnsiChar; cdecl; external 'libc' name 'inet_ntoa';
{$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
procedure HandleTunnelConnection(ClientSock: TSocket; const Options: TCntOptions);
var
  PID: TPid;
  Cmd: string;
  Args: array of PAnsiChar;
  I, ArgCount: Integer;
  ArgList: TStringList;
begin
  PID := fpFork;
  if PID < 0 then
  begin
    WriteLn('Error: fork failed');
    Exit;
  end;

  if PID = 0 then
  begin
    // Child process: redirect socket to stdin/stdout/stderr
    fpDup2(ClientSock, 0);  // stdin
    fpDup2(ClientSock, 1);  // stdout
    fpDup2(ClientSock, 2);  // stderr

    // Close all other FDs (basic approach: close up to 256)
    for I := 3 to 255 do
      fpClose(I);

    if Options.ShellExecCmd <> '' then
    begin
      // Shell exec mode (-c): run via /bin/sh -c
      Cmd := Options.ShellExecCmd;
      fpExecL('/bin/sh', ['/bin/sh', '-c', PAnsiChar(Cmd)]);
    end
    else if Options.ExecCmd <> '' then
    begin
      // Direct exec mode (-e): parse command and arguments
      ArgList := TStringList.Create;
      try
        // Simple argument splitting by spaces
        ArgList.Delimiter := ' ';
        ArgList.DelimitedText := Options.ExecCmd;
        SetLength(Args, ArgList.Count + 1);
        for I := 0 to ArgList.Count - 1 do
          Args[I] := PAnsiChar(AnsiString(ArgList[I]));
        Args[ArgList.Count] := nil;

        fpExecVP(Args[0], PPAnsiChar(Args));
      finally
        ArgList.Free;
      end;
    end;

    // If exec fails, exit
    fpExit(1);
  end
  else
  begin
    // Parent process
    fpClose(ClientSock);
    if Options.Verbose then
      WriteLn('Forked child PID ', PID, ' for tunnel');
    // Wait for child to finish (non-blocking)
    while fpWaitPid(PID, nil, WNOHANG) = 0 do
    begin
      if not GCntRunning then
      begin
        fpKill(PID, SIGTERM);
        Break;
      end;
      Sleep(100);
    end;
  end;
end;

procedure RunTunnelServerUnix(const Options: TCntOptions; Port: Word);
var
  ServerSock: TSocket;
  ClientSock: TSocket;
  ServerAddr: TSockAddr;
  ClientAddr: TSockAddr;
  AddrLen: Integer;
  Data: Integer;
  RemoteIP: string;
  RemotePort: Word;
begin
  ServerSock := CnNewSocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if ServerSock = INVALID_SOCKET then
  begin
    WriteLn('Error: Failed to create server socket');
    Exit;
  end;

  Data := 1;
  CnSetSockOpt(ServerSock, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@Data), SizeOf(Data));

  FillChar(ServerAddr, SizeOf(ServerAddr), 0);
  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_port := htons(Port);
  ServerAddr.sin_addr.s_addr := INADDR_ANY;

  if CnBind(ServerSock, ServerAddr, SizeOf(ServerAddr)) < 0 then
  begin
    CnCloseSocket(ServerSock);
    WriteLn('Error: Failed to bind to port ', Port);
    Exit;
  end;

  if CnListen(ServerSock, 5) < 0 then
  begin
    CnCloseSocket(ServerSock);
    WriteLn('Error: Failed to listen on port ', Port);
    Exit;
  end;

  if Options.Verbose then
    WriteLn('Tunnel server listening on port ', Port, '...');

  while GCntRunning do
  begin
    AddrLen := SizeOf(ClientAddr);
    FillChar(ClientAddr, SizeOf(ClientAddr), 0);
    ClientSock := CnAccept(ServerSock, @ClientAddr, @AddrLen);

    if ClientSock = INVALID_SOCKET then
    begin
      Sleep(100);
      Continue;
    end;

    RemoteIP := string(inet_ntoa(ClientAddr.sin_addr));
    RemotePort := ntohs(ClientAddr.sin_port);

    if Options.Verbose then
      WriteLn('Tunnel connection from ', RemoteIP, ':', RemotePort);

    HandleTunnelConnection(ClientSock, Options);

    if not Options.KeepListen then
      Break;
  end;

  CnCloseSocket(ServerSock);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure RunTunnelServerWindows(const Options: TCntOptions; Port: Word);
begin
  WriteLn('Tunnel server not yet implemented for Windows.');
end;
{$ENDIF}

procedure RunTunnelServer(const Options: TCntOptions; Port: Word);
begin
  if Port = 0 then
  begin
    WriteLn('Error: Invalid port number.');
    Exit;
  end;

  if Options.Verbose then
  begin
    WriteLn('CNT Tunnel Server on port ', Port);
    if Options.ExecCmd <> '' then
      WriteLn('Exec: ', Options.ExecCmd);
    if Options.ShellExecCmd <> '' then
      WriteLn('Shell Exec: ', Options.ShellExecCmd);
  end;

{$IFDEF UNIX}
  RunTunnelServerUnix(Options, Port);
{$ELSE}
  RunTunnelServerWindows(Options, Port);
{$ENDIF}
end;

end.
