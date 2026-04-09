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
{$ENDIF}
  CntCmdLine, CntUtils, CntConsts,
  CnSocket;

procedure RunTunnelServer(const Options: TCntOptions; Port: Word);

implementation

procedure RunTunnelServer(const Options: TCntOptions; Port: Word);
begin
  if Port = 0 then
  begin
    WriteLn('Error: Invalid port number.');
    Exit;
  end;

  if Options.Verbose then
  begin
    WriteLn('CNT Tunnel Server listening on port ', Port);
    if Options.ExecCmd <> '' then
      WriteLn('Exec: ', Options.ExecCmd);
    if Options.ShellExecCmd <> '' then
      WriteLn('Shell Exec: ', Options.ShellExecCmd);
    if Options.HexDump then
      WriteLn('Hex dump: enabled');
    if Options.KeepAlive then
      WriteLn('Keep-alive: enabled');
    if Options.KeepListen then
      WriteLn('Keep listen: enabled');
  end;

  WriteLn('Tunnel server mode not yet implemented for cross-platform.');
end;

end.
