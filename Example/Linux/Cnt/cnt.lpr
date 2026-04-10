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

program cnt;
{
  CNT - CnPack NetTool
  主程序入口 (Lazarus/FPC)

  类似于 NetCat (nc) 的命令行工具

  跨平台支持：
    编译器：FPC (Free Pascal) 3+
    操作系统：Windows / Linux / macOS / Unix
}

{$mode objfpc}{$H+}

uses
{$IFDEF MSWINDOWS}
  Windows,
  WinSock,
{$ENDIF}
  SysUtils,
  CntCmdLine,
  CntConsts,
  CntUtils,
  CntClient,
  CntServer,
  CntTunnel,
  CntScan;

procedure Main;
var
  Options: TCntOptions;
  Host, PortStr: string;
  PortNum: Word;
  Code: Integer;
begin
  ParseCommandLine(Options, Host, PortStr);

  // If no arguments provided, show usage
  if (ParamCount = 0) and (not Options.Help) then
  begin
    PrintUsage;
    Exit;
  end;

  if Options.Help then
  begin
    PrintUsage;
    Exit;
  end;

  if Options.Verbose then
  begin
    WriteLn('CNT - CnPack NetTool v', SCnCntVersion);
    WriteLn;
  end;

  // Determine mode
  if Options.Mode = cmListen then
  begin
    PortNum := Options.SourcePort;
    if PortStr <> '' then
    begin
      Val(PortStr, PortNum, Code);
      if Code <> 0 then
      begin
        WriteLn(ErrOutput, 'Error: Invalid port number "', PortStr, '".');
        Exit;
      end;
    end;

    if PortNum = 0 then
    begin
      WriteLn(ErrOutput, 'Error: Invalid port number.');
      Exit;
    end;

    // If exec/shell-exec mode, use tunnel server
    if (Options.ExecCmd <> '') or (Options.ShellExecCmd <> '') then
      RunTunnelServer(Options, PortNum)
    else
      RunServer(Options, PortNum);
  end
  else if Options.ZeroMode then
  begin
    // Port scan mode (-z)
    RunScanMode(Options, Host, PortStr);
  end
  else
  begin
    // Client mode
    RunClient(Options, Host, PortStr);
  end;
end;

{$IFDEF MSWINDOWS}
var
  WSA: TWSAData;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if WSAStartup($101, WSA) <> 0 then
  begin
    WriteLn(ErrOutput, 'WSAStartup failed.');
    Halt(1);
  end;
{$ENDIF}

  try
    Main;
  finally
{$IFDEF MSWINDOWS}
    WSACleanup;
{$ENDIF}
  end;
end.
