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

unit CntCmdLine;
{
  CNT - CnPack NetTool
  命令行参数解析单元

  类似于 NetCat (nc) 的网络命令行工具
}

interface

{$I CnPack.inc}

uses
  SysUtils;

type
  TCntMode = (cmUnknown, cmConnect, cmListen, cmScan);
  TCntProtocol = (cpTCP, cpUDP);
  TCntOutputMode = (omNormal, omHex, omQuiet);

  TCntOptions = record
    Mode: TCntMode;
    Protocol: TCntProtocol;
    OutputMode: TCntOutputMode;
    Host: string;
    Port: Word;
    SourcePort: Word;
    SourceAddr: string;
    ListenAddr: string;
    WaitTimeout: Integer;       // -w seconds
    IdleInterval: Integer;      // -i seconds
    ExecCmd: string;            // -e cmd
    ShellExecCmd: string;       // -c cmd (shell exec)
    HexDump: Boolean;           // -x
    ZeroMode: Boolean;          // -z (zero byte, for scan)
    KeepAlive: Boolean;         // -K
    Broadcast: Boolean;         // -b
    Verbose: Boolean;           // -v
    Numeric: Boolean;           // -n (no DNS resolve)
    Debug: Boolean;             // -d
    OutputFile: string;         // -o filename
    QuietDelay: Integer;        // -q seconds (quit delay)
    Ipv6: Boolean;             // -6
    KeepListen: Boolean;        // -k (keep listening)
    Help: Boolean;              // -h
  end;

procedure ParseCommandLine(out AOptions: TCntOptions; out Host: string; out Port: string);
procedure PrintUsage;

implementation

procedure PrintUsage;
begin
  WriteLn('CNT - CnPack NetTool v1.0');
  WriteLn('Usage: cnt [OPTIONS] [HOST] [PORT]');
  WriteLn;
  WriteLn('Connect mode:');
  WriteLn('  cnt HOST PORT         Connect to HOST:PORT');
  WriteLn('  cnt -l PORT           Listen on local port PORT');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -h, --help            Show this help message');
  WriteLn('  -v, --verbose         Verbose output');
  WriteLn('  -n, --numeric         Numeric output only, no DNS resolution');
  WriteLn('  -l, --listen          Listen mode (server)');
  WriteLn('  -u, --udp             Use UDP instead of TCP');
  WriteLn('  -p, --port PORT       Source/local port');
  WriteLn('  -s, --source ADDR     Bind to source address');
  WriteLn('  -e, --exec CMD        Execute command after connect');
  WriteLn('  -c, --sh-exec CMD     Execute command via shell');
  WriteLn('  -w, --wait SECS       Wait timeout for connection and data');
  WriteLn('  -i, --interval SECS   Wait interval between ports');
  WriteLn('  -x, --hex             Hex dump of sent/received data');
  WriteLn('  -z, --zero            Zero mode (for port scanning)');
  WriteLn('  -K, --keep-alive      Enable SO_KEEPALIVE');
  WriteLn('  -k, --keep-open       Keep listen mode (don''t exit after disconnect)');
  WriteLn('  -b, --broadcast       Allow broadcast');
  WriteLn('  -o, --output FILE     Output to file');
  WriteLn('  -q, --quiet SECS      Quit after EOF, delay seconds');
  WriteLn('  -6, --ipv6            IPv6 mode');
  WriteLn('  -d, --debug           Debug mode');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  cnt -l 8080                    Listen on port 8080');
  WriteLn('  cnt -l -u 53                   Listen on UDP port 53');
  WriteLn('  cnt example.com 80              Connect to example.com:80');
  WriteLn('  cnt -l -e /bin/bash 1234        Bind /bin/bash to port 1234');
  WriteLn('  cnt -l -k                      Listen with keep-alive');
  WriteLn('  cnt -x HOST 80                 Connect with hex dump');
  WriteLn('  cnt -z HOST 80                 Scan single port');
  WriteLn('  cnt -z HOST 1-1024             Scan port range');
  WriteLn('  cnt -zu HOST 53                UDP scan');
  WriteLn('  cnt -zv HOST 1-1024            Verbose scan');
end;

procedure ParseCommandLine(out AOptions: TCntOptions; out Host: string; out Port: string);
var
  I: Integer;
  S: string;
  Code: Integer;
begin
  FillChar(AOptions, SizeOf(AOptions), 0);
  AOptions.Mode := cmConnect;
  AOptions.Protocol := cpTCP;
  AOptions.WaitTimeout := 0;
  AOptions.IdleInterval := 0;
  AOptions.Numeric := False;
  AOptions.Verbose := False;
  AOptions.Debug := False;
  AOptions.HexDump := False;
  AOptions.KeepAlive := False;
  AOptions.Broadcast := False;
  AOptions.ZeroMode := False;
  AOptions.QuietDelay := 0;
  AOptions.Ipv6 := False;
  AOptions.ListenAddr := '0.0.0.0';

  Host := '';
  Port := '';

  I := 1;
  while I <= ParamCount do
  begin
    S := ParamStr(I);

    if (S = '-h') or (S = '--help') then
    begin
      AOptions.Help := True;
      Inc(I);
    end
    else if (S = '-v') or (S = '--verbose') then
    begin
      AOptions.Verbose := True;
      Inc(I);
    end
    else if (S = '-n') or (S = '--numeric') then
    begin
      AOptions.Numeric := True;
      Inc(I);
    end
    else if (S = '-d') or (S = '--debug') then
    begin
      AOptions.Debug := True;
      Inc(I);
    end
    else if (S = '-l') or (S = '--listen') then
    begin
      AOptions.Mode := cmListen;
      Inc(I);
    end
    else if (S = '-u') or (S = '--udp') then
    begin
      AOptions.Protocol := cpUDP;
      Inc(I);
    end
    else if (S = '-6') or (S = '--ipv6') then
    begin
      AOptions.Ipv6 := True;
      Inc(I);
    end
    else if (S = '-x') or (S = '--hex') then
    begin
      AOptions.HexDump := True;
      Inc(I);
    end
    else if (S = '-K') or (S = '--keep-alive') then
    begin
      AOptions.KeepAlive := True;
      Inc(I);
    end
    else if (S = '-k') or (S = '--keep-open') then
    begin
      AOptions.KeepListen := True;
      Inc(I);
    end
    else if (S = '-b') or (S = '--broadcast') then
    begin
      AOptions.Broadcast := True;
      Inc(I);
    end
    else if (S = '-z') or (S = '--zero') then
    begin
      AOptions.ZeroMode := True;
      Inc(I);
    end
    else if (S = '-zv') then
    begin
      AOptions.ZeroMode := True;
      AOptions.Verbose := True;
      Inc(I);
    end
    else if (S = '-zu') then
    begin
      AOptions.ZeroMode := True;
      AOptions.Protocol := cpUDP;
      Inc(I);
    end
    else if (S = '-zuv') or (S = '-zvu') then
    begin
      AOptions.ZeroMode := True;
      AOptions.Protocol := cpUDP;
      AOptions.Verbose := True;
      Inc(I);
    end
    else if (S = '-p') or (S = '--port') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        Val(ParamStr(I), AOptions.SourcePort, Code);
        Inc(I);
      end;
    end
    else if (S = '-s') or (S = '--source') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        AOptions.SourceAddr := ParamStr(I);
        Inc(I);
      end;
    end
    else if (S = '-w') or (S = '--wait') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        Val(ParamStr(I), AOptions.WaitTimeout, Code);
        Inc(I);
      end;
    end
    else if (S = '-i') or (S = '--interval') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        Val(ParamStr(I), AOptions.IdleInterval, Code);
        Inc(I);
      end;
    end
    else if (S = '-e') or (S = '--exec') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        AOptions.ExecCmd := ParamStr(I);
        Inc(I);
      end;
    end
    else if (S = '-c') or (S = '--sh-exec') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        AOptions.ShellExecCmd := ParamStr(I);
        Inc(I);
      end;
    end
    else if (S = '-o') or (S = '--output') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        AOptions.OutputFile := ParamStr(I);
        Inc(I);
      end;
    end
    else if (S = '-q') or (S = '--quiet') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        Val(ParamStr(I), AOptions.QuietDelay, Code);
        Inc(I);
      end;
    end
    else if (S = '--') then
    begin
      Inc(I);
      Break;
    end
    else if (S[1] = '-') then
    begin
      WriteLn('Unknown option: ' + S);
      AOptions.Help := True;
      Inc(I);
    end
    else
    begin
      Break;
    end;
  end;

  // Remaining arguments are host and port
  if I <= ParamCount then
  begin
    Host := ParamStr(I);
    Inc(I);
  end;

  if I <= ParamCount then
    Port := ParamStr(I);

  // If in listen mode and no host given, listen on all interfaces
  if AOptions.Mode = cmListen then
  begin
    AOptions.ListenAddr := '0.0.0.0';
    // If first arg is port (numeric), that's the listen port
    if Host <> '' then
    begin
      Val(Host, AOptions.SourcePort, Code);
      Host := '';
    end;
  end;
end;

end.