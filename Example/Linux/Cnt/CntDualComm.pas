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

unit CntDualComm;
{
  CNT - CnPack NetTool
  跨平台双向通信单元（行缓冲模式）

  输入方式：用户输入字符本地回显，按回车后才发送整行到对方
  接收方式：收到数据立即显示

  - Windows: PeekConsoleInput + 行缓冲 + 短超时 select
  - Unix/FPC: select 同时监听 stdin 和 socket（终端默认行缓冲）
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
  Posix.SysTime,
{$ENDIF}
  CnSocket,
  CntCmdLine, CntUtils, CntConsts;

type
  // 数据接收回调：收到远程数据时调用，用于显示/记录
  TOnDataReceived = procedure(const Data: Pointer; Len: Integer) of object;

  // 运行状态回调：检查是否应该继续运行
  TRunningCheck = function: Boolean of object;

// 执行双向通信循环（阻塞直到断开或 Running 返回 False）
// 输入：行缓冲模式（回车后发送整行）
// 输出：立即显示
procedure CnDualCommRun(
  Sock: TSocket;
  const Options: TCntOptions;
  var TotalSent: Int64;
  var TotalReceived: Int64;
  RunningCheck: TRunningCheck;
  OnRecv: TOnDataReceived
);

implementation

const
  MAX_LINE_BUF = 4096;

// 前向声明
function WaitForSocketReady(Sock: TSocket; TimeOutMs: Integer): Boolean; forward;

// 平台特定的实现函数
{$IFDEF MSWINDOWS}
procedure DoWinDualComm(
  Sock: TSocket;
  const Options: TCntOptions;
  var TotalSent: Int64;
  var TotalReceived: Int64;
  RunningCheck: TRunningCheck;
  OnRecv: TOnDataReceived
); forward;
{$ELSE}{$IFDEF UNIX}
procedure DoUnixDualComm(
  Sock: TSocket;
  const Options: TCntOptions;
  var TotalSent: Int64;
  var TotalReceived: Int64;
  RunningCheck: TRunningCheck;
  OnRecv: TOnDataReceived
); forward;
{$ENDIF}{$ENDIF}

procedure CnDualCommRun(
  Sock: TSocket;
  const Options: TCntOptions;
  var TotalSent: Int64;
  var TotalReceived: Int64;
  RunningCheck: TRunningCheck;
  OnRecv: TOnDataReceived
);
begin
{$IFDEF MSWINDOWS}
  DoWinDualComm(Sock, Options, TotalSent, TotalReceived, RunningCheck, OnRecv);
{$ELSE}
{$IFDEF UNIX}
  DoUnixDualComm(Sock, Options, TotalSent, TotalReceived, RunningCheck, OnRecv);
{$ENDIF}
{$ENDIF}
end;

{==================== Windows 双向通信（行缓冲）====================}
{
  Windows 下：
  - 每个按键本地回显并缓存
  - 回车键触发发送整行（含 CR+LF）
  - 支持退格删除
}

{$IFDEF MSWINDOWS}

procedure DoWinDualComm(
  Sock: TSocket;
  const Options: TCntOptions;
  var TotalSent: Int64;
  var TotalReceived: Int64;
  RunningCheck: TRunningCheck;
  OnRecv: TOnDataReceived
);
var
  RecvBuf: array[0..4095] of Byte;
  LineBuf: array[0..MAX_LINE_BUF - 1] of Byte;
  LinePos: Integer;      // 当前行缓冲区位置
  Len: Integer;
  cIn: TInputRecord;
  nRead: DWORD;
  hCon: THandle;

  procedure SendLine;
  begin
    if LinePos > 0 then
    begin
      // 发送整行 + CRLF
      LineBuf[LinePos] := 13;   // CR
      LineBuf[LinePos + 1] := 10; // LF
      CnSend(Sock, LineBuf, LinePos + 2, 0);
      Inc(TotalSent, LinePos + 2);
      LinePos := 0;
    end;
  end;

begin
  hCon := GetStdHandle(STD_INPUT_HANDLE);
  LinePos := 0;

  while RunningCheck do
  begin
    // 1. 检查 socket 是否可读（非阻塞）
    if WaitForSocketReady(Sock, 0) then
    begin
      Len := CnRecv(Sock, RecvBuf, SizeOf(RecvBuf), 0);
      if Len > 0 then
      begin
        if Assigned(OnRecv) then
          OnRecv(@RecvBuf, Len);
        Inc(TotalReceived, Len);
      end
      else if Len <= 0 then
      begin
        if Options.Verbose then
          WriteLn('Connection closed');
        Break;
      end;
    end;

    // 2. 检查控制台是否有键盘输入
    if PeekConsoleInput(hCon, cIn, 1, nRead) and (nRead > 0) then
    begin
      ReadConsoleInput(hCon, cIn, 1, nRead);
      if (cIn.EventType = KEY_EVENT) and cIn.Event.KeyEvent.bKeyDown then
      begin
        case cIn.Event.KeyEvent.wVirtualKeyCode of
          VK_RETURN:
            begin
              // 回车 -> 发送整行并换行
              SendLine;
              WriteLn;
            end;
          VK_BACK:
            begin
              // 退格 -> 删除最后一个字符
              if LinePos > 0 then
              begin
                Dec(LinePos);
                Write(#8' '#8);  // 光标左移、擦除、再左移
                Flush(Output);
              end;
            end;
          else
            begin
              // 普通可打印字符 -> 缓存并回显
              if (cIn.Event.KeyEvent.AsciiChar <> #0)
                 and (LinePos < MAX_LINE_BUF - 2) then
              begin
                LineBuf[LinePos] := Byte(cIn.Event.KeyEvent.AsciiChar);
                Inc(LinePos);
                // 本地回显
                Write(AnsiChar(cIn.Event.KeyEvent.AsciiChar));
                Flush(Output);
              end;
            end;
        end;
      end;
    end
    else
      Sleep(1);  // 避免 CPU 空转
  end;
end;

{$ENDIF}  // MSWINDOWS

{==================== Unix/FPC 双向通信（行缓冲）====================}
{
  Unix 终端默认就是行缓冲模式（cooked mode），
  FileRead(StdInFd) 只在用户按回车后返回整行。
  无需额外处理。
}

{$IFNDEF MSWINDOWS}{$IFDEF UNIX}

procedure DoUnixDualComm(
  Sock: TSocket;
  const Options: TCntOptions;
  var TotalSent: Int64;
  var TotalReceived: Int64;
  RunningCheck: TRunningCheck;
  OnRecv: TOnDataReceived
);
var
  Buffer: array[0..8191] of Byte;
  Len: Integer;
  Readfds: TCnFDSet;
  Tv: TTimeVal;
  SelResult: Longint;
  MaxFd: Integer;
  StdInFd: Integer;
begin
  StdInFd := StdInputHandle;

  while RunningCheck do
  begin
    CnFDZero(Readfds);
    CnFDSet(Sock, Readfds);
    CnFDSet(StdInFd, Readfds);

    MaxFd := Sock;
    if StdInFd > MaxFd then
      MaxFd := StdInFd;

    Tv.tv_sec := 1;
    Tv.tv_usec := 0;

    SelResult := CnSelect(MaxFd + 1, @Readfds, nil, nil, @Tv);

    if SelResult <= 0 then
      Continue;

    // stdin 可读 -> 读取并发送（终端已自动行缓冲，回车后才到这里）
    if CnFDIsSet(StdInFd, Readfds) then
    begin
      Len := FileRead(StdInFd, Buffer, SizeOf(Buffer));
      if Len > 0 then
      begin
        CnSend(Sock, Buffer, Len, 0);
        Inc(TotalSent, Len);
      end
      else if Len <= 0 then
      begin
        // EOF on stdin
        if Options.Verbose then
          WriteLn(ErrOutput, 'EOF on stdin');
        Break;
      end;
    end;

    // socket 可读 -> 接收并回调
    if CnFDIsSet(Sock, Readfds) then
    begin
      Len := CnRecv(Sock, Buffer, SizeOf(Buffer), 0);
      if Len > 0 then
      begin
        if Assigned(OnRecv) then
          OnRecv(@Buffer, Len);
        Inc(TotalReceived, Len);
      end
      else if Len <= 0 then
      begin
        if Options.Verbose then
          WriteLn(ErrOutput, 'Connection closed');
        Break;
      end;
    end;
  end;
end;

{$ENDIF}{$ENDIF}  // UNIX / not MSWINDOWS

{==================== 辅助函数 ====================}

function WaitForSocketReady(Sock: TSocket; TimeOutMs: Integer): Boolean;
var
  Readfds: TCnFDSet;
{$IFDEF POSIX}
  Tv: timeval;
{$ELSE}
  Tv: TTimeVal;
{$ENDIF}
begin
  if Sock = INVALID_SOCKET then
  begin
    Result := False;
    Exit;
  end;

  CnFDZero(Readfds);
  CnFDSet(Sock, Readfds);
  Tv.tv_sec := TimeOutMs div 1000;
  Tv.tv_usec := (TimeOutMs mod 1000) * 1000;

  Result := CnSelect(Sock + 1, @Readfds, nil, nil, @Tv) > 0;
end;

end.
