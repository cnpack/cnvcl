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

unit CnSocket;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯 Socket 公共声明与跨平台函数封装单元
* 单元作者：CnPack 开发组
* 备    注：需要支持 Delphi 及 FPC 编译器以及 Windows、Mac、Linux 平台
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.12.06 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows, WinSock {$ELSE}
  {$IFDEF FPC}, Sockets, BaseUnix {$ELSE}, System.Net.Socket,
  Posix.Base, Posix.NetIf, Posix.SysSocket, Posix.ArpaInet, Posix.NetinetIn,
  Posix.Unistd, Posix.SysSelect, Posix.SysTime, Posix.Errno {$ENDIF}{$ENDIF};

type
{$IFDEF MSWINDOWS}
  TCnFDSet = TFDSet;
  PCnFDSet = PFDSet;
  TSocket = WinSock.TSocket;
  TSockAddr = WinSock.sockaddr_in;
{$ELSE}
{$IFDEF FPC}
  TCnFDSet = TFDSet;
  PCnFDSet = PFDSet;
  TSocket = LongInt;
  TSockAddr = TInetSockAddr;
{$ELSE}
  TCnFDSet = fd_set;
  PCNFDSet = Pfd_set;
  TSocket = Integer;
  TSockAddr = sockaddr_in;
{$ENDIF}
{$ENDIF}

const
  SD_BOTH = 2;
{$IFNDEF MSWINDOWS}
  SOCKET_ERROR   = -1;
  INVALID_SOCKET = -1;
  SO_DONTLINGER  = $FF7F;

{$IFNDEF FPC}

function getifaddrs(var Ifap: pifaddrs): Integer; cdecl; external libc name _PU + 'getifaddrs';

procedure freeifaddrs(Ifap: pifaddrs); cdecl; external libc name _PU + 'freeifaddrs';

{$ENDIF}

{$ENDIF}

function CnGetHostName: string;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 gethostname 函数的封装，返回本机名}

function CnNewSocket(Af, Struct, Protocol: Integer): TSocket;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 socket 函数的封装}

function CnConnect(S: TSocket; var Name: TSockAddr; NameLen: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 connect 函数的封装}

function CnBind(S: TSocket; var Addr: TSockAddr; NameLen: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 bind 函数的封装}

function CnGetSockName(S: TSocket; var Name: TSockAddr; var NameLen: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 getsockname 函数的封装}

function CnListen(S: TSocket; Backlog: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 listen 函数的封装}

function CnAccept(S: TSocket; Addr: PSockAddr; AddrLen: PInteger): TSocket;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 accept 函数的封装}

function CnSend(S: TSocket; var Buf; Len, Flags: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 send 函数的封装}

function CnRecv(S: TSocket; var Buf; Len, Flags: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 recv 函数的封装}

function CnSendTo(S: TSocket; var Buf; Len, Flags: Integer;
  var AddrTo: TSockAddr; ToLen: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 sendto 函数的封装}

function CnRecvFrom(S: TSocket; var Buf; Len, Flags: Integer;
  var AddrFrom: TSockAddr; var FromLen: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 recvfrom 函数的封装}

function CnSelect(Nfds: Integer; Readfds, Writefds, Exceptfds: PCnFDSet;
  Timeout: PTimeVal): Longint;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 select 函数的封装}

function CnSetSockOpt(S: TSocket; Level, OptName: Integer; OptVal: PAnsiChar;
  OptLen: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 setsockopt 函数的封装}

function CnShutdown(S: TSocket; How: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 shutdown 函数的封装}

function CnCloseSocket(S: TSocket): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 closesocket 函数的封装}

procedure CnFDZero(var FD: TCnFDSet);
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 FD_ZERO 函数的封装}

procedure CnFDSet(F: Integer; var FD: TCnFDSet);
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 FD_SET 函数的封装}

procedure CnFDClear(F: Integer; var FD: TCnFDSet);
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 FD_CLR 函数的封装}

function CnFDIsSet(F: Integer; var FD: TCnFDSet): Boolean;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 FD_ISSET 函数的封装}

function CnGetNetErrorNo: Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台返回最近一次网络错误码}

implementation

{$IFNDEF MSWINDOWS}
{$IFDEF FPC}
function c_gethostname(name: PAnsiChar; len: LongInt): LongInt; cdecl; external 'libc' name 'gethostname';
{$ENDIF}
{$ENDIF}

function CnGetHostName: string;
var
  S: array[0..256] of AnsiChar;
begin
{$IFDEF MSWINDOWS}
  WinSock.gethostname(@S[0], SizeOf(S));
{$ELSE}
{$IFDEF FPC}
  FillChar(S[0], SizeOf(S), 0);
  c_gethostname(@S[0], SizeOf(S));
{$ELSE}
  Posix.Unistd.gethostname(@S[0], SizeOf(S));
{$ENDIF}
{$ENDIF}
  Result := string(S);
end;

function CnNewSocket(Af, Struct, Protocol: Integer): TSocket;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.socket(Af, Struct, Protocol);
{$ELSE}
{$IFDEF FPC}
  Result := fpSocket(Af, Struct, Protocol);
{$ELSE}
  Result := Posix.SysSocket.socket(Af, Struct, Protocol);
{$ENDIF}
{$ENDIF}
end;

function CnConnect(S: TSocket; var Name: TSockAddr; NameLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.connect(S, Name, NameLen);
{$ELSE}
{$IFDEF FPC}
  Result := fpConnect(S, @Name, SizeOf(Name));
{$ELSE}
  Result := Posix.SysSocket.connect(S, sockaddr(Name), NameLen);
{$ENDIF}
{$ENDIF}
end;

function CnBind(S: TSocket; var Addr: TSockAddr; NameLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.bind(S, Addr, NameLen);
{$ELSE}
{$IFDEF FPC}
  Result := fpBind(S, @Addr, SizeOf(Addr));
{$ELSE}
  Result := Posix.SysSocket.bind(S, sockaddr(Addr), NameLen);
{$ENDIF}
{$ENDIF}
end;

function CnGetSockName(S: TSocket; var Name: TSockAddr; var NameLen: Integer): Integer;
{$IFNDEF MSWINDOWS}
{$IFDEF FPC}
var
  SockLen: TSockLen;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.getsockname(S, Name, NameLen);
{$ELSE}
{$IFDEF FPC}
  SockLen := NameLen;
  Result := fpGetSockName(S, @Name, @SockLen);
  NameLen := SockLen;
{$ELSE}
  Result := Posix.SysSocket.getsockname(S, sockaddr(Name), Cardinal(NameLen));
{$ENDIF}
{$ENDIF}
end;

function CnListen(S: TSocket; Backlog: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.listen(S, Backlog);
{$ELSE}
{$IFDEF FPC}
  Result := fpListen(S, Backlog);
{$ELSE}
  Result := Posix.SysSocket.listen(S, Backlog);
{$ENDIF}
{$ENDIF}
end;

function CnAccept(S: TSocket; Addr: PSockAddr; AddrLen: PInteger): TSocket;
{$IFNDEF MSWINDOWS}
{$IFDEF FPC}
var
  SockLen: TSockLen;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.accept(S, Addr, AddrLen);
{$ELSE}
{$IFDEF FPC}
  SockLen := AddrLen^;
  Result := fpAccept(S, Addr, @SockLen);
  AddrLen^ := SockLen;
{$ELSE}
  Result := Posix.SysSocket.accept(S, Addr^, Cardinal(AddrLen^));
{$ENDIF}
{$ENDIF}
end;

function CnSend(S: TSocket; var Buf; Len, Flags: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.send(S, Buf, Len, Flags);
{$ELSE}
{$IFDEF FPC}
  Result := fpSend(S, @Buf, Len, Flags);
{$ELSE}
  Result := Posix.SysSocket.send(S, Buf, Len, Flags);
{$ENDIF}
{$ENDIF}
end;

function CnRecv(S: TSocket; var Buf; Len, Flags: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.recv(S, Buf, Len, Flags);
{$ELSE}
{$IFDEF FPC}
  Result := fpRecv(S, @Buf, Len, Flags);
{$ELSE}
  Result := Posix.SysSocket.recv(S, Buf, Len, Flags);
{$ENDIF}
{$ENDIF}
end;

function CnSendTo(S: TSocket; var Buf; Len, Flags: Integer;
  var AddrTo: TSockAddr; ToLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.sendto(S, Buf, Len, Flags, AddrTo, ToLen);
{$ELSE}
{$IFDEF FPC}
  Result := fpSendTo(S, @Buf, Len, Flags, @AddrTo, ToLen);
{$ELSE}
  Result := Posix.SysSocket.sendto(S, Buf, Len, Flags, sockaddr(AddrTo), ToLen);
{$ENDIF}
{$ENDIF}
end;

function CnRecvFrom(S: TSocket; var Buf; Len, Flags: Integer;
  var AddrFrom: TSockAddr; var FromLen: Integer): Integer;
{$IFNDEF MSWINDOWS}
{$IFDEF FPC}
var
  SockLen: TSockLen;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.recvfrom(S, Buf, Len, Flags, AddrFrom, FromLen);
{$ELSE}
{$IFDEF FPC}
  SockLen := FromLen;
  Result := fpRecvFrom(S, @Buf, Len, Flags, @AddrFrom, @SockLen);
  FromLen := SockLen;
{$ELSE}
  Result := Posix.SysSocket.recvfrom(S, Buf, Len, Flags, sockaddr(AddrFrom), Cardinal(FromLen));
{$ENDIF}
{$ENDIF}
end;

function CnSelect(Nfds: Integer; Readfds, Writefds, Exceptfds: PCnFDSet;
  Timeout: PTimeVal): Longint;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.select(Nfds, Readfds, Writefds, Exceptfds, Timeout);
{$ELSE}
{$IFDEF FPC}
  Result := fpSelect(Nfds, Readfds, Writefds, Exceptfds, Timeout);
{$ELSE}
  Result := Posix.SysSelect.select(Nfds, Readfds, Writefds, Exceptfds, Timeout);
{$ENDIF}
{$ENDIF}
end;

function CnSetSockOpt(S: TSocket; Level, OptName: Integer; OptVal: PAnsiChar;
  OptLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.setsockopt(S, Level, OptName, OptVal, OptLen);
{$ELSE}
{$IFDEF FPC}
  Result := fpSetSockOpt(S, Level, OptName, OptVal, OptLen);
{$ELSE}
  Result := Posix.SysSocket.setsockopt(S, Level, OptName, OptVal, OptLen);
{$ENDIF}
{$ENDIF}
end;

function CnShutdown(S: TSocket; How: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.shutdown(S, How);
{$ELSE}
{$IFDEF FPC}
  Result := fpShutdown(S, How);
{$ELSE}
  Result := Posix.SysSocket.shutdown(S, How);
{$ENDIF}
{$ENDIF}
end;

function CnCloseSocket(S: TSocket): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.closesocket(S);
{$ELSE}
{$IFDEF FPC}
  Result := fpClose(S);
{$ELSE}
  Result := Posix.Unistd.__close(S);
{$ENDIF}
{$ENDIF}
end;

procedure CnFDZero(var FD: TCnFDSet);
begin
{$IFDEF FPC}
  fpFD_ZERO(FD);
{$ELSE}
  FD_ZERO(FD);
{$ENDIF}
end;

procedure CnFDSet(F: Integer; var FD: TCnFDSet);
begin
{$IFDEF MSWINDOWS}
  FD_SET(F, FD);
{$ELSE}
{$IFDEF FPC}
  fpFD_SET(F, FD);
{$ELSE}
  _FD_SET(F, FD);
{$ENDIF}
{$ENDIF}
end;

procedure CnFDClear(F: Integer; var FD: TCnFDSet);
begin
{$IFDEF FPC}
  fpFD_CLR(F, FD);
{$ELSE}
  FD_CLR(F, FD);
{$ENDIF}
end;

function CnFDIsSet(F: Integer; var FD: TCnFDSet): Boolean;
begin
{$IFDEF FPC}
  Result := fpFD_ISSET(F, FD) <> 0;
{$ELSE}
  Result := FD_ISSET(F, FD);
{$ENDIF}
end;

function CnGetNetErrorNo: Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WSAGetLastError;
{$ELSE}
{$IFDEF FPC}
  Result := fpgeterrno;
{$ELSE}
  Result := Posix.Errno.errno;
{$ENDIF}
{$ENDIF}
end;

end.
