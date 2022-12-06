{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnSocket;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯 Socket 公共声明与跨平台函数封装单元
* 单元作者：CnPack 开发组
* 备    注：
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
  SysUtils, Classes {$IFDEF MSWINDOWS}, WinSock {$ELSE}, System.Net.Socket,
  Posix.Base, Posix.NetIf, Posix.SysSocket, Posix.ArpaInet, Posix.NetinetIn
  {$ENDIF};

const
  SD_BOTH = 2;

{$IFNDEF MSWINDOWS}

type
  TSocket = Integer;
  TSockAddr = sockaddr_in;

const
  SOCKET_ERROR   = -1;
  INVALID_SOCKET = -1;

function getifaddrs(var Ifap: pifaddrs): Integer; cdecl; external libc name _PU + 'getifaddrs';

procedure freeifaddrs(Ifap: pifaddrs); cdecl; external libc name _PU + 'freeifaddrs';

{$ENDIF}

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

function CnSend(S: TSocket; const Buf; Len, Flags: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 send 函数的封装}

function CnRecv(S: TSocket; var Buf; Len, Flags: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 recv 函数的封装}

function CnShutdown(S: TSocket; How: Integer): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 shutdown 函数的封装}

function CnCloseSocket(S: TSocket): Integer;
{* 对 Windows 以及 POSIX（包括 MAC、Linux 等）平台上的 closesocket 函数的封装}

implementation

function CnNewSocket(Af, Struct, Protocol: Integer): TSocket;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.socket(Af, Struct, Protocol);
{$ELSE}
  Result := Posix.SysSocket.socket(Af, Struct, Protocol);
{$ENDIF}
end;

function CnConnect(S: TSocket; var Name: TSockAddr; NameLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.connect(S, Name, NameLen);
{$ELSE}
  Result := Posix.SysSocket.connect(S, sockaddr(Name), NameLen);
{$ENDIF}
end;

function CnBind(S: TSocket; var Addr: TSockAddr; NameLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.bind(S, Addr, NameLen);
{$ELSE}
  Result := Posix.SysSocket.bind(S, sockaddr(Addr), NameLen);
{$ENDIF}
end;

function CnGetSockName(S: TSocket; var Name: TSockAddr; var NameLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.getsockname(S, Name, NameLen);
{$ELSE}
  Result := Posix.SysSocket.getsockname(S, sockaddr(Name), NameLen);
{$ENDIF}
end;

function CnListen(S: TSocket; Backlog: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.listen(S, Backlog);
{$ELSE}
  Result := Posix.SysSocket.listen(S, Backlog);
{$ENDIF}
end;

function CnAccept(S: TSocket; Addr: PSockAddr; AddrLen: PInteger): TSocket;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.accept(S, Addr, AddrLen);
{$ELSE}
  Result := Posix.SysSocket.accept(S, Addr^, AddrLen);
{$ENDIF}
end;

function CnSend(S: TSocket; const Buf; Len, Flags: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.send(S, Buf, Len, Flags);
{$ELSE}
  Result := Posix.SysSocket.send(S, Buf, Len, Flags);
{$ENDIF}
end;

function CnRecv(S: TSocket; var Buf; Len, Flags: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.recv(S, Buf, Len, Flags);
{$ELSE}
  Result := Posix.SysSocket.recv(S, Buf, Len, Flags);
{$ENDIF}
end;

function CnShutdown(S: TSocket; How: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.shutdown(S, How);
{$ELSE}
  Result := Posix.SysSocket.shutdown(S, How);
{$ENDIF}
end;

function CnCloseSocket(S: TSocket): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := WinSock.closesocket(S);
{$ELSE}
  Result := Posix.Unistd.__close(S);
{$ENDIF}
end;

end.
