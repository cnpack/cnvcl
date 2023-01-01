{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2007 CnPack 开发组                       }
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

unit CnIP;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：IP 基础函数实现单元
* 单元作者：胡昌洪 Sesame (sesamehch@163.com)
* 备    注：支持 Windows 与 POSIX
*           收集整理了网络使用 IP 时常见的实现函数，增加 IP 地址计算功能
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.12.17 V1.4
*                增加对 MacOS 的支持，部分功能不可用
*           2019.03.03 V1.3
*                将部分函数改为 class 以外部也可调用
*           2011.05.15 V1.2
*                修正将 127.0.0.1 作为默认地址的问题
*           2009.08.14 V1.1
*                增加对 D2009 的支持
*           2008.04.14 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  {$IFDEF MSWINDOWS} Windows, Winsock, Nb30, {$ELSE}
  System.Net.Socket, Posix.NetinetIn, Posix.NetDB, Posix.ArpaInet, Posix.SysSocket,
  Posix.NetIf, Posix.StrOpts, Posix.Errno, {$ENDIF}
  SysUtils, Classes, Controls, StdCtrls,
  CnClasses, CnConsts, CnNetConsts, CnNative, CnSocket;

const
  MAXIPNOTE = 255;
  IPJOIN = '.';
  IPADDRFORMAT = '%0:D.%1:D.%2:D.%3:D';

  SIO_GET_INTERFACE_LIST = $4004747F;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;

  IPNOTE1 = $FF000000;
  IPNOTE2 = $00FF0000;
  IPNOTE3 = $0000FF00;
  IPNOTE4 = $000000FF;

type
  TIPNotes = array[1..4] of Byte;
  {* IP 地址的各子节点,如 192.168.20.102，其中 Note[1]=192 ... Note[4]=102}

  TIP_NetType = (iptNone, iptANet, iptBNet, iptCNet, iptDNet, iptENet,
    iptBroadCast, iptKeepAddr);
  {* IP 地址分类：不是IP地址、A 类地址、B 类地址、C 类地址、D 类地址、E 类地址、
    广播地址、保留地址（如 127 等）}

  TIP_Info = packed record
    IPAddress: Cardinal;                 // IP地址，此处用整形存储
    SubnetMask: Cardinal;                // 子网掩码，此处用整形存储
    BroadCast: Cardinal;                 // 广播地址，此处用整形存储
    HostName: array[0..255] of AnsiChar; // 主机名
    NetType: TIP_NetType;                // IP 地址的网络类型
    Notes: TIPNotes;                     // IP 地址的各子节点
    UpState: Boolean;                    // 启用状态
    Loopback: Boolean;                   // 是否环回地址
    SupportBroadcast: Boolean;           // 是否支持广播
  end;
  TIPGroup = array of TIP_Info;          // IP地址组

{$IFDEF MSWINDOWS}

  sockaddr_gen = packed record
    AddressIn: sockaddr_in;
    filler: packed array[0..7] of AnsiChar;
  end;

  TINTERFACE_INFO = packed record
    iiFlags:  u_long; // Interface flags
    iiAddress: sockaddr_gen; // Interface address
    iiBroadcastAddress: sockaddr_gen; // Broadcast address
    iiNetmask: sockaddr_gen; // Network mask
  end;

{$ENDIF}

  { TCnIp }

  TCnIp = class(TCnComponent)
  private
    FIP: TIP_Info;
    FLocalIPs: TIPGroup;
    FNotes: TIPNotes;
{$IFDEF MSWINDOWS}
    FWSAData: TWSAData;
{$ENDIF}
    function GetIPAddress: string;
    procedure SetIPAddress(const Value: string);
    function GetBroadCastIP: string;
    function GetSubnetMask: string;
    procedure SetSubnetMask(const Value: string);
    function GetHosts: Cardinal;
    class function GetIPNotes(const aIP: string; var aResult: TIPNotes): Boolean;
    {* 分解 IP 地址各结点，IP 错误时将抛出错误信息}
    function GetLocalIPCount: Integer;
    function GetComputerName: string;
    function GetMacAddress: string;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string);
      override;
    class function EnumLocalIP(var aLocalIP: TIPGroup): Integer;
    {* 枚举本机所有 IP 及其子网掩码等，返回值为 IP 地址数}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LocalIPGroup: TIPGroup read FLocalIPs;
    {* 本机 IP 地址相关信息，包含本机实际 IP 及 127.0.0.1}
    property LocalIPCount: Integer read GetLocalIPCount;
    {* 本机 IP 地址数，已经排除 127.0.0.1}
    class function IPTypeCheck(const aIP: string): TIP_NetType;
    {* 检查 IP 地址类型以及是否合法}
    class function IPToInt(const aIP: string): Cardinal;
    {* 转换 IP 地址为整数}
    class function IntToIP(const aIP: Cardinal): string;
    {* 转换整数为 IP 地址}
    function NextIP(const aIP: string): string;
    {* 取下一个 IP 地址}
    function PrevIP(const aIP: string): string;
    {* 取前一个 IP 地址}
    function GetIpNum(const aStartIP, aEndIP: string): Integer;
    {* 计算两个 IP 地址之间的 IP 数}
    property Hosts: Cardinal read GetHosts;
    {* 返回指定的 IP 地址和子网掩码时主机数}
    class function GetIPByName(var aIp: string; const aName: string = ''): Boolean;
    {* 通过主机名称得到 IP，aName='' 表示取本机名称}
    class function GetNameByIP(var aName: string; const aIP: string = ''): Boolean;
    {* 通过 IP 得到主机名称，aIpAddr='' 表示取本机 IP}
  published
    property IPAddress: string read GetIPAddress write SetIPAddress;
    {* IP 地址字符串形式，默认为本机 IP 地址}
    property SubnetMask: string read GetSubnetMask write SetSubnetMask;
    {* IP 地址的子网掩码}
    property ComputerName: string read GetComputerName;
    {* 本机名称}
    property MacAddress: string read GetMacAddress;
    {* 本机 Mac 地址}
    property BroadCastIP: string read GetBroadCastIP;
    {* 广播地址}
  end;

implementation

{$R-}

{$IFDEF MSWINDOWS}

const
  WS2_32DLL = 'WS2_32.DLL';

type
  { 从 Winsock 2.0 导入函数 WSAIOCtl -- 在 Win98/ME/2K/Xp and 95 OSR2, NT srv pack #3下才有 Winsock 2 }
  TWSAIoctl = function (s: TSocket; cmd: DWORD; lpInBuffer: PByte; dwInBufferLen:
                        DWORD; lpOutBuffer: PByte; dwOutBufferLen: DWORD;
                        lpdwOutBytesReturned: LPDWORD; lpOverLapped: POINTER;
                        lpOverLappedRoutine: POINTER): Integer; stdcall;

var
  WSAIoctl: TWSAIoctl = nil;
  WS2_32DllHandle: THandle = 0;

procedure InitWSAIoctl;
begin
  WS2_32DllHandle := LoadLibrary(WS2_32DLL);
  if WS2_32DllHandle <> 0 then
  begin
    @WSAIoctl := GetProcAddress(WS2_32DllHandle, 'WSAIoctl');
  end;
end;

procedure FreeWSAIoctl;
begin
  if WS2_32DllHandle <> 0 then
    FreeLibrary(WS2_32DllHandle);
end;  

{$ELSE}

type
  sockaddr_dl = record
    sdl_len: Byte;    //* Total length of sockaddr */
    sdl_family: Byte; //* AF_LINK */
    sdl_index: Word;  //* if != 0, system given index for interface */
    sdl_type: Byte;   //* interface type */
    sdl_nlen: Byte;   //* interface name length, no trailing 0 reqd. */
    sdl_alen: Byte;   //* link level address length */
    sdl_slen: Byte;   //* link layer selector length */
    sdl_data: array[0..11] of AnsiChar; //* minimum work area, can be larger;
                                        //contains both if name and ll address */
  end;
  Psockaddr_dl = ^sockaddr_dl;

{$ENDIF}

{ TCnIp }

constructor TCnIp.Create(AOwner: TComponent);
var
  IPs, I: Integer;
begin
  inherited Create(AOwner);
  IPs := EnumLocalIP(FLocalIPs);
  if IPs = 1 then // Only ONE IP address
  begin
    FIP.IPAddress := FLocalIPs[0].IPAddress;
    FIP.SubnetMask := FLocalIPs[0].SubnetMask;
  end
  else if IPs > 1 then // IF more than one, do not use 127.0.0.1 as default
  begin
    for I := 0 to IPs - 1 do
    begin
      if IntToIP(FLocalIPs[I].IPAddress) <> '127.0.0.1' then
      begin
        FIP.IPAddress := FLocalIPs[I].IPAddress;
        FIP.SubnetMask := FLocalIPs[I].SubnetMask;
        Break;
      end;
      if FIP.IPAddress = 0 then
      begin
        FIP.IPAddress := FLocalIPs[0].IPAddress;
        FIP.SubnetMask := FLocalIPs[0].SubnetMask;
      end;
    end;
  end;
end;

destructor TCnIp.Destroy;
begin

  inherited;
end;

procedure TCnIp.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnIPName;
  Author := SCnPack_Sesame;
  Email := SCnPack_SesameEmail;
  Comment := SCnIPComment;
end;

function TCnIp.GetLocalIPCount: Integer;
begin
  Result := Length(FLocalIPs) - 1;
end;

function TCnIp.GetIPAddress: string;
begin
  Result := IntToIP(FIP.IPAddress);
end;

function TCnIp.GetIpNum(const aStartIP, aEndIP: string): Integer;
begin
  Result := IPToInt(aEndIP) - IPToInt(aStartIP);
end;

class function TCnIp.GetIPNotes(const aIP: string; var aResult: TIPNotes): Boolean;
var
  iPos, iNote: Integer;
  sIP: string;

  function CheckIpNote(aNote: string): Byte;
  begin
    iNote := StrToInt(aNote);
    if (iNote < 0) or (iNote > MAXIPNOTE) then
      raise Exception.Create(aNote + SCnErrorAddrRang);
    Result := iNote;
  end;
begin
  iPos := Pos(IPJOIN, aIP);
  aResult[1] := CheckIpNote(Copy(aIP, 1, iPos - 1));
  sIP := Copy(aIP, iPos + 1, 20);
  iPos := Pos(IPJOIN, sIP);
  aResult[2] := CheckIpNote(Copy(sIP, 1, iPos - 1));
  sIP := Copy(sIP, iPos + 1, 20);
  iPos := Pos(IPJOIN, sIP);
  aResult[3] := CheckIpNote(Copy(sIP, 1, iPos - 1));
  aResult[4] := CheckIpNote(Copy(sIP, iPos + 1, 20));
  Result := aResult[1] > 0;
end;

class function TCnIp.IntToIP(const aIP: Cardinal): string;
var
  Notes: TIPNotes;
begin
  Notes[1] := aIP and IPNOTE1 shr 24;
  Notes[2] := aIP and IPNOTE2 shr 16;
  Notes[3] := aIP and IPNOTE3 shr 8;
  Notes[4] := aIP and IPNOTE4;
  Result := Format(IPADDRFORMAT, [Notes[1], Notes[2], Notes[3], Notes[4]]);
end;

class function TCnIp.IPToInt(const aIP: string): Cardinal;
var
  Notes: TIPNotes;
begin
  Result := 0;
  if IPTypeCheck(aIP) = iptNone then
  begin
    // raise Exception.Create(SCnErrorAddress);
    Exit;
  end;
  if GetIPNotes(aIP, Notes) then
  begin
    Result := Result or Notes[1] shl 24 or Notes[2] shl 16 or Notes[3] shl 8
      or Notes[4];
  end;
end;

class function TCnIp.IPTypeCheck(const aIP: string): TIP_NetType;
var
  Notes: TIPNotes;
begin
  Result := iptNone;
  if GetIPNotes(aIP, Notes) then
  begin
    case Notes[1] of
      1..126: Result := iptANet;
      127: Result := iptKeepAddr;
      128..191: Result := iptBNet;
      192..223: Result := iptCNet;
      224..239: Result := iptDNet;
      240..255: Result := iptENet;
      else
        Result := iptNone;
    end;
  end;
end;

function TCnIp.NextIP(const aIP: string): string;
begin
  Result := IntToIP(IPToInt(aIP) + 1);
end;

function TCnIp.PrevIP(const aIP: string): string;
begin
  Result := IntToIP(IPToInt(aIP) - 1);
end;

procedure TCnIp.SetIPAddress(const Value: string);
begin
  FIP.IPAddress := IPToInt(Value);
end;

function TCnIp.GetBroadCastIP: string;
var
  IpNote, MaskNote: TIPNotes;
begin
  Result := '255.255.255.255';
  if GetIPNotes(SubnetMask, MaskNote) and GetIPNotes(IPAddress, IpNote) then
  begin
    MaskNote[1] := not MaskNote[1];
    MaskNote[2] := not MaskNote[2];
    MaskNote[3] := not MaskNote[3];
    MaskNote[4] := not MaskNote[4];
    MaskNote[1] := MaskNote[1] or IpNote[1];
    MaskNote[2] := MaskNote[2] or IpNote[2];
    MaskNote[3] := MaskNote[3] or IpNote[3];
    MaskNote[4] := MaskNote[4] or IpNote[4];
    FIP.BroadCast := MaskNote[1] shl 24 or MaskNote[2] shl 16 or MaskNote[3]
      shl 8 or MaskNote[4];
    Result := IntToIP(FIP.BroadCast);
  end;
end;

function TCnIp.GetSubnetMask: string;
begin
  Result := IntToIP(FIP.SubnetMask);
end;

procedure TCnIp.SetSubnetMask(const Value: string);
begin
  FIP.SubnetMask := IPToInt(Value);
end;

function TCnIp.GetHosts: Cardinal;
var
  iHost: Int64;
begin
  Result := 0;
  if GetIPNotes(SubnetMask, FNotes) then
  begin
    FNotes[1] := not FNotes[1];
    FNotes[2] := not FNotes[2];
    FNotes[3] := not FNotes[3];
    FNotes[4] := not FNotes[4];
    iHost := FNotes[1] shl 24 or FNotes[2] shl 16 or FNotes[3] shl 8 or FNotes[4]
      - 2;
    if iHost > 0 then
      Result := iHost;
  end;
end;

function TCnIp.GetComputerName: string;
{$IFDEF MSWINDOWS}
var
  sName: array[0..255] of AnsiChar;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  WSAStartup(2, FWSAData);
  try
    GetHostName(@sName, SizeOf(sName));
    Result := {$IFDEF UNICODE}string{$ENDIF}(sName);
  finally
    WSACleanup;
  end;
{$ELSE}
  Result := CnGetHostName;
{$ENDIF}
end;

function TCnIp.GetMacAddress: string;
var
{$IFDEF MSWINDOWS}
{$IFDEF WIN32}
  Lib: Cardinal;
  Func: function(GUID: PGUID): Longint; stdcall;
  GUID1, GUID2: TGUID;
{$ENDIF}
  AdapterList: TLanaEnum;
  NCB: TNCB;
{$ELSE}
  OldPif, Pif: Pifaddrs;
  Ifn: PAnsiChar;
  pAddrInet: sockaddr_in;
  Sdl: Psockaddr_dl;
{$ENDIF}

{$IFDEF MSWINDOWS}
  function GetAdapterInfo(Lana: AnsiChar): string;
  var
    Adapter: TAdapterStatus;
    NCB: TNCB;
  begin
    Result := '';
    FillChar(NCB, SizeOf(NCB), 0);
    NCB.ncb_command := AnsiChar(NCBRESET);
    NCB.ncb_lana_num := Lana;
    if Netbios(@NCB) <> AnsiChar(NRC_GOODRET) then
      Exit;

    FillChar(NCB, SizeOf(NCB), 0);
    NCB.ncb_command := AnsiChar(NCBASTAT);
    NCB.ncb_lana_num := Lana;
    NCB.ncb_callname := '*';

    FillChar(Adapter, SizeOf(Adapter), 0);
    NCB.ncb_buffer := @Adapter;
    NCB.ncb_length := SizeOf(Adapter);
    if Netbios(@NCB) <> AnsiChar(NRC_GOODRET) then
      Exit;

    Result :=
      IntToHex(Byte(Adapter.adapter_address[0]), 2) + '-'+
      IntToHex(Byte(Adapter.adapter_address[1]), 2) + '-'+
      IntToHex(Byte(Adapter.adapter_address[2]), 2) + '-'+
      IntToHex(Byte(Adapter.adapter_address[3]), 2) + '-'+
      IntToHex(Byte(Adapter.adapter_address[4]), 2) + '-'+
      IntToHex(Byte(Adapter.adapter_address[5]), 2) ;
  end;
{$ENDIF}
begin
  Result := '';
{$IFDEF WIN32}
  Lib := LoadLibrary('rpcrt4.dll'); // 该方法只在 Win32 下有效
  if Lib <> 0 then
  try
    if Win32Platform <> VER_PLATFORM_WIN32_NT then
      @Func := GetProcAddress(Lib, 'UuidCreate')
    else
      @Func := GetProcAddress(Lib, 'UuidCreateSequential');
    if Assigned(Func) then
    begin
      if (Func(@GUID1) = 0) and
        (Func(@GUID2) = 0) and
        (GUID1.D4[2] = GUID2.D4[2]) and
        (GUID1.D4[3] = GUID2.D4[3]) and
        (GUID1.D4[4] = GUID2.D4[4]) and
        (GUID1.D4[5] = GUID2.D4[5]) and
        (GUID1.D4[6] = GUID2.D4[6]) and
        (GUID1.D4[7] = GUID2.D4[7]) then
      begin
        Result :=
          IntToHex(GUID1.D4[2], 2) + '-' +
          IntToHex(GUID1.D4[3], 2) + '-' +
          IntToHex(GUID1.D4[4], 2) + '-' +
          IntToHex(GUID1.D4[5], 2) + '-' +
          IntToHex(GUID1.D4[6], 2) + '-' +
          IntToHex(GUID1.D4[7], 2);
      end;
    end;
  finally
    FreeLibrary(Lib);
  end;
{$ENDIF}

  if Result <> '' then
    Exit;

{$IFDEF MSWINDOWS}
  // Win32 失败后，或 Win64 下，拿 MAC 地址
  FillChar(NCB, SizeOf(NCB), 0);
  NCB.ncb_command := AnsiChar(NCBENUM);
  NCB.ncb_buffer := @AdapterList;
  NCB.ncb_length := SizeOf(AdapterList);
  Netbios(@NCB);
  if Byte(AdapterList.length) > 0 then
    Result := GetAdapterInfo(AdapterList.lana[0])
{$ELSE}
  // POSIX 下拿 MAC 地址
  getifaddrs(Pif);
  OldPif := Pif;
  Ifn := nil;

  while Pif <> nil do
  begin
    if (Pif^.ifa_addr.sa_family = AF_INET) and ((Pif^.ifa_flags and IFF_LOOPBACK) = 0)
      and (Pif^.ifa_name <> nil) then
    begin
      if Ifn = nil then
        Ifn := PAnsiChar(Pif^.ifa_name);

      // 拿 IP，如果不是 127.0.0.1 则用它
      pAddrInet := Psockaddr_in(Pif^.ifa_addr)^;
      if inet_ntoa(pAddrInet.sin_addr) <> '127.0.0.1' then
      begin
        Ifn := PAnsiChar(Pif^.ifa_name);
        Break;
      end;
    end;
    Pif := Pif^.ifa_next;
  end;

  if Ifn = nil then
    Exit;

  Pif := OldPif;
  while Pif <> nil do
  begin
    if (Pif^.ifa_addr.sa_family = AF_LINK) and
      (StrComp(Ifn, PAnsiChar(Pif^.ifa_name)) = 0) then
    begin
      Sdl := Psockaddr_dl(Pif^.ifa_addr);
      if Sdl <> nil  then
      begin
        // Sdl^.sdl_data[Sdl^.sdl_nlen] 开始的 Sdl^.sdl_alen 字节就是 MAC 地址
        if Sdl^.sdl_alen = 6 then
        begin
        Result :=
          IntToHex(Ord(Sdl^.sdl_data[Sdl^.sdl_nlen]), 2) + '-' +
          IntToHex(Ord(Sdl^.sdl_data[Sdl^.sdl_nlen + 1]), 2) + '-' +
          IntToHex(Ord(Sdl^.sdl_data[Sdl^.sdl_nlen + 2]), 2) + '-' +
          IntToHex(Ord(Sdl^.sdl_data[Sdl^.sdl_nlen + 3]), 2) + '-' +
          IntToHex(Ord(Sdl^.sdl_data[Sdl^.sdl_nlen + 4]), 2) + '-' +
          IntToHex(Ord(Sdl^.sdl_data[Sdl^.sdl_nlen + 5]), 2);
        end;
      end;
    end;
    Pif := Pif^.ifa_next;
  end;
{$ENDIF}
end;

class function TCnIp.GetIPByName(var aIP: string; const aName: string): Boolean;
{$IFDEF MSWINDOWS}
var
  aWSAData: TWSAData;
  pHost: PHostEnt;
  sName: array[0..256] of Char;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  sName[256] := #0;
  StrPLCopy(sName, aName, 255);

  WSAStartup($101, aWSAData);
  try
    if sName = '' then
      GetHostName(PAnsiChar({$IFDEF UNICODE}AnsiString{$ELSE}string{$ENDIF}(sName)), SizeOf(sName));
    pHost := GetHostByName(@sName);
    Result := pHost <> nil;
    if Result then
      aIP := {$IFDEF UNICODE}string{$ENDIF}(inet_ntoa(PInAddr(pHost^.h_addr_list^)^));
  finally
    WSACleanup;
  end;
{$ELSE}
  aIP := TIPAddress.LookupName(aName).Address;
  Result := aIP <> '';
{$ENDIF}
end;

class function TCnIp.GetNameByIP(var aName: string; const aIP: string): Boolean;
var
{$IFDEF MSWINDOWS}
  aWSAData: TWSAData;
{$ENDIF}
  HostEnt: PHostEnt;
  InetAddr: Cardinal;
  sIP: string;
begin
  sIP := aIP;
  aName := '';
  if sIP = '' then
    sIP := '127.0.0.1';

{$IFDEF MSWINDOWS}
  WSAStartup(2, aWSAData);
{$ENDIF}
  try
    InetAddr := inet_addr(PAnsiChar({$IFDEF UNICODE}AnsiString{$ENDIF}(sIP)));
{$IFDEF MSWINDOWS}
    HostEnt := gethostbyaddr(@InetAddr, SizeOf(InetAddr), PF_INET);
{$ELSE}
    HostEnt := gethostbyaddr(InetAddr, SizeOf(InetAddr), PF_INET);
{$ENDIF}
    Result := HostEnt <> nil;
    if Result then
      aName := {$IFDEF UNICODE}string{$ENDIF}(StrPas(HostEnt^.h_name));
  finally
{$IFDEF MSWINDOWS}
    WSACleanup;
{$ENDIF}
  end;
end;

{-------------------------------------------------------------------
1. 创建一个 Socket
2. 调用 WSAIOCtl 获取网络连接
3. 对每个连接，获取它的 IP、掩码、广播地址、状态
4. 将信息填充到 IP 数组中
5. 结束关闭 Socket
--------------------------------------------------------------------}
class function TCnIp.EnumLocalIP(var aLocalIP: TIPGroup): Integer;
var
  iIP: Integer;
  pAddrInet: sockaddr_in;
{$IFDEF MSWINDOWS}
  aWSAData: TWSAData;
  skLocal: TSocket;
  PtrA: pointer;
  BytesReturned, SetFlags: u_long;
  Buffer: array[0..20] of TINTERFACE_INFO;
{$ELSE}
  OldPif, Pif: Pifaddrs;
  SetFlags: Cardinal;
{$ENDIF}
begin
  Result := 0;
{$IFDEF MSWINDOWS}
  WSAStartup($101, aWSAData);
  try
    skLocal := Socket(AF_INET, SOCK_STREAM, 0); // Open a socket
    if (skLocal = INVALID_SOCKET) then
      Exit;

    try // Call WSAIoCtl
      PtrA := @BytesReturned;
      if (WSAIoCtl(skLocal, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA,
        nil, nil) <> SOCKET_ERROR) then
      begin // If ok, find out how
        Result := BytesReturned div SizeOf(TINTERFACE_INFO);
        SetLength(aLocalIP, Result);
        for iIP := 0 to Result - 1 do // For every interface
        begin
          pAddrInet := Buffer[iIP].iiAddress.AddressIn;
          aLocalIP[iIP].IPAddress := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
          pAddrInet := Buffer[iIP].iiNetMask.AddressIn;
          aLocalIP[iIP].SubnetMask := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
          pAddrInet := Buffer[iIP].iiBroadCastAddress.AddressIn;
          aLocalIP[iIP].BroadCast := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
          SetFlags := Buffer[iIP].iiFlags;
          aLocalIP[iIP].UpState := (SetFlags and IFF_UP) = IFF_UP;
          aLocalIP[iIP].Loopback := (SetFlags and IFF_LOOPBACK) = IFF_LOOPBACK;
          aLocalIP[iIP].SupportBroadcast := (SetFlags and IFF_BROADCAST) =
            IFF_BROADCAST;
        end;
      end;
    except
      ;
    end;
    CloseSocket(skLocal);
  finally
    WSACleanUp;
  end;
{$ELSE}
  getifaddrs(Pif);
  OldPif := Pif;
  while Pif <> nil do // 先统计符合条件的数量
  begin
    if (Pif^.ifa_addr.sa_family = AF_INET) and ((Pif^.ifa_flags and IFF_LOOPBACK) = 0) then
      Inc(Result);
    Pif := Pif^.ifa_next;
  end;
  SetLength(aLocalIP, Result);
  if Result <= 0 then
    Exit;

  Pif := OldPif;
  iIP := 0;
  while Pif <> nil do
  begin
    if (Pif^.ifa_addr.sa_family = AF_INET) and ((Pif^.ifa_flags and IFF_LOOPBACK) = 0) then
    begin
      pAddrInet := Psockaddr_in(Pif^.ifa_addr)^;
      aLocalIP[iIP].IPAddress := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
      pAddrInet := Psockaddr_in(Pif^.ifa_netmask)^;
      aLocalIP[iIP].SubnetMask := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
      pAddrInet := Psockaddr_in(Pif^.ifa_dstaddr)^;
      aLocalIP[iIP].BroadCast := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
      SetFlags := Pif^.ifa_flags;
      aLocalIP[iIP].UpState := (SetFlags and IFF_UP) = IFF_UP;
      aLocalIP[iIP].Loopback := (SetFlags and IFF_LOOPBACK) = IFF_LOOPBACK;
      aLocalIP[iIP].SupportBroadcast := (SetFlags and IFF_BROADCAST) =
        IFF_BROADCAST;

      Inc(iIP);
    end;
    Pif := Pif^.ifa_next;
  end;
  freeifaddrs(Pif);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

initialization
  InitWSAIoctl;

finalization
  FreeWSAIoctl;

{$ENDIF}
end.

