{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2007 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnIP;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�IP ��������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ������� Sesame (sesamehch@163.com)
* ��    ע��֧�� Windows �� POSIX
*           �ռ�����������ʹ�� IP ʱ������ʵ�ֺ��������� IP ��ַ���㹦��
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.08.30 V1.5
*                �����������������Ա����� Local*Count �ų����ص�ַ������һ��
*           2025.03.09 V1.5
*                ���� Windows �� IPv6 ��֧��
*           2022.12.17 V1.4
*                ���Ӷ� MacOS ��֧�֣����ֹ��ܲ�����
*           2019.03.03 V1.3
*                �����ֺ�����Ϊ class ���ⲿҲ�ɵ���
*           2011.05.15 V1.2
*                ������ 127.0.0.1 ��ΪĬ�ϵ�ַ������
*           2009.08.14 V1.1
*                ���Ӷ� D2009 ��֧��
*           2008.04.14 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  {$IFDEF MSWINDOWS} Windows, Winsock, Nb30, {$ELSE}
  System.Net.Socket, Posix.NetinetIn, Posix.NetDB, Posix.ArpaInet, Posix.SysSocket,
  Posix.NetIf, Posix.StrOpts, Posix.Errno, {$ENDIF}
  SysUtils, Classes, Controls, StdCtrls, Consts, {$IFNDEF COMPILER5} RTLConsts, {$ENDIF}
  CnInt128, CnClasses, CnConsts, CnNetConsts, CnNative, CnSocket;

type
  ECnIPException = class(Exception);

  TCnIPv4Notes = array[1..4] of Byte;
  {* IP ��ַ�ĸ��ӽڵ㣬�� 192.168.20.102������ Note[1]=192 ... Note[4]=102}

  TCnIPv6Notes = array[1..8] of Word;
  {* IPv6 ��ַ�ĸ��ӽڵ㣬�˸�˫�ֽ�}

  TCnIPv6NetMask = array[0..15] of Byte;
  {* IPv6 ��ַ���������룬16 ���ֽڣ�0 �±�����λ}

  TCnIPv4NetType = (iptNone, iptANet, iptBNet, iptCNet, iptDNet, iptENet,
    iptBroadCast, iptKeepAddr);
  {* IP ��ַ���ࣺ���� IP ��ַ��A ���ַ��B ���ַ��C ���ַ��D ���ַ��E ���ַ��
    �㲥��ַ��������ַ���� 127 �ȣ�}

  TCnIPInfo = packed record
    IPAddress: Cardinal;                 // IPv4 ��ַ���˴������δ洢
    SubnetMask: Cardinal;                // �������룬�˴������δ洢
    BroadCast: Cardinal;                 // �㲥��ַ���˴������δ洢
    HostName: array[0..255] of AnsiChar; // ������
    NetType: TCnIPv4NetType;             // IPv4 ��ַ����������
    Notes: TCnIPv4Notes;                 // IPv4 ��ַ�ĸ��ӽڵ�
    UpState: Boolean;                    // ����״̬
    Loopback: Boolean;                   // �Ƿ񻷻ص�ַ
    SupportBroadcast: Boolean;           // �Ƿ�֧�ֹ㲥
  end;
  TCnIPGroup = array of TCnIPInfo;       // IPv4 ��ַ��

  TCnIPv6Info = packed record
    IPv6Address: TCnUInt128;             // IPv6 ��ַ���˴��� UInt128 �洢
    SubnetMask: TCnIPv6NetMask;          // �������룬�±� 0 �Ǹ�λ�������Ķ�ϰ��
    PrefixLength: Integer;               // ����ǰ׺���ȣ�IPv6 �б����������������ձ�����������Ӧ SubnetMask �ĸ�λ�� 1 �ĸ���
    BroadCast: TCnUInt128;               // �㲥��ַ���˴��� UInt128 �洢
    HostName: array[0..255] of AnsiChar; // ������
    NetType: TCnIPv4NetType;             // IPv6 ��ַ����������
    Notes: TCnIPv6Notes;                 // IPv6 ��ַ�ĸ��ӽڵ�
    UpState: Boolean;                    // ����״̬
    Loopback: Boolean;                   // �Ƿ񻷻ص�ַ
    SupportBroadcast: Boolean;           // �Ƿ�֧�ֹ㲥
  end;
  TCnIPv6Group = array of TCnIPv6Info;   // IPv6 ��ַ��

  { TCnIp }

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnIp = class(TCnComponent)
  private
    FIP: TCnIPInfo;
    FIPv6: TCnIPv6Info;
    FLocalIPs: TCnIPGroup;
    FLocalIPv6s: TCnIPv6Group;
    FNotes: TCnIPv4Notes;
{$IFDEF MSWINDOWS}
    FWSAData: TWSAData;
{$ENDIF}
    function GetIPAddress: string;
    procedure SetIPAddress(const Value: string);
    function GetBroadCastIP: string;
    function GetSubnetMask: string;
    procedure SetSubnetMask(const Value: string);
    function GetHosts: Cardinal;
    class function GetIPNotes(const aIP: string; var aResult: TCnIPv4Notes): Boolean;
    {* �ֽ� IP ��ַ����㣬IP ����ʱ���׳�������Ϣ}
    class function GetIPv6Notes(const aIPv6: string; var aResult: TCnIPv6Notes): Boolean;
    {* �ֽ� IPv6 ��ַ����㣬IPv6 ����ʱ���׳�������Ϣ}
    function GetLocalIPCount: Integer;
    function GetLocalIPs(Index: Integer): TCnIPInfo;
    function GetComputerName: string;
    function GetMacAddress: string;

    function GetIPv6Address: string;
    procedure SetIPv6Address(const Value: string);
    function GetIPv6SubnetMask: string;
    function GetIPv6PrefixLength: Integer;
    procedure SetIPv6PrefixLength(const Value: Integer);
    function GetLocalIPv6Count: Integer;
    function GetLocalIPv6s(Index: Integer): TCnIPv6Info;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string);
      override;
    class function EnumLocalIP(var aLocalIP: TCnIPGroup): Integer;
    {* ö�ٱ������� IP ������������ȣ�����ֵΪ IP ��ַ��}
    class function EnumLocalIPv6(var aLocalIPv6: TCnIPv6Group): Integer;
    {* ö�ٱ������� IPv6 ������������ȣ�����ֵΪ IPv6 ��ַ��}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LocalIPGroup: TCnIPGroup read FLocalIPs;
    {* ���� IP ��ַ�����Ϣ��ԭʼ���飬��������ʵ�� IP �� 127.0.0.1}

    property LocalIPs[Index: Integer]: TCnIPInfo read GetLocalIPs;
    {* ���� IP ��ַ�������Ѿ��ų� 127.0.0.1}
    property LocalIPCount: Integer read GetLocalIPCount;
    {* ���� IP ��ַ�����Ѿ��ų� 127.0.0.1}

    property LocalIPv6Group: TCnIPv6Group read FLocalIPv6s;
    {* ���� IPv6 ��ַ�����Ϣ��ԭʼ���ݣ���������ʵ�� IPv6 �����ص�ַ}

    property LocalIPv6s[Index: Integer]: TCnIPv6Info read GetLocalIPv6s;
    {* ���� IPv6 ��ַ�������Ѿ��ų����ص�ַ}
    property LocalIPv6Count: Integer read GetLocalIPv6Count;
    {* ���� IPv6 ��ַ�����Ѿ��ų����ص�ַ}

    class function IPTypeCheck(const aIP: string): TCnIPv4NetType;
    {* ��� IP ��ַ�����Լ��Ƿ�Ϸ�}
    class function IPToInt(const aIP: string): Cardinal;
    {* ת�� IP ��ַΪ����}
    class function IntToIP(const aIP: Cardinal): string;
    {* ת������Ϊ IP ��ַ}

    class function IPv6ToInt128(const aIPv6: string): TCnUInt128;
    {* ת�� IP ��ַΪ����}
    class function Int128ToIPv6(var aIPv6: TCnUInt128): string;
    {* ת������Ϊ IPv6 ��ַ}

    function NextIP(const aIP: string): string;
    {* ȡ��һ�� IP ��ַ}
    function PrevIP(const aIP: string): string;
    {* ȡǰһ�� IP ��ַ}
    function GetIpNum(const aStartIP, aEndIP: string): Integer;
    {* �������� IP ��ַ֮��� IP ��}
    property Hosts: Cardinal read GetHosts;
    {* ����ָ���� IP ��ַ����������ʱ������}
    class function GetIPByName(var aIp: string; const aName: string = ''): Boolean;
    {* ͨ���������Ƶõ� IP��aName='' ��ʾȡ��������}
    class function GetNameByIP(var aName: string; const aIP: string = ''): Boolean;
    {* ͨ�� IP �õ��������ƣ�aIpAddr='' ��ʾȡ���� IP}

  published
    property IPAddress: string read GetIPAddress write SetIPAddress stored False;
    {* IP ��ַ�ַ�����ʽ��Ĭ��Ϊ���� IP ��ַ}
    property SubnetMask: string read GetSubnetMask write SetSubnetMask stored False;
    {* IP ��ַ����������}

    property IPv6Address: string read GetIPv6Address write SetIPv6Address stored False;
    {* IPv6 ��ַ�ַ�����ʽ��Ĭ��Ϊ���� IPv6 ��ַ}
    property IPv6PrefixLength: Integer read GetIPv6PrefixLength write SetIPv6PrefixLength stored False;
    {* IPv6 ��ַ��ǰ׺���ȣ���������������˵�����ձ���}
    property IPv6SubnetMask: string read GetIPv6SubnetMask stored False;
    {* IPv6 ��ַ����������}

    property ComputerName: string read GetComputerName stored False;
    {* ��������}
    property MacAddress: string read GetMacAddress stored False;
    {* ���� Mac ��ַ}
    property BroadCastIP: string read GetBroadCastIP stored False;
    {* �㲥��ַ}
  end;

implementation

{$R-}

const
  MAXIPNOTE = 255;
  MAXIPV6NOTE = 65535;
  IPJOIN = '.';
  IPV6JOIN = ':';
  IPADDRFORMAT = '%0:D.%1:D.%2:D.%3:D';

  IPNOTE1 = $FF000000;
  IPNOTE2 = $00FF0000;
  IPNOTE3 = $0000FF00;
  IPNOTE4 = $000000FF;

{$IFDEF MSWINDOWS}

const
  WS2_32DLL = 'WS2_32.DLL';

  SIO_GET_INTERFACE_LIST = $4004747F;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;

  AF_INET6 = 23;
  MAX_ADAPTER_ADDRESS_LENGTH = 8;
  MAX_DHCPV6_DUID_LENGTH = 130;
  MAX_DNS_SUFFIX_STRING_LENGTH = 256;

type
  { �� Winsock 2.0 ���뺯�� WSAIOCtl -- �� Win98/ME/2K/Xp and 95 OSR2, NT srv pack #3�²��� Winsock 2 }
  TWSAIoctl = function (s: TSocket; cmd: DWORD; lpInBuffer: PByte; dwInBufferLen:
                        DWORD; lpOutBuffer: PByte; dwOutBufferLen: DWORD;
                        lpdwOutBytesReturned: LPDWORD; lpOverLapped: POINTER;
                        lpOverLappedRoutine: POINTER): Integer; stdcall;

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

  // IPv6 ������ض���
  IFTYPE = ULONG;
  IF_INDEX = ULONG;
  NET_IF_COMPARTMENT_ID = Cardinal;

  SOCKET_ADDRESS = record
    lpSockaddr: PSOCKADDR;
    iSockaddrLength: Integer;
  end;
  PSOCKET_ADDRESS = ^SOCKET_ADDRESS;

  IP_PREFIX_ORIGIN = (
    IpPrefixOriginOther,
    IpPrefixOriginManual,
    IpPrefixOriginWellKnown,
    IpPrefixOriginDhcp,
    IpPrefixOriginRouterAdvertisement);

  IP_SUFFIX_ORIGIN = (
    IpSuffixOriginOther,
    IpSuffixOriginManual,
    IpSuffixOriginWellKnown,
    IpSuffixOriginDhcp,
    IpSuffixOriginLinkLayerAddress,
    IpSuffixOriginRandom);

  IP_DAD_STATE = (
    IpDadStateInvalid,
    IpDadStateTentative,
    IpDadStateDuplicate,
    IpDadStateDeprecated,
    IpDadStatePreferred);

  // ���ṹӦ�� 48 �ֽڣ�����ö�����͸�ռ 4 �ֽڣ���ֱ��ʹ��ö�ٵĻ�û��������
  // ���ܱ�� 1 �ֽڵ����ܳ� 40 �ֽڣ�������� DWORD �����Է��� Windows ��ʵ�����
  PIP_ADAPTER_UNICAST_ADDRESS = ^_IP_ADAPTER_UNICAST_ADDRESS;
  _IP_ADAPTER_UNICAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: Int64);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_UNICAST_ADDRESS;
    Address: SOCKET_ADDRESS;
    PrefixOrigin: DWORD; // IP_PREFIX_ORIGIN
    SuffixOrigin: DWORD; // IP_SUFFIX_ORIGIN
    DadState: DWORD;     // IP_DAD_STATE
    ValidLifetime: ULONG;
    PreferredLifetime: ULONG;
    LeaseLifetime: ULONG;
    OnLinkPrefixLength: UCHAR;
  end;

  PIP_ADAPTER_PREFIX = ^IP_ADAPTER_PREFIX;
  IP_ADAPTER_PREFIX = record
    Union: record
    case Integer of
      0: (
        Alignment: Int64);
      1: (
        Length: ULONG;
        Flags: DWORD);
    end;
    Next: PIP_ADAPTER_PREFIX;
    Address: SOCKET_ADDRESS;
    PrefixLength: ULONG;
  end;

  PIP_ADAPTER_ANYCAST_ADDRESS = ^IP_ADAPTER_ANYCAST_ADDRESS;
  IP_ADAPTER_ANYCAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: Int64);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_ANYCAST_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;

  PIP_ADAPTER_MULTICAST_ADDRESS = ^IP_ADAPTER_MULTICAST_ADDRESS;
  IP_ADAPTER_MULTICAST_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: Int64);
        1: (
          Length: ULONG;
          Flags: DWORD);
    end;
    Next: PIP_ADAPTER_MULTICAST_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;

  PIP_ADAPTER_DNS_SERVER_ADDRESS = ^_IP_ADAPTER_DNS_SERVER_ADDRESS;
  _IP_ADAPTER_DNS_SERVER_ADDRESS = record
    Union: record
      case Integer of
        0: (
          Alignment: Int64);
        1: (
          Length: ULONG;
          Reserved: DWORD);
    end;
    Next: PIP_ADAPTER_DNS_SERVER_ADDRESS;
    Address: SOCKET_ADDRESS;
  end;

  IF_OPER_STATUS = (
    IfOperStatusInvalid,
    IfOperStatusUp,
    IfOperStatusDown,
    IfOperStatusTesting,
    IfOperStatusUnknown,
    IfOperStatusDormant,
    IfOperStatusNotPresent,
    IfOperStatusLowerLayerDown);

  PIP_ADAPTER_WINS_SERVER_ADDRESS_LH = ^IP_ADAPTER_WINS_SERVER_ADDRESS_LH;
  IP_ADAPTER_WINS_SERVER_ADDRESS_LH = record
    Union: record
      case Integer of
        0: (
          Alignment: Int64);
        1: (
          Length: ULONG;
          Reserved: DWORD);
    end;
    Next: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
    Address: SOCKET_ADDRESS;
  end;

  PIP_ADAPTER_GATEWAY_ADDRESS_LH = ^IP_ADAPTER_GATEWAY_ADDRESS_LH;
  IP_ADAPTER_GATEWAY_ADDRESS_LH = record
    Union: record
      case Integer of
        0: (
          Alignment: Int64);
        1: (
          Length: ULONG;
          Reserved: DWORD);
    end;
    Next: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
    Address: SOCKET_ADDRESS;
  end;

  NET_IF_NETWORK_GUID = TGUID;

  NET_IF_CONNECTION_TYPE = (
    NetIfConnectionInvalid,
    NetIfConnectionDedicated,
    NetIfConnectionPassive,
    NetIfConnectionDemand,
    NetIfConnectionMaximum);

  TUNNEL_TYPE = (
    TunnelTypeNone,
    TunnelTypeOther,
    TunnelTypeDirect,
    TunnelType6To4,      // 11
    TunnelTypeIsatap,    // 13
    TunnelTypeTeredo,
    TunnelTypeIPHTTPS);

  PIP_ADAPTER_DNS_SUFFIX = ^IP_ADAPTER_DNS_SUFFIX;
  IP_ADAPTER_DNS_SUFFIX = record
    Next: PIP_ADAPTER_DNS_SUFFIX;
    AString: array[0..MAX_DNS_SUFFIX_STRING_LENGTH - 1] of WCHAR;
  end;

  PIP_ADAPTER_ADDRESSES = ^IP_ADAPTER_ADDRESSES;
  IP_ADAPTER_ADDRESSES = record
    Union: record
      case Integer of
        0: (
          Alignment: Int64);
        1: (
          Length: ULONG;
          IfIndex: DWORD);
    end;
    Next: PIP_ADAPTER_ADDRESSES;
    AdapterName: PAnsiChar;
    FirstUnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
    FirstAnycastAddress: PIP_ADAPTER_ANYCAST_ADDRESS;
    FirstMulticastAddress: PIP_ADAPTER_MULTICAST_ADDRESS;
    FirstDnsServerAddress: PIP_ADAPTER_DNS_SERVER_ADDRESS;
    DnsSuffix: PWCHAR;
    Description: PWCHAR;
    FriendlyName: PWCHAR;
    PhysicalAddress: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    PhysicalAddressLength: DWORD;
    Flags: DWORD;
    Mtu: DWORD;
    IfType: IFTYPE;
    OperStatus: Integer; // IF_OPER_STATUS;
    Ipv6IfIndex: IF_INDEX;
    ZoneIndices: array [0..15] of DWORD;
    FirstPrefix: PIP_ADAPTER_PREFIX;
    TransmitLinkSpeed: Int64;
    ReceiveLinkSpeed: Int64;
    FirstWinsServerAddress: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
    FirstGatewayAddress: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
    Ipv4Metric: ULONG;
    Ipv6Metric: ULONG;
    Luid: Int64;
    Dhcpv4Server: SOCKET_ADDRESS;
    CompartmentId: NET_IF_COMPARTMENT_ID;
    NetworkGuid: NET_IF_NETWORK_GUID;
    ConnectionType: NET_IF_CONNECTION_TYPE;
    TunnelType: DWORD; // TUNNEL_TYPE;
    //
    // DHCP v6 Info.
    //
    Dhcpv6Server: SOCKET_ADDRESS;
    Dhcpv6ClientDuid: array [0..MAX_DHCPV6_DUID_LENGTH - 1] of Byte;
    Dhcpv6ClientDuidLength: ULONG;
    Dhcpv6Iaid: ULONG;
    FirstDnsSuffix: PIP_ADAPTER_DNS_SUFFIX;
  end;

  IN6_ADDR = record
    case Integer of
      0: (s6_bytes: array[0..15] of Byte);
      1: (s6_words: array[0..7] of Word);
  end;
  PIN6_ADDR = ^IN6_ADDR;

  sockaddr_in6_union = record
    case Integer of
      0 : (sin6_scope_id : ULONG);     // Set of interfaces for a scope.
      1 : (sin6_scope_struct : ULONG);
  end;

  SOCKADDR_IN6 = record
    sin6_family: u_short;        // AF_INET6.
    sin6_port: u_short;          // Transport level port number.
    sin6_flowinfo: ULONG;        // IPv6 flow information.
    sin6_addr: IN6_ADDR;         // IPv6 address.
    a : sockaddr_in6_union;
  end;
  PSOCKADDR_IN6 = ^SOCKADDR_IN6;

  TGetAdaptersAddresses = function(Family: ULONG; Flags: DWORD; Reserved: Pointer;
    pAdapterAddresses: PIP_ADAPTER_ADDRESSES; pOutBufLen: PULONG): DWORD; stdcall;

var
  WSAIoctl: TWSAIoctl = nil;
  GetAdaptersAddresses: TGetAdaptersAddresses = nil;

  WS2_32DllHandle: THandle = 0;
  IphlpApiHandle: THandle = 0;

  in6addr_loopback: IN6_ADDR;

procedure InitWSAIoctl;
begin
  WS2_32DllHandle := LoadLibrary(WS2_32DLL);
  if WS2_32DllHandle <> 0 then
  begin
    @WSAIoctl := GetProcAddress(WS2_32DllHandle, 'WSAIoctl');
  end;

  IphlpApiHandle := LoadLibrary('iphlpapi.dll');
  if IphlpApiHandle <> 0 then
  begin
    @GetAdaptersAddresses := GetProcAddress(IphlpApiHandle, 'GetAdaptersAddresses');
  end;
end;

procedure FreeWSAIoctl;
begin
  if WS2_32DllHandle <> 0 then
    FreeLibrary(WS2_32DllHandle);
  if IphlpApiHandle <> 0 then
    FreeLibrary(IphlpApiHandle);
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

// ���ַ�����ð�Ų�֣�ȷ������ð��ʱ���ڿ��ַ�����������ð������ǰ������������Ҫ����ǰ��Ŀ��ַ���
function SplitStringByColon(const S: string; Res: TStringList): Integer;
var
  I: Integer;
  StartPos: Integer;
  Temp: string;
begin
  Res.Clear;
  StartPos := 1;
  while StartPos <= Length(S) do
  begin
    // ������һ��ð�ŵ�λ��
    I := StartPos;
    while (I <= Length(S)) and (S[I] <> ':') do
      Inc(I);

    // ����ҵ���ð�ţ����ð��ǰ���ַ����Ƿ�Ϊ��
    if I = StartPos then
    begin
      // �����������ð�ţ�����ӿ��ַ���
      Res.Add('');
    end
    else
    begin
      // ���ð��ǰ���ַ����������
      Temp := Copy(S, StartPos, I - StartPos);
      Res.Add(Temp);
    end;

    // ׼����һ��������������ǰ��ð��
    StartPos := I + 1;
  end;
  Result := Res.Count;
end;

{ TCnIp }

constructor TCnIp.Create(AOwner: TComponent);
var
  IPs, IPv6s, I: Integer;
  aIP: TCnIPInfo;
  aIPv6: TCnIPv6Info;
begin
  inherited Create(AOwner);
  IPs := EnumLocalIP(FLocalIPs);
  IPv6s := EnumLocalIPv6(FLocalIPv6s);

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

    for I := 0 to IPs - 1 do
    begin
      if IntToIP(FLocalIPs[I].IPAddress) = '127.0.0.1' then
      begin
        if I <> IPs - 1 then
        begin
          // �����һ��������Ҳ���ǰѻ��ص�ַ�����
          Move(FLocalIPs[I], aIP, SizeOf(TCnIPInfo));
          Move(FLocalIPs[IPs - 1], FLocalIPs[I], SizeOf(TCnIPInfo));
          Move(aIP, FLocalIPs[IPs - 1], SizeOf(TCnIPInfo));
        end;
        Break;
      end;
    end;
  end;

  if IPv6s = 1 then // Only ONE IP address
  begin
    FIPv6.IPv6Address := FLocalIPv6s[0].IPv6Address;
    FIPv6.SubnetMask := FLocalIPv6s[0].SubnetMask;
    FIPv6.PrefixLength := FLocalIPv6s[0].PrefixLength;
  end
  else if IPv6s > 1 then // IF more than one, do not use Loopback as default
  begin
    for I := 0 to IPv6s - 1 do
    begin
      if Int128ToIPv6(FLocalIPv6s[I].IPv6Address) <> '0000:0000:0000:0000:0000:000:0000:0001' then
      begin
        FIPv6.IPv6Address := FLocalIPv6s[I].IPv6Address;
        FIPv6.SubnetMask := FLocalIPv6s[I].SubnetMask;
        FIPv6.PrefixLength := FLocalIPv6s[I].PrefixLength;
        Break;
      end;
      if UInt128IsZero(FIPv6.IPv6Address) then
      begin
        FIPv6.IPv6Address := FLocalIPv6s[0].IPv6Address;
        FIPv6.SubnetMask := FLocalIPv6s[0].SubnetMask;
        FIPv6.PrefixLength := FLocalIPv6s[0].PrefixLength;
      end;
    end;

    for I := 0 to IPv6s - 1 do
    begin
      if Int128ToIPv6(FLocalIPv6s[I].IPv6Address) = '0000:0000:0000:0000:0000:000:0000:0001' then
      begin
        if I <> IPs - 1 then
        begin
          // �����һ��������Ҳ���ǰѻ��ص�ַ�����
          Move(FLocalIPv6s[I], aIPv6, SizeOf(TCnIPv6Info));
          Move(FLocalIPv6s[IPs - 1], FLocalIPv6s[I], SizeOf(TCnIPv6Info));
          Move(aIPv6, FLocalIPv6s[IPs - 1], SizeOf(TCnIPv6Info));
        end;
        Break;
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

function TCnIp.GetLocalIPs(Index: Integer): TCnIPInfo;
begin
  if (Index >= 0) and (Index < GetLocalIPCount) then
    Result := FLocalIPs[Index]
  else
    raise EListError.CreateFmt(SListIndexError, [Index]);
end;

function TCnIp.GetIPAddress: string;
begin
  Result := IntToIP(FIP.IPAddress);
end;

function TCnIp.GetIpNum(const aStartIP, aEndIP: string): Integer;
begin
  Result := IPToInt(aEndIP) - IPToInt(aStartIP);
end;

function CheckIpNote(aNote: string): Byte;
var
  iNote: Integer;
begin
  iNote := StrToInt(aNote);
  if (iNote < 0) or (iNote > MAXIPNOTE) then
    raise ECnIPException.Create(aNote + SCnErrorAddrRang);
  Result := iNote;
end;

function CheckIpv6Note(aNote: string): Word;
var
  iNote: Integer;
begin
  iNote := HexToInt(aNote);

  if (iNote < 0) or (iNote > MAXIPV6NOTE) then
    raise ECnIPException.Create(aNote + SCnErrorAddrRang);
  Result := iNote;
end;

class function TCnIp.GetIPNotes(const aIP: string; var aResult: TCnIPv4Notes): Boolean;
var
  iPos: Integer;
  sIP: string;
begin
  iPos := Pos(IPJOIN, aIP);
  aResult[1] := CheckIpNote(Copy(aIP, 1, iPos - 1));
  sIP := Copy(aIP, iPos + 1, MaxInt);
  iPos := Pos(IPJOIN, sIP);
  aResult[2] := CheckIpNote(Copy(sIP, 1, iPos - 1));
  sIP := Copy(sIP, iPos + 1, MaxInt);
  iPos := Pos(IPJOIN, sIP);
  aResult[3] := CheckIpNote(Copy(sIP, 1, iPos - 1));
  aResult[4] := CheckIpNote(Copy(sIP, iPos + 1, MaxInt));
  Result := aResult[1] > 0;
end;

class function TCnIp.IntToIP(const aIP: Cardinal): string;
var
  Notes: TCnIPv4Notes;
begin
  Notes[1] := aIP and IPNOTE1 shr 24;
  Notes[2] := aIP and IPNOTE2 shr 16;
  Notes[3] := aIP and IPNOTE3 shr 8;
  Notes[4] := aIP and IPNOTE4;
  Result := Format(IPADDRFORMAT, [Notes[1], Notes[2], Notes[3], Notes[4]]);
end;

class function TCnIp.IPToInt(const aIP: string): Cardinal;
var
  Notes: TCnIPv4Notes;
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

class function TCnIp.IPTypeCheck(const aIP: string): TCnIPv4NetType;
var
  Notes: TCnIPv4Notes;
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
  IpNote, MaskNote: TCnIPv4Notes;
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
  Lib := LoadLibrary('rpcrt4.dll'); // �÷���ֻ�� Win32 ����Ч
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
  // Win32 ʧ�ܺ󣬻� Win64 �£��� MAC ��ַ
  FillChar(NCB, SizeOf(NCB), 0);
  NCB.ncb_command := AnsiChar(NCBENUM);
  NCB.ncb_buffer := @AdapterList;
  NCB.ncb_length := SizeOf(AdapterList);
  Netbios(@NCB);
  if Byte(AdapterList.length) > 0 then
    Result := GetAdapterInfo(AdapterList.lana[0])
{$ELSE}
  // POSIX ���� MAC ��ַ
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

      // �� IP��������� 127.0.0.1 ������
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
        // Sdl^.sdl_data[Sdl^.sdl_nlen] ��ʼ�� Sdl^.sdl_alen �ֽھ��� MAC ��ַ
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
1. ����һ�� Socket
2. ���� WSAIOCtl ��ȡ��������
3. ��ÿ�����ӣ���ȡ���� IP�����롢�㲥��ַ��״̬
4. ����Ϣ��䵽 IP ������
5. �����ر� Socket
--------------------------------------------------------------------}
class function TCnIp.EnumLocalIP(var aLocalIP: TCnIPGroup): Integer;
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
    skLocal := socket(AF_INET, SOCK_STREAM, 0); // Open a socket��AF_INET 2
    if (skLocal = INVALID_SOCKET) then
      Exit;

    try // Call WSAIoCtl
      PtrA := @BytesReturned;
      if (WSAIoctl(skLocal, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA,
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
  while Pif <> nil do // ��ͳ�Ʒ�������������
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

class function TCnIp.EnumLocalIPv6(var aLocalIPv6: TCnIPv6Group): Integer;
{$IFDEF MSWINDOWS}
const
  GAA_FLAG_INCLUDE_PREFIX = $0010;
  MAX_ADAPTER_ADDRESS_LENGTH = 8;
var
  Ret: DWORD;
  BufLen: ULONG;
  Adapter, AdapterList: PIP_ADAPTER_ADDRESSES;
  UnicastAddr: PIP_ADAPTER_UNICAST_ADDRESS;
  Addr6: Psockaddr_in6;

  function IN6_IS_ADDR_LOOPBACK(const A: in6_addr): Boolean;
  begin
    Result := CompareMem(@A, @in6addr_loopback, SizeOf(IN6_ADDR));
  end;

begin
  Result := 0;
  SetLength(aLocalIPv6, 0);
  BufLen := 0;
  Ret := GetAdaptersAddresses(AF_INET6, GAA_FLAG_INCLUDE_PREFIX, nil, nil, @BufLen);
  if Ret <> ERROR_BUFFER_OVERFLOW then Exit;

  GetMem(AdapterList, BufLen);
  try
    Ret := GetAdaptersAddresses(AF_INET6, GAA_FLAG_INCLUDE_PREFIX, nil, AdapterList, @BufLen);
    if Ret <> ERROR_SUCCESS then Exit;

    Adapter := AdapterList;
    while Adapter <> nil do
    begin
      if {(Adapter.OperStatus = Ord(IfOperStatusUp)) and } (Adapter.FirstUnicastAddress <> nil) then
      begin
        UnicastAddr := Adapter.FirstUnicastAddress;
        while UnicastAddr <> nil do
        begin
          if (UnicastAddr^.Address.lpSockaddr <> nil) and
             (UnicastAddr^.Address.lpSockaddr.sin_family = AF_INET6) then
          begin
            Addr6 := Psockaddr_in6(UnicastAddr^.Address.lpSockaddr);
            if not IN6_IS_ADDR_LOOPBACK(Addr6.sin6_addr) then
            begin
              SetLength(aLocalIPv6, Result + 1);
              Move(Addr6^.sin6_addr, aLocalIPv6[Result].IPv6Address, SizeOf(in6_addr));
              aLocalIPv6[Result].PrefixLength := UnicastAddr^.OnLinkPrefixLength;

              // �������루ǰ׺����ת����
              FillChar(aLocalIPv6[Result].SubnetMask[0], SizeOf(TCnIPv6NetMask), $FF);
              MemoryShiftLeft(@aLocalIPv6[Result].SubnetMask[0], @aLocalIPv6[Result].SubnetMask[0],
                SizeOf(TCnIPv6NetMask), 128 - UnicastAddr^.OnLinkPrefixLength);

              aLocalIPv6[Result].UpState := (Adapter.OperStatus = Ord(IfOperStatusUp));
              Inc(Result);
            end;
          end;
          UnicastAddr := UnicastAddr.Next;
        end;
      end;
      Adapter := Adapter.Next;
    end;
  finally
    FreeMem(AdapterList);
  end;
{$ELSE}
var
  Pif, OldPif: Pifaddrs;
  Addr6: Psockaddr_in6;
begin
  Result := 0;
  SetLength(aLocalIPv6, 0);
  if getifaddrs(Pif) <> 0 then Exit;
  OldPif := Pif;
  try
    while Pif <> nil do
    begin
      if (Pif^.ifa_addr <> nil) and (Pif^.ifa_addr.sa_family = AF_INET6) then
      begin
        Addr6 := Psockaddr_in6(Pif^.ifa_addr);
        if not IN6_IS_ADDR_LOOPBACK(Addr6^.sin6_addr) then
        begin
          SetLength(aLocalIPv6, Result + 1);
          Move(Addr6^.sin6_addr, aLocalIPv6[Result].IPv6Address, SizeOf(in6_addr));

          // �������루��� ifa_netmask ��ȡ��
          if Pif^.ifa_netmask <> nil then
          begin
            Addr6 := Psockaddr_in6(Pif^.ifa_netmask);
            Move(Addr6^.sin6_addr, aLocalIPv6[Result].SubnetMask, SizeOf(in6_addr));
          end;

          // ���� SubnetMask ���������ٸ� 1 ��Ϊ PrefixLength
          aLocalIPv6[Result].PrefixLength := MemoryGetLowBits(@aLocalIPv6[Result].SubnetMask[0], SizeOf(TCnIPv6NetMask));
          aLocalIPv6[Result].UpState := (Pif^.ifa_flags and IFF_UP) <> 0;
          Inc(Result);
        end;
      end;
      Pif := Pif^.ifa_next;
    end;
  finally
    freeifaddrs(OldPif);
  end;
{$ENDIF}
end;

function TCnIp.GetIPv6Address: string;
begin
  Result := Int128ToIPv6(FIPv6.IPv6Address);
end;

function TCnIp.GetIPv6SubnetMask: string;
begin
  Result := DataToHex(@FIPv6.SubnetMask[0], SizeOf(TCnIPv6NetMask));
end;

function TCnIp.GetIPv6PrefixLength: Integer;
begin
  Result := FIPv6.PrefixLength;
end;

procedure TCnIp.SetIPv6PrefixLength(const Value: Integer);
begin
  FIPv6.PrefixLength := Value;
end;

function TCnIp.GetLocalIPv6Count: Integer;
begin
  Result := Length(FLocalIPv6s) - 1;
end;

function TCnIp.GetLocalIPv6s(Index: Integer): TCnIPv6Info;
begin
  if (Index >= 0) and (Index < GetLocalIPv6Count) then
    Result := FLocalIPv6s[Index]
  else
    raise EListError.CreateFmt(SListIndexError, [Index]);
end;

class function TCnIp.Int128ToIPv6(var aIPv6: TCnUInt128): string;
var
  R: TCnIPv6Notes;
begin
  Move(aIPv6, R[1], SizeOf(TCnUInt128));
  Result := Format('%4.4x:%4.4x:%4.4x:%4.4x:%4.4x:%4.4x:%4.4x:%4.4x',
    [UInt16HostToNetwork(R[1]), UInt16HostToNetwork(R[2]), UInt16HostToNetwork(R[3]),
    UInt16HostToNetwork(R[4]), UInt16HostToNetwork(R[5]), UInt16HostToNetwork(R[6]),
    UInt16HostToNetwork(R[7]), UInt16HostToNetwork(R[8])]);
end;

class function TCnIp.IPv6ToInt128(const aIPv6: string): TCnUInt128;
var
  R: TCnIPv6Notes;
begin
  if GetIPv6Notes(aIPv6, R) then
    Move(R[1], Result, SizeOf(TCnUInt128))
  else
    raise ECnIPException.Create(aIPv6 + SCnErrorAddress);
end;

procedure TCnIp.SetIPv6Address(const Value: string);
begin
  FIPv6.IPv6Address := IPv6ToInt128(Value);
end;

// �Ĳ����������˸�ð�ŷָ��ģ�����ð�ŵģ�������� /64 ��
class function TCnIp.GetIPv6Notes(const aIPv6: string;
  var aResult: TCnIPv6Notes): Boolean;
var
  SL: TStringList;
  I, ZC, E: Integer;
begin
  SL := TStringList.Create;
  try
    I := Pos('/', aIPv6);
    if I > 1 then // �����������������ȥ��
      SplitStringByColon(Copy(aIPv6, 1, I - 1), SL)
    else
      SplitStringByColon(aIPv6, SL);

    // ���� :: ��ʾ������ 0��ȷ��չ��Ϊ 8 ��
    E := SL.IndexOf('');
    if E > -1 then
    begin
      ZC := 8 - SL.Count;
      if ZC > 0 then
      begin
        SL.Delete(E);
        for I := 0 to ZC do
          SL.Insert(E, '0000');
      end;
    end;

    Result := SL.Count = 8;
    if Result then
    begin
      // ÿһ��ת���� 16 ����
      for I := 0 to SL.Count - 1 do
        aResult[I + 1] := UInt16NetworkToHost(CheckIpv6Note(SL[I]));
    end;
  finally
    SL.Free;
  end;
end;

{$IFDEF MSWINDOWS}

initialization
  FillChar(in6addr_loopback.s6_bytes, SizeOf(IN6_ADDR), 0);
  in6addr_loopback.s6_bytes[15] := 1;

  InitWSAIoctl;

finalization
  FreeWSAIoctl;

{$ENDIF}
end.

