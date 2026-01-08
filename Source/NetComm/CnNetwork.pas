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

unit CnNetwork;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯组件包网络结构定义单元
* 单元作者：CnPack 开发组
* 备    注：需要支持 Delphi 及 FPC 编译器以及 Windows、Mac、Linux 平台
* 开发平台：PWinXP + Delphi XE
* 兼容测试：PWinXP/7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.12.20 V1.2
*                增加一批设置函数
*           2022.07.23 V1.1
*                更改单元名称
*           2016.10.05 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative {$IFNDEF MSWINDOWS}
  {$IFDEF FPC}, Sockets{$ELSE}, Posix.NetinetIn{$ENDIF} {$ENDIF};

const
  {* IP 包头中的版本字段的定义}
  CN_IP_VERSION_V4                          = 4;
  CN_IP_VERSION_V6                          = 6;
  CN_IP_HEADER_LENGTH_V4                    = 5;

  {* IP 包头中 Type of Service 字段中的 Precedence 定义}
  CN_IP_TOS_PRECEDENCE_ROUTINE              = 0;
  CN_IP_TOS_PRECEDENCE_PRIORITY             = 1;
  CN_IP_TOS_PRECEDENCE_IMMEDIATE            = 2;
  CN_IP_TOS_PRECEDENCE_FLASH                = 3;
  CN_IP_TOS_PRECEDENCE_FLASH_OVERRIDE       = 4;
  CN_IP_TOS_PRECEDENCE_CRITIC_ECP           = 5;
  CN_IP_TOS_PRECEDENCE_INTERNETWORK_CONTROL = 6;
  CN_IP_TOS_PRECEDENCE_NETWORK_CONTROL      = 7;

  {* IP 包头中 Type of Service 字段中的其他服务类型标记定义}
  CN_IP_TOS_PRECEDENCE_MASK                 = $E0;
  CN_IP_TOS_DELAY_MASK                      = $10;
  CN_IP_TOS_THROUGHPUT_MASK                 = $8;
  CN_IP_TOS_RELIABILITY_MASK                = $4;

  {* IP 包头中 Fragment Flag 字段中的分片标记定义}
  CN_IP_FLAG_DONT_FRAGMENT_WORD_MASK        = $4000;
  CN_IP_FLAG_MORE_FRAGMENT_WORD_MASK        = $2000;
  CN_IP_FLAG_FRAGMENT_OFFSET_WORD_MASK      = $1FFF;

  {* IP 包头中协议字段的定义，参考自维基百科}
  CN_IP_PROTOCOL_HOPOPT          = $00; // IPv6 Hop-by-Hop Option
  CN_IP_PROTOCOL_ICMP            = $01; // *** Internet Control Message Protocol
  CN_IP_PROTOCOL_IGMP            = $02; // *** Internet Group Management Protocol
  CN_IP_PROTOCOL_GGP             = $03; // Gateway-to-Gateway Protocol
  CN_IP_PROTOCOL_IP_IN_IP        = $04; // IP in IP?(encapsulation)
  CN_IP_PROTOCOL_ST              = $05; // Internet Stream Protocol
  CN_IP_PROTOCOL_TCP             = $06; // *** Transmission Control Protocol
  CN_IP_PROTOCOL_CBT             = $07; // Core-based trees
  CN_IP_PROTOCOL_EGP             = $08; // Exterior Gateway Protocol
  CN_IP_PROTOCOL_IGP             = $09; // Interior Gateway Protocol
  CN_IP_PROTOCOL_BBN_RCC_MON     = $0A; // BBN RCC Monitoring
  CN_IP_PROTOCOL_NVP_II          = $0B; // Network Voice Protocol
  CN_IP_PROTOCOL_PUP             = $0C; // Xerox PUP
  CN_IP_PROTOCOL_ARGUS           = $0D; // ARGUS
  CN_IP_PROTOCOL_EMCON           = $0E; // EMCON
  CN_IP_PROTOCOL_XNET            = $0F; // Cross Net Debugger
  CN_IP_PROTOCOL_CHAOS           = $10; // Chaos
  CN_IP_PROTOCOL_UDP             = $11; // *** User Datagram Protocol
  CN_IP_PROTOCOL_MUX             = $12; // Multiplexing
  CN_IP_PROTOCOL_DCN_MEAS        = $13; // DCN Measurement Subsystems
  CN_IP_PROTOCOL_HMP             = $14; // Host Monitoring Protocol
  CN_IP_PROTOCOL_PRM             = $15; // Packet Radio Measurement
  CN_IP_PROTOCOL_XNS_IDP         = $16; // XEROX NS IDP
  CN_IP_PROTOCOL_TRUNK_1         = $17; // Trunk-1
  CN_IP_PROTOCOL_TRUNK_2         = $18; // Trunk-2
  CN_IP_PROTOCOL_LEAF_1          = $19; // Leaf-1
  CN_IP_PROTOCOL_LEAF_2          = $1A; // Leaf-2
  CN_IP_PROTOCOL_RDP             = $1B; // Reliable Datagram Protocol
  CN_IP_PROTOCOL_IRTP            = $1C; // Internet Reliable Transaction Protocol
  CN_IP_PROTOCOL_ISO_TP4         = $1D; // ISO Transport Protocol Class 4
  CN_IP_PROTOCOL_NETBLT          = $1E; // Bulk Data Transfer Protocol
  CN_IP_PROTOCOL_MFE_NSP         = $1F; // MFE Network Services Protocol
  CN_IP_PROTOCOL_MERIT_INP       = $20; // MERIT Internodal Protocol
  CN_IP_PROTOCOL_DCCP            = $21; // Datagram Congestion Control Protocol
  CN_IP_PROTOCOL_3PC             = $22; // Third Party Connect Protocol
  CN_IP_PROTOCOL_IDPR            = $23; // Inter-Domain Policy Routing Protocol
  CN_IP_PROTOCOL_XTP             = $24; // Xpress Transport Protocol
  CN_IP_PROTOCOL_DDP             = $25; // Datagram Delivery Protocol
  CN_IP_PROTOCOL_IDPR_CMTP       = $26; // IDPR Control Message Transport Protocol
  CN_IP_PROTOCOL_TP_PP           = $27; // TP++ Transport Protocol
  CN_IP_PROTOCOL_IL              = $28; // IL Transport Protocol
  CN_IP_PROTOCOL_IPV6            = $29; // IPv6 Encapsulation
  CN_IP_PROTOCOL_SDRP            = $2A; // Source Demand Routing Protocol
  CN_IP_PROTOCOL_IPV6_ROUTE      = $2B; // Routing Header for?IPv6
  CN_IP_PROTOCOL_IPV6_FRAG       = $2C; // Fragment Header for?IPv6
  CN_IP_PROTOCOL_IDRP            = $2D; // Inter-Domain Routing Protocol
  CN_IP_PROTOCOL_RSVP            = $2E; // Resource Reservation Protocol
  CN_IP_PROTOCOL_GRE             = $2F; // Generic Routing Encapsulation
  CN_IP_PROTOCOL_MHRP            = $30; // Mobile Host Routing Protocol
  CN_IP_PROTOCOL_BNA             = $31; // BNA
  CN_IP_PROTOCOL_ESP             = $32; // Encapsulating Security Payload
  CN_IP_PROTOCOL_AH              = $33; // Authentication Header
  CN_IP_PROTOCOL_I_NLSP          = $34; // Integrated Net Layer Security Protocol
  CN_IP_PROTOCOL_SWIPE           = $35; // SwIPe
  CN_IP_PROTOCOL_NARP            = $36; // NBMA Address Resolution Protocol
  CN_IP_PROTOCOL_MOBILE          = $37; // IP Mobility?(Min Encap)
  CN_IP_PROTOCOL_TLSP            = $38; // Transport Layer Security Protocol
  CN_IP_PROTOCOL_SKIP            = $39; // Simple Key-Management for Internet Protocol
  CN_IP_PROTOCOL_IPV6_ICMP       = $3A; // ICMP for IPv6
  CN_IP_PROTOCOL_IPV6_NONXT      = $3B; // No Next Header for?IPv6
  CN_IP_PROTOCOL_IPV6_OPTS       = $3C; // Destination Options for?IPv6
  CN_IP_PROTOCOL_ANY_HOST        = $3D; // Any host internal protocol
  CN_IP_PROTOCOL_CFTP            = $3E; // CFTP
  CN_IP_PROTOCOL_ANY_LOCAL       = $3F; // Any local network
  CN_IP_PROTOCOL_SAT_EXPAK       = $40; // SATNET and Backroom EXPAK
  CN_IP_PROTOCOL_KRYPTOLAN       = $41; // Kryptolan
  CN_IP_PROTOCOL_RVD             = $42; // MIT?Remote Virtual Disk Protocol
  CN_IP_PROTOCOL_IPPC            = $43; // Internet Pluribus Packet Core
  CN_IP_PROTOCOL_ANY_DFS         = $44; // Any distributed file system
  CN_IP_PROTOCOL_SAT_MON         = $45; // SATNET Monitoring
  CN_IP_PROTOCOL_VISA            = $46; // VISA Protocol
  CN_IP_PROTOCOL_IPCU            = $47; // Internet Packet Core Utility
  CN_IP_PROTOCOL_CPNX            = $48; // Computer Protocol Network Executive
  CN_IP_PROTOCOL_CPHB            = $49; // Computer Protocol Heart Beat
  CN_IP_PROTOCOL_WSN             = $4A; // Wang Span Network
  CN_IP_PROTOCOL_PVP             = $4B; // Packet Video Protocol
  CN_IP_PROTOCOL_BR_SAT_MON      = $4C; // Backroom SATNET Monitoring
  CN_IP_PROTOCOL_SUN_ND          = $4D; // SUN ND PROTOCOL-Temporary
  CN_IP_PROTOCOL_WB_MON          = $4E; // WIDEBAND Monitoring
  CN_IP_PROTOCOL_WB_EXPAK        = $4F; // WIDEBAND EXPAK
  CN_IP_PROTOCOL_ISO_IP          = $50; // International Organization for Standardization Internet Protocol
  CN_IP_PROTOCOL_VMTP            = $51; // Versatile Message Transaction Protocol
  CN_IP_PROTOCOL_SECURE_VMTP     = $52; // Secure Versatile Message Transaction Protocol
  CN_IP_PROTOCOL_VINES           = $53; // VINES
  CN_IP_PROTOCOL_TTP             = $54; // TTP
  CN_IP_PROTOCOL_IPTM            = $54; // Internet Protocol Traffic Manager
  CN_IP_PROTOCOL_NSFNET_IGP      = $55; // NSFNET-IGP
  CN_IP_PROTOCOL_DGP             = $56; // Dissimilar Gateway Protocol
  CN_IP_PROTOCOL_TCF             = $57; // TCF
  CN_IP_PROTOCOL_EIGRP           = $58; // EIGRP
  CN_IP_PROTOCOL_OSPF            = $59; // Open Shortest Path First
  CN_IP_PROTOCOL_SPRITE_RPC      = $5A; // Sprite RPC Protocol
  CN_IP_PROTOCOL_LARP            = $5B; // Locus Address Resolution Protocol
  CN_IP_PROTOCOL_MTP             = $5C; // Multicast Transport Protocol
  CN_IP_PROTOCOL_AX_25           = $5D; // AX.25
  CN_IP_PROTOCOL_OS              = $5E; // KA9Q NOS compatible IP over IP tunneling
  CN_IP_PROTOCOL_MICP            = $5F; // Mobile Internetworking Control Protocol
  CN_IP_PROTOCOL_SCC_SP          = $60; // Semaphore Communications Sec. Pro
  CN_IP_PROTOCOL_ETHERIP         = $61; // Ethernet-within-IP Encapsulation
  CN_IP_PROTOCOL_ENCAP           = $62; // Encapsulation Header
  CN_IP_PROTOCOL_ANY_PRIVATE     = $63; // Any private encryption scheme
  CN_IP_PROTOCOL_GMTP            = $64; // GMTP
  CN_IP_PROTOCOL_IFMP            = $65; // Ipsilon Flow Management Protocol
  CN_IP_PROTOCOL_PNNI            = $66; // PNNI over IP
  CN_IP_PROTOCOL_PIM             = $67; // Protocol Independent Multicast
  CN_IP_PROTOCOL_ARIS            = $68; // IBM's ARIS (Aggregate Route IP Switching) Protocol
  CN_IP_PROTOCOL_SCPS            = $69; // SCPS (Space Communications Protocol Standards)
  CN_IP_PROTOCOL_QNX             = $6A; // QNX
  CN_IP_PROTOCOL_A_N             = $6B; // Active Networks
  CN_IP_PROTOCOL_IPCOMP          = $6C; // IP Payload Compression Protocol
  CN_IP_PROTOCOL_SNP             = $6D; // Sitara Networks Protocol
  CN_IP_PROTOCOL_COMPAQ_PEER     = $6E; // Compaq Peer Protocol
  CN_IP_PROTOCOL_IPX_IN_IP       = $6F; // IPX in IP
  CN_IP_PROTOCOL_VRRP            = $70; // Virtual Router Redundancy Protocol,?Common Address Redundancy Protocol?(not?IANAassigned)
  CN_IP_PROTOCOL_PGM             = $71; // PGM Reliable Transport Protocol
  CN_IP_PROTOCOL_ANY_0HOP        = $72; // Any 0-hop protocol
  CN_IP_PROTOCOL_L2TP            = $73; // Layer Two Tunneling Protocol Version 3
  CN_IP_PROTOCOL_DDX             = $74; // D-II Data Exchange (DDX)
  CN_IP_PROTOCOL_IATP            = $75; // Interactive Agent Transfer Protocol
  CN_IP_PROTOCOL_STP             = $76; // Schedule Transfer Protocol
  CN_IP_PROTOCOL_SRP             = $77; // SpectraLink Radio Protocol
  CN_IP_PROTOCOL_UTI             = $78; // Universal Transport Interface Protocol
  CN_IP_PROTOCOL_SMP             = $79; // Simple Message Protocol
  CN_IP_PROTOCOL_SM              = $7A; // Simple Multicast Protocol
  CN_IP_PROTOCOL_PTP             = $7B; // Performance Transparency Protocol
  CN_IP_PROTOCOL_IS_IS_OVER_IPV4 = $7C; // Intermediate System to Intermediate System (IS-IS) Protocol?over?IPv4
  CN_IP_PROTOCOL_FIRE            = $7D; // Flexible Intra-AS Routing Environment
  CN_IP_PROTOCOL_CRTP            = $7E; // Combat Radio Transport Protocol
  CN_IP_PROTOCOL_CRUDP           = $7F; // Combat Radio User Datagram
  CN_IP_PROTOCOL_SSCOPMCE        = $80; // Service-Specific Connection-Oriented Protocol in a Multilink and Connectionless Environment
  CN_IP_PROTOCOL_IPLT            = $81; // IPLT
  CN_IP_PROTOCOL_SPS             = $82; // Secure Packet Shield
  CN_IP_PROTOCOL_PIPE            = $83; // Private IP Encapsulation within IP
  CN_IP_PROTOCOL_SCTP            = $84; // Stream Control Transmission Protocol
  CN_IP_PROTOCOL_FC              = $85; // Fibre Channel
  CN_IP_PROTOCOL_RSVP_E2E_IGNORE = $86; // Reservation Protocol (RSVP) End-to-End Ignore                                              
  CN_IP_PROTOCOL_MOBILITY_HEADER = $87; // Mobility Extension Header for IPv6                                                         
  CN_IP_PROTOCOL_UDPLITE         = $88; // Lightweight User Datagram Protocol
  CN_IP_PROTOCOL_MPLS_IN_IP      = $89; // Multiprotocol Label Switching?Encapsulated in IP
  CN_IP_PROTOCOL_MANET           = $8A; // MANET?Protocols
  CN_IP_PROTOCOL_HIP             = $8B; // Host Identity Protocol
  CN_IP_PROTOCOL_SHIM6           = $8C; // Site Multihoming by IPv6 Intermediation
  CN_IP_PROTOCOL_WESP            = $8D; // Wrapped Encapsulating Security Payload
  CN_IP_PROTOCOL_ROHC            = $8E; // Robust Header Compression
  CN_IP_PROTOCOL_RESERVE         = $FF; // Reserved

  {* TCP 包头中的标记字段的定义}
  CN_TCP_FLAG_URG_MASK = $20;
  CN_TCP_FLAG_ACK_MASK = $10;
  CN_TCP_FLAG_PSH_MASK = $8;
  CN_TCP_FLAG_RST_MASK = $4;
  CN_TCP_FLAG_SYN_MASK = $2;
  CN_TCP_FLAG_FIN_MASK = $1;

  {* ICMP 包头中的消息类型定义}
  CN_ICMP_TYPE_ECHO_REPLY                   = 0;
  CN_ICMP_TYPE_DESTINATION_UNREACHABLE      = 3;
  CN_ICMP_TYPE_SOURCE_QUENCH                = 4;
  CN_ICMP_TYPE_REDIRECT                     = 5;
  CN_ICMP_TYPE_ALTERNATE_HOST_ADDRESS       = 6;
  CN_ICMP_TYPE_ECHO                         = 8;
  CN_ICMP_TYPE_ROUTER_ADVERTISEMENT         = 9;
  CN_ICMP_TYPE_ROUTER_SOLICITATION          = 10;
  CN_ICMP_TYPE_TIME_EXCEEDED                = 11;
  CN_ICMP_TYPE_PARAMETER_PROBLEM            = 12;
  CN_ICMP_TYPE_TIMESTAMP                    = 13;
  CN_ICMP_TYPE_TIMESTAMP_REPLY              = 14;
  CN_ICMP_TYPE_INFORMATION_REQUEST          = 15;
  CN_ICMP_TYPE_INFORMATION_REPLY            = 16;
  CN_ICMP_TYPE_ADDRESS_MASK_REQUEST         = 17;
  CN_ICMP_TYPE_ADDRESS_MASK_REPLY           = 18;
  CN_ICMP_TYPE_TRACEROUTE                   = 30;
  CN_ICMP_TYPE_DATAGRAM_CONVERSION_ERROR    = 31;
  CN_ICMP_TYPE_MOBILE_HOST_REDIRECT         = 32;
  CN_ICMP_TYPE_IPV6_WHERE_ARE_YOU           = 33;
  CN_ICMP_TYPE_IPV6_I_AM_HERE               = 34;
  CN_ICMP_TYPE_MOBILE_REGISTRATION_REQUEST  = 35;
  CN_ICMP_TYPE_MOBILE_REGISTRATION_REPLY    = 36;
  CN_ICMP_TYPE_DOMAIN_NAME_REQUEST          = 37;
  CN_ICMP_TYPE_DOMAIN_NAME_REPLY            = 38;
  CN_ICMP_TYPE_SKIP                         = 39;
  CN_ICMP_TYPE_PHOTURIS                     = 40;
  CN_ICMP_TYPE_UTILIZED_BY_MOBILITY         = 41;

  {* ICMP 包头中的消息代码定义}
  CN_ICMP_CODE_NO_CODE                      = 0;

  // 目的不可达类型 CN_ICMP_TYPE_DESTINATION_UNREACHABLE
  CN_ICMP_CODE_NET_UNREACHABLE              = 0;   // Net Unreachable
  CN_ICMP_CODE_HOST_UNREACHABLE             = 1;   // Host Unreachable
  CN_ICMP_CODE_PROTOCOL_UNREACHABLE         = 2;   // Protocol Unreachable
  CN_ICMP_CODE_PORT_UNREACHABLE             = 3;   // Port Unreachable
  CN_ICMP_CODE_FRAGMENTATION_NEEDED         = 4;   // Fragmentation Needed and Don't Fragment was Set
  CN_ICMP_CODE_SOURCE_ROUTE_FAILED          = 5;   // Source Route Failed
  CN_ICMP_CODE_DEST_NETWORK_UNKNOWN         = 6;   // Destination Network Unknown
  CN_ICMP_CODE_DEST_HOST_UNKNOWN            = 7;   // Destination Host Unknown
  CN_ICMP_CODE_SOURCE_HOST_ISOLATED         = 8;   // Source Host Isolated
  CN_ICMP_CODE_NETWORK_PROHIBITED           = 9;   // Communication with Destination Network is Administratively Prohibited
  CN_ICMP_CODE_HOST_PROHIBITED              = 10;  // Communication with Destination Host is Administratively Prohibited
  CN_ICMP_CODE_NETWORK_UNREACHABLE_FOR_TOS  = 11;  // Destination Network Unreachable for Type of Service
  CN_ICMP_CODE_HOST_UNREACHABLE_FOR_TOS     = 12;  // Destination Host Unreachable for Type of Service
  CN_ICMP_CODE_COMMUNICATION_PROHIBITED     = 13;  // Communication Administratively Prohibited
  CN_ICMP_CODE_HOST_PRECEDENCE_VIOLATION    = 14;  // Host Precedence Violation
  CN_ICMP_CODE_PRECEDENCE_CUTOFF_IN_EFFECT  = 15;  // Precedence cutoff in effect

  // 重定向类型 CN_ICMP_TYPE_REDIRECT
  CN_ICMP_CODE_REDIRECT_FOR_NETWORK         = 0;
  CN_ICMP_CODE_REDIRECT_FOR_HOST            = 1;
  CN_ICMP_CODE_REDIRECT_FOR_TOS_NETWORK     = 2;
  CN_ICMP_CODE_REDIRECT_FOR_TOS_HOST        = 3;

  // 更改主机地址类型 CN_ICMP_TYPE_ALTERNATE_HOST_ADDRESS
  CN_ICMP_CODE_ALTERNATE_ADDRESS_FOR_HOST   = 0;

  // 路由公告类型 CN_ICMP_TYPE_ROUTER_ADVERTISEMENT
  CN_ICMP_CODE_NORMAL_ROUTER_ADVERTISEMENT  = 0;
  CN_ICMP_CODE_NOT_ROUTE_COMMON_TRAFFIC     = 1;

  // 超时类型 CN_ICMP_TYPE_TIME_EXCEEDED
  CN_ICMP_CODE_TTL_EXCEEDED_IN_TRANSIT      = 0;
  CN_ICMP_CODE_FRAGMENT_REASSEMBLY          = 1;

  // 参数问题类型 CN_ICMP_TYPE_PARAMETER_PROBLEM
  CN_ICMP_CODE_POINTER_INDICATES_THE_ERROR  = 0;
  CN_ICMP_CODE_MISSING_A_REQUIRED_OPTION    = 1;
  CN_ICMP_CODE_BAD_LENGTH                   = 2;

  // Photuris 类型 CN_ICMP_TYPE_PHOTURIS
  CN_ICMP_CODE_BAD_SPI                      = 0;
  CN_ICMP_CODE_AUTHENTICATION_FAILED        = 1;
  CN_ICMP_CODE_DECOMPRESSION_FAILED         = 2;
  CN_ICMP_CODE_DECRYPTION_FAILED            = 3;
  CN_ICMP_CODE_NEED_AUTHENTICATION          = 4;
  CN_ICMP_CODE_NEED_AUTHORIZATION           = 5;

  {* NTP 包中的闰秒标记定义}
  CN_NTP_LEAP_INDICATOR_NONE                = 0;   // 无闰秒
  CN_NTP_LEAP_INDICATOR_LEAP_61             = 1;   // 本分钟闰秒 61
  CN_NTP_LEAP_INDICATOR_LEAP_59             = 2;   // 本分钟闰秒 59
  CN_NTP_LEAP_INDICATOR_LEAP_NO_SYNC        = 3;   // 时钟未同步

  {* NTP 包中的版本号字段值}
  CN_NTP_VERSION_V3                         = 3;

  {* NTP 包中的模式字段值}
  CN_NTP_MODE_UNSPECIFIED                   = 0;   // 未指定
  CN_NTP_MODE_SYMMETRIC_ACTIVE              = 1;   // 对称主动
  CN_NTP_MODE_SYMMETRIC_PASSIVE             = 2;   // 对称被动
  CN_NTP_MODE_CLIENT                        = 3;   // 客户端
  CN_NTP_MODE_SERVER                        = 4;   // 服务器
  CN_NTP_MODE_BROADCAST                     = 5;   // 广播
  CN_NTP_MODE_CONTROL_MSG                   = 6;   // NTP 控制消息

  {* NTP 包中的时间戳的秒的小数部分与微秒的换算比例值}
  CN_NTP_MICRO_SEC_FRACTION                 = 4294.967296;

  {* DNS 包头中的 QR 字段值}
  CN_DNS_HEADER_TYPE_QUERY                  = 0;   // 请求
  CN_DNS_HEADER_TYPE_RESPONSE               = 1;   // 应答

  {* DNS 包头中的 OpCode 字段值}
  CN_DNS_HEADER_OPCODE_STANDARD_QUERY       = 0;   // 标准查询
  CN_DNS_HEADER_OPCODE_INVERSE_QUERY        = 1;   // 反向查询
  CN_DNS_HEADER_OPCODE_SERVER_STATUS        = 2;   // 服务器状态查询

  {* DNS 包头中的 ResponseCode 字段值}
  CN_DNS_HEADER_RCODE_NOERROR               = 0;   // 无错误
  CN_DNS_HEADER_RCODE_FORMAT_ERROR          = 1;   // 报文格式错误
  CN_DNS_HEADER_RCODE_SERVER_FAILURE        = 2;   // 服务器失败
  CN_DNS_HEADER_RCODE_NAME_ERROR            = 3;   // 名字错误
  CN_DNS_HEADER_RCODE_NOT_IMPLEMENTED       = 4;   // 没有实现
  CN_DNS_HEADER_RCODE_REFUSED               = 5;   // 拒绝

  {* DNS 包中的 TYPE 字段值，包括 QUESTION 区的 QTYPE 字段值}
  CN_DNS_TYPE_A                             = 1;   // a host address
  CN_DNS_TYPE_NS                            = 2;   // an authoritative name server
  CN_DNS_TYPE_MD                            = 3;   // a mail destination (Obsolete - use MX)
  CN_DNS_TYPE_MF                            = 4;   // a mail forwarder (Obsolete - use MX)
  CN_DNS_TYPE_CNAME                         = 5;   // the canonical name for an alias
  CN_DNS_TYPE_SOA                           = 6;   // marks the start of a zone of authority
  CN_DNS_TYPE_MB                            = 7;   // a mailbox domain name (EXPERIMENTAL)
  CN_DNS_TYPE_MG                            = 8;   // a mail group member (EXPERIMENTAL)
  CN_DNS_TYPE_MR                            = 9;   // a mail rename domain name (EXPERIMENTAL)
  CN_DNS_TYPE_NULL                          = 10;  // a null RR (EXPERIMENTAL)
  CN_DNS_TYPE_WKS                           = 11;  // a well known service description
  CN_DNS_TYPE_PTR                           = 12;  // a domain name pointer
  CN_DNS_TYPE_HINFO                         = 13;  // host information
  CN_DNS_TYPE_MINFO                         = 14;  // mailbox or mail list information
  CN_DNS_TYPE_MX                            = 15;  // mail exchange
  CN_DNS_TYPE_TXT                           = 16;  // text strings
  CN_DNS_TYPE_SRV                           = 33;  // Service 类型，RFC 2052 中新增，用于 mDNS 服务发现等场合

  CN_DNS_QTYPE_AXFR                         = 252; // A request for a transfer of an entire zone
  CN_DNS_QTYPE_MAILB                        = 253; // A request for mailbox-related records (MB, MG or MR)
  CN_DNS_QTYPE_MAILA                        = 254; // A request for mail agent RRs (Obsolete - see MX)
  CN_DNS_QTYPE_ALL                          = 255; // A request for all records

  {* DNS 包中的 CLASS 字段值，包括 QCLASS 字段值}
  CN_DNS_CLASS_IN                           = 1;   // the Internet
  CN_DNS_CLASS_CS                           = 2;   // the CSNET class (Obsolete)
  CN_DNS_CLASS_CH                           = 3;   // the CHAOS class
  CN_DNS_CLASS_HS                           = 4;   // Hesiod [Dyer 87]
  CN_DNS_QCLASS_ANY                         = 255; // any class

  {* Socks 代理协议的握手包中的版本字段的定义}
  CN_SOCKS_VERSION_V4                       = 4;
  CN_SOCKS_VERSION_V5                       = 5;

  {* Socks 代理协议的握手包中的方法字段的定义}
  CN_SOCKS_METHOD_NO_AUTH_REQUIRED          = $00; // 不需身份验证
  CN_SOCKS_METHOD_GSSAPI                    = $01; // GSSAPI 验证
  CN_SOCKS_METHOD_USERNAME_PASSWORD         = $02; // 用户名密码验证
  CN_SOCKS_METHOD_IANA_ASSIGNED_BEGIN       = $03; // IANA 分配开始
  CN_SOCKS_METHOD_IANA_ASSIGNED_END         = $7F; // IANA 分配结束
  CN_SOCKS_METHOD_RESERVED_PRIVATE_BEGIN    = $80; // 私有保留开始
  CN_SOCKS_METHOD_RESERVED_PRIVATE_END      = $FE; // 私有保留结束
  CN_SOCKS_METHOD_NO_ACCEPTABLE_METHODS     = $FF; // 无可用验证方法

  {* Socks 代理协议请求包中的命令字段的定义}
  CN_SOCKS_CMD_CONNECT                      = $01;
  CN_SOCKS_CMD_BIND                         = $02;
  CN_SOCKS_CMD_UDP                          = $03;

  {* Socks 代理协议请求包中的地址类型字段的定义}
  CN_SOCKS_ADDRESS_TYPE_IPV4                = $01;
  CN_SOCKS_ADDRESS_TYPE_DOMAINNAME          = $03;
  CN_SOCKS_ADDRESS_TYPE_IPV6                = $04;

  {* Socks 代理协议的握手包中用户名密码验证中版本字段的定义}
  CN_SOCKS_USERNAME_PASSWORD_VER            = $01;

  {* Socks 代理协议的握手包中用户名密码验证中结果字段的定义}
  CN_SOCKS_USERNAME_PASSWORD_STATUS_SUCCESS = $00; // 身份验证成功

  {* Socks 代理协议的应答包中的响应字段的定义}
  CN_SOCKS_REPLY_SUCCESS                    = $00; // 成功
  CN_SOCKS_REPLY_GENERAL_FAILURE            = $01; // 服务器错误
  CN_SOCKS_REPLY_NOT_ALLOWED                = $02; // 规则不允许
  CN_SOCKS_REPLY_NETWORK_UNREACHABLE        = $03; // 网络不可达
  CN_SOCKS_REPLY_HOST_UNREACHABLE           = $04; // 主机不可达
  CN_SOCKS_REPLY_CONNECTION_REFUSED         = $05; // 连接被拒绝
  CN_SOCKS_REPLY_TTL_EXPIRED                = $06; // TTL 过期
  CN_SOCKS_REPLY_COMMAND_NOT_SUPPORTED      = $07; // 命令不支持
  CN_SOCKS_REPLY_ADDRESS_TYPE_NOT_SUPPORTED = $08; // 地址类型不支持

  {* BGP 边界网关协议的数据类型字段的定义}
  CN_BGP_TYPE_OPEN                          = $01; // BGP 的 TCP 连接建立后的首包
  CN_BGP_TYPE_UPDATE                        = $02; // BGP 的路由更新
  CN_BGP_TYPE_NOTIFICATION                  = $03; // 出错中断
  CN_BGP_TYPE_KEEPALIVE                     = $04; // 只有头的保持连接类型
  CN_BGP_TYPE_ROUTE_REFRESH                 = $05; // 刷新路由信息

  {* BGP 边界网关协议的主错误码的定义}
  CN_BGP_ERRORCODE_HEAD_ERROE               = $01; // 包头消息错误
  CN_BGP_ERRORCODE_OPEN_ERROE               = $02; // Open 消息错误
  CN_BGP_ERRORCODE_UPDATE_ERROE             = $03; // Update 消息错误
  CN_BGP_ERRORCODE_HOLDTIMER_EXPIRED        = $04; // 超时错误
  CN_BGP_ERRORCODE_FINITE_STATE_MACHINE     = $05; // 有限状态机错误
  CN_BGP_ERRORCODE_CEASE                    = $06; // 终止

  {* BGP 边界网关协议的头子错误码的定义}
  CN_BGP_ERRORSUBCODE_HEAD_CONNECTION_NOT_SYNCHRONIZED = $01; // Connection Not Synchronized
  CN_BGP_ERRORSUBCODE_HEAD_BAD_MESSAGE_LENGTH          = $02; // BAD MESSAGE LENGTH
  CN_BGP_ERRORSUBCODE_HEAD_BAD_MESSAGE_TYPE            = $03; // Bad Message Type

  {* BGP 边界网关协议的 Open 子错误码的定义}
  CN_BGP_ERRORSUBCODE_OPEN_UNSUPPORTED_VERSION         = $01; // Unsupported Version Number
  CN_BGP_ERRORSUBCODE_OPEN_BAD_PEER_AS                 = $02; // Bad Peer AS
  CN_BGP_ERRORSUBCODE_OPEN_BAD_BGP_IDENTIFIER          = $03; // Bad BGP Identifier
  CN_BGP_ERRORSUBCODE_OPEN_UNSUPPORTED_OPTIONAL        = $04; // Unsupported Optional Parameter
  CN_BGP_ERRORSUBCODE_OPEN_DEPRECATED                  = $05; // Deprecated
  CN_BGP_ERRORSUBCODE_OPEN_UNACCEPTABLE_HOLDTIME       = $06; // Unacceptable Hold Time

  {* BGP 边界网关协议的 Update 子错误码的定义}
  CN_BGP_ERRORSUBCODE_UPDATE_MALFORMED_ATTRIBUTE_LIST            = $01; // Malformed Attribute List
  CN_BGP_ERRORSUBCODE_UPDATE_UNRECOGNIZED_WELLKNOWN_ATTRIBUTE    = $02; // Unrecognized Well-known Attribute
  CN_BGP_ERRORSUBCODE_UPDATE_MISSING_WELLKNOWN_ATTRIBUTE         = $03; // Missing Well-known Attribute
  CN_BGP_ERRORSUBCODE_UPDATE_ATTRIBUTE_FLAGS_ERROR               = $04; // Attribute Flags Error
  CN_BGP_ERRORSUBCODE_UPDATE_ATTRIBUTE_LENGTH_ERROR              = $05; // Attribute Length Error
  CN_BGP_ERRORSUBCODE_UPDATE_INVALID_ORIGIN_ATTRIBUTE            = $06; // Invalid ORIGIN Attribute
  CN_BGP_ERRORSUBCODE_UPDATE_DEPRECATED                          = $07; // Deprecated
  CN_BGP_ERRORSUBCODE_UPDATE_INVALID_NEXT_HOP_ATTRIBUTE          = $08; // Invalid NEXT_HOP Attribute
  CN_BGP_ERRORSUBCODE_UPDATE_OPTIONAL_ATTRIBUTE_ERROR            = $09; // Optional Attribute Error
  CN_BGP_ERRORSUBCODE_UPDATE_INVALID_NETWORK_FIELD               = $0A; // Invalid Network Field
  CN_BGP_ERRORSUBCODE_UPDATE_MALFORMED_AS_PATH                   = $0B; // Malformed AS_PATH

  {* TLS/SSL 各版本的版本号}
  CN_TLS_SSL_VERSION_SSL_30                                      = $0300;
  CN_TLS_SSL_VERSION_TLS_10                                      = $0301;
  CN_TLS_SSL_VERSION_TLS_11                                      = $0302;
  CN_TLS_SSL_VERSION_TLS_12                                      = $0303;
  CN_TLS_SSL_VERSION_TLS_13                                      = $0304;

  {* TLS/SSL 协议中的内容类型}
  CN_TLS_CONTENT_TYPE_HANDSHAKE                                  = 22;  // 握手协议
  CN_TLS_CONTENT_TYPE_ALERT                                      = 21;  // 告警协议
  CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC                         = 20;  // 改变密码格式协议
  CN_TLS_CONTENT_TYPE_APPLICATION_DATA                           = 23;  // 应用数据协议

  {* TLS/SSL 协议中的握手协议的报文类型}
  CN_TLS_HANDSHAKE_TYPE_HELLO_REQUEST_RESERVED                   = 0;   //
  CN_TLS_HANDSHAKE_TYPE_CLIENT_HELLO                             = 1;   //
  CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO                             = 2;   //
  CN_TLS_HANDSHAKE_TYPE_HELLO_VERIFY_REQUEST_RESERVED            = 3;   //
  CN_TLS_HANDSHAKE_TYPE_NEW_SESSION_TICKET                       = 4;   //
  CN_TLS_HANDSHAKE_TYPE_END_OF_EARLY_DATA                        = 5;   //
  CN_TLS_HANDSHAKE_TYPE_HELLO_RETRY_REQUEST_RESERVED             = 6;   //
  CN_TLS_HANDSHAKE_TYPE_ENCRYPTED_EXTENSIONS                     = 8;   //
  CN_TLS_HANDSHAKE_TYPE_CERTIFICATE                              = 11;  //
  CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED             = 12;  //
  CN_TLS_HANDSHAKE_TYPE_CERTIFICATE_REQUEST                      = 13;  //
  CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED               = 14;  //
  CN_TLS_HANDSHAKE_TYPE_CERTIFICATE_VERIFY                       = 15;  //
  CN_TLS_HANDSHAKE_TYPE_CLIENT_KEY_EXCHANGE_RESERVED             = 16;  //
  CN_TLS_HANDSHAKE_TYPE_FINISHED                                 = 20;  //
  CN_TLS_HANDSHAKE_TYPE_CERTIFICATE_URL_RESERVED                 = 21;  //
  CN_TLS_HANDSHAKE_TYPE_CERTIFICATE_STATUS_RESERVED              = 22;  //
  CN_TLS_HANDSHAKE_TYPE_SUPPLEMENTAL_DATA_RESERVED               = 23;  //
  CN_TLS_HANDSHAKE_TYPE_KEY_UPDATE                               = 24;  //
  CN_TLS_HANDSHAKE_TYPE_MESSAGE_HASH                             = 254; //

  {* TLS/SSL 协议中的告警协议的告警级别}
  CN_TLS_ALERT_LEVEL_WARNING                                     = 1;   // 警告，可记录并继续
  CN_TLS_ALERT_LEVEL_FATAL                                       = 2;   // 致命错误

  {* TLS/SSL 协议中的告警协议的错误描述代码}
  CN_TLS_ALERT_DESC_CLOSE_NOTIFY                                 = 0;   //
  CN_TLS_ALERT_DESC_UNEXPECTED_MESSAGE                           = 10;  // 收到了未知的报文
  CN_TLS_ALERT_DESC_BAD_RECORD_MAC                               = 20;  // 收到了不正确的 MAC
  CN_TLS_ALERT_DESC_DECRYPTION_FAILED_RESERVED                   = 21;  // 解密失败
  CN_TLS_ALERT_DESC_RECORD_OVERFLOW                              = 22;  // 记录溢出
  CN_TLS_ALERT_DESC_DECOMPRESSION_FAILURE_RESERVED               = 30;  // 解压缩失败
  CN_TLS_ALERT_DESC_HANDSHAKE_FAILURE                            = 40;  // 握手失败
  CN_TLS_ALERT_DESC_NO_CERTIFICATE_RESERVED                      = 41;  // 未提供证书
  CN_TLS_ALERT_DESC_BAD_CERTIFICATE                              = 42;  // 证书格式错误
  CN_TLS_ALERT_DESC_UNSUPPORTED_CERTIFICATE                      = 43;  // 证书类型不支持
  CN_TLS_ALERT_DESC_CERTIFICATE_REVOKED                          = 44;  // 证书已被废弃
  CN_TLS_ALERT_DESC_CERTIFICATE_EXPIRED                          = 45;  // 证书已过期
  CN_TLS_ALERT_DESC_CERTIFICATE_UNKNOWN                          = 46;  // 未知证书
  CN_TLS_ALERT_DESC_ILLEGAL_PARAMETER                            = 47;  // 握手报文中的字段超出范围或与其他字段不兼容
  CN_TLS_ALERT_DESC_UNKNOWN_CA                                   = 48;  // 未知 CA
  CN_TLS_ALERT_DESC_ACCESS_DENIED                                = 49;  // 拒绝访问
  CN_TLS_ALERT_DESC_DECODE_ERROR                                 = 50;  // 解码错误
  CN_TLS_ALERT_DESC_DECRYPT_ERROR                                = 51;  // 解密错误
  CN_TLS_ALERT_DESC_EXPORT_RESTRICTION_RESERVED                  = 60;  //
  CN_TLS_ALERT_DESC_PROTOCOL_VERSION                             = 70;  //
  CN_TLS_ALERT_DESC_INSUFFICIENT_SECURITY                        = 71;  //
  CN_TLS_ALERT_DESC_INTERNAL_ERROR                               = 80;  //
  CN_TLS_ALERT_DESC_INAPPROPRIATE_FALLBACK                       = 86;  //
  CN_TLS_ALERT_DESC_USER_CANCELED                                = 90;  //
  CN_TLS_ALERT_DESC_NO_RENEGOTIATION_RESERVED                    = 100; //
  CN_TLS_ALERT_DESC_MISSING_EXTENSION                            = 109; //
  CN_TLS_ALERT_DESC_UNSUPPORTED_EXTENSION                        = 110; //
  CN_TLS_ALERT_DESC_CERTIFICATE_UNOBTAINABLE_RESERVED            = 111; //
  CN_TLS_ALERT_DESC_UNRECOGNIZED_NAME                            = 112; //
  CN_TLS_ALERT_DESC_BAD_CERTIFICATE_STATUS_RESPONSE              = 113; //
  CN_TLS_ALERT_DESC_BAD_CERTIFICATE_HASH_VALUE_RESERVED          = 114; //
  CN_TLS_ALERT_DESC_UNKNOWN_PSK_IDENTITY                         = 115; //
  CN_TLS_ALERT_DESC_CERTIFICATE_REQUIRED                         = 116; //
  CN_TLS_ALERT_DESC_NO_APPLICATION_PROTOCOL                      = 120; //

  {* TLS/SSL 的 Cipher 列表，注意 DHE 的 E 是 Ephemeral 表示临时}
  {* TLS 1.3}
  CN_CIPHER_TLS_AES_128_GCM_SHA256                               = $1301;
  CN_CIPHER_TLS_AES_256_GCM_SHA384                               = $1302;
  CN_CIPHER_TLS_CHACHA20_POLY1305_SHA256                         = $1303;
  CN_CIPHER_TLS_AES_128_CCM_SHA256                               = $1304;
  CN_CIPHER_TLS_AES_128_CCM_8_SHA256                             = $1305;
  CN_CIPHER_TLS_SM4_GCM_SM3                                      = $00C6; // RFC 8998
  CN_CIPHER_TLS_SM4_CCM_SM3                                      = $00C7; // RFC 8998

  {* SSLv3，已停用}
  CN_CIPHER_DHE_RSA_AES256_SHA                                   = $0039;
  CN_CIPHER_DHE_RSA_AES128_SHA                                   = $0033;
  CN_CIPHER_SRP_RSA_AES_256_CBC_SHA                              = $C021;
  CN_CIPHER_SRP_AES_256_CBC_SHA                                  = $C020;
  CN_CIPHER_RSA_PSK_AES256_CBC_SHA                               = $0095;
  CN_CIPHER_DHE_PSK_AES256_CBC_SHA                               = $0091;
  CN_CIPHER_AES256_SHA                                           = $0035;
  CN_CIPHER_PSK_AES256_CBC_SHA                                   = $008D;
  CN_CIPHER_SRP_RSA_AES_128_CBC_SHA                              = $C01E;
  CN_CIPHER_SRP_AES_128_CBC_SHA                                  = $C01D;
  CN_CIPHER_RSA_PSK_AES128_CBC_SHA                               = $0094;
  CN_CIPHER_DHE_PSK_AES128_CBC_SHA                               = $0090;
  CN_CIPHER_AES128_SHA                                           = $002F;
  CN_CIPHER_PSK_AES128_CBC_SHA                                   = $008C;

  {* TLS 1，已停用}
  CN_CIPHER_ECDHE_ECDSA_AES256_SHA                               = $C00A;
  CN_CIPHER_ECDHE_RSA_AES256_SHA                                 = $C014;
  CN_CIPHER_ECDHE_ECDSA_AES128_SHA                               = $C009;
  CN_CIPHER_ECDHE_RSA_AES128_SHA                                 = $C013;
  CN_CIPHER_ECDHE_PSK_AES256_CBC_SHA384                          = $C038;
  CN_CIPHER_ECDHE_PSK_AES256_CBC_SHA                             = $C036;
  CN_CIPHER_RSA_PSK_AES256_CBC_SHA384                            = $00B7;
  CN_CIPHER_DHE_PSK_AES256_CBC_SHA384                            = $00B3;
  CN_CIPHER_PSK_AES256_CBC_SHA384                                = $00AF;
  CN_CIPHER_ECDHE_PSK_AES128_CBC_SHA256                          = $C037;
  CN_CIPHER_ECDHE_PSK_AES128_CBC_SHA                             = $C035;
  CN_CIPHER_RSA_PSK_AES128_CBC_SHA256                            = $00B6;
  CN_CIPHER_DHE_PSK_AES128_CBC_SHA256                            = $00B2;
  CN_CIPHER_PSK_AES128_CBC_SHA256                                = $00AE;

  {* TLS 1.2，已停用}
  CN_CIPHER_ECDHE_ECDSA_AES256_GCM_SHA384                        = $C02C;
  CN_CIPHER_ECDHE_RSA_AES256_GCM_SHA384                          = $C030;
  CN_CIPHER_DHE_RSA_AES256_GCM_SHA384                            = $009F;
  CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305                        = $CCA9;
  CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305                          = $CCA8;
  CN_CIPHER_DHE_RSA_CHACHA20_POLY1305                            = $CCAA;
  CN_CIPHER_ECDHE_ECDSA_AES128_GCM_SHA256                        = $C02B;
  CN_CIPHER_ECDHE_RSA_AES128_GCM_SHA256                          = $C02F;
  CN_CIPHER_DHE_RSA_AES128_GCM_SHA256                            = $009E;
  CN_CIPHER_ECDHE_ECDSA_AES256_SHA384                            = $C024;
  CN_CIPHER_ECDHE_RSA_AES256_SHA384                              = $C028;
  CN_CIPHER_DHE_RSA_AES256_SHA256                                = $006B;
  CN_CIPHER_ECDHE_ECDSA_AES128_SHA256                            = $C023;
  CN_CIPHER_ECDHE_RSA_AES128_SHA256                              = $C027;
  CN_CIPHER_DHE_RSA_AES128_SHA256                                = $0067;
  CN_CIPHER_RSA_PSK_AES256_GCM_SHA384                            = $00AD;
  CN_CIPHER_DHE_PSK_AES256_GCM_SHA384                            = $00AB;
  CN_CIPHER_RSA_PSK_CHACHA20_POLY1305                            = $CCAE;
  CN_CIPHER_DHE_PSK_CHACHA20_POLY1305                            = $CCAD;
  CN_CIPHER_ECDHE_PSK_CHACHA20_POLY1305                          = $CCAC;
  CN_CIPHER_AES256_GCM_SHA384                                    = $009D;
  CN_CIPHER_PSK_AES256_GCM_SHA384                                = $00A9;
  CN_CIPHER_PSK_CHACHA20_POLY1305                                = $CCAB;
  CN_CIPHER_RSA_PSK_AES128_GCM_SHA256                            = $00AC;
  CN_CIPHER_DHE_PSK_AES128_GCM_SHA256                            = $00AA;
  CN_CIPHER_AES128_GCM_SHA256                                    = $009C;
  CN_CIPHER_PSK_AES128_GCM_SHA256                                = $00A8;
  CN_CIPHER_AES256_SHA256                                        = $003D;
  CN_CIPHER_AES128_SHA256                                        = $003C;

  {* TLS/SSL 中的 ExtensionType，来自 IANA，被 RFC 3546、6006、8422、8446 等引用}
  CN_TLS_EXTENSIONTYPE_SERVER_NAME                               = 0;
  CN_TLS_EXTENSIONTYPE_MAX_FRAGMENT_LENGTH                       = 1;
  CN_TLS_EXTENSIONTYPE_CLIENT_CERTIFICATE_URL                    = 2;
  CN_TLS_EXTENSIONTYPE_TRUSTED_CA_KEYS                           = 3;
  CN_TLS_EXTENSIONTYPE_TRUNCATED_HMAC                            = 4;
  CN_TLS_EXTENSIONTYPE_STATUS_REQUEST                            = 5;
  CN_TLS_EXTENSIONTYPE_USER_MAPPING                              = 6;
  CN_TLS_EXTENSIONTYPE_CLIENT_AUTHZ                              = 7;
  CN_TLS_EXTENSIONTYPE_SERVER_AUTHZ                              = 8;
  CN_TLS_EXTENSIONTYPE_CERT_TYPE                                 = 9;
  CN_TLS_EXTENSIONTYPE_SUPPORTED_GROUPS                          = 10;
  CN_TLS_EXTENSIONTYPE_EC_POINT_FORMATS                          = 11;
  CN_TLS_EXTENSIONTYPE_SRP                                       = 12;
  CN_TLS_EXTENSIONTYPE_SIGNATURE_ALGORITHMS                      = 13;
  CN_TLS_EXTENSIONTYPE_USE_SRTP                                  = 14;
  CN_TLS_EXTENSIONTYPE_HEARTBEAT                                 = 15;
  CN_TLS_EXTENSIONTYPE_APPLICATION_LAYER_PROTOCOL_NEGOTIATION    = 16;
  CN_TLS_EXTENSIONTYPE_SIGNED_CERTIFICATE_TIMESTAMP              = 18;
  CN_TLS_EXTENSIONTYPE_CLIENT_CERTIFICATE_TYPE                   = 19;
  CN_TLS_EXTENSIONTYPE_SERVER_CERTIFICATE_TYPE                   = 20;
  CN_TLS_EXTENSIONTYPE_PADDING                                   = 21;
  CN_TLS_EXTENSIONTYPE_ENCRYPT_THEN_MAC                          = 22;
  CN_TLS_EXTENSIONTYPE_EXTENDED_MASTER_SECRET                    = 23;
  CN_TLS_EXTENSIONTYPE_TOKEN_BINDING                             = 24;
  CN_TLS_EXTENSIONTYPE_CACHED_INFO                               = 25;
  CN_TLS_EXTENSIONTYPE_TLS_LTS                                   = 26;
  CN_TLS_EXTENSIONTYPE_COMPRESS_CERTIFICATE                      = 27;
  CN_TLS_EXTENSIONTYPE_RECORD_SIZE_LIMIT                         = 28;
  CN_TLS_EXTENSIONTYPE_PWD_PROTECT                               = 29;
  CN_TLS_EXTENSIONTYPE_PWD_CLEAR                                 = 30;
  CN_TLS_EXTENSIONTYPE_PASSWORD_SALT                             = 31;
  CN_TLS_EXTENSIONTYPE_TICKET_PINNING                            = 32;
  CN_TLS_EXTENSIONTYPE_TLS_CERT_WITH_EXTERN_PSK                  = 33;
  CN_TLS_EXTENSIONTYPE_DELEGATED_CREDENTIAL                      = 34;
  CN_TLS_EXTENSIONTYPE_SESSION_TICKET                            = 35;
  CN_TLS_EXTENSIONTYPE_TLMSP                                     = 36;
  CN_TLS_EXTENSIONTYPE_TLMSP_PROXYING                            = 37;
  CN_TLS_EXTENSIONTYPE_TLMSP_DELEGATE                            = 38;
  CN_TLS_EXTENSIONTYPE_SUPPORTED_EKT_CIPHERS                     = 39;
  CN_TLS_EXTENSIONTYPE_PRE_SHARED_KEY                            = 41;
  CN_TLS_EXTENSIONTYPE_EARLY_DATA                                = 42;
  CN_TLS_EXTENSIONTYPE_SUPPORTED_VERSIONS                        = 43;
  CN_TLS_EXTENSIONTYPE_COOKIE                                    = 44;
  CN_TLS_EXTENSIONTYPE_PSK_KEY_EXCHANGE_MODES                    = 45;
  CN_TLS_EXTENSIONTYPE_CERTIFICATE_AUTHORITIES                   = 47;
  CN_TLS_EXTENSIONTYPE_OID_FILTERS                               = 48;
  CN_TLS_EXTENSIONTYPE_POST_HANDSHAKE_AUTH                       = 49;
  CN_TLS_EXTENSIONTYPE_SIGNATURE_ALGORITHMS_CERT                 = 50;
  CN_TLS_EXTENSIONTYPE_KEY_SHARE                                 = 51;
  CN_TLS_EXTENSIONTYPE_TRANSPARENCY_INFO                         = 52;
  CN_TLS_EXTENSIONTYPE_CONNECTION_ID                             = 54;
  CN_TLS_EXTENSIONTYPE_EXTERNAL_ID_HASH                          = 55;
  CN_TLS_EXTENSIONTYPE_EXTERNAL_SESSION_ID                       = 56;
  CN_TLS_EXTENSIONTYPE_QUIC_TRANSPORT_PARAMETERS                 = 57;
  CN_TLS_EXTENSIONTYPE_TICKET_REQUEST                            = 58;
  CN_TLS_EXTENSIONTYPE_DNSSEC_CHAIN                              = 59;
  CN_TLS_EXTENSIONTYPE_SEQUENCE_NUMBER_ENCRYPTION_ALGORITHMS     = 60;
  CN_TLS_EXTENSIONTYPE_RRC                                       = 61;
  CN_TLS_EXTENSIONTYPE_TLS_FLAGS                                 = 62;
  CN_TLS_EXTENSIONTYPE_ECH_OUTER_EXTENSIONS                      = 64768;
  CN_TLS_EXTENSIONTYPE_ENCRYPTED_CLIENT_HELLO                    = 65037;
  CN_TLS_EXTENSIONTYPE_RENEGOTIATION_INFO                        = 65281;

  {* TLS/SSL 中的 Extension 中的 Server Name Indication 中的 NameType，来自 RFC 6066}
  CN_TLS_EXTENSION_NAMETYPE_HOSTNAME                             = 0;

  {* TLS/SSL 中的 Extension 中的 signature_algorithms 字段，来自 RFC8446}
  CN_TLS_SIGN_ALG_RSA_PKCS1_SHA256                               = $0401;
  CN_TLS_SIGN_ALG_RSA_PKCS1_SHA384                               = $0501;
  CN_TLS_SIGN_ALG_RSA_PKCS1_SHA512                               = $0601;
  CN_TLS_SIGN_ALG_ECDSA_SECP256R1_SHA256                         = $0403;
  CN_TLS_SIGN_ALG_ECDSA_SECP384R1_SHA384                         = $0503;
  CN_TLS_SIGN_ALG_ECDSA_SECP521R1_SHA512                         = $0603;
  CN_TLS_SIGN_ALG_RSA_PSS_RSAE_SHA256                            = $0804;
  CN_TLS_SIGN_ALG_RSA_PSS_RSAE_SHA384                            = $0805;
  CN_TLS_SIGN_ALG_RSA_PSS_RSAE_SHA512                            = $0806;
  CN_TLS_SIGN_ALG_ED25519                                        = $0807;
  CN_TLS_SIGN_ALG_ED448                                          = $0808;
  CN_TLS_SIGN_ALG_RSA_PSS_PSS_SHA256                             = $0809;
  CN_TLS_SIGN_ALG_RSA_PSS_PSS_SHA384                             = $080A;
  CN_TLS_SIGN_ALG_RSA_PSS_PSS_SHA512                             = $080B;
  CN_TLS_SIGN_ALG_RSA_PKCS1_SHA1                                 = $0201;
  CN_TLS_SIGN_ALG_ECDSA_SHA1                                     = $0203;

  {* TLS/SSL 中的 Extension 中的椭圆曲线 Supported Groups 类型，来自 RFC 8446}
  CN_TLS_NAMED_GROUP_SECP256R1                                   = $0017;
  CN_TLS_NAMED_GROUP_SECP384R1                                   = $0018;
  CN_TLS_NAMED_GROUP_SECP521R1                                   = $0019;
  CN_TLS_NAMED_GROUP_X25519                                      = $001D;
  CN_TLS_NAMED_GROUP_X448                                        = $001E;
  CN_TLS_NAMED_GROUP_FFDHE2048                                   = $0100;
  CN_TLS_NAMED_GROUP_FFDHE3072                                   = $0101;
  CN_TLS_NAMED_GROUP_FFDHE4096                                   = $0102;
  CN_TLS_NAMED_GROUP_FFDHE6144                                   = $0103;
  CN_TLS_NAMED_GROUP_FFDHE8192                                   = $0104;

  {* TLS/SSL 中的 Extension 中的椭圆曲线点的格式类型，来自 RFC 8446}
  CN_TLS_EC_POINT_FORMATS_UNCOMPRESSED                           = 0;

  {* TLS/SSL 中的 Server Key Exchange 中的椭圆曲线命名类型，来自 RFC 8446}
  CN_TLS_EC_CURVETYPE_EXPLICIT_PRIME                             = 1; // 详细传递参数的素域椭圆曲线
  CN_TLS_EC_CURVETYPE_EXPLICIT_CHAR2                             = 2; // 详细传递参数的二次域椭圆曲线
  CN_TLS_EC_CURVETYPE_NAMED_CURVE                                = 3; // 约定的命名曲线

  {* TLS/SSL 中的 Server Key Exchange 中的椭圆曲线签名算法，来自 RFC 8446}
  CN_TLS_SIGN_PARAM_SIG_ALG_ECDSA                                = 3;
  CN_TLS_SIGN_PARAM_SIG_ALG_ED25519                              = 7;
  CN_TLS_SIGN_PARAM_SIG_ALG_ED448                                = 8;

  {* TLS/SSL 中的 Extension 中的椭圆曲线点的转换类型，默认 04，不压缩}
  CN_TLS_EC_POINT_CONVERSION_FORM_UNCOMPRESSED                   = 4;

  {* TLS/SSL 中的 ChangeCipherSpec 中的 Content 值}
  CN_TLS_CHANGE_CIPHER_SPEC                                      = 1;

type
  TCnIPv6Array = array[0..7] of Word;

{*
  IP 包头示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |Version|  IHL  |Type of Service|          Total Length         |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |         Identification        |Flags|     Fragment Offset     |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |  Time to Live |    Protocol   |         Header Checksum       |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                     Source IP Address                         |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                  Destination IP Address                       |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                    Options                    |    Padding    |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                             Data                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnIPHeader = packed record
    VerionHeaderLength: Byte;           // 版本和包头长度
    TypeOfService:      Byte;           // 服务类型，高三位是 Precedence 值，再三位是 Delay、Throughtput、Reliability 标记
    TotalLength:        Word;           // 总长度，最大 65535
    Identification:     Word;           // 标识
    FlagOffset:         Word;           // 标志和片偏移
    TTL:                Byte;           // 生存时间
    Protocol:           Byte;           // 协议
    Checksum:           Word;           // 包头校验和
    SourceIp:           Cardinal;       // 源 IP 地址
    DestIp:             Cardinal;       // 目的 IP 地址
  end;

  PCnIPHeader = ^TCnIPHeader;

{*
  TCP 包头示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |          Source Port          |       Destination Port        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                        Sequence Number                        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                    Acknowledgment Number                      |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |  Data |           |U|A|P|R|S|F|                               |
  | Offset| Reserved  |R|C|S|S|Y|I|            Window             |
  |       |           |G|K|H|T|N|N|                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |           Checksum            |         Urgent Pointer        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                    Options                    |    Padding    |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                             Data                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnTCPHeader = packed record
    SourcePort:            Word;        // 源端口
    DestPort:              Word;        // 目的端口
    SequenceNumber:        Cardinal;    // 序列号
    AcknowledgementNumber: Cardinal;    // 响应序列号
    Offset:                Byte;        // 数据偏移，仅最左 4 bit，等同于包头长度
    Flags:                 Byte;        // TCP 包头标记
    Window:                Word;        // 窗口大小
    Checksum:              Word;        // 校验和，是所在 IP 头的源地址起到本 TCP 数据结束的校验
    UrgentPointer:         Word;        // 紧急指针
  end;

  PCnTCPHeader = ^TCnTCPHeader;

{*
  UDP 包头示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |          Source Port          |       Destination Port        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |             Length            |            Checksum           |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                             Data                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnUDPHeader = packed record
    SourcePort:            Word;        // 源端口
    DestPort:              Word;        // 目的端口
    Length:                Word;        // 数据包长度，包括 UDP 头
    Checksum:              Word;        // 校验和，是所在 IP 头的源地址起到本 UDP 数据结束的校验
  end;

  PCnUDPHeader = ^TCnUDPHeader;

{*
  ICMP 包头示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |     Type      |     Code      |          Checksum             |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                          变 体 定 义                          |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnICMPHeader = packed record
    MessageType:           Byte;         // 包的消息类型
    Code:                  Byte;         // 包的消息代码
    Checksum:              Word;         // 校验和，只针对本包头以及包括的数据
    case Integer of
      0: (Unused:          Cardinal);
      1: (Ptr:             Byte;         // 指针
          Unused1:         Byte;
          Unused2:         Word);
      2: (GatewayAddress:  Cardinal);    // 网关地址
      3: (Identifier:      Word;         // 标识
          SequenceNumber:  Word);        // 序列号
  end;

  PCnICMPHeader = ^TCnICMPHeader;

{*
  NTP 包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |LI | VN  |Mode |    Stratum    |      Poll     |   Precision   |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                         Root Delay                            |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                         Root Dispersion                       |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                          Reference ID                         |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +        Reference Timestamp (64: 32 Sec, 32 Fraction)          +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +         Origin Timestamp (64: 32 Sec, 32 Fraction)            +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +        Receive Timestamp (64: 32 Sec, 32 Fraction)            +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +        Transmit Timestamp (64: 32 Sec, 32 Fraction)           +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  .                                                               .
  .                    Extension Field 1 (variable)               .
  .                                                               .
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  .                                                               .
  .                    Extension Field 2 (variable)               .
  .                                                               .
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                         Key Identifier                        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  |                          Digest (128)                         |
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

  客户端接收答复时的时间戳 T4 无需存入此包中
  时间戳为 64 位，前 32 位是 1900 年 1 月 1 日后的秒数，
  后 32 位是小于秒的内容，值为微秒数的 （2^32/10^6）也就是 4294.967296 倍
}

  TCnNTPPacket = packed record
    LIVNMode:              Byte;         // 闰秒标识、版本号、工作模式
    Stratum:               Byte;         // 系统时钟层数
    Poll:                  Byte;         // 轮询间隔
    Precision:             Byte;         // 系统时钟精度
    RootDelay:             Cardinal;
    RootDispersion:        Cardinal;
    ReferenceID:           Cardinal;
    ReferenceTimestamp:    Int64;
    OriginateTimestamp:    Int64;        // 客户端发送请求时的时间戳 T1
    ReceiveTimestamp:      Int64;        // 服务器接收到请求的时间戳 T2
    TransmitTimestamp:     Int64;        // 服务器发送答复时的时间戳 T3
  end;

  PCnNTPPacket = ^TCnNTPPacket;

{*
  DNS 包头格式示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

    0                             1
    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
    7  6  5  4  3  2  1  0  7  6  5  4  3  2  1  0
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                      ID                       |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    QDCOUNT                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    ANCOUNT                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    NSCOUNT                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    ARCOUNT                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
}

  TCnDNSHeader = packed record
    Id:                    Word;     // 请求时客户端设置的 16 位标识符，服务器给出应答的时候会带相同的标识字段
    QrOpcodeAATCRD:        Byte;     // 请求应答、查询种类、授权应答、截断、期望递归
    RAZRCode:              Byte;     // 支持递归、保留、应答码
    QDCount:               Word;     // 报文请求段中的问题记录数
    ANCount:               Word;     // 报文回答段中的问题记录数
    NSCount:               Word;     // 报文授权段中的问题记录数
    ARCount:               Word;     // 报文附加段中的问题记录数
    SectionData:           array[0..0] of Byte;  // 报文附加段数据起始点
  end;

  PCnDNSHeader = ^TCnDNSHeader;

{*
  DNS 包头之后的 Question Section 格式，也就是 QD 所指示的格式
    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                                               |
  /                     QNAME                     /
  |                                               |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                     QTYPE                     |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                     QCLASS                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

  Name 是可变长度内容，因而本结构只表示其后部分
}

  TCnDNSQuestionSectionAfterName = packed record
    QType:                 Word;     // 查询类型
    QClass:                Word;     // 查询类
  end;

  PCnDNSQuestionSectionAfterName = ^TCnDNSQuestionSectionAfterName;

{*
  DNS 包头之后的 Resource record 格式，也就是 AN/NS/AR 所指示的格式
    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                                               |
  /                     NAME                      /
  |                                               |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                     TYPE                      |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                    CLASS                      |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                     TTL                       |
  |                                               |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
  |                   RDLENGTH                    |
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
  /                    RDATA                      /
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

  Name 是可变长度内容，因而本结构只表示其后部分
}

  TCnDNSResourceRecordAfterName = packed record
    RType:                 Word;     // 资源类型
    RClass:                Word;     // 资源类
    TTL:                   Cardinal; // 存活时间
    RDLength:              Word;     // 资源数据长度
    RData:                 array[0..0] of Byte;  // 资源数据，如 IP 等
  end;

  PCnDNSResourceRecordAfterName = ^TCnDNSResourceRecordAfterName;

{*
  Socks 代理协议客户端发起连接握手包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version   |    Method     |    Methods    ...              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksNegotiationRequest = packed record
    Version:               Byte;
    Method:                Byte;
    Methods:               array[1..255] of Byte;
  end;

  PCnSocksNegotiationRequest = ^TCnSocksNegotiationRequest;

{*
  Socks 代理协议服务端握手回应包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |    Method     |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksNegotiationResponse = packed record
    Version:               Byte;
    Method:                Byte;
  end;

  PCnSocksNegotiationResponse = ^TCnSocksNegotiationResponse;

{*
  Socks 代理协议客户端用户名密码验证请求包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    | UsernameLen   |    Username  1..255           |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | PasswordLen   |  Password 1..255                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksUsernamePasswordSubNegotiationRequest = packed record
    Version:               Byte;
    UsernameLen:           Byte;
    Username:              array[1..255] of AnsiChar; // 255 是最大长度，并非真实长度
    PasswordLen:           Byte;
    Password:              array[1..255] of AnsiChar; // 255 是最大长度，并非真实长度
  end;

  PCnSocksUsernamePasswordSubNegotiationRequest = ^TCnSocksUsernamePasswordSubNegotiationRequest;

{*
  Socks 代理协议客户端用户名密码验证回应包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |    Status     |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksUsernamePasswordSubNegotiationResponse = packed record
    Version:               Byte;
    Status:                Byte;
  end;

  PCnSocksUsernamePasswordSubNegotiationResponse = ^TCnSocksUsernamePasswordSubNegotiationResponse;

{*
  Socks 代理协议客户端请求包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |    Command    |   Reserved    |  Adress Type  |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Destination Address        |        Destination Port       |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksAddress = packed record
    case Integer of
      0: (IpV4Address:     Cardinal);
      1: (DomainNameLen:   Byte;
         DomainName:       array[0..255] of AnsiChar);
      2: (IpV6Address:     array[0..15] of AnsiChar);
  end;

  TCnSocksRequest = packed record
    Version:               Byte;
    Command:               Byte;
    Reserved:              Byte;
    AddressType:           Byte;
    DestionationAddress:   TCnSocksAddress;
    DestionationPort:      array[0..1] of AnsiChar;   // 上述字段可变长，本字段位置不固定
  end;

  PCnSocksRequest = ^TCnSocksRequest;

{*
  Socks 代理协议服务端应答包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |     Reply     |   Reserved    |  Adress Type  |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |         Bind Address          |            Bind Port          |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnSocksResponse = packed record
    Version:               Byte;
    Reply:                 Byte;
    Reserved:              Byte;
    AddressType:           Byte;
    BindAddress:           TCnSocksAddress;
    BindPort:              array[0..1] of AnsiChar;   // 上一字段可变长，本字段位置不固定
  end;

  PCnSocksResponse = ^TCnSocksResponse;

{*
  BGP 边界网关协议 Notification 型数据包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | Error code    | Error subcode |   Data (variable)             |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnBGPNotificationData = packed record
    ErrorCode:             Byte;        // 对应 CN_BGP_ERRORCODE_*
    ErrorSubcode:          Byte;        // 对应 CN_BGP_ERRORSUBCODE_*
    Data:                  Word;
  end;

  PTCnBGPNotificationData = ^TCnBGPNotificationData;

{*
  BGP 边界网关协议 Route-Refresh 型数据包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |             AFI               |      Res.     |     SAFI      |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnBGPRouteRefreshData = packed record
    AFI:                   Word;
    Res:                   Byte;
    SAFI:                  Byte;
  end;

  PCnBGPRouteRefreshData = ^TCnBGPRouteRefreshData;

{*
  BGP 边界网关协议 Open 型数据包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |    Version    |                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |     My Autonomous System      |                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |           Hold Time           |                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                         BGP Identifier                        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | Opt Parm Len  |                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  |             Optional Parameters (variable)                    |
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnBGPOpenData = packed record
    Version:               Byte;                      // Version 4
      Padding1:            Byte;
      Padding2:            Word;
    MyAS:                  Word;                      // My Autonomous System Number
      Padding3:            Word;
    HoldTime:              Word;                      // 单位秒，默认 180，0 表示不发 Keepalive
      Padding4:            Word;
    BGPIdentifier:         Cardinal;                  // 发送者的 Router ID
    OptParamLen:           Byte;                      // OptionalParameters 的长度
      Padding5:            Byte;
      Padding6:            Word;
    OptionalParameters:    Cardinal;
  end;
  PCnBGPOpenData = ^TCnBGPOpenData;

{*
  BGP 边界网关协议包头示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意它跑在 TCP 协议上，因而本包头是 TCP 包头后的负载内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                                                               |
  +                                                               +
  |                                                               |
  +                            Marker                             +
  |                                                               |
  +                                                               +
  |                                                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |          Length               |   Type        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
}

  TCnBGPHeader = packed record
    Marker:                array[0..15] of Byte;      // 兼容头，必须全 1
    Length:                Word;                      // 包括本包头在内的总包长度
    BGPType:               Byte;                      // 类型，对应常量 CN_BGP_TYPE_*
    case Integer of
      1: (OpenData:          TCnBGPOpenData);
      // 2: (UpdateData:     TCnBGPUpdateData);
      3: (NotificationData:  TCnBGPNotificationData);
      5: (RouteRefreshData:  TCnBGPRouteRefreshData);
      // 4 KeepAlive 无数据，2 Update 类型数据变长不易声明
  end;

  PCnBGPHeader = ^TCnBGPHeader;

{
  TLS/SSL 记录层协议包头示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意它跑在 TCP 协议上，因而本包头是 TCP 包头后的负载内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | Content Type  | Major Version | Minor Version | Body Length H |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | Body Length L |     Body      |           ......              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |           Mac                 |                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSRecordLayer = packed record
    ContentType:           Byte;                      // 内容类型，对应常量 CN_TLS_CONTENT_TYPE_*
    MajorVersion:          Byte;                      // 主要版本，SSLv3 则为 3
    MinorVersion:          Byte;                      // 次要版本，TLS 1.0 则为 1
    BodyLength:            Word;                      // Body 的数据长度
    Body:                  array[0..0] of Byte;       // Body 数据，是 SSL/TLS 协议报文
  end;

  PCnTLSRecordLayer = ^TCnTLSRecordLayer;

{
  TLS/SSL 握手协议包头示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意它跑在 TLS/SSL 记录层协议上，因而本包头是 TLS/SSL 记录层协议包头后的 Body 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | HandShakeType |                 Content Length                |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |   Content     |                     ......                    |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeHeader = packed record
    HandShakeType:         Byte;                      // 握手类型，对应常量 CN_TLS_HANDSHAKE_TYPE_*
    LengthHi:              Byte;
    LengthLo:              Word;                      // 3 字节长度的报文长度
    Content:               array[0..0] of Byte;       // 不同握手类型对应不同的握手报文
  end;

  PCnTLSHandShakeHeader = ^TCnTLSHandShakeHeader;

{
  TLS/SSL 改变密码格式协议包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意它跑在 TLS/SSL 记录层协议上，因而本包头是 TLS/SSL 记录层协议包头后的 Body 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+
  | Content 为 1  |
  +-+-+-+-+-+-+-+-+

}

  TCnTLSChangeCipherSpecPacket = packed record
    Content:               Byte;                      // 内容固定为 1
  end;

  PCnTLSChangeCipherSpecPacket = ^TCnTLSChangeCipherSpecPacket;

{
  TLS/SSL 告警协议包示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意它跑在 TLS/SSL 记录层协议上，因而本包头是 TLS/SSL 记录层协议包头后的 Body 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |  Alert Level  |  Alert Code   |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSAlertPacket = packed record
    AlertLevel:               Byte;                  // 告警级别，对应常量 CN_TLS_ALERT_LEVEL_*
    AlertDescription:         Byte;                  // 错误代码，对应常量 CN_TLS_ALERT_DESC_*
  end;

  PCnTLSAlertPacket = ^TCnTLSAlertPacket;

{
  TLS/SSL 握手包扩展条目示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeClientHello 后的附加内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |       ExtensionType           |      ExtensionDataLength      |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | ExtensionData     ...         |                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeExtensionItem = packed record
    ExtensionType:            Word;                    // 扩展类型，使用 CN_TLS_EXTENSIONTYPE_*
    ExtensionDataLength:      Word;                    // 扩展数据的字节长度
    ExtensionData:            array[0..0] of Byte;     // 扩展数据，实际长度可变
  end;

  PCnTLSHandShakeExtensionItem = ^TCnTLSHandShakeExtensionItem;

{
  TLS/SSL 握手包扩展示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeClientHello 后的附加内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |       ExtensionLength         |        Extension ...          |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeExtensions = packed record
    ExtensionLength:          Word;                    // 扩展数据总字节长度，不包含本 Word
    Extension:                TCnTLSHandShakeExtensionItem; // 第一个起始数据
  end;

  PCnTLSHandShakeExtensions = ^TCnTLSHandShakeExtensions;

{
  TLS/SSL 握手包扩展包中的 SNI 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeExtensions 的 ExtensionData 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |         ListLength            | NameType      | NameLength Hi |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | NameLength Lo |         Name ...                              |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeServerNameIndication = packed record
    ListLength:               Word;                    // 列表字节长度，不包含本 Word
    NameType:                 Byte;                    // 主机名类型，使用 CN_TLS_EXTENSION_NAMETYPE_*
    NameLength:               Word;                    // 主机名字节长度
    Name:                     array[0..0] of AnsiChar; // 主机名内容
                                                       // 后续重复上面三项
  end;

  PCnTLSHandShakeServerNameIndication = ^TCnTLSHandShakeServerNameIndication;

{
  TLS/SSL 握手包扩展包中的 Supported Groups 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeExtensions 的 ExtensionData 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |         NameLength            |        Name[0]...             |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeSupportedGroups = packed record
    NameLength:               Word;                    // 支持的椭圆曲线的数组字节长度
    Name:                     array[0..0] of Word;     // 支持的椭圆曲线数组，使用 CN_TLS_NAMED_GROUP_*
  end;

  PCnTLSHandShakeSupportedGroups = ^TCnTLSHandShakeSupportedGroups;

{
  TLS/SSL 握手包扩展包中的 ECPointFormats 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeExtensions 的 ExtensionData 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | FormatLength  |  PointFormat  |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeECPointFormats = packed record
    FormatLength:             Byte;                    // 支持的椭圆曲线的点的格式长度，一般是 1
    PointFormat:              Byte;                    // 支持的椭圆曲线的点的格式，使用 CN_TLS_EC_POINT_FORMATS_*，一般是 0
  end;

  PCnTLSHandShakeECPointFormats = ^TCnTLSHandShakeECPointFormats;

{
  TLS/SSL 握手包扩展包中的 Signature Algorithms 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeExtensions 的 ExtensionData 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |  SignatureAlgorithmsLength    |  SignatureAlgorithms[0]...    |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ +-+

}

  TCnTLSHandShakeSignatureAlgorithms = packed record
    SignatureAlgorithmsLength:  Word;                  // 支持的签名算法字节长度
    SignatureAlgorithms:      array[0..0] of Word;     // 支持的签名算法列表，使用 CN_TLS_SIGN_ALG_*
  end;

  PCnTLSHandShakeSignatureAlgorithms = ^TCnTLSHandShakeSignatureAlgorithms;

{
  TLS/SSL 握手包 ClientHello 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeHeader 的 Content 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |       ProtocolVersion         |        Random ...             |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeClientHello = packed record
    ProtocolVersion:          Word;                    // 真正有效的 TLS/SSL 版本号，对应 CN_TLS_SSL_VERSION_*
    Random:                   array[0..31] of Byte;    // 32 字节随机数，其中前 4 字节可能是时间戳
    SessionLength:            Byte;                    // 1 字节 SessionId 长度，常用 32
    SessionId:                array[0..31] of Byte;    // 实际长度为 [0..SessionLength - 1]
    CipherSuitesLength:       Word;                    // 以字节为单位的 CipherSuites 列表长度，占 2 字节
    CipherSuites:             array[0..3] of Word;     // CipherSuites 列表，实际字节长度为 CipherSuitesLength
    CompressionMethodLength:  Byte;                    // 1 字节压缩方法长度，常用 1
    CompressionMethod:        array[0..0] of Byte;     // 压缩方法列表，实际字节长度为 CompressionMethodLength
    Extensions:               TCnTLSHandShakeExtensions;  // 扩展握手包头，可能没有
  end;

  PCnTLSHandShakeClientHello = ^TCnTLSHandShakeClientHello;

{
  TLS/SSL 握手包 ServerHello 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeHeader 的 Content 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |       ProtocolVersion         |        Random ...             |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeServerHello = packed record
    ProtocolVersion:          Word;                    // 真正有效的 TLS/SSL 版本号，对应 CN_TLS_SSL_VERSION_*
    Random:                   array[0..31] of Byte;    // 32 字节随机数，其中前 4 字节可能是时间戳
    SessionLength:            Byte;                    // 1 字节 SessionId 长度，常用 32
    SessionId:                array[0..31] of Byte;    // 实际长度为 [0..SessionLength - 1]
    CipherSuite:              Word;                    // 单个选中的 CipherSuite
    CompressionMethod:        Byte;                    // 1 字节压缩方法
    Extensions:               TCnTLSHandShakeExtensions;  // 扩展握手包头，可能没有
  end;

  PCnTLSHandShakeServerHello = ^TCnTLSHandShakeServerHello;

{
  TLS/SSL 握手包 CertificateItem 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeCertificate 的 Certificates 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |              CertificateLength                |Certificate ...|
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeCertificateItem = packed record
    CertificateLength:        array[0..2] of Byte;          // 证书总字节长度，不包含本仨字节
    Certificate:              array[0..0] of Byte;          // 证书编码数据
  end;

  PCnTLSHandShakeCertificateItem = ^TCnTLSHandShakeCertificateItem;

{
  TLS/SSL 握手包 Certificate 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeHeader 的 Content 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |            CertificateListLength              |Certificates...|
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeCertificate = packed record
    CertificateListLength:    array[0..2] of Byte;          // 证书列表总字节长度，不包含本仨字节
    Certificates:             TCnTLSHandShakeCertificateItem;   // 第一个起始数据
  end;

  PCnTLSHandShakeCertificate = ^TCnTLSHandShakeCertificate;

{
  TLS/SSL 握手包中的签名及参数部分示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeServerKeyExchange 的 SignedParams 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |      SignatureAlgorithm       |        SignatureLength        |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                        Signature ...                          |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeSignedParams = packed record
    SignatureAlgorithm:       Word;                         // 签名算法，使用 CN_TLS_SIGN_ALG_*
    SignatureLength:          Word;                         // 签名长度
    Signature:                array[0..0] of Byte;          // 签名内容
  end;

  PCnTLSHandShakeSignedParams = ^TCnTLSHandShakeSignedParams;

{
  TLS/SSL 握手包中的 Server Key Exchange 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeHeader 的 Content 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |  ECCurveType  |          NamedCurve           | ECPointLength |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                        ECPoint ...                            |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                     SignedParams ...                          |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeServerKeyExchange = packed record
    ECCurveType:              Byte;                         // 椭圆曲线命名类型，使用 CN_TLS_EC_CURVETYPE_*
    NamedCurve:               Word;                         // 椭圆曲线类型，当 ECCurveType 是命名曲线时
    ECPointLength:            Byte;                         // 椭圆曲线公钥也就是点坐标的长度
    ECPoint:                  array[0..63] of Byte;         // 椭圆曲线公钥长度，实际长度由 ECPointLength 决定
    SignedParams:             TCnTLSHandShakeSignedParams;  // 签名部分，注意不能直接访问
  end;

  PCnTLSHandShakeServerKeyExchange = ^TCnTLSHandShakeServerKeyExchange;

{
  TLS/SSL 握手包中的 Client Key Exchange 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeHeader 的 Content 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  | PointConvForm |                     X ...                     |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                           Y ...                               |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeClientKeyExchange = packed record
    PointConversionForm:      Byte;                         // 椭圆曲线点格式，使用 CN_TLS_EC_POINT_CONVERSION_FORM_*
    XY:                       array[0..0] of Byte;          // 椭圆曲线公钥点的 X Y 坐标，实际长度可变
  end;

  PCnTLSHandShakeClientKeyExchange = ^TCnTLSHandShakeClientKeyExchange;

{
  TLS/SSL 握手包中的 Finished 示意图，字节内左边是高位，右边是低位。
  字节之间采用 Big-Endian 的网络字节顺序，高位在低地址，符合阅读习惯。
  注意本包头是 TLS/SSL 的 TCnTLSHandShakeHeader 的 Content 内容

   0                   1                   2                   3
   0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
  |                       VerifyData ...                          |
  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

}

  TCnTLSHandShakeFinished = packed record
    VerifyData:               array[0..0] of Byte;          // 验证数据，长度由握手算法规定，TLS 1.2 是 12 字节
  end;

  PCnTLSHandShakeFinished = ^TCnTLSHandShakeFinished;

// ======================== IP 包头系列函数 ====================================

function CnGetIPVersion(const IPHeader: PCnIPHeader): Integer;
{* 获得 IP 包头内的 IP 版本号}

function CnGetIPHeaderLength(const IPHeader: PCnIPHeader): Integer;
{* 获得 IP 包头内的 IP 包头长度，单位为 4 字节}

function CnGetIPTypeOfServicePrecedence(const IPHeader: PCnIPHeader): Integer;
{* 获得 IP 包头内的 Type of Service 字段中的 Precedence 值}

function CnGetIPTypeOfServiceDelay(const IPHeader: PCnIPHeader): Boolean;
{* 获得 IP 包头内的 Type of Service 字段中的 Delay 值，True 为 Low，False 为 Normal}

function CnGetIPTypeOfServiceThroughput(const IPHeader: PCnIPHeader): Boolean;
{* 获得 IP 包头内的 Type of Service 字段中的 Throughput 值，True 为 High，False 为 Normal}

function CnGetIPTypeOfServiceReliability(const IPHeader: PCnIPHeader): Boolean;
{* 获得 IP 包头内的 Type of Service 字段中的 Reliability 值，True 为 High，False 为 Normal}

function CnGetIPTotalLength(const IPHeader: PCnIPHeader): Integer;
{* 获得 IP 包头内的包总长度，存在网络字节转换}

function CnGetIPIdentification(const IPHeader: PCnIPHeader): Integer;
{* 获得 IP 包头内的标识，存在网络字节转换}

function CnGetIPFlagDontFragment(const IPHeader: PCnIPHeader): Boolean;
{* 获得 IP 包头内的是否分段标记，返回 True 为不分段，False 为允许分段}

function CnGetIPFlagMoreFragment(const IPHeader: PCnIPHeader): Boolean;
{* 获得 IP 包头内的是否有更多分段标记，返回 True 为有更多分段，False 为最后一个分段}

function CnGetIPFragmentOffset(const IPHeader: PCnIPHeader): Integer;
{* 获得 IP 包头内的分段偏移，存在网络字节转换}

function CnGetIPChecksum(const IPHeader: PCnIPHeader): Word;
{* 获得 IP 包头内的校验和，存在网络字节转换}

function CnGetIPSourceIP(const IPHeader: PCnIPHeader): Cardinal;
{* 获得 IP 包头内的源 IP 地址，存在网络字节转换}

function CnGetIPDestIP(const IPHeader: PCnIPHeader): Cardinal;
{* 获得 IP 包头内的目的 IP 地址，存在网络字节转换}

procedure CnSetIPVersion(const IPHeader: PCnIPHeader; Version: Byte);
{* 设置 IP 包头内的 IP 版本号}

procedure CnSetIPHeaderLength(const IPHeader: PCnIPHeader; HeaderLength: Byte);
{* 设置 IP 包头内的 IP 包头长度，单位为 4 字节}

procedure CnSetIPTypeOfServicePrecedence(const IPHeader: PCnIPHeader; Precedence: Byte);
{* 设置 IP 包头内的 Type of Service 字段中的 Precedence 值}

procedure CnSetIPTypeOfServiceDelay(const IPHeader: PCnIPHeader; Delay: Boolean);
{* 设置  IP 包头内的 Type of Service 字段中的 Delay 值，True 为 Low，False 为 Normal}

procedure CnSetIPTypeOfServiceThroughput(const IPHeader: PCnIPHeader; Throughput: Boolean);
{* 设置  IP 包头内的 Type of Service 字段中的 Throughput 值，True 为 High，False 为 Normal}

procedure CnSetIPTypeOfServiceReliability(const IPHeader: PCnIPHeader; Reliability: Boolean);
{* 设置  IP 包头内的 Type of Service 字段中的 Reliability 值，True 为 High，False 为 Normal}

procedure CnSetIPTotalLength(const IPHeader: PCnIPHeader; TotalLength: Word);
{* 设置 IP 包头内的包总长度，存在网络字节转换}

procedure CnSetIPIdentification(const IPHeader: PCnIPHeader; Identification: Word);
{* 设置 IP 包头内的标识，存在网络字节转换}

procedure CnSetIPFragmentOffset(const IPHeader: PCnIPHeader; FragmentOffset: Word);
{* 设置 IP 包头内的分段偏移，存在网络字节转换}

procedure CnSetIPChecksum(const IPHeader: PCnIPHeader; Checksum: Word);
{* 设置 IP 包头内的校验和，存在网络字节转换}

procedure CnSetIPSourceIP(const IPHeader: PCnIPHeader; SourceIP: Cardinal);
{* 设置 IP 包头内的源 IP 地址，存在网络字节转换}

procedure CnSetIPDestIP(const IPHeader: PCnIPHeader; DestIP: Cardinal);
{* 设置 IP 包头内的目的 IP 地址，存在网络字节转换}

// ======================== TCP 包头系列函数 ===================================

function CnGetTCPSourcePort(const TCPHeader: PCnTCPHeader): Integer;
{* 获得 TCP 包头内的源端口号，存在网络字节转换}

function CnGetTCPDestPort(const TCPHeader: PCnTCPHeader): Integer;
{* 获得 TCP 包头内的目的端口号，存在网络字节转换}

function CnGetTCPSequenceNumber(const TCPHeader: PCnTCPHeader): Cardinal;
{* 获得 TCP 包头内的序列号，存在网络字节转换}

function CnGetTCPAcknowledgementNumber(const TCPHeader: PCnTCPHeader): Cardinal;
{* 获得 TCP 包头内的响应号，存在网络字节转换}

function CnGetTCPOffset(const TCPHeader: PCnTCPHeader): Integer;
{* 获得 TCP 包头内的数据偏移值}

function CnGetTCPFlagURG(const TCPHeader: PCnTCPHeader): Boolean;
{* 获得 TCP 包头内是否有 URG 标记，是则返回 True，否则返回 False}

function CnGetTCPFlagACK(const TCPHeader: PCnTCPHeader): Boolean;
{* 获得 TCP 包头内是否有 ACK 标记，是则返回 True，否则返回 False}

function CnGetTCPFlagPSH(const TCPHeader: PCnTCPHeader): Boolean;
{* 获得 TCP 包头内是否有 PSH 标记，是则返回 True，否则返回 False}

function CnGetTCPFlagRST(const TCPHeader: PCnTCPHeader): Boolean;
{* 获得 TCP 包头内是否有 RST 标记，是则返回 True，否则返回 False}

function CnGetTCPFlagSYN(const TCPHeader: PCnTCPHeader): Boolean;
{* 获得 TCP 包头内是否有 SYN 标记，是则返回 True，否则返回 False}

function CnGetTCPFlagFIN(const TCPHeader: PCnTCPHeader): Boolean;
{* 获得 TCP 包头内是否有 FIN 标记，是则返回 True，否则返回 False}

function CnGetTCPWindow(const TCPHeader: PCnTCPHeader): Integer;
{* 获得 TCP 包头内的窗口大小，存在网络字节转换}

function CnGetTCPChecksum(const TCPHeader: PCnTCPHeader): Word;
{* 获得 TCP 包头内的校验和，存在网络字节转换}

function CnGetTCPUrgentPointer(const TCPHeader: PCnTCPHeader): Word;
{* 获得 TCP 包头内的紧急指针，存在网络字节转换}

procedure CnSetTCPSourcePort(const TCPHeader: PCnTCPHeader; SourcePort: Word);
{* 设置 TCP 包头内的源端口号，存在网络字节转换}

procedure CnSetTCPDestPort(const TCPHeader: PCnTCPHeader; DestPort: Word);
{* 设置 TCP 包头内的目的端口号，存在网络字节转换}

procedure CnSetTCPSequenceNumber(const TCPHeader: PCnTCPHeader; SequenceNumber: Cardinal);
{* 设置 TCP 包头内的序列号，存在网络字节转换}

procedure CnSetTCPAcknowledgementNumber(const TCPHeader: PCnTCPHeader; AcknowledgementNumber: Cardinal);
{* 设置 TCP 包头内的响应号，存在网络字节转换}

procedure CnSetTCPWindow(const TCPHeader: PCnTCPHeader; Window: Word);
{* 设置 TCP 包头内的窗口大小，存在网络字节转换}

procedure CnSetTCPChecksum(const TCPHeader: PCnTCPHeader; Checksum: Word);
{* 设置 TCP 包头内的校验和，存在网络字节转换}

procedure CnSetTCPUrgentPointer(const TCPHeader: PCnTCPHeader; UrgentPointer: Word);
{* 设置 TCP 包头内的紧急指针，存在网络字节转换}

// ======================== UDP 包头系列函数 ===================================

function CnGetUDPSourcePort(const UDPHeader: PCnUDPHeader): Integer;
{* 获得 UDP 包头内的源端口号，存在网络字节转换}

function CnGetUDPDestPort(const UDPHeader: PCnUDPHeader): Integer;
{* 获得 UDP 包头内的目的端口号，存在网络字节转换}

function CnGetUDPLength(const UDPHeader: PCnUDPHeader): Integer;
{* 获得 UDP 包头内的包总长度，存在网络字节转换}

function CnGetUDPChecksum(const UDPHeader: PCnUDPHeader): Word;
{* 获得 UDP 包头内的校验和，存在网络字节转换}

procedure CnSetUDPSourcePort(const UDPHeader: PCnUDPHeader; SourcePort: Word);
{* 设置 UDP 包头内的源端口号，存在网络字节转换}

procedure CnSetUDPDestPort(const UDPHeader: PCnUDPHeader; DestPort: Word);
{* 设置 UDP 包头内的目的端口号，存在网络字节转换}

procedure CnSetUDPLength(const UDPHeader: PCnUDPHeader; ByteLen: Word);
{* 设置 UDP 包头内的包总长度，存在网络字节转换}

procedure CnSetUDPChecksum(const UDPHeader: PCnUDPHeader; Checksum: Word);
{* 设置 UDP 包头内的校验和，存在网络字节转换}

// ======================== ICMP 包头系列函数 ==================================

function CnGetICMPType(const ICMPHeader: PCnICMPHeader): Integer;
{* 获得 ICMP 包头内的消息类型}

function CnGetICMPCode(const ICMPHeader: PCnICMPHeader): Integer;
{* 获得 ICMP 包头内的消息代码}

function CnGetICMPChecksum(const ICMPHeader: PCnICMPHeader): Word;
{* 获得 ICMP 包头内的校验和，存在网络字节转换}

function CnGetICMPPointer(const ICMPHeader: PCnICMPHeader): Integer;
{* 获得 ICMP 包头内的指针字段值}

function CnGetICMPGatewayAddress(const ICMPHeader: PCnICMPHeader): Cardinal;
{* 获得 ICMP 包头内的网关地址，存在网络字节转换}

function CnGetICMPIdentifier(const ICMPHeader: PCnICMPHeader): Word;
{* 获得 ICMP 包头内的标识号，存在网络字节转换}

function CnGetICMPSequenceNumber(const ICMPHeader: PCnICMPHeader): Word;
{* 获得 ICMP 包头内的序列号，存在网络字节转换}

procedure CnSetICMPType(const ICMPHeader: PCnICMPHeader; MessageType: Integer);
{* 设置 ICMP 包头内的消息类型}

procedure CnSetICMPCode(const ICMPHeader: PCnICMPHeader; Code: Integer);
{* 设置 ICMP 包头内的消息代码}

procedure CnSetICMPChecksum(const ICMPHeader: PCnICMPHeader; Checksum: Word);
{* 设置 ICMP 包头内的校验和，存在网络字节转换}

procedure CnSetICMPGatewayAddress(const ICMPHeader: PCnICMPHeader; GatewayAddress: Cardinal);
{* 设置 ICMP 包头内的网关地址，存在网络字节转换}

procedure CnSetICMPIdentifier(const ICMPHeader: PCnICMPHeader; Identifier: Word);
{* 设置 ICMP 包头内的标识号，存在网络字节转换}

procedure CnSetICMPSequenceNumber(const ICMPHeader: PCnICMPHeader; SequenceNumber: Word);
{* 设置 ICMP 包头内的序列号，存在网络字节转换}

// ========================== NTP 包系列函数 ===================================

function CnGetNTPLeapIndicator(const NTPPacket: PCnNTPPacket): Integer;
{* 获得 NTP 包内的闰秒标识}

function CnGetNTPVersionNumber(const NTPPacket: PCnNTPPacket): Integer;
{* 获得 NTP 包内的版本号}

function CnGetNTPMode(const NTPPacket: PCnNTPPacket): Integer;
{* 获得 NTP 包内的模式}

procedure CnSetNTPLeapIndicator(const NTPPacket: PCnNTPPacket; LeapIndicator: Integer);
{* 设置 NTP 包内的闰秒标识，使用 CN_NTP_LEAP_INDICATOR_* 系列常数}

procedure CnSetNTPVersionNumber(const NTPPacket: PCnNTPPacket; VersionNumber: Integer);
{* 设置 NTP 包内的版本号，使用 CN_NTP_VERSION_* 系列常数}

procedure CnSetNTPMode(const NTPPacket: PCnNTPPacket; NTPMode: Integer);
{* 设置 NTP 包内的模式，使用 CN_NTP_MODE_* 系列常数}

function CnConvertNTPTimestampToDateTime(Stamp: Int64): TDateTime;
{* 将 NTP 包中的时间戳值转换成日期时间}

function CnConvertDateTimeToNTPTimestamp(ADateTime: TDateTime): Int64;
{* 将日期时间转换成 NTP 包中的时间戳值}

// ========================== DNS 包系列函数 ===================================

function CnGetDNSHeaderId(const DNSHeader: PCnDNSHeader): Word;
{* 获得 DNS 包头内的 Id，存在网络字节转换}

function CnGetDNSHeaderQR(const DNSHeader: PCnDNSHeader): Integer;
{* 获得 DNS 包头内的 QR 标识，查询或响应，返回 CN_DNS_HEADER_TYPE_*}

function CnGetDNSHeaderOpCode(const DNSHeader: PCnDNSHeader): Integer;
{* 获得 DNS 包头内的 OpCode 查询种类，返回 CN_DNS_HEADER_OPCODE_*}

function CnGetDNSHeaderAA(const DNSHeader: PCnDNSHeader): Boolean;
{* 获得 DNS 包头内的授权应答位是否置位，返回 True 或 False}

function CnGetDNSHeaderTC(const DNSHeader: PCnDNSHeader): Boolean;
{* 获得 DNS 包头内的截断位是否置位，返回 True 或 False}

function CnGetDNSHeaderRD(const DNSHeader: PCnDNSHeader): Boolean;
{* 获得 DNS 包头内的期望递归位是否置位，返回 True 或 False}

function CnGetDNSHeaderRA(const DNSHeader: PCnDNSHeader): Boolean;
{* 获得 DNS 包头内的支持递归位是否置位，返回 True 或 False}

function CnGetDNSHeaderRCode(const DNSHeader: PCnDNSHeader): Integer;
{* 获得 DNS 包头内的应答码，返回 CN_DNS_HEADER_RCODE_*}

function CnGetDNSHeaderQDCount(const DNSHeader: PCnDNSHeader): Integer;
{* 获得 DNS 包头内的请求记录数量，存在网络字节转换}

function CnGetDNSHeaderANCount(const DNSHeader: PCnDNSHeader): Integer;
{* 获得 DNS 包头内的回答记录数量，存在网络字节转换}

function CnGetDNSHeaderNSCount(const DNSHeader: PCnDNSHeader): Integer;
{* 获得 DNS 包头内的授权记录数量，存在网络字节转换}

function CnGetDNSHeaderARCount(const DNSHeader: PCnDNSHeader): Integer;
{* 获得 DNS 包头内的附加记录数量，存在网络字节转换}

procedure CnSetDNSHeaderId(const DNSHeader: PCnDNSHeader; Id: Word);
{* 设置 DNS 包头内的 Id，存在网络字节转换}

procedure CnSetDNSHeaderQR(const DNSHeader: PCnDNSHeader; IsQuery: Boolean);
{* 设置 DNS 包头内的 QR 标识，是查询还是响应}

procedure CnSetDNSHeaderOpCode(const DNSHeader: PCnDNSHeader; QueryType: Byte);
{* 设置 DNS 包头内的 OpCode 查询种类，使用 CN_DNS_HEADER_OPCODE_*}

procedure CnSetDNSHeaderAA(const DNSHeader: PCnDNSHeader; AuthoritativeAnswer: Boolean);
{* 设置 DNS 包头内的授权应答位是否置位}

procedure CnSetDNSHeaderTC(const DNSHeader: PCnDNSHeader; TrunCation: Boolean);
{* 设置 DNS 包头内的截断位是否置位}

procedure CnSetDNSHeaderRD(const DNSHeader: PCnDNSHeader; RecursionDesired: Boolean);
{* 设置 DNS 包头内的期望递归位是否置位}

procedure CnSetDNSHeaderRA(const DNSHeader: PCnDNSHeader; RecursionAvailable: Boolean);
{* 设置 DNS 包头内的支持递归位是否置位}

procedure CnSetDNSHeaderRCode(const DNSHeader: PCnDNSHeader; RCode: Byte);
{* 设置 DNS 包头内的应答码，使用 CN_DNS_HEADER_RCODE_*}

procedure CnSetDNSHeaderQDCount(const DNSHeader: PCnDNSHeader; Count: Word);
{* 设置 DNS 包头内的请求记录数量，存在网络字节转换}

procedure CnSetDNSHeaderANCount(const DNSHeader: PCnDNSHeader; Count: Word);
{* 设置 DNS 包头内的回答记录数量，存在网络字节转换}

procedure CnSetDNSHeaderNSCount(const DNSHeader: PCnDNSHeader; Count: Word);
{* 设置 DNS 包头内的授权记录数量，存在网络字节转换}

procedure CnSetDNSHeaderARCount(const DNSHeader: PCnDNSHeader; Count: Word);
{* 设置 DNS 包头内的附加记录数量，存在网络字节转换}

// ======================== Socks 代理包系列函数 ===============================

function CnGetSocksRequestDestinationAddress(const SocksReq: PCnSocksRequest): string;
{* 返回 Socks 请求中的目的地址，支持 IPv4/v6 与域名的情形}

function CnGetSocksRequestDestinationPort(const SocksReq: PCnSocksRequest): Word;
{* 返回 Socks 请求中的目的地址端口}

procedure CnSetSocksRequestDestinationAddress(const SocksReq: PCnSocksRequest; Address: string);
{* 设置 Socks 请求中的目的地址，支持 IPv4 或域名的情形}

function CnSetSocksRequestDestinationPort(const SocksReq: PCnSocksRequest; Port: Word): Integer;
{* 设置 Socks 请求中的目的端口号，返回 SocksReq 结构总长度}

function CnGetSocksResponseBindAddress(const SocksResp: PCnSocksResponse): string;
{* 返回 Socks 应答中的绑定地址，支持 IPv4/v6 与域名的情形}

function CnGetSocksResponseBindPort(const SocksResp: PCnSocksResponse): Word;
{* 返回 Socks 应答中的绑定端口号}

procedure CnSetSocksResponseBindAddress(const SocksResp: PCnSocksResponse; Address: string);
{* 设置 Socks 应答中的绑定地址，支持 IPv4/v6 与域名的情形}

function CnSetSocksResponseBindPort(const SocksResp: PCnSocksResponse; Port: Word): Integer;
{* 设置 Socks 应答中的绑定端口号，返回 SocksResp 结构总长度}

// ===================== BGP 边界网关协议包系列函数 ============================

procedure CnFillBGPHeaderMarkers(const BGPHeader: PCnBGPHeader);
{* 填充 BGP 边界网关协议包头中的 Marker 字段}

function CnGetBGPHeaderLength(const BGPHeader: PCnBGPHeader): Word;
{* 返回 BGP 边界网关协议包头中的长度}

procedure CnSetBGPHeaderLength(const BGPHeader: PCnBGPHeader; Length: Word);
{* 设置 BGP 边界网关协议包头中的长度}

// ===================== TLS/SSL 记录层协议包系列函数 ==========================

function CnGetTLSRecordLayerBodyLength(const RecordLayer: PCnTLSRecordLayer): Word;
{* 返回 TLS/SSL 记录层协议包头中的数据长度}

procedure CnSetTLSRecordLayerBodyLength(const RecordLayer: PCnTLSRecordLayer; BodyLength: Word);
{* 设置 TLS/SSL 记录层协议包头中的数据长度}

function CnGetTLSHandShakeHeaderContentLength(const HandShakeHeader: PCnTLSHandShakeHeader): Cardinal;
{* 返回 TLS/SSL 握手协议报文的内容长度}

procedure CnSetTLSHandShakeHeaderContentLength(const HandShakeHeader: PCnTLSHandShakeHeader; ContentLength: Cardinal);
{* 设置 TLS/SSL 握手协议报文的内容长度}

function CnGetTLSHandShakeExtensionsExtensionLength(const Extensions: PCnTLSHandShakeExtensions): Word;
{* 返回 TLS/SSL 握手协议扩展报文的内容总长度}

procedure CnSetTLSHandShakeExtensionsExtensionLength(const Extensions: PCnTLSHandShakeExtensions; ExtLenth: Word);
{* 设置 TLS/SSL 握手协议扩展报文的内容总长度}

procedure CnSetTLSHandShakeExtensionsExtensionLengthByItemCount(const Extensions: PCnTLSHandShakeExtensions; ExtItemCount: Integer);
{* 根据已经设置好的 Item 内容的数量，统计并设置 TLS/SSL 握手协议扩展报文的内容总长度}

function CnGetTLSHandShakeExtensionsExtensionItem(const Extensions: PCnTLSHandShakeExtensions;
  PrevItem: PCnTLSHandShakeExtensionItem = nil): PCnTLSHandShakeExtensionItem;
{* 返回 TLS/SSL 握手协议扩展报文的某个扩展条目地址的下一个扩展条目，如果指定条目为 nil，返回第一个}

function CnGetTLSHandShakeExtensionsExtensionType(const ExtensionItem: PCnTLSHandShakeExtensionItem): Word;
{* 返回 TLS/SSL 握手协议扩展报文的指定扩展条目的扩展类型}

procedure CnSetTLSHandShakeExtensionsExtensionType(const ExtensionItem: PCnTLSHandShakeExtensionItem; ExtType: Word);
{* 设置 TLS/SSL 握手协议扩展报文的指定扩展条目的扩展类型}

function CnGetTLSHandShakeExtensionsExtensionDataLength(const ExtensionItem: PCnTLSHandShakeExtensionItem): Word;
{* 返回 TLS/SSL 握手协议扩展报文的指定扩展条目的数据长度}

procedure CnSetTLSHandShakeExtensionsExtensionDataLength(const ExtensionItem: PCnTLSHandShakeExtensionItem; ExtDataLength: Word);
{* 设置 TLS/SSL 握手协议扩展报文的指定扩展字段的长度}

function CnGetTLSHandShakeExtensionsExtensionData(const ExtensionItem: PCnTLSHandShakeExtensionItem): Pointer;
{* 返回 TLS/SSL 握手协议扩展报文的指定扩展条目的数据地址}

procedure CnSetTLSHandShakeExtensionsExtensionData(const ExtensionItem: PCnTLSHandShakeExtensionItem; Data: TBytes);
{* 设置 TLS/SSL 握手协议扩展报文的指定扩展条目的数据，内部会同时设置相应的数据长度}

function CnGetTLSHandShakeServerNameIndicationListLength(const SNI: PCnTLSHandShakeServerNameIndication): Word;
{* 返回 TLS/SSL 中的 Extension 中的 Server Name Indication 中的列表长度}

procedure CnSetTLSHandShakeServerNameIndicationListLength(const SNI: PCnTLSHandShakeServerNameIndication; ListLength: Word);
{* 设置 TLS/SSL 中的 Extension 中的 Server Name Indication 中的列表长度}

function CnGetTLSHandShakeServerNameIndicationNameLength(const SNI: PCnTLSHandShakeServerNameIndication): Word;
{* 返回 TLS/SSL 中的 Extension 中的 Server Name Indication 中的主机名长度}

procedure CnSetTLSHandShakeServerNameIndicationNameLength(const SNI: PCnTLSHandShakeServerNameIndication; NameLength: Word);
{* 设置 TLS/SSL 中的 Extension 中的 Server Name Indication 中的主机名长度}

function CnTLSHandShakeServerNameIndicationAddHost(const SNI: PCnTLSHandShakeServerNameIndication; const HostName: AnsiString): Integer;
{* 将一个主机名添加至已初始化好的 SNI 头中，返回 SNI 头的新的总字节长度}

function CnGetTLSHandShakeSupportedGroupsNameLength(const SG: PCnTLSHandShakeSupportedGroups): Word;
{* 返回 TLS/SSL 中的 Extension 中的 Supported Groups 中的名称长度}

procedure CnSetTLSHandShakeSupportedGroupsNameLength(const SG: PCnTLSHandShakeSupportedGroups; NameLength: Word);
{* 设置 TLS/SSL 中的 Extension 中的 Supported Groups 中的名称长度}

function CnGetTLSHandShakeSupportedGroups(const ExtensionItem: PCnTLSHandShakeExtensionItem): TWords;
{* 获得 TLS/SSL 中的 Extension 中的一扩展的支持椭圆曲线类型数组}

procedure CnSetTLSHandShakeSupportedGroups(const ExtensionItem: PCnTLSHandShakeExtensionItem; Groups: TWords);
{* 设置 TLS/SSL 中的 Extension 中的一扩展的支持椭圆曲线类型数组，同时设置类型与数据长度}

procedure CnSetTLSHandShakeECPointFormats(const ExtensionItem: PCnTLSHandShakeExtensionItem; PointFormat: Byte);
{* 设置 TLS/SSL 中的 Extension 中的一扩展的单个支持椭圆曲线点格式，同时设置类型与数据长度}

function CnGetTLSHandShakeSignatureAlgorithmsSigAlgLength(const SG: PCnTLSHandShakeSignatureAlgorithms): Word;
{* 返回 TLS/SSL 中的 Extension 中的一扩展签名算法类型数组中的算法数组长度}

procedure CnSetTLSHandShakeSignatureAlgorithmsSigAlgLength(const SG: PCnTLSHandShakeSignatureAlgorithms; SigAlgLength: Word);
{* 设置 TLS/SSL 中的 Extension 中的一扩展签名算法类型数组中的算法数组长度}

function CnGetTLSHandShakeSignatureAlgorithms(const ExtensionItem: PCnTLSHandShakeExtensionItem): TWords;
{* 获得 TLS/SSL 中的 Extension 中的一扩展的支持签名算法类型数组}

procedure CnSetTLSHandShakeSignatureAlgorithms(const ExtensionItem: PCnTLSHandShakeExtensionItem; SigAlgs: TWords);
{* 设置 TLS/SSL 中的 Extension 中的一扩展的支持签名算法类型数组，同时设置类型与数据长度}

function CnGetTLSHandShakeClientHelloSessionId(const ClientHello: PCnTLSHandShakeClientHello): TBytes;
{* 获取 TLS/SSL 握手协议报文 ClientHello 类型中的 SessionId}

procedure CnSetTLSHandShakeClientHelloSessionId(const ClientHello: PCnTLSHandShakeClientHello; SessionId: TBytes);
{* 设置 TLS/SSL 握手协议报文 ClientHello 类型中的 SessionId}

function CnGetTLSHandShakeClientHelloCipherSuitesLength(const ClientHello: PCnTLSHandShakeClientHello): Word;
{* 获取 TLS/SSL 握手协议报文 ClientHello 类型中的 CipherSuites 长度，单位是字节}

procedure CnSetTLSHandShakeClientHelloCipherSuitesLength(const ClientHello: PCnTLSHandShakeClientHello; CipherSuitesLength: Word);
{* 设置 TLS/SSL 握手协议报文 ClientHello 类型中的 CipherSuites 长度，单位是字节}

function CnGetTLSHandShakeClientHelloCipherSuites(const ClientHello: PCnTLSHandShakeClientHello): TWords;
{* 获取 TLS/SSL 握手协议报文 ClientHello 类型中的 CipherSuites}

procedure CnSetTLSHandShakeClientHelloCipherSuites(const ClientHello: PCnTLSHandShakeClientHello; CipherSuites: TWords);
{* 设置 TLS/SSL 握手协议报文 ClientHello 类型中的 CipherSuites}

function CnGetTLSHandShakeClientHelloCompressionMethodLength(const ClientHello: PCnTLSHandShakeClientHello): Byte;
{* 获取 TLS/SSL 握手协议报文 ClientHello 类型中的 CompressionMethod 长度，单位是字节}

procedure CnSetTLSHandShakeClientHelloCompressionMethodLength(const ClientHello: PCnTLSHandShakeClientHello; CompressionMethodLength: Byte);
{* 设置 TLS/SSL 握手协议报文 ClientHello 类型中的 CompressionMethod 长度，单位是字节}

function CnGetTLSHandShakeClientHelloCompressionMethod(const ClientHello: PCnTLSHandShakeClientHello): TBytes;
{* 获取 TLS/SSL 握手协议报文 ClientHello 类型中的 CompressionMethod}

procedure CnSetTLSHandShakeClientHelloCompressionMethod(const ClientHello: PCnTLSHandShakeClientHello; CompressionMethod: TBytes);
{* 设置 TLS/SSL 握手协议报文 ClientHello 类型中的 CompressionMethod}

function CnGetTLSHandShakeClientHelloExtensions(const ClientHello: PCnTLSHandShakeClientHello): PCnTLSHandShakeExtensions; overload;
{* 获取 TLS/SSL 握手协议报文 ClientHello 类型中扩展包头的地址，但单纯从本包内容看，无法判断其是否真实存在}

function CnGetTLSHandShakeClientHelloExtensions(const HandShakeHeader: PCnTLSHandShakeHeader): PCnTLSHandShakeExtensions; overload;
{* 获取 TLS/SSL 握手协议报文 ClientHello 类型中完整包头的长度，结合本包之外的握手包内容长度，可判断是否存在，无则返回 nil}

function CnGetTLSHandShakeServerHelloSessionId(const ServerHello: PCnTLSHandShakeServerHello): TBytes;
{* 获取 TLS/SSL 握手协议报文 ServerHello 类型中的 SessionId}

procedure CnSetTLSHandShakeServerHelloSessionId(const ServerHello: PCnTLSHandShakeServerHello; SessionId: TBytes);
{* 设置 TLS/SSL 握手协议报文 ServerHello 类型中的 SessionId}

function CnGetTLSHandShakeServerHelloCipherSuite(const ServerHello: PCnTLSHandShakeServerHello): Word;
{* 获取 TLS/SSL 握手协议报文 ServerHello 类型中的单个 CipherSuite}

procedure CnSetTLSHandShakeServerHelloCipherSuite(const ServerHello: PCnTLSHandShakeServerHello; CipherSuite: Word);
{* 设置 TLS/SSL 握手协议报文 ServerHello 类型中的单个 CipherSuite}

function CnGetTLSHandShakeServerHelloCompressionMethod(const ServerHello: PCnTLSHandShakeServerHello): Byte;
{* 获取 TLS/SSL 握手协议报文 ServerHello 类型中的单个 CompressionMethod}

procedure CnSetTLSHandShakeServerHelloCompressionMethod(const ServerHello: PCnTLSHandShakeServerHello; CompressionMethod: Byte);
{* 设置 TLS/SSL 握手协议报文 ServerHello 类型中的单个 CompressionMethod}

function CnGetTLSHandShakeServerHelloExtensions(const ServerHello: PCnTLSHandShakeServerHello): PCnTLSHandShakeExtensions; overload;
{* 获取 TLS/SSL 握手协议报文 ServerHello 类型中扩展包头的地址，但单纯从本包内容看，无法判断其是否真实存在}

function CnGetTLSHandShakeServerHelloExtensions(const HandShakeHeader: PCnTLSHandShakeHeader): PCnTLSHandShakeExtensions; overload;
{* 获取 TLS/SSL 握手协议报文 ServerHello 类型中完整包头的长度，结合本包之外的握手包内容长度，可判断是否存在，无则返回 nil}

function CnGetTLSHandShakeCertificateItemCertificateLength(const CertificateItem: PCnTLSHandShakeCertificateItem): Cardinal;
{* 获取 TLS/SSL 握手协议报文 Certificate 中的单个条目的证书长度}

procedure CnSetTLSHandShakeCertificateItemCertificateLength(const CertificateItem: PCnTLSHandShakeCertificateItem; CertificateLength: Cardinal);
{* 设置 TLS/SSL 握手协议报文 Certificate 中的单个条目的证书长度}

function CnGetTLSHandShakeCertificateItemCertificate(const CertificateItem: PCnTLSHandShakeCertificateItem): TBytes;
{* 获取 TLS/SSL 握手协议报文 Certificate 中的单个条目的证书内容}

procedure CnSetTLSHandShakeCertificateItemCertificate(const CertificateItem: PCnTLSHandShakeCertificateItem; Certificate: TBytes);
{* 设置 TLS/SSL 握手协议报文 Certificate 中的单个条目的证书内容}

function CnGetTLSHandShakeCertificateListLength(const Certificate: PCnTLSHandShakeCertificate): Cardinal;
{* 获取 TLS/SSL 握手协议报文 Certificate 中的所有条目的总长度}

procedure CnSetTLSHandShakeCertificateListLength(const Certificate: PCnTLSHandShakeCertificate; CertificateListLength: Cardinal);
{* 设置 TLS/SSL 握手协议报文 Certificate 中的所有条目的总长度}

function CnGetTLSHandShakeCertificateItem(const Certificate: PCnTLSHandShakeCertificate;
  PrevItem: PCnTLSHandShakeCertificateItem = nil): PCnTLSHandShakeCertificateItem;
{* 返回 TLS/SSL 握手协议扩展报文 Certificate 指定证书条目后的下一个证书条目，如果指定条目为 nil，返回第一个}

function CnGetTLSHandShakeSignedParamsFromServerKeyExchange(const SKE: PCnTLSHandShakeServerKeyExchange): PCnTLSHandShakeSignedParams;
{* 返回 TLS/SSL 握手协议报文 Server Key Exchange 中的签名及参数报文的地址}

function CnGetTLSHandShakeServerKeyExchangeNamedCurve(const SKE: PCnTLSHandShakeServerKeyExchange): Word;
{* 返回 TLS/SSL 握手协议报文 Server Key Exchange 中的命名椭圆曲线类型}

procedure CnSetTLSHandShakeServerKeyExchangeNamedCurve(const SKE: PCnTLSHandShakeServerKeyExchange; Name: Word);
{* 设置 TLS/SSL 握手协议报文 Server Key Exchange 中的命名椭圆曲线类型}

function CnGetTLSHandShakeServerKeyExchangeECPoint(const SKE: PCnTLSHandShakeServerKeyExchange): TBytes;
{* 获取 TLS/SSL 握手协议报文 Server Key Exchange 中的椭圆曲线点内容}

procedure CnSetTLSHandShakeServerKeyExchangeECPoint(const SKE: PCnTLSHandShakeServerKeyExchange; ECPoint: TBytes);
{* 设置 TLS/SSL 握手协议报文 Server Key Exchange 中的椭圆曲线点内容}

function CnGetTLSHandShakeSignedParamsSignatureAlgorithm(const SP: PCnTLSHandShakeSignedParams): Word;
{* 返回 TLS/SSL 握手协议报文签名及参数部分的签名算法类型}

procedure CnSetTLSHandShakeSignedParamsSignatureAlgorithm(const SP: PCnTLSHandShakeSignedParams; SigAlg: Word);
{* 设置 TLS/SSL 握手协议报文签名及参数部分的签名算法类型}

function CnGetTLSHandShakeSignedParamsSignatureLength(const SP: PCnTLSHandShakeSignedParams): Word;
{* 返回 TLS/SSL 握手协议报文签名及参数部分的签名长度}

procedure CnSetTLSHandShakeSignedParamsSignatureLength(const SP: PCnTLSHandShakeSignedParams; SigLength: Word);
{* 设置 TLS/SSL 握手协议报文签名及参数部分的签名长度}

function CnGetTLSHandShakeSignedParamsSignature(const SP: PCnTLSHandShakeSignedParams): TBytes;
{* 获取 TLS/SSL 握手协议报文签名及参数部分的签名内容}

procedure CnSetTLSHandShakeSignedParamsSignature(const SP: PCnTLSHandShakeSignedParams; Signature: TBytes);
{* 设置 TLS/SSL 握手协议报文签名及参数部分的签名内容}

function CnGetTLSHandShakeClientKeyExchangeECPoint(const CKE: PCnTLSHandShakeClientKeyExchange; EccFiniteFieldSize: Integer): TBytes;
{* 获取 TLS/SSL 握手协议报文 Client Key Exchange 中的椭圆曲线点内容，包括前缀 04，内部不知长度，需要传入对应椭圆曲线的有限域素数字节长度}

procedure CnSetTLSHandShakeClientKeyExchangeECPoint(const CKE: PCnTLSHandShakeClientKeyExchange; ECPoint: TBytes);
{* 设置 TLS/SSL 握手协议报文 Client Key Exchange 中的椭圆曲线点内容，字节数组需包括前缀 04}

function CnGetTLSTLSHandShakeFinishedVerifyData(const F: PCnTLSHandShakeFinished; DataSize: Integer = 12): TBytes;
{* 获取 TLS/SSL 握手协议报文 Finished 中的 VerifyData，长度需要外界指定，默认 12 字节}

procedure CnSetTLSTLSHandShakeFinishedVerifyData(const F: PCnTLSHandShakeFinished; Data: TBytes);
{* 设置 TLS/SSL 握手协议报文 Finished 中的 VerifyData}

// =========================== IP 地址转换函数 =================================

function CnCardinalToIPv4String(const IP: Cardinal): string;
{* IPv4 整型转换为字符串，IP 要求为 Host 字节顺序，从网络上取来时需要转换}

function CnIPv4StringToCardinal(const IP: string): Cardinal;
{* IPv4 字符串转换为整型，结果为 Host 字节顺序，网络传输时需要再转换}

function CnArrayToIPv6String(const IP: TCnIPv6Array): string;
{* IPv6 双字数组转换为字符串，IP 要求为 Host 字节顺序，从网络上取来时需要转换}

function CnIPv6StringToArray(const IP: string): TCnIPv6Array;
{* IPv6 字符串转换为双字数组，结果为 Host 字节顺序，网络传输时需要再转换，如字符串非法，则填充全 0}

// ============================ 校验和计算函数 =================================

function CnGetNetworkCheckSum(const Buf: Pointer; ByteLength: Cardinal): Word;
{* 以反码进位规则计算一块区域的校验和，以 2 字节为单位，如果区域长度非偶数则补 0 计算
  返回的校验和会转成网络字节顺序}

procedure CnFillIPHeaderCheckSum(const IPHeader: PCnIPHeader);
{* 计算 IP 包头内的校验和并填到包头中}

procedure CnFillICMPHeaderCheckSum(const ICMPHeader: PCnICMPHeader; DataByteLen: Cardinal);
{* 计算 ICMP 包内的校验和并填到包头中，DataByteLen 须是 ICMP 包头后部的数据长度}

// =============================== 其他函数 ====================================

function GetNameFromCipher(Cipher: Word): string;
{* 从 SSL/TLS 的 Cipher 值获取其名称}

implementation

function CnCardinalToIPv4String(const IP: Cardinal): string;
var
  A, B, C, D: Byte;
begin
  A := IP and $FF000000 shr 24;
  B := IP and $00FF0000 shr 16;
  C := IP and $0000FF00 shr 8;
  D := IP and $000000FF;
  Result := Format('%d.%d.%d.%d', [A, B, C, D]);
end;

function CnIPv4StringToCardinal(const IP: string): Cardinal;
var
  MyIP: string;
  P: Integer;
  A, B, C, D: string;
  AA, BB, CC, DD: Byte;
begin
  Result := 0;
  MyIP := IP;

  P := Pos('.', MyIP);
  if P = 0 then
    Exit;

  A := Copy(MyIP, 1, P - 1);
  Delete(MyIP, 1, P);

  P := Pos('.', MyIP);
  if P = 0 then
    Exit;

  B := Copy(MyIP, 1, P - 1);
  Delete(MyIP, 1, P);

  P := Pos('.', MyIP);
  if P = 0 then
    Exit;

  C := Copy(MyIP, 1, P - 1);
  Delete(MyIP, 1, P);

  D := Copy(MyIP, 1, MaxInt);

  try
    AA := StrToInt(A);
    BB := StrToInt(B);
    CC := StrToInt(C);
    DD := StrToInt(D);
  except
    Exit;
  end;
  Result := (AA shl 24) or (BB shl 16) or (CC shl 8) or DD;
end;

function CnArrayToIPv6String(const IP: TCnIPv6Array): string;
begin
  Result := Format('%4.4x:%4.4x:%4.4x:%4.4x:%4.4x:%4.4x:%4.4x:%4.4x',
    [UInt16HostToNetwork(IP[0]), UInt16HostToNetwork(IP[1]), UInt16HostToNetwork(IP[2]),
    UInt16HostToNetwork(IP[3]), UInt16HostToNetwork(IP[4]), UInt16HostToNetwork(IP[5]),
    UInt16HostToNetwork(IP[6]), UInt16HostToNetwork(IP[7])]);
end;

// 将字符串按冒号拆分，确保连续冒号时存在空字符串，但单个冒号在最前、最后的情形又要忽略前后的空字符串
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
    // 查找下一个冒号的位置
    I := StartPos;
    while (I <= Length(S)) and (S[I] <> ':') do
      Inc(I);

    // 如果找到了冒号，检查冒号前的字符串是否为空
    if I = StartPos then
    begin
      // 如果是连续的冒号，则添加空字符串
      Res.Add('');
    end
    else
    begin
      // 添加冒号前的字符串到结果中
      Temp := Copy(S, StartPos, I - StartPos);
      Res.Add(Temp);
    end;

    // 准备下一次搜索，跳过当前的冒号
    StartPos := I + 1;
  end;
  Result := Res.Count;
end;

// 与 CnIP 中类似
function CnIPv6StringToArray(const IP: string): TCnIPv6Array;
var
  SL: TStringList;
  I, ZC, E: Integer;
begin
  FillChar(Result[0], 0, SizeOf(TCnIPv6Array));

  SL := TStringList.Create;
  try
    I := Pos('/', IP);
    if I > 1 then // 子网长度如果存在则去除
      SplitStringByColon(Copy(IP, 1, I - 1), SL)
    else
      SplitStringByColon(IP, SL);

    // 处理 :: 表示连续的 0，确保展开为 8 个
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

    if SL.Count = 8 then
    begin
      // 每一个转换成 16 进制
      for I := 0 to SL.Count - 1 do
        Result[I] := UInt16NetworkToHost(HexToInt(SL[I]));
    end;
  finally
    SL.Free;
  end;
end;

function CnGetNetworkCheckSum(const Buf: Pointer; ByteLength: Cardinal): Word;
var
  Sum: Cardinal;
  S: Word;
  P: PWord;
begin
  Result := 0;
  if (Buf = nil) or (ByteLength = 0) then
    Exit;

  Sum := 0;
  P := PWord(Buf);
  while ByteLength > 1 do
  begin
    Sum := Sum + P^;
    Inc(P);
    Dec(ByteLength, 2);
  end;

  if ByteLength > 0 then
  begin
    S := (PByte(P))^;
    if CurrentByteOrderIsBigEndian then
      S := S shl 8;

    Sum := Sum + S;
  end;

  Result := (Sum and $FFFF) + (Sum shr 16);
  Result := not Result;
end;

procedure CnFillIPHeaderCheckSum(const IPHeader: PCnIPHeader);
var
  W: Word;
begin
  if IPHeader <> nil then
  begin
    IPHeader^.Checksum := 0;
    W := CnGetNetworkCheckSum(IPHeader, SizeOf(TCnIPHeader));
    CnSetIPCheckSum(IPHeader, W);
  end;
end;

procedure CnFillICMPHeaderCheckSum(const ICMPHeader: PCnICMPHeader; DataByteLen: Cardinal);
var
  W: Word;
begin
  if ICMPHeader <> nil then
  begin
    ICMPHeader^.Checksum := 0;
    W := CnGetNetworkCheckSum(ICMPHeader, SizeOf(TCnICMPHeader) + DataByteLen);
    ICMPHeader^.Checksum := W;
  end;
end;

function GetNameFromCipher(Cipher: Word): string;
begin
  case Cipher of
    {* TLS 1.3}
    CN_CIPHER_TLS_AES_128_GCM_SHA256:
      Result := 'TLS_AES_128_GCM_SHA256';
    CN_CIPHER_TLS_AES_256_GCM_SHA384:
      Result := 'TLS_AES_256_GCM_SHA384';
    CN_CIPHER_TLS_CHACHA20_POLY1305_SHA256:
      Result := 'TLS_CHACHA20_POLY1305_SHA256';
    CN_CIPHER_TLS_AES_128_CCM_SHA256:
      Result := 'TLS_AES_128_CCM_SHA256';
    CN_CIPHER_TLS_AES_128_CCM_8_SHA256:
      Result := 'TLS_AES_128_CCM_8_SHA256';
    CN_CIPHER_TLS_SM4_GCM_SM3:
      Result := 'TLS_SM4_GCM_SM3';
    CN_CIPHER_TLS_SM4_CCM_SM3:
      Result := 'TLS_SM4_CCM_SM3';

    {* SSLv3，已停用}
    CN_CIPHER_DHE_RSA_AES256_SHA:
      Result := 'DHE_RSA_AES256_SHA';
    CN_CIPHER_DHE_RSA_AES128_SHA:
      Result := 'DHE_RSA_AES128_SHA';
    CN_CIPHER_SRP_RSA_AES_256_CBC_SHA:
      Result := 'SRP_RSA_AES_256_CBC_SHA';
    CN_CIPHER_SRP_AES_256_CBC_SHA:
      Result := 'SRP_AES_256_CBC_SHA';
    CN_CIPHER_RSA_PSK_AES256_CBC_SHA:
      Result := 'RSA_PSK_AES256_CBC_SHA';
    CN_CIPHER_DHE_PSK_AES256_CBC_SHA:
      Result := 'DHE_PSK_AES256_CBC_SHA';
    CN_CIPHER_AES256_SHA:
      Result := 'AES256_SHA';
    CN_CIPHER_PSK_AES256_CBC_SHA:
      Result := 'PSK_AES256_CBC_SHA';
    CN_CIPHER_SRP_RSA_AES_128_CBC_SHA:
      Result := 'SRP_RSA_AES_128_CBC_SHA';
    CN_CIPHER_SRP_AES_128_CBC_SHA:
      Result := 'SRP_AES_128_CBC_SHA';
    CN_CIPHER_RSA_PSK_AES128_CBC_SHA:
      Result := 'RSA_PSK_AES128_CBC_SHA';
    CN_CIPHER_DHE_PSK_AES128_CBC_SHA:
      Result := 'DHE_PSK_AES128_CBC_SHA';
    CN_CIPHER_AES128_SHA:
      Result := 'AES128_SHA';
    CN_CIPHER_PSK_AES128_CBC_SHA:
      Result := 'PSK_AES128_CBC_SHA';

    {* TLS 1，已停用}
    CN_CIPHER_ECDHE_ECDSA_AES256_SHA:
      Result := 'ECDHE_ECDSA_AES256_SHA';
    CN_CIPHER_ECDHE_RSA_AES256_SHA:
      Result := 'ECDHE_RSA_AES256_SHA';
    CN_CIPHER_ECDHE_ECDSA_AES128_SHA:
      Result := 'ECDHE_ECDSA_AES128_SHA';
    CN_CIPHER_ECDHE_RSA_AES128_SHA:
      Result := 'ECDHE_RSA_AES128_SHA';
    CN_CIPHER_ECDHE_PSK_AES256_CBC_SHA384:
      Result := 'ECDHE_PSK_AES256_CBC_SHA384';
    CN_CIPHER_ECDHE_PSK_AES256_CBC_SHA:
      Result := 'ECDHE_PSK_AES256_CBC_SHA';
    CN_CIPHER_RSA_PSK_AES256_CBC_SHA384:
      Result := 'RSA_PSK_AES256_CBC_SHA384';
    CN_CIPHER_DHE_PSK_AES256_CBC_SHA384:
      Result := 'DHE_PSK_AES256_CBC_SHA384';
    CN_CIPHER_PSK_AES256_CBC_SHA384:
      Result := 'PSK_AES256_CBC_SHA384';
    CN_CIPHER_ECDHE_PSK_AES128_CBC_SHA256:
      Result := 'ECDHE_PSK_AES128_CBC_SHA256';
    CN_CIPHER_ECDHE_PSK_AES128_CBC_SHA:
      Result := 'ECDHE_PSK_AES128_CBC_SHA';
    CN_CIPHER_RSA_PSK_AES128_CBC_SHA256:
      Result := 'RSA_PSK_AES128_CBC_SHA256';
    CN_CIPHER_DHE_PSK_AES128_CBC_SHA256:
      Result := 'DHE_PSK_AES128_CBC_SHA256';
    CN_CIPHER_PSK_AES128_CBC_SHA256:
      Result := 'PSK_AES128_CBC_SHA256';

    {* TLS 1.2，已停用}
    CN_CIPHER_ECDHE_ECDSA_AES256_GCM_SHA384:
      Result := 'ECDHE_ECDSA_AES256_GCM_SHA384';
    CN_CIPHER_ECDHE_RSA_AES256_GCM_SHA384:
      Result := 'ECDHE_RSA_AES256_GCM_SHA384';
    CN_CIPHER_DHE_RSA_AES256_GCM_SHA384:
      Result := 'DHE_RSA_AES256_GCM_SHA384';
    CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305:
      Result := 'ECDHE_ECDSA_CHACHA20_POLY1305';
    CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305:
      Result := 'ECDHE_RSA_CHACHA20_POLY1305';
    CN_CIPHER_DHE_RSA_CHACHA20_POLY1305:
      Result := 'DHE_RSA_CHACHA20_POLY1305';
    CN_CIPHER_ECDHE_ECDSA_AES128_GCM_SHA256:
      Result := 'ECDHE_ECDSA_AES128_GCM_SHA256';
    CN_CIPHER_ECDHE_RSA_AES128_GCM_SHA256:
      Result := 'ECDHE_RSA_AES128_GCM_SHA256';
    CN_CIPHER_DHE_RSA_AES128_GCM_SHA256:
      Result := 'DHE_RSA_AES128_GCM_SHA256';
    CN_CIPHER_ECDHE_ECDSA_AES256_SHA384:
      Result := 'ECDHE_ECDSA_AES256_SHA384';
    CN_CIPHER_ECDHE_RSA_AES256_SHA384:
      Result := 'ECDHE_RSA_AES256_SHA384';
    CN_CIPHER_DHE_RSA_AES256_SHA256:
      Result := 'DHE_RSA_AES256_SHA256';
    CN_CIPHER_ECDHE_ECDSA_AES128_SHA256:
      Result := 'ECDHE_ECDSA_AES128_SHA256';
    CN_CIPHER_ECDHE_RSA_AES128_SHA256:
      Result := 'ECDHE_RSA_AES128_SHA256';
    CN_CIPHER_DHE_RSA_AES128_SHA256:
      Result := 'DHE_RSA_AES128_SHA256';
    CN_CIPHER_RSA_PSK_AES256_GCM_SHA384:
      Result := 'RSA_PSK_AES256_GCM_SHA384';
    CN_CIPHER_DHE_PSK_AES256_GCM_SHA384:
      Result := 'DHE_PSK_AES256_GCM_SHA384';
    CN_CIPHER_RSA_PSK_CHACHA20_POLY1305:
      Result := 'RSA_PSK_CHACHA20_POLY1305';
    CN_CIPHER_DHE_PSK_CHACHA20_POLY1305:
      Result := 'DHE_PSK_CHACHA20_POLY1305';
    CN_CIPHER_ECDHE_PSK_CHACHA20_POLY1305:
      Result := 'ECDHE_PSK_CHACHA20_POLY1305';
    CN_CIPHER_AES256_GCM_SHA384:
      Result := 'AES256_GCM_SHA384';
    CN_CIPHER_PSK_AES256_GCM_SHA384:
      Result := 'PSK_AES256_GCM_SHA384';
    CN_CIPHER_PSK_CHACHA20_POLY1305:
      Result := 'PSK_CHACHA20_POLY1305';
    CN_CIPHER_RSA_PSK_AES128_GCM_SHA256:
      Result := 'RSA_PSK_AES128_GCM_SHA256';
    CN_CIPHER_DHE_PSK_AES128_GCM_SHA256:
      Result := 'DHE_PSK_AES128_GCM_SHA256';
    CN_CIPHER_AES128_GCM_SHA256:
      Result := 'AES128_GCM_SHA256';
    CN_CIPHER_PSK_AES128_GCM_SHA256:
      Result := 'PSK_AES128_GCM_SHA256';
    CN_CIPHER_AES256_SHA256:
      Result := 'AES256_SHA256';
    CN_CIPHER_AES128_SHA256:
      Result := 'AES128_SHA256';
  else
    Result := 'CIPHER_UNKNOWN';
  end;
end;
function CnGetIPVersion(const IPHeader: PCnIPHeader): Integer;
begin
  Result := (IPHeader^.VerionHeaderLength and $F0) shr 4;
end;

function CnGetIPHeaderLength(const IPHeader: PCnIPHeader): Integer;
begin
  Result := IPHeader^.VerionHeaderLength and $0F;
end;

function CnGetIPTypeOfServicePrecedence(const IPHeader: PCnIPHeader): Integer;
begin
  Result := IPHeader^.TypeOfService shr 5;
end;

function CnGetIPTypeOfServiceDelay(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (IPHeader^.TypeOfService and CN_IP_TOS_DELAY_MASK) <> 0;
end;

function CnGetIPTypeOfServiceThroughput(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (IPHeader^.TypeOfService and CN_IP_TOS_THROUGHPUT_MASK) <> 0;
end;

function CnGetIPTypeOfServiceReliability(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (IPHeader^.TypeOfService and CN_IP_TOS_RELIABILITY_MASK) <> 0;
end;

function CnGetIPTotalLength(const IPHeader: PCnIPHeader): Integer;
begin
  Result := UInt16NetworkToHost(IPHeader^.TotalLength);
end;

function CnGetIPIdentification(const IPHeader: PCnIPHeader): Integer;
begin
  Result := UInt16NetworkToHost(IPHeader^.Identification);
end;

function CnGetIPFlagDontFragment(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (UInt16NetworkToHost(IPHeader^.FlagOffset) and CN_IP_FLAG_DONT_FRAGMENT_WORD_MASK) <> 0;
end;

function CnGetIPFlagMoreFragment(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (UInt16NetworkToHost(IPHeader^.FlagOffset) and CN_IP_FLAG_MORE_FRAGMENT_WORD_MASK) <> 0;
end;

function CnGetIPFragmentOffset(const IPHeader: PCnIPHeader): Integer;
begin
  Result := UInt16NetworkToHost(IPHeader^.FlagOffset) and CN_IP_FLAG_FRAGMENT_OFFSET_WORD_MASK;
end;

function CnGetIPChecksum(const IPHeader: PCnIPHeader): Word;
begin
  Result := UInt16NetworkToHost(IPHeader^.Checksum);
end;

function CnGetIPSourceIP(const IPHeader: PCnIPHeader): Cardinal;
begin
  Result := UInt32NetworkToHost(IPHeader^.SourceIp);
end;

function CnGetIPDestIP(const IPHeader: PCnIPHeader): Cardinal;
begin
  Result := UInt32NetworkToHost(IPHeader^.DestIp);
end;

procedure CnSetIPVersion(const IPHeader: PCnIPHeader; Version: Byte);
begin
  IPHeader^.VerionHeaderLength := (Version shl 4) or (IPHeader^.VerionHeaderLength and $0F);
end;

procedure CnSetIPHeaderLength(const IPHeader: PCnIPHeader; HeaderLength: Byte);
begin
  IPHeader^.VerionHeaderLength := (HeaderLength and $0F) or (IPHeader^.VerionHeaderLength and $F0);
end;

procedure CnSetIPTypeOfServicePrecedence(const IPHeader: PCnIPHeader; Precedence: Byte);
begin
  IPHeader^.TypeOfService := (Precedence shl 5) or (IPHeader^.TypeOfService and not CN_IP_TOS_PRECEDENCE_MASK);
end;

procedure CnSetIPTypeOfServiceDelay(const IPHeader: PCnIPHeader; Delay: Boolean);
begin
  if Delay then
    IPHeader^.TypeOfService := IPHeader^.TypeOfService or CN_IP_TOS_DELAY_MASK
  else
    IPHeader^.TypeOfService := IPHeader^.TypeOfService and not CN_IP_TOS_DELAY_MASK;
end;

procedure CnSetIPTypeOfServiceThroughput(const IPHeader: PCnIPHeader; Throughput: Boolean);
begin
  if Throughput then
    IPHeader^.TypeOfService := IPHeader^.TypeOfService or CN_IP_TOS_THROUGHPUT_MASK
  else
    IPHeader^.TypeOfService := IPHeader^.TypeOfService and not CN_IP_TOS_THROUGHPUT_MASK;
end;

procedure CnSetIPTypeOfServiceReliability(const IPHeader: PCnIPHeader; Reliability: Boolean);
begin
  if Reliability then
    IPHeader^.TypeOfService := IPHeader^.TypeOfService or CN_IP_TOS_RELIABILITY_MASK
  else
    IPHeader^.TypeOfService := IPHeader^.TypeOfService and not CN_IP_TOS_RELIABILITY_MASK;
end;

procedure CnSetIPTotalLength(const IPHeader: PCnIPHeader; TotalLength: Word);
begin
  IPHeader^.TotalLength := UInt16HostToNetwork(TotalLength);
end;

procedure CnSetIPIdentification(const IPHeader: PCnIPHeader; Identification: Word);
begin
  IPHeader^.Identification := UInt16HostToNetwork(Identification);
end;

procedure CnSetIPFragmentOffset(const IPHeader: PCnIPHeader; FragmentOffset: Word);
begin
  IPHeader^.FlagOffset := UInt16HostToNetwork((FragmentOffset and CN_IP_FLAG_FRAGMENT_OFFSET_WORD_MASK)
    or (UInt16NetworkToHost(IPHeader^.FlagOffset) and not CN_IP_FLAG_FRAGMENT_OFFSET_WORD_MASK));
end;

procedure CnSetIPChecksum(const IPHeader: PCnIPHeader; Checksum: Word);
begin
  IPHeader^.Checksum := UInt16HostToNetwork(Checksum);
end;

procedure CnSetIPSourceIP(const IPHeader: PCnIPHeader; SourceIP: Cardinal);
begin
  IPHeader^.SourceIp := UInt32HostToNetwork(SourceIP);
end;

procedure CnSetIPDestIP(const IPHeader: PCnIPHeader; DestIP: Cardinal);
begin
  IPHeader^.DestIp := UInt32HostToNetwork(DestIP);
end;

function CnGetTCPSourcePort(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := UInt16NetworkToHost(TCPHeader^.SourcePort);
end;

function CnGetTCPDestPort(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := UInt16NetworkToHost(TCPHeader^.DestPort);
end;

function CnGetTCPSequenceNumber(const TCPHeader: PCnTCPHeader): Cardinal;
begin
  Result := UInt32NetworkToHost(TCPHeader^.SequenceNumber);
end;

function CnGetTCPAcknowledgementNumber(const TCPHeader: PCnTCPHeader): Cardinal;
begin
  Result := UInt32NetworkToHost(TCPHeader^.AcknowledgementNumber);
end;

function CnGetTCPOffset(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := TCPHeader^.Offset shr 4;
end;

function CnGetTCPFlagURG(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_URG_MASK) <> 0;
end;

function CnGetTCPFlagACK(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_ACK_MASK) <> 0;
end;

function CnGetTCPFlagPSH(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_PSH_MASK) <> 0;
end;

function CnGetTCPFlagRST(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_RST_MASK) <> 0;
end;

function CnGetTCPFlagSYN(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_SYN_MASK) <> 0;
end;

function CnGetTCPFlagFIN(const TCPHeader: PCnTCPHeader): Boolean;
begin
  Result := (TCPHeader^.Flags and CN_TCP_FLAG_FIN_MASK) <> 0;
end;

function CnGetTCPWindow(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := UInt16NetworkToHost(TCPHeader^.Window);
end;

function CnGetTCPChecksum(const TCPHeader: PCnTCPHeader): Word;
begin
  Result := UInt16NetworkToHost(TCPHeader^.Checksum);
end;

function CnGetTCPUrgentPointer(const TCPHeader: PCnTCPHeader): Word;
begin
  Result := UInt16NetworkToHost(TCPHeader^.UrgentPointer);
end;

procedure CnSetTCPSourcePort(const TCPHeader: PCnTCPHeader; SourcePort: Word);
begin
  TCPHeader^.SourcePort := UInt16HostToNetwork(SourcePort);
end;

procedure CnSetTCPDestPort(const TCPHeader: PCnTCPHeader; DestPort: Word);
begin
  TCPHeader^.DestPort := UInt16HostToNetwork(DestPort);
end;

procedure CnSetTCPSequenceNumber(const TCPHeader: PCnTCPHeader; SequenceNumber: Cardinal);
begin
  TCPHeader^.SequenceNumber := UInt32HostToNetwork(SequenceNumber);
end;

procedure CnSetTCPAcknowledgementNumber(const TCPHeader: PCnTCPHeader; AcknowledgementNumber: Cardinal);
begin
  TCPHeader^.AcknowledgementNumber := UInt32HostToNetwork(AcknowledgementNumber);
end;

procedure CnSetTCPWindow(const TCPHeader: PCnTCPHeader; Window: Word);
begin
  TCPHeader^.Window := UInt16HostToNetwork(Window);
end;

procedure CnSetTCPChecksum(const TCPHeader: PCnTCPHeader; Checksum: Word);
begin
  TCPHeader^.Checksum := UInt16HostToNetwork(Checksum);
end;

procedure CnSetTCPUrgentPointer(const TCPHeader: PCnTCPHeader; UrgentPointer: Word);
begin
  TCPHeader^.UrgentPointer := UInt16HostToNetwork(UrgentPointer);
end;

function CnGetUDPSourcePort(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := UInt16NetworkToHost(UDPHeader^.SourcePort);
end;

function CnGetUDPDestPort(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := UInt16NetworkToHost(UDPHeader^.DestPort);
end;

function CnGetUDPLength(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := UInt16NetworkToHost(UDPHeader^.Length);
end;

function CnGetUDPChecksum(const UDPHeader: PCnUDPHeader): Word;
begin
  Result := UInt16NetworkToHost(UDPHeader^.Checksum);
end;

procedure CnSetUDPSourcePort(const UDPHeader: PCnUDPHeader; SourcePort: Word);
begin
  UDPHeader^.SourcePort := UInt16HostToNetwork(SourcePort);
end;

procedure CnSetUDPDestPort(const UDPHeader: PCnUDPHeader; DestPort: Word);
begin
  UDPHeader^.DestPort := UInt16HostToNetwork(DestPort);
end;

procedure CnSetUDPLength(const UDPHeader: PCnUDPHeader; ByteLen: Word);
begin
  UDPHeader^.Length := UInt16HostToNetwork(ByteLen);
end;

procedure CnSetUDPChecksum(const UDPHeader: PCnUDPHeader; Checksum: Word);
begin
  UDPHeader^.Checksum := UInt16HostToNetwork(Checksum);
end;

function CnGetICMPType(const ICMPHeader: PCnICMPHeader): Integer;
begin
  Result := ICMPHeader^.MessageType;
end;

function CnGetICMPCode(const ICMPHeader: PCnICMPHeader): Integer;
begin
  Result := ICMPHeader^.Code;
end;

function CnGetICMPChecksum(const ICMPHeader: PCnICMPHeader): Word;
begin
  Result := UInt16NetworkToHost(ICMPHeader^.Checksum);
end;

function CnGetICMPPointer(const ICMPHeader: PCnICMPHeader): Integer;
begin
  Result := ICMPHeader^.Ptr;
end;

function CnGetICMPGatewayAddress(const ICMPHeader: PCnICMPHeader): Cardinal;
begin
  Result := UInt32NetworkToHost(ICMPHeader^.GatewayAddress);
end;

function CnGetICMPIdentifier(const ICMPHeader: PCnICMPHeader): Word;
begin
  Result := UInt16NetworkToHost(ICMPHeader^.Identifier);
end;

function CnGetICMPSequenceNumber(const ICMPHeader: PCnICMPHeader): Word;
begin
  Result := UInt16NetworkToHost(ICMPHeader^.SequenceNumber);
end;

procedure CnSetICMPType(const ICMPHeader: PCnICMPHeader; MessageType: Integer);
begin
  ICMPHeader^.MessageType := MessageType;
end;

procedure CnSetICMPCode(const ICMPHeader: PCnICMPHeader; Code: Integer);
begin
  ICMPHeader^.Code := Code;
end;

procedure CnSetICMPChecksum(const ICMPHeader: PCnICMPHeader; Checksum: Word);
begin
  ICMPHeader^.Checksum := UInt16HostToNetwork(Checksum);
end;

procedure CnSetICMPGatewayAddress(const ICMPHeader: PCnICMPHeader; GatewayAddress: Cardinal);
begin
  ICMPHeader^.GatewayAddress := UInt32HostToNetwork(GatewayAddress);
end;

procedure CnSetICMPIdentifier(const ICMPHeader: PCnICMPHeader; Identifier: Word);
begin
  ICMPHeader^.Identifier := UInt16HostToNetwork(Identifier);
end;

procedure CnSetICMPSequenceNumber(const ICMPHeader: PCnICMPHeader; SequenceNumber: Word);
begin
  ICMPHeader^.SequenceNumber := UInt16HostToNetwork(SequenceNumber);
end;

function CnGetNTPLeapIndicator(const NTPPacket: PCnNTPPacket): Integer;
begin
  Result := (NTPPacket^.LIVNMode and $C0) shr 6;
end;

function CnGetNTPVersionNumber(const NTPPacket: PCnNTPPacket): Integer;
begin
  Result := (NTPPacket^.LIVNMode and $38) shr 3;
end;

function CnGetNTPMode(const NTPPacket: PCnNTPPacket): Integer;
begin
  Result := NTPPacket^.LIVNMode and $07;
end;

procedure CnSetNTPLeapIndicator(const NTPPacket: PCnNTPPacket; LeapIndicator: Integer);
begin
  NTPPacket^.LIVNMode := NTPPacket^.LIVNMode or ((LeapIndicator and $03) shl 6);
end;

procedure CnSetNTPVersionNumber(const NTPPacket: PCnNTPPacket; VersionNumber: Integer);
begin
  NTPPacket^.LIVNMode := NTPPacket^.LIVNMode or ((VersionNumber and $07) shl 3);
end;

procedure CnSetNTPMode(const NTPPacket: PCnNTPPacket; NTPMode: Integer);
begin
  NTPPacket^.LIVNMode := NTPPacket^.LIVNMode or (NTPMode and $07);
end;

function CnConvertNTPTimestampToDateTime(Stamp: Int64): TDateTime;
var
  Sec, Frac: Cardinal;
begin
  Stamp := Int64NetworkToHost(Stamp);
  Sec := Int64Rec(Stamp).Hi;
  Frac := Int64Rec(Stamp).Lo;

  // Sec 的秒数从 1900年1月1日0点开始，但 TDateTime 的日数从1899年12月30日0点开始，差两天
  Result := 2 + (Sec div 86400) + (Sec mod 86400) / 86400.00 +
    Frac / (CN_NTP_MICRO_SEC_FRACTION * 1000 * 1000 * 86400.00);
end;

function CnConvertDateTimeToNTPTimestamp(ADateTime: TDateTime): Int64;
var
  H, M, S, Ms: Word;
  Sec, Frac: Cardinal;
begin
  // Sec 的秒数从 1900年1月1日0点开始，但 TDateTime 的日数从1899年12月30日0点开始，差两天
  ADateTime := ADateTime - 2;
  DecodeTime(ADateTime, H, M, S, Ms);
  Sec := Trunc(ADateTime) * 86400 + H * 3600 + M * 60 + S;
  Frac := Trunc(Ms * 1000 * CN_NTP_MICRO_SEC_FRACTION);

  Int64Rec(Result).Lo := Frac;
  Int64Rec(Result).Hi := Sec;
  Result := Int64HostToNetwork(Result); // 互相转换
end;

function CnGetDNSHeaderId(const DNSHeader: PCnDNSHeader): Word;
begin
  Result := UInt16NetworkToHost(DNSHeader^.Id);
end;

function CnGetDNSHeaderQR(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $80) shr 7;
end;

function CnGetDNSHeaderOpCode(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $78) shr 3;
end;

function CnGetDNSHeaderAA(const DNSHeader: PCnDNSHeader): Boolean;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $04) <> 0;
end;

function CnGetDNSHeaderTC(const DNSHeader: PCnDNSHeader): Boolean;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $02) <> 0;
end;

function CnGetDNSHeaderRD(const DNSHeader: PCnDNSHeader): Boolean;
begin
  Result := (DNSHeader^.QrOpcodeAATCRD and $01) <> 0;
end;

function CnGetDNSHeaderRA(const DNSHeader: PCnDNSHeader): Boolean;
begin
  Result := (DNSHeader^.RAZRCode and $80) <> 0;
end;

function CnGetDNSHeaderRCode(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := DNSHeader^.RAZRCode and $0F;
end;

function CnGetDNSHeaderQDCount(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := UInt16NetworkToHost(DNSHeader^.QDCount);
end;

function CnGetDNSHeaderANCount(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := UInt16NetworkToHost(DNSHeader^.ANCount);
end;

function CnGetDNSHeaderNSCount(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := UInt16NetworkToHost(DNSHeader^.NSCount);
end;

function CnGetDNSHeaderARCount(const DNSHeader: PCnDNSHeader): Integer;
begin
  Result := UInt16NetworkToHost(DNSHeader^.ARCount);
end;

procedure CnSetDNSHeaderId(const DNSHeader: PCnDNSHeader; Id: Word);
begin
  DNSHeader^.Id := UInt16HostToNetwork(Id);
end;

procedure CnSetDNSHeaderQR(const DNSHeader: PCnDNSHeader; IsQuery: Boolean);
begin
  if IsQuery then
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD and $7F
  else
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD or $80;
end;

procedure CnSetDNSHeaderOpCode(const DNSHeader: PCnDNSHeader; QueryType: Byte);
begin
  DNSHeader^.QrOpcodeAATCRD := (DNSHeader^.QrOpcodeAATCRD and $87) or Byte(QueryType shl 3);
end;

procedure CnSetDNSHeaderAA(const DNSHeader: PCnDNSHeader; AuthoritativeAnswer: Boolean);
begin
  if AuthoritativeAnswer then
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD or $04
  else
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD and $FB;
end;

procedure CnSetDNSHeaderTC(const DNSHeader: PCnDNSHeader; TrunCation: Boolean);
begin
  if TrunCation then
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD or $02
  else
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD and $FD;
end;

procedure CnSetDNSHeaderRD(const DNSHeader: PCnDNSHeader; RecursionDesired: Boolean);
begin
  if RecursionDesired then
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD or $01
  else
    DNSHeader^.QrOpcodeAATCRD := DNSHeader^.QrOpcodeAATCRD and $FE;
end;

procedure CnSetDNSHeaderRA(const DNSHeader: PCnDNSHeader; RecursionAvailable: Boolean);
begin
  if RecursionAvailable then
    DNSHeader^.RAZRCode := DNSHeader^.RAZRCode or $80
  else
    DNSHeader^.RAZRCode := DNSHeader^.RAZRCode and $7F;
end;

procedure CnSetDNSHeaderRCode(const DNSHeader: PCnDNSHeader; RCode: Byte);
begin
  DNSHeader^.RAZRCode := (DNSHeader^.RAZRCode and $F0) or (RCode and $0F);
end;

procedure CnSetDNSHeaderQDCount(const DNSHeader: PCnDNSHeader; Count: Word);
begin
  DNSHeader^.QDCount := UInt16HostToNetwork(Count);
end;

procedure CnSetDNSHeaderANCount(const DNSHeader: PCnDNSHeader; Count: Word);
begin
  DNSHeader^.ANCount := UInt16HostToNetwork(Count);
end;

procedure CnSetDNSHeaderNSCount(const DNSHeader: PCnDNSHeader; Count: Word);
begin
  DNSHeader^.NSCount := UInt16HostToNetwork(Count);
end;

procedure CnSetDNSHeaderARCount(const DNSHeader: PCnDNSHeader; Count: Word);
begin
  DNSHeader^.ARCount := UInt16HostToNetwork(Count);
end;

function CnGetSocksRequestDestinationAddress(const SocksReq: PCnSocksRequest): string;
var
  Len: Integer;
  Res: AnsiString;
  IPv6: TCnIPv6Array;
begin
  Result := '';
  case SocksReq^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      begin
        Result := CnCardinalToIPv4String(UInt32NetworkToHost(SocksReq^.DestionationAddress.IpV4Address));
      end;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      begin
        Move(SocksReq^.DestionationAddress.IpV6Address[0], IPv6[0], SizeOf(TCnIPv6Array));
        Result := CnArrayToIPv6String(IPv6);
      end;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksReq^.DestionationAddress.DomainNameLen;
        SetLength(Res, Len);
        Move(SocksReq^.DestionationAddress.DomainName, Res[1], Len);
        Result := string(Res);
      end;
  end;
end;

function CnGetSocksRequestDestinationPort(const SocksReq: PCnSocksRequest): Word;
var
  Len: Integer;
  PortAddr: PWORD;
begin
  Result := 0;
  Len := 0;

  case SocksReq^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      Len := 4;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      Len := 6;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksReq^.DestionationAddress.DomainNameLen + 1;
      end;
  end;

  if Len > 0 then
  begin
{$IFDEF CPU64BITS}
    PortAddr := PWORD(NativeInt(@(SocksReq^.DestionationAddress)) + Len);
{$ELSE}
    PortAddr := PWORD(Integer(@(SocksReq^.DestionationAddress)) + Len);
{$ENDIF}
    Result := UInt16NetworkToHost(PortAddr^);
  end;
end;

function CnGetSocksResponseBindAddress(const SocksResp: PCnSocksResponse): string;
var
  Len: Integer;
  Res: AnsiString;
  IPv6: TCnIPv6Array;
begin
  Result := '';
  case SocksResp^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      begin
        Result := CnCardinalToIPv4String(UInt32NetworkToHost(SocksResp^.BindAddress.IpV4Address));
      end;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      begin
        Move(SocksResp^.BindAddress.IpV6Address[0], IPv6[0], SizeOf(TCnIPv6Array));
        Result := CnArrayToIPv6String(IPv6);
      end;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksResp^.BindAddress.DomainNameLen;
        SetLength(Res, Len);
        Move(SocksResp^.BindAddress.DomainName, Res[1], Len);
        Result := string(Res);
      end;
  end;
end;

function CnGetSocksResponseBindPort(const SocksResp: PCnSocksResponse): Word;
var
  Len: Integer;
  PortAddr: PWORD;
begin
  Result := 0;
  Len := 0;

  case SocksResp^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      Len := 4;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      Len := 6;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksResp^.BindAddress.DomainNameLen + 1;
      end;
  end;

  if Len > 0 then
  begin
{$IFDEF CPU64BITS}
    PortAddr := PWORD(NativeInt(@(SocksResp^.BindAddress)) + Len);
{$ELSE}
    PortAddr := PWORD(Integer(@(SocksResp^.BindAddress)) + Len);
{$ENDIF}
    Result := UInt16NetworkToHost(PortAddr^);
  end;
end;

procedure CnSetSocksRequestDestinationAddress(const SocksReq: PCnSocksRequest; Address: string);
var
  IP: Cardinal;
  AnsiAddress: AnsiString;
begin
  IP := UInt32HostToNetwork(CnIPv4StringToCardinal(Address));
  if IP = 0 then // 非法 IP，表示是域名
  begin
    SocksReq^.AddressType := CN_SOCKS_ADDRESS_TYPE_DOMAINNAME;
    AnsiAddress := AnsiString(Address);
    SocksReq^.DestionationAddress.DomainNameLen := Length(AnsiAddress);
    if AnsiAddress <> '' then
      Move(AnsiAddress[1], SocksReq^.DestionationAddress.DomainName[0],
        SocksReq^.DestionationAddress.DomainNameLen);
  end
  else
  begin
    SocksReq^.AddressType := CN_SOCKS_ADDRESS_TYPE_IPV4;
    SocksReq^.DestionationAddress.IpV4Address := IP;
  end;
end;

function CnSetSocksRequestDestinationPort(const SocksReq: PCnSocksRequest;
  Port: Word): Integer;
var
  Len: Integer;
  PortAddr: PWORD;
begin
  Len := 0;

  case SocksReq^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      Len := 4;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      Len := 6;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksReq^.DestionationAddress.DomainNameLen + 1;
      end;
  end;

  if Len > 0 then
  begin
{$IFDEF CPU64BITS}
    PortAddr := PWORD(NativeInt(@(SocksReq^.DestionationAddress)) + Len);
{$ELSE}
    PortAddr := PWORD(Integer(@(SocksReq^.DestionationAddress)) + Len);
{$ENDIF}
    PortAddr^ := UInt16HostToNetwork(Port);
  end;
  Result := Len + 4 + SizeOf(Word);
end;

procedure CnSetSocksResponseBindAddress(const SocksResp: PCnSocksResponse; Address: string);
var
  IP: Cardinal;
  AnsiAddress: AnsiString;
begin
  IP := UInt32HostToNetwork(CnIPv4StringToCardinal(Address));
  if IP = 0 then // 非法 IP，表示是域名
  begin
    SocksResp^.AddressType := CN_SOCKS_ADDRESS_TYPE_DOMAINNAME;
    AnsiAddress := AnsiString(Address);
    SocksResp^.BindAddress.DomainNameLen := Length(AnsiAddress);
    if AnsiAddress <> '' then
      Move(AnsiAddress[1], SocksResp^.BindAddress.DomainName[0],
        SocksResp^.BindAddress.DomainNameLen);
  end
  else
  begin
    SocksResp^.AddressType := CN_SOCKS_ADDRESS_TYPE_IPV4;
    SocksResp^.BindAddress.IpV4Address := IP;
  end;
end;

function CnSetSocksResponseBindPort(const SocksResp: PCnSocksResponse; Port: Word): Integer;
var
  Len: Integer;
  PortAddr: PWORD;
begin
  Len := 0;

  case SocksResp^.AddressType of
    CN_SOCKS_ADDRESS_TYPE_IPV4:
      Len := 4;
    CN_SOCKS_ADDRESS_TYPE_IPV6:
      Len := 6;
    CN_SOCKS_ADDRESS_TYPE_DOMAINNAME:
      begin
        Len := SocksResp^.BindAddress.DomainNameLen + 1;
      end;
  end;

  if Len > 0 then
  begin
{$IFDEF CPU64BITS}
    PortAddr := PWORD(NativeInt(@(SocksResp^.BindAddress)) + Len);
{$ELSE}
    PortAddr := PWORD(Integer(@(SocksResp^.BindAddress)) + Len);
{$ENDIF}
    PortAddr^ := UInt16HostToNetwork(Port);
  end;
  Result := Len + 4 + SizeOf(Word);
end;

procedure CnFillBGPHeaderMarkers(const BGPHeader: PCnBGPHeader);
begin
  FillChar(BGPHeader^.Marker[0], Length(BGPHeader^.Marker), $FF);
end;

function CnGetBGPHeaderLength(const BGPHeader: PCnBGPHeader): Word;
begin
  Result := UInt16NetworkToHost(BGPHeader^.Length);
end;

procedure CnSetBGPHeaderLength(const BGPHeader: PCnBGPHeader; Length: Word);
begin
  BGPHeader^.Length := UInt16HostToNetwork(Length);
end;

function CnGetTLSRecordLayerBodyLength(const RecordLayer: PCnTLSRecordLayer): Word;
begin
  Result := UInt16NetworkToHost(RecordLayer^.BodyLength);
end;

procedure CnSetTLSRecordLayerBodyLength(const RecordLayer: PCnTLSRecordLayer; BodyLength: Word);
begin
  RecordLayer^.BodyLength := UInt16HostToNetwork(BodyLength);
end;

function CnGetTLSHandShakeHeaderContentLength(const HandShakeHeader: PCnTLSHandShakeHeader): Cardinal;
begin
  Result := (HandShakeHeader^.LengthHi shl 16) + UInt16NetworkToHost(HandShakeHeader^.LengthLo);
end;

procedure CnSetTLSHandShakeHeaderContentLength(const HandShakeHeader: PCnTLSHandShakeHeader; ContentLength: Cardinal);
begin
  HandShakeHeader^.LengthHi := (ContentLength shr 16) and $00FF;
  HandShakeHeader^.LengthLo := UInt16HostToNetwork(ContentLength and $FFFF);
end;

function CnGetTLSHandShakeExtensionsExtensionLength(const Extensions: PCnTLSHandShakeExtensions): Word;
begin
  Result := UInt16NetworkToHost(Extensions^.ExtensionLength);
end;

procedure CnSetTLSHandShakeExtensionsExtensionLength(const Extensions: PCnTLSHandShakeExtensions; ExtLenth: Word);
begin
  Extensions^.ExtensionLength := UInt16HostToNetwork(ExtLenth);
end;

procedure CnSetTLSHandShakeExtensionsExtensionLengthByItemCount(const Extensions: PCnTLSHandShakeExtensions; ExtItemCount: Integer);
var
  T, W: Word;
  P: PByte;
begin
  if ExtItemCount <= 0 then
    CnSetTLSHandShakeExtensionsExtensionLength(Extensions, 0)
  else
  begin
    T := 0;
    P :=  PByte(CnGetTLSHandShakeExtensionsExtensionItem(Extensions)); // 拿第一个 Item
    repeat
      // 计算这个 Item 的长度
      W := 0;
      Inc(W, SizeOf(Word));
      Inc(W, SizeOf(Word));
      Inc(W, CnGetTLSHandShakeExtensionsExtensionDataLength(PCnTLSHandShakeExtensionItem(P)));

      // P 步进至下一 Item
      Inc(P, W);

      // 累计长度到 T
      Inc(T, W);
      Dec(ExtItemCount);
    until ExtItemCount = 0;

    CnSetTLSHandShakeExtensionsExtensionLength(Extensions, T);
  end;
end;

function CnGetTLSHandShakeExtensionsExtensionItem(const Extensions: PCnTLSHandShakeExtensions;
  PrevItem: PCnTLSHandShakeExtensionItem): PCnTLSHandShakeExtensionItem;
var
  P: PByte;
begin
  if PrevItem = nil then
  begin
    Result := @(Extensions^.Extension);
  end
  else
  begin
    P := PByte(PrevItem);
    Inc(P, SizeOf(Word));
    Inc(P, SizeOf(Word));
    Inc(P, CnGetTLSHandShakeExtensionsExtensionDataLength(PrevItem));
    Result := PCnTLSHandShakeExtensionItem(P);
  end;
end;

function CnGetTLSHandShakeExtensionsExtensionType(const ExtensionItem: PCnTLSHandShakeExtensionItem): Word;
begin
  Result := UInt16NetworkToHost(ExtensionItem^.ExtensionType);
end;

procedure CnSetTLSHandShakeExtensionsExtensionType(const ExtensionItem: PCnTLSHandShakeExtensionItem; ExtType: Word);
begin
  ExtensionItem^.ExtensionType := UInt16HostToNetwork(ExtType);
end;

function CnGetTLSHandShakeExtensionsExtensionDataLength(const ExtensionItem: PCnTLSHandShakeExtensionItem): Word;
begin
  Result := UInt16NetworkToHost(ExtensionItem^.ExtensionDataLength);
end;

procedure CnSetTLSHandShakeExtensionsExtensionDataLength(const ExtensionItem: PCnTLSHandShakeExtensionItem; ExtDataLength: Word);
begin
  ExtensionItem^.ExtensionDataLength := UInt16HostToNetwork(ExtDataLength);
end;

function CnGetTLSHandShakeExtensionsExtensionData(const ExtensionItem: PCnTLSHandShakeExtensionItem): Pointer;
begin
  Result := @(ExtensionItem^.ExtensionData[0]);
end;

procedure CnSetTLSHandShakeExtensionsExtensionData(const ExtensionItem: PCnTLSHandShakeExtensionItem; Data: TBytes);
begin
  if Length(Data) > 0 then
  begin
    CnSetTLSHandShakeExtensionsExtensionDataLength(ExtensionItem, Length(Data));
    Move(Data[0], CnGetTLSHandShakeExtensionsExtensionData(ExtensionItem)^, Length(Data));
  end
  else
    CnSetTLSHandShakeExtensionsExtensionDataLength(ExtensionItem, 0);
end;

function CnGetTLSHandShakeServerNameIndicationListLength(const SNI: PCnTLSHandShakeServerNameIndication): Word;
begin
  Result := UInt16NetworkToHost(SNI^.ListLength);
end;

procedure CnSetTLSHandShakeServerNameIndicationListLength(const SNI: PCnTLSHandShakeServerNameIndication; ListLength: Word);
begin
  SNI^.ListLength := UInt16HostToNetwork(ListLength);
end;

function CnGetTLSHandShakeServerNameIndicationNameLength(const SNI: PCnTLSHandShakeServerNameIndication): Word;
begin
  Result := UInt16NetworkToHost(SNI^.NameLength);
end;

procedure CnSetTLSHandShakeServerNameIndicationNameLength(const SNI: PCnTLSHandShakeServerNameIndication; NameLength: Word);
begin
  SNI^.NameLength := UInt16HostToNetwork(NameLength);
end;

function CnTLSHandShakeServerNameIndicationAddHost(const SNI: PCnTLSHandShakeServerNameIndication; const HostName: AnsiString): Integer;
var
  OL: Word;
  LSNI: PCnTLSHandShakeServerNameIndication;
begin
  Result := 0;
  if HostName = '' then
    Exit;

  OL := CnGetTLSHandShakeServerNameIndicationListLength(SNI);
  if OL < 2 then
  begin
    LSNI := SNI;
    OL := 0;
  end
  else
    LSNI := PCnTLSHandShakeServerNameIndication(TCnIntAddress(SNI) + OL - SizeOf(Word));

  // 给新的位置增加内容
  LSNI^.NameType := CN_TLS_EXTENSION_NAMETYPE_HOSTNAME;
  CnSetTLSHandShakeServerNameIndicationNameLength(LSNI, Length(HostName));
  Move(HostName[1], LSNI^.Name[0], Length(HostName));

  // 更新旧的位置的列表长度
  CnSetTLSHandShakeServerNameIndicationListLength(SNI, OL + Length(HostName) + 3); // 3 表示一个 NameType 一个 NameLength
  Result := CnGetTLSHandShakeServerNameIndicationListLength(SNI) + SizeOf(Word);   // 加上 ListLength 自身
end;

function CnGetTLSHandShakeSupportedGroupsNameLength(const SG: PCnTLSHandShakeSupportedGroups): Word;
begin
  Result := UInt16NetworkToHost(SG^.NameLength);
end;

procedure CnSetTLSHandShakeSupportedGroupsNameLength(const SG: PCnTLSHandShakeSupportedGroups; NameLength: Word);
begin
  SG^.NameLength := UInt16HostToNetwork(NameLength);
end;

function CnGetTLSHandShakeSupportedGroups(const ExtensionItem: PCnTLSHandShakeExtensionItem): TWords;
var
  L: Integer;
  SG: PCnTLSHandShakeSupportedGroups;
  T: PWord;
begin
  if (CnGetTLSHandShakeExtensionsExtensionType(ExtensionItem) = CN_TLS_EXTENSIONTYPE_SUPPORTED_GROUPS)
    and (ExtensionItem^.ExtensionDataLength > 2) then
  begin
    SG := PCnTLSHandShakeSupportedGroups(CnGetTLSHandShakeExtensionsExtensionData(ExtensionItem));
    L := CnGetTLSHandShakeSupportedGroupsNameLength(SG);

    if L >= 2 then
    begin
      SetLength(Result, L shr 1);
      T := PWord(@SG^.Name[0]);

      for L := 0 to Length(Result) - 1 do
      begin
        Result[L] := UInt16NetworkToHost(T^);
        Inc(T);
      end;
    end;
  end
  else
    Result := nil;
end;

procedure CnSetTLSHandShakeSupportedGroups(const ExtensionItem: PCnTLSHandShakeExtensionItem; Groups: TWords);
var
  I: Integer;
  SG: PCnTLSHandShakeSupportedGroups;
  T: PWord;
begin
  CnSetTLSHandShakeExtensionsExtensionType(ExtensionItem, CN_TLS_EXTENSIONTYPE_SUPPORTED_GROUPS);
  CnSetTLSHandShakeExtensionsExtensionDataLength(ExtensionItem, Length(Groups) * SizeOf(Word) + SizeOf(Word));

  SG := PCnTLSHandShakeSupportedGroups(CnGetTLSHandShakeExtensionsExtensionData(ExtensionItem));
  CnSetTLSHandShakeSupportedGroupsNameLength(SG, Length(Groups) * SizeOf(Word));

  T := PWord(@SG^.Name[0]);
  for I := 0 to Length(Groups) - 1 do
  begin
    T^ := UInt16HostToNetwork(Groups[I]);
    Inc(T);
  end;
end;

procedure CnSetTLSHandShakeECPointFormats(const ExtensionItem: PCnTLSHandShakeExtensionItem; PointFormat: Byte);
var
  PF: PCnTLSHandShakeECPointFormats;
begin
  CnSetTLSHandShakeExtensionsExtensionType(ExtensionItem, CN_TLS_EXTENSIONTYPE_EC_POINT_FORMATS);
  CnSetTLSHandShakeExtensionsExtensionDataLength(ExtensionItem, SizeOf(Byte) + SizeOf(Byte));

  PF := PCnTLSHandShakeECPointFormats(CnGetTLSHandShakeExtensionsExtensionData(ExtensionItem));
  PF^.FormatLength := SizeOf(Byte);
  PF^.PointFormat := PointFormat;
end;

function CnGetTLSHandShakeSignatureAlgorithmsSigAlgLength(const SG: PCnTLSHandShakeSignatureAlgorithms): Word;
begin
  Result := UInt16NetworkToHost(SG^.SignatureAlgorithmsLength);
end;

procedure CnSetTLSHandShakeSignatureAlgorithmsSigAlgLength(const SG: PCnTLSHandShakeSignatureAlgorithms; SigAlgLength: Word);
begin
  SG^.SignatureAlgorithmsLength := UInt16HostToNetwork(SigAlgLength);
end;

function CnGetTLSHandShakeSignatureAlgorithms(const ExtensionItem: PCnTLSHandShakeExtensionItem): TWords;
var
  L: Integer;
  SA: PCnTLSHandShakeSignatureAlgorithms;
  T: PWord;
begin
  if (CnGetTLSHandShakeExtensionsExtensionType(ExtensionItem) = CN_TLS_EXTENSIONTYPE_SIGNATURE_ALGORITHMS)
    and (ExtensionItem^.ExtensionDataLength > 2) then
  begin
    SA := PCnTLSHandShakeSignatureAlgorithms(CnGetTLSHandShakeExtensionsExtensionData(ExtensionItem));
    L := CnGetTLSHandShakeSignatureAlgorithmsSigAlgLength(SA);

    if L >= 2 then
    begin
      SetLength(Result, L shr 1);
      T := PWord(@SA^.SignatureAlgorithms[0]);

      for L := 0 to Length(Result) - 1 do
      begin
        Result[L] := UInt16NetworkToHost(T^);
        Inc(T);
      end;
    end;
  end
  else
    Result := nil;
end;

procedure CnSetTLSHandShakeSignatureAlgorithms(const ExtensionItem: PCnTLSHandShakeExtensionItem; SigAlgs: TWords);
var
  I: Integer;
  SA: PCnTLSHandShakeSignatureAlgorithms;
  T: PWord;
begin
  CnSetTLSHandShakeExtensionsExtensionType(ExtensionItem, CN_TLS_EXTENSIONTYPE_SIGNATURE_ALGORITHMS);
  CnSetTLSHandShakeExtensionsExtensionDataLength(ExtensionItem, Length(SigAlgs) * SizeOf(Word) + SizeOf(Word));

  SA := PCnTLSHandShakeSignatureAlgorithms(CnGetTLSHandShakeExtensionsExtensionData(ExtensionItem));
  CnSetTLSHandShakeSignatureAlgorithmsSigAlgLength(SA, Length(SigAlgs) * SizeOf(Word));

  T := PWord(@SA^.SignatureAlgorithms[0]);
  for I := 0 to Length(SigAlgs) - 1 do
  begin
    T^ := UInt16HostToNetwork(SigAlgs[I]);
    Inc(T);
  end;
end;

function CnGetTLSHandShakeClientHelloSessionId(const ClientHello: PCnTLSHandShakeClientHello): TBytes;
begin
  SetLength(Result, ClientHello^.SessionLength);
  if ClientHello^.SessionLength > 0 then
    Move(ClientHello^.SessionId[0], Result[0], ClientHello^.SessionLength);
end;

procedure CnSetTLSHandShakeClientHelloSessionId(const ClientHello: PCnTLSHandShakeClientHello; SessionId: TBytes);
begin
  ClientHello^.SessionLength := Byte(Length(SessionId));
  if ClientHello^.SessionLength > 0 then
    Move(SessionId[0], ClientHello^.SessionId[0], ClientHello^.SessionLength);
end;

function CnGetTLSHandShakeClientHelloCipherSuitesLength(const ClientHello: PCnTLSHandShakeClientHello): Word;
var
  P: PByte;
  W: PCnWord;
begin
  P := @(ClientHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^); // SessionLength 本身加 SessionId 长度
  W := PCnWord(P);
  Result := UInt16NetworkToHost(W^);
end;

procedure CnSetTLSHandShakeClientHelloCipherSuitesLength(const ClientHello: PCnTLSHandShakeClientHello; CipherSuitesLength: Word);
var
  P: PByte;
  W: PCnWord;
begin
  P := @(ClientHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  W := PCnWord(P);
  W^ := UInt16HostToNetwork(CipherSuitesLength);
end;

function CnGetTLSHandShakeClientHelloCipherSuites(const ClientHello: PCnTLSHandShakeClientHello): TWords;
var
  I: Integer;
  L: Word;
  P: PByte;
begin
  P := @(ClientHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  L := CnGetTLSHandShakeClientHelloCipherSuitesLength(ClientHello) shr 1;
  Inc(P, SizeOf(Word));

  SetLength(Result, L);
  if L > 0 then
  begin
    Move(P^, Result[0], L shl 1);
    for I := 0 to Length(Result) - 1 do
      Result[I] := UInt16HostToNetwork(Result[I]);
  end;
end;

procedure CnSetTLSHandShakeClientHelloCipherSuites(const ClientHello: PCnTLSHandShakeClientHello; CipherSuites: TWords);
var
  I: Integer;
  L: Word;
  P: PByte;
begin
  L := Length(CipherSuites);
  if L > 0 then
  begin
    CnSetTLSHandShakeClientHelloCipherSuitesLength(ClientHello, L * SizeOf(Word));

    P := @(ClientHello^.SessionLength);
    Inc(P, SizeOf(Byte) + P^);
    Inc(P, SizeOf(Word));

    for I := 0 to Length(CipherSuites) - 1 do
      CipherSuites[I] := UInt16HostToNetwork(CipherSuites[I]);
    Move(CipherSuites[0], P^, L * SizeOf(Word));
  end;
end;

function CnGetTLSHandShakeClientHelloCompressionMethodLength(const ClientHello: PCnTLSHandShakeClientHello): Byte;
var
  P: PByte;
  L: Word;
begin
  P := @(ClientHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  L := CnGetTLSHandShakeClientHelloCipherSuitesLength(ClientHello);
  Inc(P, SizeOf(Word) + L);
  Result := P^;
end;

procedure CnSetTLSHandShakeClientHelloCompressionMethodLength(const ClientHello: PCnTLSHandShakeClientHello; CompressionMethodLength: Byte);
var
  P: PByte;
  L: Word;
begin
  P := @(ClientHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  L := CnGetTLSHandShakeClientHelloCipherSuitesLength(ClientHello);
  Inc(P, SizeOf(Word) + L);
  P^ := CompressionMethodLength;
end;

function CnGetTLSHandShakeClientHelloCompressionMethod(const ClientHello: PCnTLSHandShakeClientHello): TBytes;
var
  P: PByte;
  L: Word;
  B: Byte;
begin
  P := @(ClientHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  L := CnGetTLSHandShakeClientHelloCipherSuitesLength(ClientHello);
  Inc(P, SizeOf(Word) + L);
  B := P^;

  Inc(P);
  SetLength(Result, B);
  if B > 0 then
    Move(P^, Result[0], B);
end;

procedure CnSetTLSHandShakeClientHelloCompressionMethod(const ClientHello: PCnTLSHandShakeClientHello; CompressionMethod: TBytes);
var
  P: PByte;
  L: Word;
begin
  P := @(ClientHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  L := CnGetTLSHandShakeClientHelloCipherSuitesLength(ClientHello);
  Inc(P, SizeOf(Word) + L);
  P^ := Length(CompressionMethod);

  Inc(P);
  if Length(CompressionMethod) > 0 then
    Move(CompressionMethod[0], P^, Length(CompressionMethod));
end;

function CnGetTLSHandShakeClientHelloExtensions(const ClientHello: PCnTLSHandShakeClientHello): PCnTLSHandShakeExtensions;
var
  B: PByte;
begin
  B := @ClientHello^.SessionId[0];
  Inc(B, ClientHello^.SessionLength);
  Inc(B, SizeOf(Word)); // 跳过 CipherSuitesLength 双字节
  Inc(B, CnGetTLSHandShakeClientHelloCipherSuitesLength(ClientHello));    // 跳过 CipherSuites，指向 CompressionMethodLength
  Inc(B, B^ + SizeOf(Byte)); // 跳过这个 Byte 和其 CompressionMethod
  Result := PCnTLSHandShakeExtensions(B);
end;

function CnGetTLSHandShakeClientHelloExtensions(const HandShakeHeader: PCnTLSHandShakeHeader): PCnTLSHandShakeExtensions;
var
  L: Cardinal;
  P: PCnTLSHandShakeExtensions;
begin
  L := CnGetTLSHandShakeHeaderContentLength(HandShakeHeader);
  P := CnGetTLSHandShakeClientHelloExtensions(PCnTLSHandShakeClientHello(@HandShakeHeader^.Content[0]));

  // 如果握手包头里的内容长度大于 ClientHello 的实际长度，才有扩展头存在
  if L > (TCnIntAddress(P) - TCnIntAddress(HandShakeHeader)) then
    Result := PCnTLSHandShakeExtensions(P)
  else
    Result := nil;
end;

function CnGetTLSHandShakeServerHelloSessionId(const ServerHello: PCnTLSHandShakeServerHello): TBytes;
begin
  SetLength(Result, ServerHello^.SessionLength);
  if ServerHello^.SessionLength > 0 then
    Move(ServerHello^.SessionId[0], Result[0], ServerHello^.SessionLength);
end;

procedure CnSetTLSHandShakeServerHelloSessionId(const ServerHello: PCnTLSHandShakeServerHello; SessionId: TBytes);
begin
  ServerHello^.SessionLength := Byte(Length(SessionId));
  if ServerHello^.SessionLength > 0 then
    Move(SessionId[0], ServerHello^.SessionId[0], ServerHello^.SessionLength);
end;

function CnGetTLSHandShakeServerHelloCipherSuite(const ServerHello: PCnTLSHandShakeServerHello): Word;
var
  P: PByte;
  T: PWord;
begin
  P := @(ServerHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  T := PWord(P);
  Result := UInt16NetworkToHost(T^);
end;

procedure CnSetTLSHandShakeServerHelloCipherSuite(const ServerHello: PCnTLSHandShakeServerHello; CipherSuite: Word);
var
  P: PByte;
  T: PWord;
begin
  P := @(ServerHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  T := PWord(P);
  T^ := UInt16HostToNetwork(CipherSuite);
end;

function CnGetTLSHandShakeServerHelloCompressionMethod(const ServerHello: PCnTLSHandShakeServerHello): Byte;
var
  P: PByte;
begin
  P := @(ServerHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  Inc(P, SizeOf(Word));
  Result := P^;
end;

procedure CnSetTLSHandShakeServerHelloCompressionMethod(const ServerHello: PCnTLSHandShakeServerHello; CompressionMethod: Byte);
var
  P: PByte;
begin
  P := @(ServerHello^.SessionLength);
  Inc(P, SizeOf(Byte) + P^);
  Inc(P, SizeOf(Word));
  P^ := CompressionMethod;
end;

function CnGetTLSHandShakeServerHelloExtensions(const ServerHello: PCnTLSHandShakeServerHello): PCnTLSHandShakeExtensions;
var
  B: PByte;
begin
  B := @ServerHello^.SessionId[0];
  Inc(B, ServerHello^.SessionLength);
  Inc(B, SizeOf(Word)); // 跳过 CipherSuite 双字节，指向 CompressionMethod
  Inc(B, SizeOf(Byte)); // 跳过 CompressionMethod 字节
  Result := PCnTLSHandShakeExtensions(B);
end;

function CnGetTLSHandShakeServerHelloExtensions(const HandShakeHeader: PCnTLSHandShakeHeader): PCnTLSHandShakeExtensions;
var
  L: Cardinal;
  P: PCnTLSHandShakeExtensions;
begin
  L := CnGetTLSHandShakeHeaderContentLength(HandShakeHeader);
  P := CnGetTLSHandShakeServerHelloExtensions(PCnTLSHandShakeServerHello(@HandShakeHeader^.Content[0]));

  // 如果握手包头里的内容长度大于 ServerHello 的实际长度，才有扩展头存在
  if L > (TCnIntAddress(P) - TCnIntAddress(HandShakeHeader)) then
    Result := PCnTLSHandShakeExtensions(P)
  else
    Result := nil;
end;

function CnGetTLSHandShakeCertificateItemCertificateLength(const CertificateItem: PCnTLSHandShakeCertificateItem): Cardinal;
var
  P: PByteArray;
begin
  P := PByteArray(@(CertificateItem^.CertificateLength[0]));
  Result := (P^[0] shl 16) or (P^[1] shl 8) or P^[2];
end;

procedure CnSetTLSHandShakeCertificateItemCertificateLength(const CertificateItem: PCnTLSHandShakeCertificateItem; CertificateLength: Cardinal);
var
  P: PByteArray;
begin
  P := PByteArray(@(CertificateItem^.CertificateLength[0]));
  P^[0] := (CertificateLength and $FF0000) shr 16;
  P^[1] := (CertificateLength and $FF00) shr 8;
  P^[2] := CertificateLength and $FF;
end;

function CnGetTLSHandShakeCertificateItemCertificate(const CertificateItem: PCnTLSHandShakeCertificateItem): TBytes;
var
  L: Integer;
begin
  L := CnGetTLSHandShakeCertificateItemCertificateLength(CertificateItem);
  if L > 0 then
  begin
    SetLength(Result, L);
    Move(CertificateItem^.Certificate[0], Result[0], L);
  end
  else
    Result := nil;
end;

procedure CnSetTLSHandShakeCertificateItemCertificate(const CertificateItem: PCnTLSHandShakeCertificateItem; Certificate: TBytes);
var
  L: Integer;
begin
  L := Length(Certificate);
  CnSetTLSHandShakeCertificateItemCertificateLength(CertificateItem, Cardinal(L));
  if L > 0 then
    Move(Certificate[0], CertificateItem^.Certificate[0], L);
end;

function CnGetTLSHandShakeCertificateListLength(const Certificate: PCnTLSHandShakeCertificate): Cardinal;
var
  P: PByteArray;
begin
  P := PByteArray(@(Certificate^.CertificateListLength[0]));
  Result := (P^[0] shl 16) or (P^[1] shl 8) or P^[2];
end;

procedure CnSetTLSHandShakeCertificateListLength(const Certificate: PCnTLSHandShakeCertificate; CertificateListLength: Cardinal);
var
  P: PByteArray;
begin
  P := PByteArray(@(Certificate^.CertificateListLength[0]));
  P^[0] := (CertificateListLength and $FF0000) shr 16;
  P^[1] := (CertificateListLength and $FF00) shr 8;
  P^[2] := CertificateListLength and $FF;
end;

function CnGetTLSHandShakeCertificateItem(const Certificate: PCnTLSHandShakeCertificate;
  PrevItem: PCnTLSHandShakeCertificateItem): PCnTLSHandShakeCertificateItem;
var
  P: PByte;
begin
  if PrevItem = nil then
  begin
    Result := @(Certificate^.Certificates);
  end
  else
  begin
    P := PByte(PrevItem);
    Inc(P, 3);
    Inc(P, CnGetTLSHandShakeCertificateItemCertificateLength(PrevItem));
    Result := PCnTLSHandShakeCertificateItem(P);
  end;
end;

function CnGetTLSHandShakeSignedParamsFromServerKeyExchange(const SKE: PCnTLSHandShakeServerKeyExchange): PCnTLSHandShakeSignedParams;
var
  P: PByte;
begin
  P := PByte(SKE);
  Inc(P, SizeOf(Byte));
  Inc(P, SizeOf(Word));
  Inc(P, P^ + SizeOf(Byte));
  Result := PCnTLSHandShakeSignedParams(P);
end;

function CnGetTLSHandShakeServerKeyExchangeNamedCurve(const SKE: PCnTLSHandShakeServerKeyExchange): Word;
begin
  Result := UInt16NetworkToHost(SKE^.NamedCurve);
end;

procedure CnSetTLSHandShakeServerKeyExchangeNamedCurve(const SKE: PCnTLSHandShakeServerKeyExchange; Name: Word);
begin
  SKE^.NamedCurve := UInt16HostToNetwork(Name);
end;

function CnGetTLSHandShakeServerKeyExchangeECPoint(const SKE: PCnTLSHandShakeServerKeyExchange): TBytes;
begin
  if SKE^.ECPointLength > 0 then
  begin
    SetLength(Result, SKE^.ECPointLength);
    Move(SKE^.ECPoint[0], Result[0], SKE^.ECPointLength);
  end
  else
    Result := nil;
end;

procedure CnSetTLSHandShakeServerKeyExchangeECPoint(const SKE: PCnTLSHandShakeServerKeyExchange; ECPoint: TBytes);
begin
  SKE^.ECPointLength := Length(ECPoint);
  if SKE^.ECPointLength > 0 then
    Move(ECPoint[0], SKE^.ECPoint[0], SKE^.ECPointLength);
end;

function CnGetTLSHandShakeSignedParamsSignatureAlgorithm(const SP: PCnTLSHandShakeSignedParams): Word;
begin
  Result := UInt16NetworkToHost(SP^.SignatureAlgorithm);
end;

procedure CnSetTLSHandShakeSignedParamsSignatureAlgorithm(const SP: PCnTLSHandShakeSignedParams; SigAlg: Word);
begin
  SP^.SignatureAlgorithm := UInt16HostToNetwork(SigAlg);
end;

function CnGetTLSHandShakeSignedParamsSignatureLength(const SP: PCnTLSHandShakeSignedParams): Word;
begin
  Result := UInt16NetworkToHost(SP^.SignatureLength);
end;

procedure CnSetTLSHandShakeSignedParamsSignatureLength(const SP: PCnTLSHandShakeSignedParams; SigLength: Word);
begin
  SP^.SignatureLength := UInt16HostToNetwork(SigLength);
end;

function CnGetTLSHandShakeSignedParamsSignature(const SP: PCnTLSHandShakeSignedParams): TBytes;
var
  L: Integer;
begin
  L := CnGetTLSHandShakeSignedParamsSignatureLength(SP);
  if L > 0 then
  begin
    SetLength(Result, L);
    Move(SP^.Signature[0], Result[0], L);
  end
  else
    Result := nil;
end;

procedure CnSetTLSHandShakeSignedParamsSignature(const SP: PCnTLSHandShakeSignedParams; Signature: TBytes);
var
  L: Integer;
begin
  L  := Length(Signature);
  if Word(L) > 0 then
  begin
    CnSetTLSHandShakeSignedParamsSignatureLength(SP, Word(L));
    Move(Signature[0], SP^.Signature[0], Word(L));
  end
  else
    CnSetTLSHandShakeSignedParamsSignatureLength(SP, 0);
end;

function CnGetTLSHandShakeClientKeyExchangeECPoint(const CKE: PCnTLSHandShakeClientKeyExchange; EccFiniteFieldSize: Integer): TBytes;
begin
  if (CKE^.PointConversionForm <> CN_TLS_EC_POINT_CONVERSION_FORM_UNCOMPRESSED) or (EccFiniteFieldSize <= 0) then
    Result := nil
  else
  begin
    SetLength(Result, 1 + EccFiniteFieldSize shl 1);
    Move(CKE^.PointConversionForm, Result[0], Length(Result));
  end;
end;

procedure CnSetTLSHandShakeClientKeyExchangeECPoint(const CKE: PCnTLSHandShakeClientKeyExchange; ECPoint: TBytes);
begin
  if Length(ECPoint) > 1 then
  begin
    if ECPoint[0] = CN_TLS_EC_POINT_CONVERSION_FORM_UNCOMPRESSED then
      Move(ECPoint[0], CKE^.PointConversionForm, Length(ECPoint));
  end;
end;

function CnGetTLSTLSHandShakeFinishedVerifyData(const F: PCnTLSHandShakeFinished; DataSize: Integer = 12): TBytes;
begin
  if DataSize > 0 then
    Result := NewBytesFromMemory(@F^.VerifyData[0], DataSize)
  else
    Result := nil;
end;

procedure CnSetTLSTLSHandShakeFinishedVerifyData(const F: PCnTLSHandShakeFinished; Data: TBytes);
begin
  if Length(Data) > 0 then
    Move(Data[0], F^.VerifyData[0], Length(Data));
end;

end.
