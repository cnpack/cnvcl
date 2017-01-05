{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2017 CnPack 开发组                       }
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

unit CnNetDecls;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯组件包网络结构定义单元
* 单元作者：CnPack开发组 Liu Xiao
* 备    注：
* 开发平台：PWinXP + Delphi XE
* 兼容测试：PWinXP/7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2016.10.05 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows;

const
  {* IP 包头中的版本字段的定义}
  CN_IP_VERSION_V4                          = 4;
  CN_IP_VERSION_V6                          = 6;

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
  CN_IP_TOS_DELAY_MASK                      = $10;
  CN_IP_TOS_THROUGHPUT_MASK                 = $8;
  CN_IP_TOS_RELIBILITY_MASK                 = $4;

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

type

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
    TypeOfService:      Byte;           // 服务类型
    TotalLength:        Word;           // 总长度，最大 65535
    Identification:     Word;           // 标识
    FlagOffset:         Word;           // 标志和片偏移
    TTL:                Byte;           // 生存时间
    Protocol:           Byte;           // 协议
    Checksum:           Word;           // 包头校验和
    SourceIp:           LongWord;       // 源 IP 地址
    DestIp:             LongWord;       // 目的 IP 地址
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
    SequenceNumber:        LongWord;    // 序列号
    AcknowledgementNumber: LongWord;    // 响应序列号
    Offset:                Byte;        // 数据偏移，仅最左 4 bit，等同于包头长度
    Flags:                 Byte;        // TCP 包头标记
    Window:                Word;        // 窗口大小
    Checksum:              Word;        // 校验和
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
    Checksum:              Word;        // 校验和
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
    Checksum:              Word;         // 校验和
    case Integer of
      0: (Unused:          LongWord);
      1: (Ptr:             Byte;         // 指针
          Unused1:         Byte;
          Unused2:         Word);
      2: (GatewayAddress:  LongWord);    // 网关地址
      3: (Identifier:      Word;         // 标识
          SequenceNumber:  Word);        // 序列号
  end;

  PCnICMPHeader = ^TCnICMPHeader;

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

function CnGetIPTypeOfServiceRelibility(const IPHeader: PCnIPHeader): Boolean;
{* 获得 IP 包头内的 Type of Service 字段中的 Relibility 值，True 为 High，False 为 Normal}

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

function CnGetIPSourceIP(const IPHeader: PCnIPHeader): LongWord;
{* 获得 IP 包头内的源 IP 地址，存在网络字节转换}

function CnGetIPDestIP(const IPHeader: PCnIPHeader): LongWord;
{* 获得 IP 包头内的目的 IP 地址，存在网络字节转换}

// ======================== TCP 包头系列函数 ===================================

function CnGetTCPSourcePort(const TCPHeader: PCnTCPHeader): Integer;
{* 获得 TCP 包头内的源端口号，存在网络字节转换}

function CnGetTCPDestPort(const TCPHeader: PCnTCPHeader): Integer;
{* 获得 TCP 包头内的目的端口号，存在网络字节转换}

function CnGetTCPSequenceNumber(const TCPHeader: PCnTCPHeader): LongWord;
{* 获得 TCP 包头内的序列号，存在网络字节转换}

function CnGetTCPAcknowledgementNumber(const TCPHeader: PCnTCPHeader): LongWord;
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

// ======================== UDP 包头系列函数 ===================================

function CnGetUDPSourcePort(const UDPHeader: PCnUDPHeader): Integer;
{* 获得 UDP 包头内的源端口号，存在网络字节转换}

function CnGetUDPDestPort(const UDPHeader: PCnUDPHeader): Integer;
{* 获得 UDP 包头内的目的端口号，存在网络字节转换}

function CnGetUDPLength(const UDPHeader: PCnUDPHeader): Integer;
{* 获得 UDP 包头内的包总长度，存在网络字节转换}

function CnGetUDPChecksum(const UDPHeader: PCnUDPHeader): Word;
{* 获得 UDP 包头内的校验和，存在网络字节转换}

// ======================== ICMP 包头系列函数 ==================================

function CnGetICMPType(const ICMPHeader: PCnICMPHeader): Integer;
{* 获得 ICMP 包头内的消息类型}

function CnGetICMPCode(const ICMPHeader: PCnICMPHeader): Integer;
{* 获得 ICMP 包头内的消息代码}

function CnGetICMPChecksum(const ICMPHeader: PCnICMPHeader): Word;
{* 获得 ICMP 包头内的校验和}

function CnGetICMPPointer(const ICMPHeader: PCnICMPHeader): Integer;
{* 获得 ICMP 包头内的指针字段值}

function CnGetICMPGatewayAddress(const ICMPHeader: PCnICMPHeader): LongWord;
{* 获得 ICMP 包头内的网关地址}

function CnGetICMPIdentifier(const ICMPHeader: PCnICMPHeader): Word;
{* 获得 ICMP 包头内的标识号}

function CnGetICMPSequenceNumber(const ICMPHeader: PCnICMPHeader): Word;
{* 获得 ICMP 包头内的序列号}

implementation

function NetworkToHostWord(Value: Word): Word;
begin
  Result := ((Value and $00FF) shl 8) or ((Value and $FF00) shr 8);
end;

function NetworkToHostLongWord(Value: LongWord): LongWord;
begin
  Result := ((Value and $000000FF) shl 24) or ((Value and $0000FF00) shl 8)
    or ((Value and $00FF0000) shr 8) or ((Value and $FF000000) shr 24);
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

function CnGetIPTypeOfServiceRelibility(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (IPHeader^.TypeOfService and CN_IP_TOS_RELIBILITY_MASK) <> 0;
end;

function CnGetIPTotalLength(const IPHeader: PCnIPHeader): Integer;
begin
  Result := NetworkToHostWord(IPHeader^.TotalLength);
end;

function CnGetIPIdentification(const IPHeader: PCnIPHeader): Integer;
begin
  Result := NetworkToHostWord(IPHeader^.Identification);
end;

function CnGetIPFlagDontFragment(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (NetworkToHostWord(IPHeader^.FlagOffset) and CN_IP_FLAG_DONT_FRAGMENT_WORD_MASK) <> 0;
end;

function CnGetIPFlagMoreFragment(const IPHeader: PCnIPHeader): Boolean;
begin
  Result := (NetworkToHostWord(IPHeader^.FlagOffset) and CN_IP_FLAG_MORE_FRAGMENT_WORD_MASK) <> 0;
end;

function CnGetIPFragmentOffset(const IPHeader: PCnIPHeader): Integer;
begin
  Result := NetworkToHostWord(IPHeader^.FlagOffset) and CN_IP_FLAG_FRAGMENT_OFFSET_WORD_MASK;
end;

function CnGetIPChecksum(const IPHeader: PCnIPHeader): Word;
begin
  Result := NetworkToHostWord(IPHeader^.Checksum);
end;

function CnGetIPSourceIP(const IPHeader: PCnIPHeader): LongWord;
begin
  Result := NetworkToHostLongWord(IPHeader^.SourceIp);
end;

function CnGetIPDestIP(const IPHeader: PCnIPHeader): LongWord;
begin
  Result := NetworkToHostLongWord(IPHeader^.DestIp);
end;

function CnGetTCPSourcePort(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := NetworkToHostWord(TCPHeader^.SourcePort);
end;

function CnGetTCPDestPort(const TCPHeader: PCnTCPHeader): Integer;
begin
  Result := NetworkToHostWord(TCPHeader^.DestPort);
end;

function CnGetTCPSequenceNumber(const TCPHeader: PCnTCPHeader): LongWord;
begin
  Result := NetworkToHostLongWord(TCPHeader^.SequenceNumber);
end;

function CnGetTCPAcknowledgementNumber(const TCPHeader: PCnTCPHeader): LongWord;
begin
  Result := NetworkToHostLongWord(TCPHeader^.AcknowledgementNumber);
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
  Result := NetworkToHostWord(TCPHeader^.Window);
end;

function CnGetTCPChecksum(const TCPHeader: PCnTCPHeader): Word;
begin
  Result := NetworkToHostWord(TCPHeader^.Checksum);
end;

function CnGetTCPUrgentPointer(const TCPHeader: PCnTCPHeader): Word;
begin
  Result := NetworkToHostWord(TCPHeader^.UrgentPointer);
end;

function CnGetUDPSourcePort(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := NetworkToHostWord(UDPHeader^.SourcePort);
end;

function CnGetUDPDestPort(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := NetworkToHostWord(UDPHeader^.DestPort);
end;

function CnGetUDPLength(const UDPHeader: PCnUDPHeader): Integer;
begin
  Result := NetworkToHostWord(UDPHeader^.Length);
end;

function CnGetUDPChecksum(const UDPHeader: PCnUDPHeader): Word;
begin
  Result := NetworkToHostWord(UDPHeader^.Checksum);
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
  Result := NetworkToHostWord(ICMPHeader^.Checksum);
end;

function CnGetICMPPointer(const ICMPHeader: PCnICMPHeader): Integer;
begin
  Result := ICMPHeader^.Ptr;
end;

function CnGetICMPGatewayAddress(const ICMPHeader: PCnICMPHeader): LongWord;
begin
  Result := NetworkToHostLongWord(ICMPHeader^.GatewayAddress);
end;

function CnGetICMPIdentifier(const ICMPHeader: PCnICMPHeader): Word;
begin
  Result := NetworkToHostWord(ICMPHeader^.Identifier);
end;

function CnGetICMPSequenceNumber(const ICMPHeader: PCnICMPHeader): Word;
begin
  Result := NetworkToHostWord(ICMPHeader^.SequenceNumber);
end;

end.
