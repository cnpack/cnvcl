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

unit CnNetConsts;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：资源字符串定义单元
* 单元作者：CnPack开发组
* 备    注：该单元定义了网络通讯类用到的资源字符串
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2005.12.24 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnRS232
  SInvalidXonXoffChar = 'Xon才籔Xoff才ぃ';
  SSerialPortAlreadyOpened = '赣﹃竒ゴ秨';
  SSerialPortOpenError = '﹃ゴ秨ア毖';
  SNotACommHandle = '﹚砞称ぃ琌Τ硄癟狠';
  SSetupBuffFail = '砞竚﹃硄癟絯侥跋ア毖';
  SCreateEventFail = '承ㄆンア毖';
  SCreateReadFail = '承弄计沮絬祘ア毖';
  SCreateWriteFail = '承糶计沮絬祘ア毖';
  SCnRS232Name = 'RS232﹃硄癟舱ン';
  SCnRS232Comment = 'RS232﹃硄癟舱ン';

  // CnModem
  SCnModemName = '夹非秸籹秆秸竟硄癟舱ン';
  SCnModemComment = '夹非秸籹秆秸竟硄癟舱ン';

  // CnRS232Dialog
  SCnRS232DialogName = 'RS232﹃砞竚癸杠舱ン';
  SCnRS232DialogComment = 'RS232﹃砞竚癸杠舱ン';
  SRS232Option = '﹃砞竚';
  SRS232TimeoutsOption = '禬砞竚';
  SBaudRateError = '猧疭瞯块俱计';
  SInputASCIICode = '叫块ASCII絪絏(0..255)';
  SInputInteger = '叫块俱计';

  // CnPing
  SCnPingName = 'Ping舱ン';
  SCnPingComment = 'Ping舱ン';
  SInitFailed = 'Winsock ﹍てア毖琌セぃタ絋';
  SInvalidAddr = 'IPぃ猭';
  SNoResponse = '[%0:S] ⊿Τ臫莱';
  STimeOut = '臫莱禬';
  SICMPRunError = 'ICMP笲︽岿粇';
  SPingResultString = '[%0:S]: 竊计:%1:D 丁: %2:Dms TTL: %3:D';

  // CnIP
  SCnIPName = 'IP舱ン';
  SCnIPComment = 'IP舱ン';
  SCnErrorAddress = '岿粇IP';
  SCnErrorAddrRang = '禬IP絛瞅';

  // CnDNS
  SCnDNSName = 'DNS舱ン';
  SCnDNSComment = 'DNS舱ン';
  SCnDNSTooLong = '才﹃禬';
  SCnDNSInvalidHeadByteFmt = '才﹃ %d  %d 矪獶猭';

  // CnThreadingTCPServer
  SCnThreadingTCPServerName = '絬祘TCP狝叭竟舱ン';
  SCnThreadingTCPServerComment = '峨Α絬祘TCP狝叭竟舱ン';

  // CnTCPClient
  SCnTCPClientName = 'TCPめ狠舱ン';
  SCnTCPClientComment = 'TCPめ狠舱ン';

  // CnTCPForwarder
  SCnTCPForwarderName = 'TCP狠锣祇舱ン';
  SCnTCPForwarderComment = 'TCP狠锣祇舱ン';

implementation

end.
