{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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
  SInvalidXonXoffChar = 'XonChar must differs from XoffChar';
  SSerialPortAlreadyOpened = 'This serial port already opened';
  SSerialPortOpenError = 'Error opening serial port';
  SNotACommHandle = 'File handle is not a comm handle ';
  SSetupBuffFail = 'Cannot setup comm buffer';
  SCreateEventFail = 'Unable to create event';
  SCreateReadFail = 'Unable to create read thread';
  SCreateWriteFail = 'Unable to create write thread';
  SCnRS232Name = 'RS232 Cerial Port Communications Component';
  SCnRS232Comment = 'RS232 Serial Port Communications Component';

  // CnModem
  SCnModemName = 'Modem Communications Component';
  SCnModemComment = 'Modem Communications Component';

  // CnRS232Dialog
  SCnRS232DialogName = 'RS232 Serial Port Option Dialog Component';
  SCnRS232DialogComment = 'RS232 Serial Port Option Dialog Component';
  SRS232Option = 'Comm Option';
  SRS232TimeoutsOption = 'Timeout Settings';
  SBaudRateError = 'Baud Rate must be an Integer Value';
  SInputASCIICode = 'Please Enter an ASCII code (0..255)';
  SInputInteger = 'Please Enter an Integer';

  // CnPing
  SCnPingName = 'CnPing Component';
  SCnPingComment = 'CnPing Component';  
  SInitFailed = 'Init Failed. Maybe Winsock Verison Error';
  SInvalidAddr = 'IP Address Error';
  SNoResponse = '[%0:S] No Response';
  STimeOut = 'Time Out';    
  SICMPRunError = 'ICMP Run Error';
  SPingResultString = '[%0:S]: Bytes:%1:D Time: %2:Dms TTL:%3:D';
  
  // CnIP
  SCnIPName = 'CnIP Component';
  SCnIPComment = 'CnIP Component'; 
  SCnErrorAddress = 'Error IP Address';  
  SCnErrorAddrRang = 'IP Address Range Error';

  // CnDNS
  SCnDNSName = 'CnDNS Component';
  SCnDNSComment = 'CnDNS Component';
  SCnDNSTooLong = 'Too Long String Length Byte and Out of Bound.';
  SCnDNSInvalidHeadByteFmt = 'Invalid String Head Byte %d at %d.';

  // CnThreadingTCPServer
  SCnThreadingTCPServerName = 'CnThreadingTCPServer Component';
  SCnThreadingTCPServerComment = 'A Blocking Multi-Threading TCPServer Component';

  // CnTCPClient
  SCnTCPClientName = 'CnTCPClient Component';
  SCnTCPClientComment = 'A Simple Blocking TCPClient Component';

  // CnTCPForwarder
  SCnTCPForwarderName = 'CnTCPForwarder Component';
  SCnTCPForwarderComment = 'A Simple TCP Port Mapping Component';

implementation

end.
