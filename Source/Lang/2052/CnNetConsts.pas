{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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
* 修改记录：2002.04.08 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnRS232
  SInvalidXonXoffChar = 'Xon字符与Xoff字符不能相同';
  SSerialPortAlreadyOpened = '该串口已经打开了';
  SSerialPortOpenError = '串口打开失败';
  SNotACommHandle = '指定的设备不是一个有效的通讯端口';
  SSetupBuffFail = '设置串口通讯缓冲区失败';
  SCreateEventFail = '创建事件失败';
  SCreateReadFail = '创建读数据线程失败';
  SCreateWriteFail = '创建写数据线程失败';
  SCnRS232Name = 'RS232串口通讯组件';
  SCnRS232Comment = 'RS232串口通讯组件';

  // CnModem
  SCnModemName = '标准调制解调器通讯组件';
  SCnModemComment = '标准调制解调器通讯组件';

  // CnRS232Dialog
  SCnRS232DialogName = 'RS232串口设置对话框组件';
  SCnRS232DialogComment = 'RS232串口设置对话框组件';
  SRS232Option = '串口设置';
  SRS232TimeoutsOption = '超时设置';
  SBaudRateError = '波特率只能输入整数';
  SInputASCIICode = '请输入ASCII编码(0..255)';
  SInputInteger = '请输入整数';

  // CnPing
  SCnPingName = 'Ping组件';
  SCnPingComment = 'Ping组件';
  SInitFailed = 'Winsock 初始化失败，可能是版本不正确';
  SInvalidAddr = 'IP地址不合法';
  SNoResponse = '[%0:S] 没有响应';
  STimeOut = '响应超时';
  SICMPRunError = 'ICMP运行错误';
  SPingResultString = '[%0:S]: 字节数:%1:D 时间: %2:Dms TTL: %3:D';

  // CnIP
  SCnIPName = 'IP组件';
  SCnIPComment = 'IP组件';
  SCnErrorAddress = '错误的IP地址';
  SCnErrorAddrRang = '超出IP地址范围';

  // CnDNS
  SCnDNSName = 'DNS组件';
  SCnDNSComment = 'DNS组件';
  SCnDNSTooLong = '字符串长度超界';
  SCnDNSInvalidHeadByteFmt = '字符串长度 %d 位于 %d 处非法';

  // CnThreadingTCPServer
  SCnThreadingTCPServerName = '多线程TCP服务器组件';
  SCnThreadingTCPServerComment = '阻塞式多线程TCP服务器组件';

  // CnTCPClient
  SCnTCPClientName = 'TCP客户端组件';
  SCnTCPClientComment = 'TCP客户端组件';

  // CnTCPForwarder
  SCnTCPForwarderName = 'TCP端口转发组件';
  SCnTCPForwarderComment = 'TCP端口转发组件';

implementation

end.
