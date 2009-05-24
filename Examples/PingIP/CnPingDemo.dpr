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

program CnPingDemo;
{* |<PRE>
================================================================================
* 软件名称：CnPing 测试程序
* 单元名称：CnPing 测试程序工程文件
* 单元作者：胡昌洪(Sesame) sesamehch@163.com
* 备    注：
* 开发平台：PWin2000 + Delphi 5
* 兼容测试：暂无（PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6）
* 本 地 化：该窗体中的字符串暂不符合本地化处理方式
* 单元标识：$Id: CnPingDemo.dpr,v 1.1 2008/05/23 14:03:51 liuxiao Exp $
* 修改记录：2008.04.12 V1.0
*               创建工程
================================================================================
|</PRE>}

uses
  Forms,
  uCnPingDemo in 'uCnPingDemo.pas' {frmCnPingDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCnPingDemo, frmCnPingDemo);
  Application.Run;
end.

