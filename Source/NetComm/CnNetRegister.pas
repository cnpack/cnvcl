{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
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

unit CnNetRegister;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯组件包注册单元
* 单元作者：CnPack开发组
* 备    注：
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2002.04.18 V1.1
*                为TCnRS232Dialog增加两个属性编辑器声明
*           2002.04.08 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  {$IFNDEF BCB5} {$IFNDEF BCB6} CnUDP, {$ENDIF} {$ENDIF}
  CnConsts, CnRS232, CnModem, CnRS232Dialog, CnIP, CnPing, CnDialUp,
  CnCameraEye, CnIISCtrl, CnTwain, CnIocpSimpleMemPool, CnIocpSocketAdapter,
  CnNetPropEditor;

procedure Register;
{* 控件、组件编辑器、属性编辑器注册过程}

implementation

procedure Register;
begin
  RegisterComponents(SCnNetPalette, [TCnRS232]);
  RegisterComponents(SCnNetPalette, [TCnModem]);
  RegisterComponents(SCnNetPalette, [TCnRS232Dialog]);
  RegisterComponents(SCnNetPalette, [TCnDialUp]);
  RegisterComponents(SCnNetPalette, [TCnIP, TCnPing]);
  RegisterComponents(SCnNetPalette, [TCnCameraEye, TCnIISCtrl, TCnTwain]);
  RegisterComponents(SCnNetPalette, [TCnIocpSimpleMemPool, TCnIocpSocketAdapter]);
{$IFNDEF BCB5} {$IFNDEF BCB6}
  RegisterComponents(SCnNetPalette, [TCnUDP]);
{$ENDIF} {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TCnRS232Config), TCnRS232, '', TCnRS232ConfigProperty);
  RegisterPropertyEditor(TypeInfo(TCnRS232Timeouts), TCnRS232, '', TCnRS232TimeoutsProperty);
  RegisterPropertyEditor(TypeInfo(TCnRS232Config), TCnRS232Dialog, '', TCnRS232ConfigProperty);
  RegisterPropertyEditor(TypeInfo(TCnRS232Timeouts), TCnRS232Dialog, '', TCnRS232TimeoutsProperty);
end;

end.
