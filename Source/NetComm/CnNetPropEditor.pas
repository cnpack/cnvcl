{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2015 CnPack 开发组                       }
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

unit CnNetPropEditor;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：网络通讯类属性编辑器单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2002.04.18 V1.1
*                为TCnRS232ConfigProperty和TCnRS232TimeoutsProperty增加
*                对TRS232Dialog组件的支持
*           2002.04.08 V1.0
*                创建单元
*                增加注释
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type

//------------------------------------------------------------------------------
// TCnRS232Config属性编辑器
//------------------------------------------------------------------------------

{ TCnRS232ConfigProperty }

  TCnRS232ConfigProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

//------------------------------------------------------------------------------
// TCnRS232Timeouts属性编辑器
//------------------------------------------------------------------------------

{ TCnRS232TimeoutsProperty }

  TCnRS232TimeoutsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

implementation

uses
  CnRS232, CnRS232Dialog, CnNetConsts;

//------------------------------------------------------------------------------
// TCnRS232Config属性编辑器
//------------------------------------------------------------------------------

{ TCnRS232ConfigProperty }

// 编辑属性
procedure TCnRS232ConfigProperty.Edit;
var
  CommDlg: TCnRS232Dialog;
  CommConfig: TCnRS232Config;
begin
  if GetComponent(0) is TCnRS232 then
    CommConfig := TCnRS232(GetComponent(0)).CommConfig
  else if GetComponent(0) is TCnRS232Dialog then
    CommConfig := TCnRS232Dialog(GetComponent(0)).CommConfig
  else
    Exit;

  CommDlg := TCnRS232Dialog.Create(nil);
  try
    CommDlg.Kind := ckExtended;
    CommDlg.Pages := [cpNormal, cpXonXoff, cpHardware];
    CommDlg.BaudRateList := False;
    CommDlg.ShowHint := csCheckHint;
    CommDlg.CommConfig.Assign(CommConfig);
    if CommDlg.Execute then
    begin
      CommConfig.Assign(CommDlg.CommConfig);
    end;
    Designer.Modified;
  finally
    CommDlg.Free;
  end;
end;

// 取属性编辑器设置
function TCnRS232ConfigProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

// 返回属性显示文本
function TCnRS232ConfigProperty.GetValue: string;
begin
  Result := SRS232Option;
end;

//------------------------------------------------------------------------------
// TCnRS232Timeouts属性编辑器
//------------------------------------------------------------------------------

{ TCnRS232TimeoutsProperty }

// 编辑属性
procedure TCnRS232TimeoutsProperty.Edit;
var
  CommDlg: TCnRS232Dialog;
  CommTimeouts: TCnRS232Timeouts;
begin
  if GetComponent(0) is TCnRS232 then
    CommTimeouts := TCnRS232(GetComponent(0)).Timeouts
  else if GetComponent(0) is TCnRS232Dialog then
    CommTimeouts := TCnRS232Dialog(GetComponent(0)).Timeouts
  else
    Exit;

  CommDlg := TCnRS232Dialog.Create(nil);
  try
    CommDlg.Kind := ckExtended;
    CommDlg.Pages := [cpTimeouts];
    CommDlg.BaudRateList := False;
    CommDlg.ShowHint := csCheckHint;
    CommDlg.Timeouts.Assign(CommTimeouts);
    if CommDlg.Execute then
    begin
      CommTimeouts.Assign(CommDlg.Timeouts);
    end;
    Designer.Modified;
  finally
    CommDlg.Free;
  end;
end;

// 取属性编辑器设置
function TCnRS232TimeoutsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly];
end;

// 返回属性显示文本
function TCnRS232TimeoutsProperty.GetValue: string;
begin
  Result := SRS232TimeoutsOption;
end;

end.

