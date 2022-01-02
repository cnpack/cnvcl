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

unit CnPropEditors;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：开发包属性组件编辑器单元
* 单元作者：CnPack开发组
* 备    注：
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2002.04.18 V1.1
*               增加组件版权信息属性编辑器TCnCopyrightProperty
*           2002.04.08 V1.0
*               创建单元（空单元）
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  CnClasses, CnCommon, CnConsts;

type

{ TCnCopyrightProperty }

  TCnCopyrightProperty = class(TPropertyEditor)
  {* TCopyright属性编辑器类，用于TCnImage中，内部测试用}
  public
    procedure Edit; override;
    {* 编辑属性}
    function GetAttributes: TPropertyAttributes; override;
    {* 取属性编辑状态}
    function GetValue: string; override;
    {* 取属性显示字体串}
  end;

implementation

type
  TCnComponentAccess = class(TCnComponent);

{ TCnCopyrightProperty }

procedure TCnCopyrightProperty.Edit;
var
  Comp: TCnComponentAccess;
  AName, Author, Email, Comment: string;
begin
  if GetComponent(0) is TCnComponent then
  begin
    Comp := TCnComponentAccess(GetComponent(0));
    Comp.GetComponentInfo(AName, Author, Email, Comment);
    InfoDlg(Format(SCopyrightFmtStr, [AName, Author, Email, Comment]),
      SCnPackAbout);
  end
  else
    InfoDlg(Format(SCopyrightFmtStr, [GetComponent(0).ClassName, SCnPackGroup,
      SCnPackEmail, '']), SCnPackAbout);
end;

function TCnCopyrightProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TCnCopyrightProperty.GetValue: string;
begin
  Result := 'CnPack';
end;

end.



