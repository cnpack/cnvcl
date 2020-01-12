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

unit CnAAFontEditor;
{* |<PRE>
================================================================================
* 软件名称：CnPack 控件包
* 单元名称：平滑特效字体属性、组件编辑器单元
* 单元作者：CnPack 开发组 周劲羽 (zjy@cnpack.org)
*           移植：e- 
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Build 5/6
* 备　　注：
* 最后更新：2002.07.02
* 移植日期：2006.08.18
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

{ TCnAAEffectProperty }

  TCnAAEffectProperty = class(TClassProperty)
  {* TAAEffect平滑字体特效参数类属性编辑器，通过它用户可在设计期可视化编辑平滑
     字体特效参数。}
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

{ TCnAALabelEditor }

  TCnAALabelEditor = class(TComponentEditor)
  {* TAALabel及其派生控件的组件编辑器，通过它用户可在设计期可视化编辑平滑
     字体特效参数。}
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  CnAAFont, CnAACtrls, CnAAFontDialog;

resourcestring
  SAAEffect = '平滑特效字体';
  SAALabelVert = '设置平滑特效字体(&F)...';

{ TCnAAEffectProperty }

type
  TCnAAEffectAccess = class(TCnAAEffect);

procedure TCnAAEffectProperty.Edit;
var
  AEffect: TCnAAEffect;
  FontLabel: TCnFontLabel;
begin
  AEffect := TCnAAEffect(Pointer(GetOrdValue));
  with TCnAAFontDialog.Create(nil) do
  try
    if (PropCount = 1) and (TCnAAEffectAccess(AEffect).GetOwner is TCnFontLabel) then
    begin
      FontLabel := TCnFontLabel(TCnAAEffectAccess(AEffect).GetOwner);
      AllowChangeFont := True;
      Font.Assign(FontLabel.Font);
    end
    else
    begin
      FontLabel := nil;
      AllowChangeFont := False;
    end;
    Effect.Assign(AEffect);
    if Execute then
    begin
      SetOrdValue(Integer(Effect));
      if FontLabel <> nil then
        FontLabel.Font.Assign(Font);
    end;
  finally
    Free;
  end;
end;

function TCnAAEffectProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paReadOnly, paMultiSelect];
end;

function TCnAAEffectProperty.GetValue: string;
begin
  Result := SAAEffect;
end;

{ TCnAALabelEditor }

procedure TCnAALabelEditor.ExecuteVerb(Index: Integer);
var
  Ctrl: TCnAALabel;
begin
  if Index = 0 then
  begin
    if Component is TCnAALabel then
    begin
      Ctrl := TCnAALabel(Component);
      with TCnAAFontDialog.Create(nil) do
      try
        AllowChangeFont := True;
        Font.Assign(Ctrl.Font);
        Effect.Assign(Ctrl.Effect.FontEffect);
        if Execute then
        begin
          Ctrl.Font.Assign(Font);
          Ctrl.Effect.FontEffect := Effect;
          Designer.Modified;
        end;
      finally
        Free;
      end;
    end;
  end
  else
    inherited;
end;

function TCnAALabelEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := SAALabelVert
  else
    Result := inherited GetVerb(Index);
end;

function TCnAALabelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.



