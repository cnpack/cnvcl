{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

unit CnLangEditors;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：多语包部分属性编辑器单元
* 单元作者：CnPack开发组 刘啸 (liuxiao@cnpack.org)
* 备    注：该单元实现了多语包的部分属性编辑器
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnLangEditors.pas,v 1.12 2009/01/02 08:27:39 liuxiao Exp $
* 修改记录：2003.08.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  Dsgnintf,
  {$ENDIF}
  SysUtils, Classes, FileCtrl;

type
  TCnLanguageItemProperty = class(TPropertyEditor)
  {* 针对 TCnLanguageItem 的 LanugageID 的属性编辑器}
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TCnLangManagerProperty = class(TPropertyEditor)
  {* 针对 TCnLanguageManager 的 CurrentLanugageIndex 的属性编辑器}
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TCnLanguagePathProperty = class(TStringProperty)
  {* 针对 TCnLangFileStorage 的 LanugagePath 的属性编辑器}
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{$IFDEF DELPHI}
  TCnStorageEditor = class(TComponentEditor)
  {* 针对多语存储组件双击的编辑语言条目的组件编辑器}
  public
    procedure Edit; override;
    {* 双击的过程 }
    procedure ExecuteVerb(Index: Integer); override;
    {* 执行右键菜单的过程 }
    function GetVerb(Index: Integer): string; override;
    {* 返回右键菜单条目 }
    function GetVerbCount: Integer; override;
    {* 返回右键菜单条目数 }
  end;
{$ENDIF}

implementation

uses
  CnLangMgr, CnLangStorage, CnLangUtils, CnLangConsts
  {$IFDEF DELPHI}, ColnEdit{$ENDIF};

{ TCnLanguageItemProperty }

function TCnLanguageItemProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TCnLanguageItemProperty.GetValue: string;
begin
  Result := InttoStr(GetOrdValue);
end;

procedure TCnLanguageItemProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := 0 to CnLanguages.Count - 1 do
    Proc(CnLanguages.ID[i] + ' ' + CnLanguages.Name[i]);
end;

procedure TCnLanguageItemProperty.SetValue(const Value: string);
begin
  if Pos(' ', Value) > 0 then
    SetOrdValue(StrToInt(Copy(Value, 1, Pos(' ', Value) - 1)))
  else
    SetOrdValue(StrToInt(Value));
end;

{ TCnLangManagerProperty }

function TCnLangManagerProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TCnLangManagerProperty.GetValue: string;
begin
  Result := InttoStr(GetOrdValue);
end;

procedure TCnLangManagerProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Storage: TCnCustomLangStorage;
begin
  if (GetComponent(0) <> nil) and
    ((GetComponent(0) as TCnCustomLangManager).LanguageStorage <> nil) then
  begin
    Storage := (GetComponent(0) as TCnCustomLangManager).LanguageStorage;
    for i := 0 to Storage.Languages.Count - 1 do
      Proc(InttoStr(i) + ' - ' + InttoStr(Storage.Languages.Items[i].LanguageID)
        + ' - ' + Storage.Languages.Items[i].LanguageName);
  end;
end;

procedure TCnLangManagerProperty.SetValue(const Value: string);
begin
  if Pos(' ', Value) > 0 then
    SetOrdValue(StrToInt(Copy(Value, 1, Pos(' ', Value) - 1)))
  else
    SetOrdValue(StrToInt(Value));
end;

{ TCnLangguagePathProperty }

procedure TCnLanguagePathProperty.Edit;
var
  S: String;
begin
  if SelectDirectory(SCnLanguagePath, '', S) then
    SetStrValue(S);
end;

function TCnLanguagePathProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paRevertable];
end;

{$IFDEF DELPHI}

{ TCnStorageEditor }

procedure TCnStorageEditor.Edit;
begin
  if Component is TCnCustomLangStorage then
    ShowCollectionEditor(Designer, Component,
      TCnCustomLangStorage(Component).Languages, 'Languages');
end;

procedure TCnStorageEditor.ExecuteVerb(Index: Integer);
begin
  // Do Nothing.
end;

function TCnStorageEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
end;

function TCnStorageEditor.GetVerbCount: Integer;
begin
  Result := 0;
end;

{$ENDIF}

end.
