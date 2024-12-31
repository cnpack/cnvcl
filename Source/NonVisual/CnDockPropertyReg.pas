{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDockPropertyReg;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：停靠组件的属性组件编辑器单元 
* 单元作者：CnPack开发组 周益波（鲁小班）
* 备    注：本单元由原作者授权CnPack开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.07.13 V1.0
*                移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  Dsgnintf,
  {$ENDIF}
  CnDockFormControl, CnVIDDockStyle;


type
  TCnDockControlEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TCnDockStyleEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TCnVIDTabPageControlEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  CnDockGlobal;

{ TCnDockControlEditor }

procedure TCnDockControlEditor.ExecuteVerb(Index: Integer);
var ABoutStr: string;
  ProductStr: string;
begin
  inherited;
  case Index of
    0:
    begin
      if Component is TCnDockServer then
        ProductStr := gs_CnDcokServerName
      else if Component is TCnDockClient then
        ProductStr := gs_CnDcokClientName
      else Exit;
      ABoutStr := Format(gs_CnDockManagerAbout,
        [ProductStr,
        gs_CnDockManagerVersion,
        gs_CnDockManagerCopyRightBegin,
        gs_CnDockManagerCopyRightEnd,
        gs_CnAuthorName,
        gs_CnComparyName,
        gs_CnHomePage,
        gs_CnEmail]);
      ShowMessage(ABoutStr);
    end;
  end;
end;

function TCnDockControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
    begin
      if Component is TCnDockServer then
        Result := Format('%s %s', [gs_CnAbout, gs_CnDcokServerName])
      else if Component is TCnDockClient then
        Result := Format('%s %s', [gs_CnAbout, gs_CnDcokClientName])
      else Exit;
    end;
  end;
end;

function TCnDockControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TCnBaseDockStyleEditor }

procedure TCnDockStyleEditor.ExecuteVerb(Index: Integer);
var ABoutStr: string;
begin
  inherited;
  case Index of
    0:
    begin
      ABoutStr := Format(gs_CnDockManagerAbout,
        [TCnBasicDockStyle(Component).GetControlName,
        gs_CnDockStyleVersion,
        gs_CnDockStyleCopyRightBegin,
        gs_CnDockStyleCopyRightEnd,
        gs_CnAuthorName,
        gs_CnComparyName,
        gs_CnHomePage,
        gs_CnEmail]);
      ShowMessage(ABoutStr);
    end;
  end;
end;

function TCnDockStyleEditor.GetVerb(Index: Integer): string;
begin
  Result := Format('%s %s', [gs_CnAbout, TCnBasicDockStyle(Component).GetControlName]);
end;

function TCnDockStyleEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TCnVIDTabPageControlEditor }

procedure TCnVIDTabPageControlEditor.ExecuteVerb(Index: Integer);
var Sheet: TCnVIDDockTabSheet;
  Page: TCnVIDTabPageControl;
begin
  inherited ExecuteVerb(Index);
  if Component is TCnVIDTabPageControl then
    Page := Component as TCnVIDTabPageControl
  else Page := TCnVIDDockTabSheet(Component).Parent as TCnVIDTabPageControl;
  case Index of
    0:
    begin
{$IFDEF COMPILER6_UP}
      Sheet := TCnVIDDockTabSheet.Create(Designer.Root);
{$ELSE}
      Sheet := TCnVIDDockTabSheet.Create(Designer.Form);
{$ENDIF}

      Sheet.PageControl := Page;
      Sheet.Name := Designer.UniqueName(TCnVIDDockTabSheet.ClassName);
      Sheet.Caption := Sheet.Name;
      Page.ActivePage := Sheet;
      Page.Panel.Invalidate;
    end;
    1:
    begin
      if Page.PageCount >= 0 then
      begin
        if Page.ActivePageIndex = Page.PageCount - 1 then
          Page.ActivePageIndex := 0
        else Page.ActivePageIndex := Page.ActivePageIndex + 1;
      end;
    end;
    2:
    begin
      if Page.PageCount >= 0 then
      begin
        if Page.ActivePageIndex = 0 then
          Page.ActivePageIndex := Page.PageCount - 1
        else Page.ActivePageIndex := Page.ActivePageIndex - 1;
      end;
    end;
    3:
    begin
      if Page.PageCount >= 0 then
        Page.ActivePage.Free;
    end;
  end;
end;

function TCnVIDTabPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Ne&w Page';
    1: Result := 'Ne&xt Page';
    2: Result := '&Pravious Page';
    3: Result := '&Delete Page';
  end;
end;

function TCnVIDTabPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

end.
