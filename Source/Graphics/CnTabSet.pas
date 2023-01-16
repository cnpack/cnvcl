{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnTabSet;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：增加了双击事件的TabSet实现单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2017.12.11
*             加入针对 Tab 的 Hint，但不会在不同 Tab 间自动切换
*           2016.05.23
*             加入 Tab 不可见时滚动到第一项的方法
*           2007.03.06
*             创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, Graphics, Forms, Tabs;

type
  TCnTabSetCloseEvent = procedure(Sender: TObject; Index: Integer;
    var CanClose: Boolean) of object;

  TCnTabSetTabHintEvent = procedure(Sender: TObject; Index: Integer;
    var HintStr: string) of object;

  TCnTabSet = class(TTabSet)
  private
    FDblClickClose: Boolean;
    FOnCloseTab: TCnTabSetCloseEvent;
    FShowTabHint: Boolean;
    FOnTabHint: TCnTabSetTabHintEvent;
    function CalcVisibleTabs(Start, Stop: Integer; Canvas: TCanvas;
      First: Integer): Integer;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
  protected
    procedure DoCloseTab(Index: Integer; var CanClose: Boolean); virtual;
  public
    procedure MakeTabVisible;
    {* 当前无 Tab 显示时滚动至第一个 Tab 处}
  published
    property DblClickClose: Boolean read FDblClickClose write FDblClickClose;
    {* 是否双击时自动关闭当前页面}
    property OnCloseTab: TCnTabSetCloseEvent read FOnCloseTab write FOnCloseTab;
    {* 双击时自动关闭页面前触发的事件}
    property ShowTabHint: Boolean read FShowTabHint write FShowTabHint;
    {* 是否针对 Tab 显示 Hint}
    property OnTabHint: TCnTabSetTabHintEvent read FOnTabHint write FOnTabHint;
    {* Tab 要显示 Hint 时触发的事件}
    property OnDblClick;
    {* 双击时触发事件}
  end;

implementation

const
  EdgeWidth = 9;

{ TCnTabSet }

function TCnTabSet.CalcVisibleTabs(Start, Stop: Integer; Canvas: TCanvas;
  First: Integer): Integer;
var
  Index, ASize: Integer;
  W: Integer;
begin
  Index := First;
  while (Start < Stop) and (Index < Tabs.Count) do
    with Canvas do
    begin
      W := TextWidth(Tabs[Index]);

      if (Style = tsOwnerDraw) then MeasureTab(Index, W);

      ASize := W;
      Inc(Start, ASize + EdgeWidth);    { next usable position }

      if Start <= Stop then
      begin
        Inc(Index);
      end;
    end;
  Result := Index - First;
end;

procedure TCnTabSet.CMHintShow(var Message: TMessage);
var
  P: TPoint;
  Index: Integer;
  S: string;
begin
  Message.Result := 1;
  P := ScreenToClient(Mouse.CursorPos);
  Index := ItemAtPos(P) + FirstIndex;

  if (Index >= 0) and Assigned(FOnTabHint) then
  begin
    S := Hint;
    FOnTabHint(Self, Index, S);
    if S <> '' then
    begin
      TCMHintShow(Message).HintInfo^.HintStr := S;
      Message.Result := 0;
    end;
  end;
end;

procedure TCnTabSet.DoCloseTab(Index: Integer; var CanClose: Boolean);
begin
  if Assigned(FOnCloseTab) then
    FOnCloseTab(Self, Index, CanClose);
end;

procedure TCnTabSet.MakeTabVisible;
var
  VTC: Integer;
begin
  // 如果当前无可见 Tab，则滚动到最开始
  VTC := CalcVisibleTabs(StartMargin + EdgeWidth, Width - EndMargin,
    Canvas, FirstIndex);
  if VTC = 0 then
    FirstIndex := 0;
end;

procedure TCnTabSet.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  P: TPoint;
  Index: Integer;
  CanClose: Boolean;
begin
  inherited;
  DblClick;

  if not FDblClickClose then
    Exit;

  P := ScreenToClient(Mouse.CursorPos);
  Index := ItemAtPos(P) + FirstIndex;

  if Index >= 0 then
  begin
    CanClose := True;
    DoCloseTab(Index, CanClose);
    
    if CanClose then
    begin
      Tabs.Delete(Index);
      MakeTabVisible;
    end;
  end;
end;

end.
