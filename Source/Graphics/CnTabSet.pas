{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2012 CnPack 开发组                       }
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
* 单元标识：$Id$
* 修改记录：2007.03.06
              创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, Tabs;

type
  TCnTabSetCloseEvent = procedure(Sender: TObject; Index: Integer;
    var CanClose: Boolean) of object;

  TCnTabSet = class(TTabSet)
  private
    FDblClickClose: Boolean;
    FOnCloseTab: TCnTabSetCloseEvent;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
  protected
    procedure DoCloseTab(Index: Integer; var CanClose: Boolean); virtual;
  published
    property DblClickClose: Boolean read FDblClickClose write FDblClickClose;
    {* 是否双击时自动关闭当前页面}
    property OnCloseTab: TCnTabSetCloseEvent read FOnCloseTab write FOnCloseTab;
    {* 双击时自动关闭页面前触发的事件}
    property OnDblClick;
    {* 双击时触发事件}
  end;

implementation

{ TCnTabSet }

procedure TCnTabSet.DoCloseTab(Index: Integer; var CanClose: Boolean);
begin
  if Assigned(FOnCloseTab) then
    FOnCloseTab(Self, Index, CanClose);
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
  Index := ItemAtPos(P);
  if Index >= 0 then
  begin
    CanClose := True;
    DoCloseTab(Index, CanClose);
    
    if CanClose then
      Tabs.Delete(Index);
  end;
end;

end.
