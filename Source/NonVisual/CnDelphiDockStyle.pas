{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2017 CnPack 开发组                       }
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

{*******************************************************}
{                                                       }
{       具有类似Delphi的停靠风格                        }
{       CnDelphiDockStyle 单元                          }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

unit CnDelphiDockStyle;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：具有类似Delphi的停靠风格的单元 
* 单元作者：CnPack开发组 周益波（鲁小班）
* 备    注：本单元由原作者授权CnPack开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2007.07.13 V1.0
*                移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses 
  Windows, Classes, Controls, Math, Messages, Graphics,
  CnDockFormControl, CnDockSupportControl, CnDockTree, CnConsts, CnCompConsts;

type

  TCnDelphiDockStyle = class(TCnBasicDockStyle)
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;  
    procedure FormDockDrop(DockClient: TCnDockClient;
      Source: TCnDragDockObject; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
//    class function GetControlName: string; override;
    function GetControlName: string; override;
  published
    property ConjoinServerOption;
    property TabServerOption;
  end;

  TCnDelphiDockSplitter = class(TCnDockSplitter);

  TCnDelphiDockPanel = class(TCnDockPanel);

  TCnDelphiConjoinPanel = class(TCnConjoinPanel);

  TCnDelphiTabPageControl = class(TCnTabPageControl)
  protected
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
  end;

  TCnDelphiDockZone = class(TCnDockZone);

  TCnDelphiDockTree = class(TCnDockTree);

  TCnDelphiDragDockObject = class(TCnDragDockObject);
  
implementation

uses
  Forms, SysUtils, CnDockSupportProc, CnDockGlobal;

{ TCnDelphiDockStyle }

constructor TCnDelphiDockStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnDockPanelClass := TCnDelphiDockPanel;
  CnDockSplitterClass := TCnDelphiDockSplitter;
  CnConjoinPanelClass := TCnDelphiConjoinPanel;
  CnTabDockClass := TCnDelphiTabPageControl;
  CnDockPanelZoneClass := TCnDelphiDockZone;
  CnDockPanelTreeClass := TCnDelphiDockTree;
  CnConjoinPanelZoneClass := TCnDelphiDockZone;
  CnConjoinPanelTreeClass := TCnDelphiDockTree;
end;

procedure TCnDelphiDockStyle.FormDockDrop(DockClient: TCnDockClient;
  Source: TCnDragDockObject; X, Y: Integer);
var
  ARect,DRect: TRect;
  DockType: TAlign;
  Host: TForm;
  APanelDock: TWinControl;
  ADockClient: TCnDockClient;
begin
  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    // 调用ComputeDockingRect函数知道停靠的类型
    Host := nil;
    { 锁住Windows桌面 }
    if not IsLoading then
      Cn_LockWindow(nil);
    try
      with DockClient do
      begin
        DockType := ComputeDockingRect(DockClient.ParentForm, ARect, Point(X, Y));
        if (ParentForm.HostDockSite is TCnDockPanel) then
        begin
          // 如果停靠服务器是TDockPanel，就停靠在TDockServer的DockPanel上。
          if DockType = alClient then
          begin
            // 如果停靠类型是alClient
            if Source.Control is TCnTabDockHostForm then
            begin
              // 如果停靠客户是TCnTabDockHostForm，
              // 就先把Parent停靠到TCnTabDockHostForm中，
              // 再把TCnTabDockHostForm停靠到TCnDockPanel中。
              APanelDock := ParentForm.HostDockSite;
              ARect := ParentForm.BoundsRect;
              ParentForm.ManualDock(TCnTabDockHostForm(Source.Control).PageControl, nil, alClient);
              TCnTabDockHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
              Source.Control.BoundsRect := ARect;
              Source.Control.ManualDock(APanelDock, nil, alClient);
              if ParentForm.FormStyle = fsStayOnTop then
                TForm(Source.Control).FormStyle := fsStayOnTop;
            end else
            begin
              // 否则就创建TCnTabDockHostForm，
              // 把把Parent停靠到TCnTabDockHostForm中，
              // 再把TCnTabDockHostForm停靠到TCnDockPanel中。
              APanelDock := ParentForm.HostDockSite;
              DRect.TopLeft := ParentForm.HostDockSite.ClientToScreen(Point(0, 0));
              Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
              SetDockSite(ParentForm, False);
              SetDockSite(TWinControl(Source.Control), False);
              Host.Top := DRect.Top;
              Host.Left := DRect.Left;
              Host.ManualDock(APanelDock, nil, alClient);
              Host.Visible := True;
            end;
          end
          else
          begin
            // 如果停靠类型不是alClient,
            // 就把停靠窗体停靠到TCnDockPanel.
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
          end;
          Exit;
        end;

        // 创建分页的服务窗体
        if DockType = alClient then
        begin
          if Source.Control is TCnTabDockHostForm then
          begin
            ARect := DockClient.ParentForm.BoundsRect;
            DockClient.ParentForm.ManualDock(TCnTabDockHostForm(Source.Control).PageControl, nil, alClient);
            TCnTabDockHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
            Source.Control.BoundsRect := ARect;
            if DockClient.ParentForm.FormStyle = fsStayOnTop then
              TCnTabDockHostForm(Source.Control).FormStyle := fsStayOnTop;
            Exit;
          end else
          begin
            Host := DockClient.CreateTabHostAndDockControl(DockClient.ParentForm, Source.Control);
            Host.Visible := True;
          end;
        end
        // 创建平铺的服务窗体
        else if DockType <> alNone then
        begin
          Host := CreateConjoinHostAndDockControl(ParentForm, Source.Control, DockType);
          ADockClient := FindDockClient(Host);
          if ADockClient <> nil then
            ADockClient.EnableDock := False;
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end;

        if Host <> nil then
        begin
          Host.LRDockWidth := Source.Control.LRDockWidth;
          Host.TBDockHeight := Source.Control.TBDockHeight;
        end;
      end;
    finally
      { 解锁Windows桌面 }
      if not IsLoading then
        Cn_UnLockWindow;
    end;
  end;
end;

procedure TCnDelphiDockStyle.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnDelphiDockStyleName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnDelphiDockStyleComment;
end;

function TCnDelphiDockStyle.GetControlName: string;
begin
  Result := Format(gs_LikeDelphiStyle, [inherited GetControlName]);
end;

{ TCnDelphiTabPageControl }

procedure TCnDelphiTabPageControl.CMDockClient(var Message: TCMDockClient);
var i: Integer;
  AControl: TControl;
  APageCount: Integer;
begin
  if Message.DockSource.Control is TCnTabDockHostForm then
  begin
    with TCnTabDockHostForm(Message.DockSource.Control) do
    begin
      APageCount := Self.PageCount;
      for i := PageControl.DockClientCount - 1 downto 0 do
      begin
        AControl := PageControl.DockClients[i];
        DoFloat(PageControl, AControl);
        AControl.ManualDock(Self, nil, alClient);
        Self.ActivePage.PageIndex := APageCount;
      end;
    end;
  end else
    inherited;
end;

end.
