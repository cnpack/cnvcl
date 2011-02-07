{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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
{       具有类似Visual C++的停靠风格                    }
{       CnVCDockStyle 单元                              }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

unit CnVCDockStyle;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：具有类似Visual C++的停靠风格的单元 
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
  Windows, Classes, Controls, Math, Messages, Graphics, ExtCtrls, Consts,
  CnDockFormControl, CnDockSupportControl, CnDockTree, CnConsts, CnCompConsts;

const
  { 扩展按钮 }
  HTEXPAND  = 31;

  VCDefaultGrabberSize   = 15;
  VCDefaultSplitterWidth = 4;
  VCDefaultBorderWidth = 4;

type
  { VC平铺服务器的选项类 }
  TCnVCConjoinServerOption = class(TCnBasicConjoinServerOption)
  private
    FBorderWidth: Integer;
    procedure SetBorderWidth(const Value: Integer);         // 边框的宽度
  protected
    procedure ResetDockControlOption; override;
  public
    constructor Create(ADockStyle: TCnBasicDockStyle); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 4;
  end;

  { VC分页服务器的选项类 }
  TCnVCTabServerOption = class(TCnBasicTabServerOption);

  TCnVCDockStyle = class(TCnAdvDockStyle)
  private
    FOldEachOtherDock: Boolean;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure FormGetDockEdge(DockClient: TCnDockClient; Source: TCnDragDockObject;
      MousePos: TPoint; var DropAlign: TAlign); override;
    procedure FormStartDock(DockClient: TCnDockClient;
      var Source: TCnDragDockObject); override;
    procedure AssignConjoinServerOption(APanel: TCnCustomDockPanel); override;
    procedure CreateConjoinServerOption(var Option: TCnBasicConjoinServerOption); override;
    procedure CreateTabServerOption(var Option: TCnBasicTabServerOption); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanSetEachOtherDocked(ADockBaseControl: TCnDockBaseControl): Boolean; override;
    function GetControlName: string; override;
    procedure SetDockBaseControl(IsCreate: Boolean;
      DockBaseControl: TCnDockBaseControl); override;
  published
    property ConjoinServerOption;
    property TabServerOption;
  end;

  TCnVCDockSplitter = class(TCnDockSplitter)
  private
    FOldSize: Integer;
  protected
    function DoCanResize(var NewSize: Integer): Boolean; override;
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCnVCDockPanel = class(TCnAdvDockPanel)
  protected
    procedure CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean); override;
    procedure CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    function CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl; Client: TControl): Boolean; override;
    function GetDockEdge(MousePos: TPoint): TAlign; override;
    procedure CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer); override;
    procedure CustomGetDockEdge(Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign); override;
  public
  end;

  TCnVCConjoinPanel = class(TCnConjoinPanel);
  TCnVCTabPageControl = class(TCnAdvTabPageControl);

  TZoneSizeStyle = (zssMinimum, zssNormal, zssMaximum);
                   {最大化状态, 正常状态,  最小化状态}

  TCnVCDockZone = class(TCnAdvDockZone)
  private
    FZoneSizeStyle: TZoneSizeStyle;
    FExpandBtnDown: Boolean;
    procedure DoSetChildSizeStyle(ZoneSizeStyle: TZoneSizeStyle);
  public
    constructor Create(Tree: TCnDockTree); override;
    destructor Destroy; override;
    procedure Insert(DockSize: Integer; Hide: Boolean); override;
    procedure Remove(DockSize: Integer; Hide: Boolean); override;
    procedure InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean); override;
    procedure SetZoneSize(Size: Integer; Show: Boolean); override;
    property ZoneSizeStyle: TZoneSizeStyle read FZoneSizeStyle write FZoneSizeStyle;
    property ExpandBtnDown: Boolean read FExpandBtnDown write FExpandBtnDown;
  end;

  TCnVCDockTree = class(TCnAdvDockTree)
  private
    FExpandBtnZone: TCnVCDockZone;
  protected
    procedure WindowProc(var Message: TMessage); override;
    { ------------------------------------------------------------------------ }
    procedure BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer = -1); override;
    { ------------------------------------------------------------------------ }
    procedure ControlVisibilityChanged(Control: TControl; Visible: Boolean); override;
    function GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign; override;
    { ------------------------------------------------------------------------ }
    function DoLButtonDown(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer): Boolean; override;
    procedure DoLButtonUp(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure DoMouseMove(var Message: TWMMouse;
      var Zone: TCnDockZone; out HTFlag: Integer); override;
    procedure DoOtherHint(Zone: TCnDockZone;
      HTFlag: Integer; var HintStr: string); override;

    procedure CustomSaveZone(Stream: TStream;
      Zone: TCnDockZone); override;
    procedure CustomLoadZone(Stream: TStream;
      var Zone: TCnDockZone); override;
    { ------------------------------------------------------------------------ }
    procedure CalcSplitterPos; override;
    { 获得DropOnControl的父Zone }
    function GetDropOnZone(Orient: TDockOrientation; DockRect: TRect; var DropAlign: TAlign): TCnDockZone; virtual;
    { 获得DropOnControl }
    function GetDropOnControl(Orient: TDockOrientation; Zone: TCnDockZone; DockRect: TRect;
      var DropAlign: TAlign; Control: TControl): TControl; virtual;

    function GetDockEdge(DockRect: TRect; MousePos: TPoint;
      var DropAlign: TAlign; Control: TControl): TControl; override;

    function GetLeftGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; override;
    function GetTopGrabbersHTFlag(const MousePos: TPoint;
      out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone; override;
    { ------------------------------------------------------------------------ }
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override;
    procedure InsertNewParent(NewZone, SiblingZone: TCnDockZone;
      ParentOrientation: TDockOrientation; InsertLast, Update: Boolean); override;
    procedure InsertSibling(NewZone, SiblingZone: TCnDockZone;
      InsertLast, Update: Boolean); override;
    procedure DrawDockGrabber(Control: TControl;
      const ARect: TRect); override;
    procedure DrawDockSiteRect; override;
    procedure DrawSplitterRect(
      const ARect: TRect); override;
    procedure GetCaptionRect(var Rect: TRect); override;
    procedure RemoveControl(Control: TControl); override;
    procedure RemoveZone(Zone: TCnDockZone; Hide: Boolean = True); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure ResetZoneSizeStyle(Parent: TCnDockZone;
      ZoneSizeStyle: TZoneSizeStyle; Exclude: TCnDockZone);
    procedure ScaleZone(Zone: TCnDockZone); override;
    procedure ScaleChildZone(Zone: TCnDockZone); override;
    procedure ScaleSiblingZone(Zone: TCnDockZone); override;

    procedure ShiftZone(Zone: TCnDockZone); override;
    procedure SplitterMouseUp; override;

  public
    constructor Create(DockSite: TWinControl;
      CnDockZoneClass: TCnDockZoneClass); override;
  end;

  TCnVCDragDockObject = class(TCnDragDockObject)
  private
    FDockOverBrush: TBrush;         //停靠预览的画布
    FDockOverFrameWidth: Integer;   //停靠预览的框架宽度
    FCurrState,                     //当前的DockOver的State;
    FOldState: TDragState;          //前一次DockOver的State;
    FOldTarget: Pointer;            //前一次停靠服务器
    procedure SetOldState(const Value: TDragState);
    procedure SetCurrState(const Value: TDragState);
  protected
    procedure GetBrush_PenSize_DrawRect(
      var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean); override;
    procedure SetDefaultBrushStyle; virtual;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    function DragFindWindow(const Pos: TPoint): HWND; override;
    function GetDropCtl: TControl; override;
    property CurrState: TDragState read FCurrState write SetCurrState;
    property OldState: TDragState read FOldState write SetOldState;
    property OldTarget: Pointer read FOldTarget write FOldTarget;
    property DockOverFrameWidth: Integer read FDockOverFrameWidth write FDockOverFrameWidth;
    property DockOverBrush: TBrush read FDockOverBrush;
  end;

implementation

uses Forms, SysUtils, CnDockSupportProc, CnDockGlobal;

const DefaultFrameWidth = 3;
  DefaultDockOverFrameWidth = 1;
  DefaultDockOverBrushStyle = bsSolid;

                     { 方向 }           { 是否在后面 }
  DropAlignArr: array[TDockOrientation, Boolean] of TAlign =
    ((alNone, alNone), (alTop, alBottom), (alLeft, alRight));

type
  TCnTempWinControl = class(TWinControl);

{ TCnVCDockStyle }

procedure TCnVCDockStyle.AssignConjoinServerOption(
  APanel: TCnCustomDockPanel);
begin
  inherited;
  if ConjoinServerOption is TCnVCConjoinServerOption then
    APanel.CnDockManager.BorderWidth := TCnVCConjoinServerOption(ConjoinServerOption).BorderWidth;
end;

function TCnVCDockStyle.CanSetEachOtherDocked(
  ADockBaseControl: TCnDockBaseControl): Boolean;
begin
  Result := False;
end;

constructor TCnVCDockStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnDockPanelClass := TCnVCDockPanel;
  CnDockSplitterClass := TCnVCDockSplitter;
  CnConjoinPanelClass := TCnVCConjoinPanel;
  CnTabDockClass := TCnVCTabPageControl;
  CnDockPanelTreeClass := TCnVCDockTree;
  CnDockPanelZoneClass := TCnVCDockZone;
  CnConjoinPanelTreeClass := TCnVCDockTree;
  CnConjoinPanelZoneClass := TCnVCDockZone;
  CnConjoinServerOptionClass := TCnVCConjoinServerOption;
  CnTabServerOptionClass := TCnVCTabServerOption;
end;

procedure TCnVCDockStyle.CreateConjoinServerOption(
  var Option: TCnBasicConjoinServerOption);
begin
  Option := TCnVCConjoinServerOption.Create(Self);
end;

procedure TCnVCDockStyle.CreateTabServerOption(
  var Option: TCnBasicTabServerOption);
begin
  Option := TCnVCTabServerOption.Create(Self);
end;

destructor TCnVCDockStyle.Destroy;
begin
  inherited Destroy;
end;

procedure TCnVCDockStyle.FormGetDockEdge(DockClient: TCnDockClient;
  Source: TCnDragDockObject; MousePos: TPoint; var DropAlign: TAlign);
begin
end;

procedure TCnVCDockStyle.FormStartDock(DockClient: TCnDockClient; var Source: TCnDragDockObject);
begin
  inherited FormStartDock(DockClient, Source);
  Source := TCnVCDragDockObject.Create(DockClient.ParentForm);
end;

procedure TCnVCDockStyle.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnVCDockStyleName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnVCDockStyleComment;
end;

function TCnVCDockStyle.GetControlName: string;
begin
  Result := Format(gs_LikeVCStyle, [inherited GetControlName]);
end;

procedure TCnVCDockStyle.SetDockBaseControl(IsCreate: Boolean;
  DockBaseControl: TCnDockBaseControl);
var ADockClient: TCnDockClient;
begin
  if DockBaseControl is TCnDockClient then
  begin
    ADockClient := TCnDockClient(DockBaseControl);
    if IsCreate then
    begin
      FOldEachOtherDock := ADockClient.EachOtherDock;
      ADockClient.EachOtherDock := False;
      ADockClient.DirectDrag := True;
    end else
    begin
      ADockClient.EachOtherDock := FOldEachOtherDock;
    end;
  end;
end;

{ TCnVCDockPanel }

procedure TCnVCDockPanel.CustomDockDrop(Source: TCnDragDockObject; X, Y: Integer);
begin
  if Source is TCnVCDragDockObject then
  begin
    TCnVCDragDockObject(Source).CurrState := dsDragEnter;
    TCnVCDragDockObject(Source).OldState := dsDragEnter;
  end;

  if Source.DropOnControl <> Source.Control then
    inherited CustomDockDrop(Source, X, Y);
end;

procedure TCnVCDockPanel.CustomDockOver(Source: TCnDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var DropAlign: TAlign;
  VCSource: TCnVCDragDockObject;
  SysCaptionHeight: Integer; //系统的标题栏的高度
  PanelScreenRect: TRect;
begin
  inherited CustomDockOver(Source, X, Y, State, Accept);

  if Source is TCnVCDragDockObject then
  begin
    VCSource := TCnVCDragDockObject(Source);
    VCSource.OldState := VCSource.CurrState;
    VCSource.CurrState := State;
  end;

  if State = dsDragMove then
  begin
    { 获得TCnDragDockObject的DropOnControl和DropAlign }
    DropAlign := Source.DropAlign;
    Source.DropOnControl := CnDockManager.GetDockEdge(Source.EraseDockRect, Source.DragPos, DropAlign, Source.Control);
    Source.DropAlign := DropAlign;

    SysCaptionHeight := Integer(Source.Control.Floating) * Cn_GetSysCaptionHeight;

    { 映射坐标DockPanel的坐标位置 }
    PanelScreenRect := BoundsRect;
    MapWindowPoints(Parent.Handle, 0, PanelScreenRect, 2);

    if ((Align in [alTop, alBottom]) and
      (Source.DockRect.Right = PanelScreenRect.Right) and
      (Source.DockRect.Left = PanelScreenRect.Left)) or
      ((Align in [alLeft, alRight]) and
      (Source.DockRect.Top = PanelScreenRect.Top) and
      (Source.DockRect.Bottom = PanelScreenRect.Bottom)) then
        Exit;

    if ((Source.DropOnControl <> nil) and (Source.DropOnControl <> Source.Control)) and
      (Source.DropOnControl.HostDockSite <> Source.Control.HostDockSite) then
    begin
      if (DropAlign in [alTop, alBottom]) then
      begin
        if ((Source.Control.DockOrientation = doVertical) or (Source.Control.HostDockSite = nil)) then
          Source.DockRect.Bottom := Source.DockRect.Top + Source.Control.UnDockHeight - SysCaptionHeight;
      end else
      if (DropAlign in [alLeft, alRight]) then
      begin
        if (Source.Control.DockOrientation = doHorizontal) or (Source.Control.HostDockSite = nil) then
          Source.DockRect.Right := Source.DockRect.Left + Source.Control.UnDockWidth - SysCaptionHeight;
      end;
    end;
  end;
end;

function TCnVCDockPanel.CustomUnDock(Source: TCnDragDockObject; NewTarget: TWinControl;
  Client: TControl): Boolean;
var DropAlign: TAlign;
  MousePos: TPoint;
begin
  if (NewTarget = nil) or (NewTarget = Client.HostDockSite) then
  begin
    DropAlign := Source.DropAlign;
    Source.DropOnControl := CnDockManager.GetDockEdge(
      Source.DockRect, Source.DragPos, DropAlign, Source.Control);
    Source.DropAlign := DropAlign;
  end;
  MousePos := ScreenToClient(Source.DragPos);
  if ((Align in [alTop, alBottom]) and ((0 > MousePos.X) or (Width < MousePos.X)))
    or ((Align in [alLeft, alRight]) and ((0 > MousePos.Y) or (Height < MousePos.Y)))
    or (Source.CtrlDown) or Source.Floating then
    Result := inherited CustomUnDock(Source, NewTarget, Client)
  else if (Source.DropOnControl <> Source.Control) then
    Result := inherited CustomUnDock(Source, NewTarget, Client)
  else Result := True;
end;

procedure TCnVCDockPanel.CustomGetSiteInfo(Source: TCnDragDockObject; Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  GetWindowRect(Handle, InfluenceRect);
  with Client, CnGlobalDockPresident.DragObject do
  begin
    case Self.Align of
    alTop:
      begin
        if MousePos.y >= InfluenceRect.Top then
          InflateRect(InfluenceRect, 0,
            Cn_GetMinOffset(TBDockHeight, Height, MouseDeltaY));
      end;
    alBottom:
      begin
        if MousePos.y <= InfluenceRect.Top then
          InflateRect(InfluenceRect, 0,
            Cn_GetMinOffset(TBDockHeight, Height, 1 - MouseDeltaY));
      end;
    alLeft:
      begin
        if MousePos.x >= InfluenceRect.Left then
          InflateRect(InfluenceRect,
            Cn_GetMinOffset(LRDockWidth, Width, MouseDeltaX), 0);
      end;
    alRight:
      begin
        if MousePos.x <= InfluenceRect.Left then
          InflateRect(InfluenceRect,
            Cn_GetMinOffset(LRDockWidth, Width, 1 - MouseDeltaX), 0);
      end;
    end;
  end;
  CanDock := IsDockable(Self, Client, Source.DropOnControl, Source.DropAlign);
end;

function TCnVCDockPanel.GetDockEdge(MousePos: TPoint): TAlign;
begin
  Result := inherited GetDockEdge(MousePos);
end;

procedure TCnVCDockPanel.CustomPositionDockRect(Source: TCnDragDockObject; X, Y: Integer);
var ARect: TRect;
  BorderWidth: Integer;      //边框宽度
  GrabberSize: Integer;      //把手大小
  PanelScreenRect: TRect;
  SysCaptionHeight: Integer; //系统的标题栏的高度
  DockSize: Integer;

  procedure GetDockSize;
  begin
    if Align in [alLeft, alRight] then
    begin
      if (Source.Control.HostDockSite <> nil) and
        (Source.Control.HostDockSite <> Source.TargetControl) then
        DockSize := Source.Control.UndockHeight
      else
        DockSize := Source.Control.Height;
    end else
    begin
      if (Source.Control.HostDockSite <> nil) and
        (Source.Control.HostDockSite <> Source.TargetControl) then
        DockSize := Source.Control.UndockWidth
      else
        DockSize := Source.Control.Width;
    end;
  end;

  procedure SetMaxDockSize(Align: TAlign);
  begin
    if Align in [alLeft, alRight] then
    begin
      if ARect.Bottom - ARect.Top >= PanelScreenRect.Bottom - PanelScreenRect.Top then
      begin
        ARect.Top := PanelScreenRect.Top;
        ARect.Bottom := PanelScreenRect.Bottom;
      end;
    end else
    begin
      if ARect.Right - ARect.Left >= PanelScreenRect.Right - PanelScreenRect.Left then
      begin
        ARect.Left := PanelScreenRect.Left;
        ARect.Right := PanelScreenRect.Right;
      end;
    end;
  end;

begin
  { 获得Source.Control的HostDockSite的边框宽度和把手大小 }
  if Source.Control.HostDockSite is TCnCustomDockPanel then
  begin
    BorderWidth := TCnCustomDockPanel(Source.Control.HostDockSite).CnDockManager.BorderWidth;
    GrabberSize := TCnCustomDockPanel(Source.Control.HostDockSite).CnDockManager.GrabberSize;
  end else
  begin
    BorderWidth := 0;
    GrabberSize := 0;
  end;

  { 映射坐标DockPanel的坐标位置 }
  PanelScreenRect := BoundsRect;
  MapWindowPoints(Parent.Handle, 0, PanelScreenRect, 2);

  SysCaptionHeight := Integer(Source.Control.Floating) * Cn_GetSysCaptionHeight;

  GetDockSize;

  with Source.Control do
  begin
    case Self.Align of
      alTop:
      begin
        ARect.TopLeft := Self.ClientToScreen(Point(0, 0));
        ARect.BottomRight := Self.ClientToScreen(
          Point(Self.Width, TBDockHeight));
        ARect.Top := ARect.Top + Y -
          Cn_GetMinOffset(TBDockHeight, Height + 2 * BorderWidth, Source.MouseDeltaY);
        ARect.Bottom := ARect.Top + TBDockHeight;
        if (Self.Height > 0) and (ARect.Top + TBDockHeight div 2 < PanelScreenRect.Bottom) and
          (ARect.Bottom - TBDockHeight div 2 > PanelScreenRect.Top) then
        begin
          ARect.Left := ARect.Left + X - Round((Width + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaX);
          ARect.Right := ARect.Left + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
        end;
      end;
      alBottom:
      begin
        ARect.TopLeft := Self.ClientToScreen(
          Point(0, -TBDockHeight));
        ARect.BottomRight := Self.ClientToScreen(
          Point(Self.Width, 0));
        ARect.Top := ARect.Top + Y +
          Cn_GetMinOffset(TBDockHeight, Height + 2 * BorderWidth, 1 - Source.MouseDeltaY);
        ARect.Bottom := ARect.Top + TBDockHeight;
        if (Self.Height > 0) and (ARect.Top + TBDockHeight div 2 < PanelScreenRect.Bottom) and
          (ARect.Bottom - TBDockHeight div 2 > PanelScreenRect.Top) then
        begin
          ARect.Left := ARect.Left + X - Round((Width + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaX);
          ARect.Right := ARect.Left + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
        end;
      end;
      alLeft:
      begin
        ARect.TopLeft := Self.ClientToScreen(Point(0, 0));
        ARect.BottomRight := Self.ClientToScreen(
          Point(LRDockWidth, Self.Height));
        ARect.Left := ARect.Left + X -
          Cn_GetMinOffset(LRDockWidth, Width + 2*BorderWidth, Source.MouseDeltaX);
        ARect.Right := ARect.Left + LRDockWidth;
        if (Self.Width > 0) and ((ARect.Left + LRDockWidth div 2 < PanelScreenRect.Right) and
          (ARect.Right - LRDockWidth div 2 > PanelScreenRect.Left)) then
        begin
          ARect.Top := ARect.Top + Y - Round((Height + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaY);
          ARect.Bottom := ARect.Top + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
        end;
      end;
      alRight:
      begin
        ARect.TopLeft := Self.ClientToScreen(
          Point(-LRDockWidth, 0));
        ARect.BottomRight := Self.ClientToScreen(
          Point(Self.Width, Self.Height));
        ARect.Left := ARect.Left + X +
          Cn_GetMinOffset(LRDockWidth, Width + 2*BorderWidth, 1 - Source.MouseDeltaX);
        ARect.Right := ARect.Left + LRDockWidth;
        if (Self.Width > 0) and (ARect.Left + LRDockWidth div 2 > PanelScreenRect.Left) and
          (ARect.Right - LRDockWidth div 2 < PanelScreenRect.Right) then
        begin
          ARect.Top := ARect.Top + Y - Round((Height + 2 * BorderWidth + GrabberSize) * Source.MouseDeltaY);
          ARect.Bottom := ARect.Top + DockSize + 2 * BorderWidth + GrabberSize - SysCaptionHeight;
        end;
      end;
    end;
  end;
  SetMaxDockSize(Align);
  Inc(ARect.Left);
  Source.DockRect := ARect;
end;

procedure TCnVCDockPanel.CustomGetDockEdge(Source: TCnDragDockObject;
  MousePos: TPoint; var DropAlign: TAlign);
begin
end;

{ TCnVCDockTree }

procedure TCnVCDockTree.BeginDrag(Control: TControl;
      Immediate: Boolean; Threshold: Integer);
begin
  CnGlobalDockPresident.BeginDrag(Control, True, 0);
end;

constructor TCnVCDockTree.Create(DockSite: TWinControl;
  CnDockZoneClass: TCnDockZoneClass);
begin
  inherited Create(DockSite, CnDockZoneClass);
  Version := gs_VCDockTreeVersion;
  BorderWidth     := 4;
  MinSize         := 20;
end;

function TCnVCDockTree.GetDockEdge(DockRect: TRect;
  MousePos: TPoint; var DropAlign: TAlign; Control: TControl): TControl;
var Zone: TCnDockZone;
  TempOrient: TDockOrientation;
begin
  inherited GetDockEdge(DockRect, MousePos, DropAlign, Control);
  { 初始化 }
  MapWindowPoints(0, DockSite.Handle, DockRect, 2);
  SetDockHeightWidthArr(0, DockSite.Height, DockSite.Width);
  SetDockRectArr(DockRect);
  { 开始计算 }
  TempOrient := DockSiteOrient;
  Zone := GetDropOnZone(TempOrient, DockRect, DropAlign);
  TempOrient := Cn_ExchangeOrient(TempOrient);
  Result := GetDropOnControl(TempOrient, Zone, DockRect, DropAlign, Control);
  DropDockSize := DockRectArr[TempOrient, True] - DockRectArr[TempOrient, False];// + BorderWidth div 2;
end;

function TCnVCDockTree.GetDropOnZone(Orient: TDockOrientation; DockRect: TRect; var DropAlign: TAlign): TCnDockZone;

var TempZone: TCnDockZone;
  Scale: Double;

    procedure GetBeginBorderZone(BorderLimit: Integer);
    begin
      if DockRectArr[Orient, True] = BorderLimit then
        Scale := 9999
      else
        Scale := (BorderLimit - DockRectArr[Orient, False]) / (DockRectArr[Orient, True] - BorderLimit);
      if Scale >= 0 then
      begin
        if Scale >= 1 then
          Result := TempZone.BeforeClosestVisibleZone
        else Result := TempZone;
      end;
    end;

    procedure GetEndBorderZone(BorderLimit: Integer);
    begin
      if (DockRectArr[Orient, True] <= BorderLimit) then
        Scale := 9999
      else
        Scale := (BorderLimit - DockRectArr[Orient, False]) / (DockRectArr[Orient, True] - BorderLimit);
      if Scale >= 0 then
      begin
        if Scale < 1 then
          Result := TempZone.AfterClosestVisibleZone
        else Result := TempZone;
      end;
    end;

var TempOrient: TDockOrientation;
begin
  Result := nil;
  TempOrient := Cn_ExchangeOrient(Orient);
  if (DockRectArr[TempOrient, False] > DockHeightWidth[TempOrient])
    or (DockRectArr[TempOrient, True] < 0) then
      Exit;

  { 如果TopZone的Orientation属性是'垂直'，就计算DockRect和TempZone之间的位置关系 }
  if (DockRectArr[Orient, False] + DockRectArr[Orient, True]) div 2 <= 0 then
    { 是在DockSite的左边吗 }
    DropAlign := DropAlignArr[Orient, False]
  else if (DockRectArr[Orient, False] + DockRectArr[Orient, True]) div 2 >= DockHeightWidth[Orient] then
    { 是在DockSite的右边吗 }
    DropAlign := DropAlignArr[Orient, True]
  else
  begin
    { 那就是在DockSite的中间 }
    if (TopZone.ChildCount <= 1) or (TopZone.Orientation <> Orient) then
      Result := TopZone
    else
    begin
      Scale := 0;
      TempZone := TopZone.ChildZones;
      GetBeginBorderZone(0);
      while (TempZone <> nil) and (Scale <= 0) do
      begin
        GetEndBorderZone(TempZone.ZoneLimit);
        TempZone := TempZone.AfterClosestVisibleZone;
      end;
    end;
  end;
end;

function TCnVCDockTree.GetDropOnControl(Orient: TDockOrientation; Zone: TCnDockZone; DockRect: TRect;
  var DropAlign: TAlign; Control: TControl): TControl;

var TempZone: TCnDockZone;
  Scale: Double;
  BeginBorderLimit,
  EndBorderLimit: Integer;

  procedure GetBeginBorderControl(Zone: TCnDockZone);
  begin
    BeginBorderLimit := Zone.TopLeft[Orient];

    if DockRectArr[Orient, False] < BeginBorderLimit then
    begin
      Result := Zone.ChildControl;
      DropAlign := DropAlignArr[Orient, False];
    end;
  end;

  procedure GetEndBorderControl(Zone: TCnDockZone);
  begin
    BeginBorderLimit := Zone.TopLeft[Orient];
    EndBorderLimit := BeginBorderLimit + Zone.HeightWidth[Orient];

    if DockRectArr[Orient, False] < EndBorderLimit then
    begin
      Result := Zone.ChildControl;
      if DockRectArr[Orient, False] = BeginBorderLimit then
        Scale := 9999

      else       { 下边距/上边距 }
        Scale := (EndBorderLimit - DockRectArr[Orient, True]) / (DockRectArr[Orient, False] - BeginBorderLimit);
      if Scale >= 1 then
        DropAlign := DropAlignArr[Orient, False]
      else
      begin
        if (Zone.AfterClosestVisibleZone <> nil) and (Zone.AfterClosestVisibleZone.ChildControl = Control) then
        begin
          Result := Zone.AfterClosestVisibleZone.ChildControl;
          DropAlign := DropAlignArr[Orient, False];
        end else
          DropAlign := DropAlignArr[Orient, True];
      end;
    end;
  end;

begin
  Result := nil;
  Scale := 0;
  if Zone <> nil then
  begin
    if Zone.ChildCount = 0 then
    begin
      GetBeginBorderControl(Zone);
      if Result = nil then
        GetEndBorderControl(Zone);
    end else
    begin
      TempZone := Zone.ChildZones;
      if TempZone <> nil then
        GetBeginBorderControl(TempZone);
      while (TempZone <> nil) and (Result = nil) do
      begin
        GetEndBorderControl(TempZone);
        TempZone := TempZone.AfterClosestVisibleZone;
      end;
    end;
  end;
end;

procedure TCnVCDockTree.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
begin
  inherited InsertControl(Control, InsertAt, DropCtl);

end;

procedure TCnVCDockTree.DrawDockGrabber(Control: TControl;
  const ARect: TRect);
var CnVCDockZone: TCnVCDockZone;

  procedure DrawCloseButton(Left, Top: Integer);
  var ADockClient: TCnDockClient;
  begin
    if CnVCDockZone <> nil then
    begin
      ADockClient := FindDockClient(Control);
      if (ADockClient <> nil) and (not ADockClient.EnableCloseBtn) then Exit;
      DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+ButtonWidth,
        Top+ButtonHeight), DFC_CAPTION, DFCS_CAPTIONCLOSE or Integer(CnVCDockZone.CloseBtnDown) * DFCS_PUSHED)
    end;
  end;

  procedure DrawExpendBotton(Left, Top: Integer);
  const
{$IFDEF COMPILER6_UP}
    ArrowOrient: array[TAlign] of DWORD =
      (0, DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT, 0, 0);
{$ELSE}
    ArrowOrient: array[TAlign] of DWORD =
      (0, DFCS_SCROLLUP, DFCS_SCROLLDOWN, DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT, 0);
{$ENDIF}


    CurrArrow: array[Boolean, TDockOrientation] of TAlign =
      ((alNone, alLeft, alTop), (alNone, alRight, alBottom));

  var InActive: Boolean;  //是否可用
      IsMaximum: Boolean; //是否是最大化
  begin
    if CnVCDockZone <> nil then
    begin
      InActive := not ((CnVCDockZone.ParentZone.Orientation <> DockSiteOrient) and
        (CnVCDockZone.ParentZone.VisibleChildCount >= 2));
      IsMaximum := CnVCDockZone.ZoneSizeStyle in [zssMaximum];
      DrawFrameControl(Canvas.Handle, Rect(Left, Top, Left+ButtonWidth,
        Top+ButtonHeight), DFC_SCROLL,
        ArrowOrient[CurrArrow[IsMaximum, DockSiteOrient]] +
        Cardinal(InActive) * (DFCS_INACTIVE) + Cardinal(CnVCDockZone.ExpandBtnDown) * DFCS_PUSHED);
    end;
  end;

  procedure DrawGrabberLine(Left, Top, Right, Bottom: Integer);
  begin
    if (Left >= Right) or (Top >= Bottom) then Exit;
    with Canvas do
    begin
      Pen.Color := clBtnHighlight;
      MoveTo(Right, Top);
      LineTo(Left, Top);
      LineTo(Left, Bottom);
      Pen.Color := clBtnShadow;
      LineTo(Right, Bottom);
      LineTo(Right, Top-1);
    end;
  end;

var DrawRect: TRect;

begin

  CnVCDockZone := TCnVCDockZone(FindControlZone(Control));
  DrawRect := ARect;
  Canvas.Brush.Color := TCnTempWinControl(DockSite).Color;
  Canvas.FillRect(DrawRect);
  with ARect do
    case GrabbersPosition of
      gpLeft:
        begin
          DrawExpendBotton(Left+BorderWidth+LeftOffset, Top+TopOffset+ButtonHeight+ButtonSplitter+BorderWidth);
          DrawCloseButton(Left+BorderWidth+LeftOffset, Top+TopOffset+BorderWidth);
          DrawGrabberLine(Left+BorderWidth+LeftOffset+3, Top+2*ButtonHeight+TopOffset+ButtonSplitter+BottomOffset+BorderWidth+3, Left+BorderWidth+LeftOffset+5, Bottom-BorderWidth-2);
          DrawGrabberLine(Left+BorderWidth+LeftOffset+7, Top+2*ButtonHeight+TopOffset+ButtonSplitter+BottomOffset+BorderWidth+3, Left+BorderWidth+LeftOffset+9, Bottom-BorderWidth-2);
        end;
      gpTop:
        begin
          DrawExpendBotton(Right-LeftOffset-2*ButtonWidth-ButtonSplitter-BorderWidth, Top+TopOffset+BorderWidth);
          DrawCloseButton(Right-LeftOffset-ButtonWidth-BorderWidth, Top+TopOffset+BorderWidth);
          DrawGrabberLine(Left+BorderWidth, Top+BorderWidth+TopOffset+3, Right-2*ButtonWidth-RightOffset-ButtonSplitter-LeftOffset-BorderWidth-3, Top+BorderWidth+TopOffset+5);
          DrawGrabberLine(Left+BorderWidth, Top+BorderWidth+TopOffset+7, Right-2*ButtonWidth-RightOffset-ButtonSplitter-LeftOffset-BorderWidth-3, Top+BorderWidth+TopOffset+9);
        end;
      gpBottom:
        begin

        end;
      gpRight:
        begin

        end;
    end;
end;

procedure TCnVCDockTree.DrawDockSiteRect;
var
  Rect: TRect;
begin
  inherited DrawDockSiteRect;
  Rect := DockSite.ClientRect;
  InflateRect(Rect, BorderWidth, 0);
  if DockSite.Align = alTop then
    Inc(Rect.Bottom, BorderWidth)
  else if DockSite.Align = alBottom then
    Dec(Rect.Top, BorderWidth);
  Frame3D(Canvas, Rect, clBtnShadow, clBtnHighlight, 1);
  Frame3D(Canvas, Rect, clBtnHighlight, clBtnShadow, 1);

  Canvas.Pen.Color := clBlack;
  if DockSite.Align = alRight then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, DockSite.Height);
  end else if DockSite.Align = alBottom then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(DockSite.Width, 0);
  end;
end;

procedure TCnVCDockTree.DrawSplitterRect(const ARect: TRect);
var Rect: TRect;
begin
  inherited;
  Rect := ARect;
  InflateRect(Rect, 1, 1);
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
end;

procedure TCnVCDockTree.WindowProc(var Message: TMessage);
begin
  inherited WindowProc(Message);
end;

procedure TCnVCDockTree.SplitterMouseUp;
begin
  BeginUpdate;
  try
    ShiftBy := 0;
    { 根据DockSite的Align属性和SizingZone.ParentZone的Orientation属性
      得到应该调整的偏移量 }
    if (DockSiteOrient = doVertical) and
      (SizingZone.ParentZone.Orientation = doVertical) then
      ShiftBy := SizePos.x + (SplitterWidth div 2) - SizingZone.ZoneLimit
    else if (DockSiteOrient = doHorizontal) and
      (SizingZone.ParentZone.Orientation = doHorizontal) then
      ShiftBy := SizePos.y + (SplitterWidth div 2) - SizingZone.ZoneLimit;

    if (ShiftBy <> 0) and (SizingZone.AfterClosestVisibleZone <> nil) then
    begin
      if (DockSite.Align in [alLeft, alTop]) then
      begin
        ShiftScaleOrient := DockSiteOrient;
        { SizingZone后面的节点都要进行调整 }
        ForEachAt(SizingZone.AfterClosestVisibleZone, ShiftZone, tskForward);
        { 调用默认的SplitterMouseUp }
        inherited SplitterMouseUp;
      end else
      begin
        { 然后SizingZone的NextSibling后面的节点都要进行调整 }
        ShiftBy := -ShiftBy;
        ShiftScaleOrient := DockSiteOrient;
        ForEachAt(SizingZone.AfterClosestVisibleZone, ShiftZone, tskForward);
        SizePos := Point(SizePos.x + ShiftBy, SizePos.Y + ShiftBy);
        inherited SplitterMouseUp;
      end;

      { 根据DockSite的Align属性调整宽度或者高度 }
      DockSiteSize := DockSiteSize + ShiftBy;
    end else
    begin
      TCnVCDockZone(SizingZone.ParentZone).DoSetChildSizeStyle(zssNormal);
      inherited SplitterMouseUp;
    end;
  finally
    EndUpdate;
  end;

end;

procedure TCnVCDockTree.ResetBounds(Force: Boolean);
var R: TRect;
begin
  { 禁止更新 }
  BeginUpdate;
  try
    if not IsLoading then
    begin
      { 首先获得DockSite的客户区大小 }
      R := Docksite.ClientRect;
      { 只有当用手工调整DockSite大小的时候才调用下面的程序 }
      if ResizeCount > 0 then
      begin
        if TopZone.ChildZones <> nil then
        begin
          { 如果DockSite的客户区大小已经改变了，就计算出偏移量ShiftBy，然后就调用
            ForEachAt调整根节点的第一个子节点后的所有节点大小}
          if (DockSite.Align = alRight) and (R.Right <> OldRect.Right) then
          begin
            ShiftBy := - OldRect.Right + R.Right;
            ShiftScaleOrient := doVertical;
            ForEachAt(TopZone.ChildZones, ShiftZone, tskForward);
            SetNewBounds(nil);
          end;
          if (DockSite.Align = alBottom) and (R.Bottom <> OldRect.Bottom) then
          begin
            ShiftBy := - OldRect.Bottom + R.Bottom;
            ShiftScaleOrient := doHorizontal;
            ForEachAt(TopZone.ChildZones, ShiftZone, tskForward);
            SetNewBounds(nil);
          end;
        end;
      end;
      { 下面的一段代码对DockSite中的停靠控件进行比例缩放操作 }
      if (DockSiteOrient = doVertical) and (R.Bottom <> OldRect.Bottom) then
      begin
        { 垂直 }
        if OldRect.Bottom - OldRect.Top = 0 then
          ScaleBy := R.Bottom - R.Top
        else if OldRect.Bottom - OldRect.Top > 0 then
          ScaleBy := (R.Bottom - R.Top) / (OldRect.Bottom - OldRect.Top)
        else ScaleBy := 1;
        ShiftScaleOrient := doHorizontal;
        if ScaleBy <> 1 then
          ForEachAt(nil, ScaleZone, tskForward);
      end;
      if (DockSiteOrient = doHorizontal) and (R.Right <> OldRect.Right) then
      begin
        { 水平 }
        if OldRect.Right - OldRect.Left = 0 then
          ScaleBy := R.Right - R.Left
        else if OldRect.Right - OldRect.Left > 0 then
          ScaleBy := (R.Right - R.Left) / (OldRect.Right - OldRect.Left)
        else ScaleBy := 1;
        ShiftScaleOrient := doVertical;
        if ScaleBy <> 1 then
          ForEachAt(nil, ScaleZone, tskForward);
      end;
    end;
    inherited ResetBounds(Force);
  finally
    { 允许更新 }
    EndUpdate;
  end;
end;

procedure TCnVCDockTree.InsertNewParent(NewZone, SiblingZone: TCnDockZone;
  ParentOrientation: TDockOrientation; InsertLast, Update: Boolean);
begin
    inherited;
end;

procedure TCnVCDockTree.InsertSibling(NewZone, SiblingZone: TCnDockZone;
  InsertLast, Update: Boolean);
begin
    inherited;
end;

procedure TCnVCDockTree.RemoveZone(Zone: TCnDockZone; Hide: Boolean);
begin
  inherited;
end;

procedure TCnVCDockTree.ScaleZone(Zone: TCnDockZone);
begin
  if Zone <> nil then
    case TCnVCDockZone(Zone).ZoneSizeStyle of
      zssMinimum:
      begin
        Zone.ZoneLimit := Zone.LimitBegin + MinSize;
        Exit;
      end;
      zssMaximum:
      begin
        Zone.ZoneLimit := DockSiteSizeA - Zone.VisibleNextSiblingCount * MinSize;
        Exit;
      end;
    end;
  inherited ScaleZone(Zone);
end;

procedure TCnVCDockTree.RemoveControl(Control: TControl);
var DockRect: TRect;
  OldDockSize: Integer;
begin
  OldDockSize := DropDockSize;

  DockRect := GetFrameRect(Control);
  if DockSiteOrient = doHorizontal then
    { 水平 }
    DropDockSize := DockRect.Right - DockRect.Left
  else if DockSiteOrient = doVertical then
    { 垂直 }
    DropDockSize := DockRect.Bottom - DockRect.Top;

  inherited RemoveControl(Control);

  DropDockSize := OldDockSize;
end;

procedure TCnVCDockTree.ShiftZone(Zone: TCnDockZone);
begin
  inherited ShiftZone(Zone);
  if (Zone <> nil) and (Zone <> TopZone) and
    (Zone.ParentZone.Orientation = ShiftScaleOrient) then
  begin
    if Zone.LimitSize < MinSize then
      Zone.ZoneLimit := Zone.LimitBegin + MinSize;
  end;
end;

procedure TCnVCDockTree.ControlVisibilityChanged(Control: TControl;
  Visible: Boolean);
begin
  inherited ControlVisibilityChanged(Control, Visible);
end;

function TCnVCDockTree.GetLeftGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.X >= Zone.Left + BorderWidth) and (MousePos.X <= Zone.Left + BorderWidth + GrabberSize) and
    (MousePos.Y >= Zone.Top) and (MousePos.Y <= Zone.Top + Zone.Height) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Rect(
        Left - GrabberSize + LeftOffset,
        Top + TopOffset,
        Left - GrabberSize + LeftOffset + ButtonWidth,
        Top + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else if PtInRect(Rect(
        Left - GrabberSize + LeftOffset,
        Top + ButtonHeight + TopOffset + ButtonSplitter,
        Left - GrabberSize + LeftOffset + ButtonWidth,
        Top + 2*ButtonHeight + TopOffset + ButtonSplitter), MousePos) then
        HTFlag := HTEXPAND
      else HTFlag := HTCAPTION;
    end;
  end else Result := nil;
end;

function TCnVCDockTree.GetTopGrabbersHTFlag(const MousePos: TPoint;
  out HTFlag: Integer; Zone: TCnDockZone): TCnDockZone;
begin
  if (MousePos.Y >= Zone.Top + BorderWidth) and (MousePos.Y <= Zone.Top + BorderWidth + GrabberSize) and
    (MousePos.X >= Zone.Left) and (MousePos.X <= Zone.Left + Zone.Width) then
  begin
    Result := Zone;
    with Zone.ChildControl do
    begin
      if PtInRect(Rect(
        Left + Width - ButtonWidth - RightOffset,
        Top - GrabberSize + TopOffset,
        Left + Width - RightOffset,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTCLOSE
      else if PtInRect(Rect(
        Left + Width - 2*ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset,
        Left + Width - ButtonWidth - RightOffset - ButtonSplitter,
        Top - GrabberSize + TopOffset + ButtonHeight), MousePos) then
        HTFlag := HTEXPAND
      else HTFlag := HTCAPTION;
    end;
  end else Result := nil;
end;

function TCnVCDockTree.DoLButtonDown(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer): Boolean;
var TempZone: TCnVCDockZone;
  Active: Boolean;
begin
  Result := inherited DoLButtonDown(Message, Zone, HTFlag);
  if (Zone <> nil) and (HTFlag = HTEXPAND) then
  begin
    TempZone := TCnVCDockZone(Zone);
    Active := ((TempZone.ParentZone.Orientation <> DockSiteOrient) and
      (TempZone.ParentZone.VisibleChildCount >= 2));
    if Active then
    begin
      TempZone.ExpandBtnDown := True;
      TempZone.MouseDown := True;
      FExpandBtnZone := TempZone;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TCnVCDockTree.DoLButtonUp(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var TempZone: TCnVCDockZone;

begin
  inherited DoLButtonUp(Message, Zone, HTFlag);
  if (SizingZone = nil) and (FExpandBtnZone <> nil) then
  begin
    FExpandBtnZone := nil;
    if (Zone <> nil) and (HTFlag = HTEXPAND) then
    begin
      TempZone := TCnVCDockZone(Zone);
      TempZone.ExpandBtnDown := False;
      if TempZone.ZoneSizeStyle in [zssMaximum] then
      begin
        TCnVCDockZone(TempZone.ParentZone).DoSetChildSizeStyle(zssNormal);
      end else
      begin
        TCnVCDockZone(TempZone.ParentZone).DoSetChildSizeStyle(zssMinimum);
        TempZone.ZoneSizeStyle := zssMaximum;
      end;
      ResetZoneSizeStyle(TempZone.ParentZone, TempZone.ZoneSizeStyle, nil);
      DockSite.Invalidate;
    end;
  end;
end;

procedure TCnVCDockTree.DoMouseMove(var Message: TWMMouse;
  var Zone: TCnDockZone; out HTFlag: Integer);
var TempZone: TCnVCDockZone;
begin
  inherited DoMouseMove(Message, Zone, HTFlag);
  if SizingZone = nil then
  begin
    TempZone := TCnVCDockZone(Zone);
    if ((TempZone <> nil) and (TempZone.ExpandBtnDown <> (HTFlag = HTEXPAND))
      and ((FExpandBtnZone = TempZone) and FExpandBtnZone.MouseDown)) then
    begin
      TempZone.ExpandBtnDown := (HTFlag = HTEXPAND) and FExpandBtnZone.MouseDown;
      DockSite.Invalidate;
    end;
  end;
end;

procedure TCnVCDockTree.ResetZoneSizeStyle(Parent: TCnDockZone;
  ZoneSizeStyle: TZoneSizeStyle; Exclude: TCnDockZone);
var AZone: TCnVCDockZone;
  ChildCount: Integer;
  AverageSize: Integer;
begin
  ChildCount := Parent.VisibleChildCount - Integer((Exclude <> nil) and (Exclude.ParentZone = Parent));
  AverageSize := DockSiteSizeA div ChildCount;
  Assert(AverageSize > 0);
  AZone := TCnVCDockZone(Parent.FirstVisibleChildZone);
  while AZone <> nil do
  begin
    if Exclude <> AZone then
    begin
      Dec(ChildCount);
      if ZoneSizeStyle in [zssMaximum] then
      begin
        if AZone.ZoneSizeStyle = zssMinimum then
          AZone.ZoneLimit := AZone.LimitBegin + MinSize
        else if AZone.ZoneSizeStyle = zssMaximum then
        begin
          AZone.ZoneLimit := DockSiteSizeA - ChildCount * MinSize;
        end;
      end else if ZoneSizeStyle in [zssNormal] then
        AZone.ZoneLimit := AZone.LimitBegin + AverageSize;
    end else if {(ZoneSizeStyle in [zssMaximum]) and }(Exclude <> nil) then
      Exclude.ZoneLimit := Exclude.LimitBegin;

    AZone := TCnVCDockZone(AZone.AfterClosestVisibleZone);
  end;
  SetNewBounds(Parent);
  ForEachAt(Parent, UpdateZone, tskForward);
end;

procedure TCnVCDockTree.CalcSplitterPos;
var
  TestLimit: Integer;
  TempPos: TPoint;
begin
  TempPos := SizePos;
  if (SizingZone.ParentZone.Orientation = doHorizontal) then
  begin
    TestLimit := SizingZone.Top + MinSize;
    if (TempPos.y <= TestLimit) then
    begin
      if (DockSiteOrient = doVertical) then
      begin
        if TempPos.y <= (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2 then
          TempPos.y := (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2;
      end else
        TempPos.y := TestLimit;
    end;

    TestLimit := GetSplitterLimit(SizingZone, False, True) - MinSize;
    if (TempPos.y >= TestLimit) then
    begin
      if (DockSiteOrient = doVertical) then
      begin
        if TempPos.y >= DockSiteSizeA - SizingZone.VisibleNextSiblingCount * MinSize then
          TempPos.y := DockSiteSizeA - SizingZone.VisibleNextSiblingCount * MinSize;
      end else
        TempPos.y := TestLimit;
    end;
  end
  else begin
    TestLimit := SizingZone.Left + MinSize;
    if (TempPos.x <= TestLimit) then
    begin
      if (DockSiteOrient = doHorizontal) then
      begin
        if TempPos.x <= (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2 then
          TempPos.x := (SizingZone.VisiblePrevSiblingCount + 1) * MinSize - SplitterWidth div 2;
      end else
        TempPos.x := TestLimit;
    end;

    TestLimit := GetSplitterLimit(SizingZone, False, True) - MinSize;
    if (TempPos.x >= TestLimit) then
    begin
      if (DockSiteOrient = doHorizontal) then
      begin
        if TempPos.x >= DockSiteSizeA - SizingZone.VisibleNextSiblingCount * MinSize then
          TempPos.x := DockSiteSizeA - SizingZone.VisibleNextSiblingCount * MinSize;
      end else
        TempPos.x := TestLimit;
    end;
  end;
  SizePos := TempPos;

end;

procedure TCnVCDockTree.CustomLoadZone(Stream: TStream;
  var Zone: TCnDockZone);
begin
  Stream.Read(TCnVCDockZone(Zone).FZoneSizeStyle, sizeof(TZoneSizeStyle));
  inherited CustomLoadZone(Stream, Zone);
end;

procedure TCnVCDockTree.CustomSaveZone(Stream: TStream; Zone: TCnDockZone);
begin
  Stream.Write(TCnVCDockZone(Zone).FZoneSizeStyle, sizeof(TZoneSizeStyle));
  inherited CustomSaveZone(Stream, Zone);
end;

procedure TCnVCDockTree.ScaleChildZone(Zone: TCnDockZone);
begin
  if Zone <> nil then
    case TCnVCDockZone(Zone).ZoneSizeStyle of
      zssMinimum:
      begin
        Zone.ZoneLimit := Zone.LimitBegin + MinSize;
        Exit;
      end;
      zssMaximum:
      begin
        Zone.ZoneLimit := DockSiteSizeA - Zone.VisibleNextSiblingCount * MinSize;
        Exit;
      end;
    end;
  inherited ScaleChildZone(Zone);

  if (Zone <> nil) and (Zone.ParentZone <> nil) and Zone.Visibled and
    (Zone.ParentZone.Orientation = ShiftScaleOrient) then
  begin
    // Zone的LimitSize不能小于规定的最小尺寸MinSize。
    if (Zone.LimitSize < MinSize) then
      Zone.ZoneLimit := Zone.LimitBegin + MinSize;
    // Zone的LimitBegin和前一个可见的节点的ZoneLimit的位置不能小于规定的最小尺寸MinSize
    if (Zone.BeforeClosestVisibleZone <> nil) and (Zone.LimitBegin > DockSiteSizeWithOrient[Zone.ParentZone.Orientation] -
      (Zone.VisibleNextSiblingCount + 1) * MinSize + SplitterWidth div 2) then
      Zone.BeforeClosestVisibleZone.ZoneLimit := DockSiteSizeWithOrient[Zone.ParentZone.Orientation] -
        (Zone.VisibleNextSiblingCount + 1) * MinSize + SplitterWidth div 2;
  end;
end;

procedure TCnVCDockTree.ScaleSiblingZone(Zone: TCnDockZone);
begin
  inherited ScaleSiblingZone(Zone);
end;

procedure TCnVCDockTree.DoOtherHint(Zone: TCnDockZone;
  HTFlag: Integer; var HintStr: string);
begin
  inherited DoOtherHint(Zone, HTFlag, HintStr);
  if HTFlag = HTEXPAND then
    HintStr := gs_CnVCDockTreeExpandBtnHint;
end;

function TCnVCDockTree.GetDockAlign(Client: TControl; var DropCtl: TControl): TAlign;
begin
  Result := inherited GetDockAlign(Client, DropCtl);
  case DockSite.Align of
    alLeft, alRight:
    begin
      if (Result in [alLeft, alRight]) and (DropCtl <> nil) then
        DropCtl := nil;
    end;
    alTop, alBottom:
    begin
      if (Result in [alTop, alBottom]) and (DropCtl <> nil) then
        DropCtl := nil;
    end;
  end;
end;

procedure TCnVCDockTree.GetCaptionRect(var Rect: TRect);
begin
  //inherited;
  case GrabbersPosition of
    gpTop:
      Rect.Bottom := Rect.Top + GrabberSize + 2;
    gpLeft:
      Rect.Right := Rect.Left + GrabberSize + 2;
  end;
end;

{ TCnVCDockZone }

constructor TCnVCDockZone.Create(Tree: TCnDockTree);
begin
  inherited;
  { 默认情况是正常状态 }
  FZoneSizeStyle := zssNormal;

  FExpandBtnDown := False;
end;

destructor TCnVCDockZone.Destroy;
begin
  inherited Destroy;
end;

procedure TCnVCDockZone.DoSetChildSizeStyle(ZoneSizeStyle: TZoneSizeStyle);
var AZone: TCnVCDockZone;
begin
  AZone := TCnVCDockZone(ChildZones);
  while AZone <> nil do
  begin
    AZone.ZoneSizeStyle := ZoneSizeStyle;
    AZone := TCnVCDockZone(AZone.AfterClosestVisibleZone);
  end;
end;

procedure TCnVCDockZone.Insert(DockSize: Integer; Hide: Boolean);
var PrevShift,
    NextShift: Integer;
    TempSize: Integer;
    BorderSize: Integer;
    BeforeVisibleZone,
    AfterVisibleZone: TCnDockZone;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) then
    ParentZone.Insert(ParentZone.VisibleSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := True;
    Exit;
  end;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone  := AfterClosestVisibleZone;

  BorderSize := TCnVCDockTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone = nil);// div 2;

  if ParentZone.Orientation <> TCnVCDockTree(Tree).DockSiteOrient then
  begin
    if ((BeforeVisibleZone <> nil) and (TCnVCDockZone(BeforeVisibleZone).ZoneSizeStyle in [zssMaximum, zssMinimum])) or
      ((AfterVisibleZone <> nil) and (TCnVCDockZone(AfterVisibleZone).ZoneSizeStyle in [zssMaximum, zssMinimum])) then
    begin
      { 如果ParentZone中有一个停靠客户是最大化状态,就调用ResetZoneSizeStyle函数重新设置ZoneSizeStyle }
      ZoneSizeStyle := zssMinimum;
      TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssMaximum, nil);
      Visibled := True;
      Exit;
    end;
    case TCnVCDockTree(Tree).DockSiteOrient of
      doVertical:   TempSize := Tree.DockSite.Height;
      doHorizontal: TempSize := Tree.DockSite.Width;
    else
      raise Exception.Create('');
    end;

    if DockSize >= TempSize - (ParentZone.VisibleChildCount) * TCnVCDockTree(Tree).MinSize then
      DockSize := (TempSize - (ParentZone.VisibleChildCount) * TCnVCDockTree(Tree).MinSize) div 2;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    Visibled := False;

    if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
    begin
      PrevShift := 0;
      NextShift := 0;
    end else
    if BeforeVisibleZone = nil then
    begin
      { 要插入的节点是在父节点的最前面 }
      PrevShift := 0;
      NextShift := DockSize + BorderSize;
      ZoneLimit := DockSize + BorderSize;
      if ParentZone.VisibleChildCount = 1 then
        AfterVisibleZone.ZoneLimit := TempSize;
    end else if AfterVisibleZone = nil then
    begin
      { 要插入的节点是在父节点的最后面 }
      PrevShift := DockSize + BorderSize;
      NextShift := 0;
      if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
        BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
      else
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := TempSize;
    end else
    begin
      { 要插入的节点是在父节点的中间 }
      PrevShift := Round((BeforeVisibleZone.ZoneLimit) * (DockSize + BorderSize) / TempSize);
      NextShift := DockSize - PrevShift;
      if (ParentZone.VisibleChildCount = 1) and (ParentZone = Tree.TopZone) then
        BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit - PrevShift
      else
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize;
    end;

    Visibled := True;
    { 如果新节点有上一个兄弟节点 }
    if PrevShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        { 当遍历到新节点的上一个兄弟节点的时候，就停止遍历 }
        ReplacementZone := BeforeVisibleZone;
        try
          if BeforeVisibleZone.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (BeforeVisibleZone.ZoneLimit + PrevShift)
          else ScaleBy := 1;
          ShiftScaleOrient := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;
      { 对PrevSibling的ZoneLimit进行调整 }
      if BeforeVisibleZone.LimitSize < TCnVCDockTree(Tree).MinSize then
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;


    { 如果新节点有下一个兄弟节点 }
    if NextShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrient := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
      end;
      if AfterVisibleZone.LimitSize < TCnVCDockTree(Tree).MinSize then
        AfterVisibleZone.ZoneLimit := AfterVisibleZone.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;
  end else
  begin
    with TCnVCDockTree(Tree) do
    begin

      TempSize := DockHeightWidth[DockSiteOrient] - BorderSize;

      { 如果兄弟节点是nil，就要重新调整DockSite的宽度和高度 }
      if (BeforeVisibleZone <> nil) then
      begin
        if (Tree.TopZone.VisibleChildCount = 2) and Visibled then
          BeforeVisibleZone.ZoneLimit := Tree.TopXYLimit + BorderSize;
        if Visibled then
          ZoneLimit := BeforeVisibleZone.ZoneLimit + TempSize
        else
          ZoneLimit := BeforeVisibleZone.ZoneLimit + DockSize + BorderSize;

        TempSize := ZoneLimit;
      end;

      if (AfterVisibleZone <> nil) then
      begin
        { 新的节点的ZoneLimit就是她上面的控件的LRDockWidth加上BorderSize }
        if Visibled then
          ZoneLimit := LimitBegin + TempSize
        else
          ZoneLimit := LimitBegin + DockSize - BorderSize;
        { 需要调整的偏移量就是新的节点的ZoneLimit }
        ShiftBy := ZoneLimit;
        { 设置偏移方向为垂直的 }
        ShiftScaleOrient := DockSiteOrient;
        { 开始偏移量调整 }
        ForEachAt(AfterVisibleZone, ShiftZone, tskForward);
        { 重新给DockSite的宽度或者高度赋值 }
        TempSize := DockSiteSize + ZoneLimit - LimitBegin;
      end;
      Visibled := True;

      DockSiteSize := TempSize;
      { 调用GetClientAlignControl触发DockServer的GetClientAlignSize事件 }
      TCnDockPanel(DockSite).DockServer.GetClientAlignControl(DockSite.Align);
    end;
  end;
  Visibled := True;
end;

procedure TCnVCDockZone.Remove(DockSize: Integer; Hide: Boolean);
var PrevShift,
    NextShift: Integer;
    TempSize: Integer;
    BorderSize: Integer;
    BeforeVisibleZone,
    AfterVisibleZone: TCnDockZone;
begin
  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 1) and (ParentZone <> Tree.TopZone) then
    ParentZone.Remove(ParentZone.LimitSize, Hide);

  if (ParentZone = nil) or ((ParentZone = Tree.TopZone) and (ParentZone.ChildCount <= 1)) then
  begin
    Visibled := False;
    Exit;
  end;

  BeforeVisibleZone := BeforeClosestVisibleZone;
  AfterVisibleZone  := AfterClosestVisibleZone;

  BorderSize := TCnVCDockTree(Tree).BorderWidth * Integer(AfterClosestVisibleZone = nil);// div 2;

  if ParentZone.Orientation <> TCnVCDockTree(Tree).DockSiteOrient then
  begin
    if (ZoneSizeStyle in [zssMaximum, zssMinimum]) then
    begin
      if ZoneSizeStyle = zssMinimum then
        TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssMaximum, self)
      else if ZoneSizeStyle = zssMaximum then
      begin
        TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssNormal, self);
        TCnVCDockZone(ParentZone).DoSetChildSizeStyle(zssNormal);
      end;
      Visibled := False;
      Exit;
    end;

    case TCnVCDockTree(Tree).DockSiteOrient of
      doVertical:   TempSize := Tree.DockSite.Height;
      doHorizontal: TempSize := Tree.DockSite.Width;
    else
      raise Exception.Create('');
    end;

    if DockSize > TempSize - (ParentZone.VisibleChildCount-1) * TCnVCDockTree(Tree).MinSize then
      DockSize := TempSize - (ParentZone.VisibleChildCount-1) * TCnVCDockTree(Tree).MinSize;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderSize;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    Visibled := False;

    if (BeforeVisibleZone = nil) and (AfterVisibleZone = nil) then
      Exit;

    if BeforeVisibleZone = nil then
    begin
      { 要插入的节点是在父节点的最前面 }
      PrevShift := 0;
      NextShift := -DockSize + BorderSize;
      ZoneLimit := -DockSize + BorderSize;
    end else if AfterVisibleZone = nil then
    begin
      { 要插入的节点是在父节点的最后面 }
      PrevShift := -DockSize + BorderSize;
      NextShift := 0;
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := TempSize;
    end else
    begin
      { 要插入的节点是在父节点的中间 }
      PrevShift := -Round((BeforeVisibleZone.ZoneLimit) * (DockSize + BorderSize) / (TempSize - (DockSize + BorderSize)));
      NextShift := -DockSize - PrevShift;
      BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.ZoneLimit - PrevShift;
      ZoneLimit := BeforeVisibleZone.ZoneLimit;
    end;

    { 如果新节点有上一个兄弟节点 }
    if PrevShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        { 当遍历到新节点的上一个兄弟节点的时候，就停止遍历 }
        ReplacementZone := BeforeVisibleZone;
        try
          if BeforeVisibleZone.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (BeforeVisibleZone.ZoneLimit + PrevShift)
          else ScaleBy := 1;
          ShiftScaleOrient := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;
      { 对PrevSibling的ZoneLimit进行调整 }
      if BeforeVisibleZone.LimitSize < TCnVCDockTree(Tree).MinSize then
        BeforeVisibleZone.ZoneLimit := BeforeVisibleZone.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;


    { 如果新节点有下一个兄弟节点 }
    if NextShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrient := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(AfterVisibleZone, ScaleSiblingZone, tskForward);
      end;
      if AfterVisibleZone.LimitSize < TCnVCDockTree(Tree).MinSize then
        AfterVisibleZone.ZoneLimit := AfterVisibleZone.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;
  end else
  begin
    Visibled := False;
    with TCnVCDockTree(Tree) do
    begin
      { 如果父节点的停靠方向和DockSite的停靠方向相同的话，就进行相应的比例缩放 }
      ZoneLimit := LimitBegin - BorderSize;
      ShiftBy := -DockSize - BorderSize;
      ShiftScaleOrient := DockSiteOrient;
      { 如果Zone的前一个兄弟存在，就从Zone的前一个兄弟开始调整 }
      if BeforeClosestVisibleZone <> nil then
        DockSiteSize := DockSiteSize - DockSize - BorderSize
      { 否者，如果Zone的后一个兄弟存在，就从Zone的后一个兄弟开始调整 }
      else if AfterClosestVisibleZone <> nil then
      begin
        ForEachAt(AfterClosestVisibleZone, ShiftZone, tskForward);
        { 下面开始调整DockSite的宽度和高度 }
        DockSiteSize := DockSiteSize - DockSize - BorderSize;
      end;
    end;
  end;
end;

procedure TCnVCDockZone.InsertOrRemove(DockSize: Integer; Insert: Boolean; Hide: Boolean);
var PrevShift,
    NextShift: Integer;
    TempSize: Integer;
    BorderWidth: Integer;
begin
  if not Insert then
    Visibled := False;

  if (ParentZone <> nil) and (ParentZone.VisibleChildCount = 0) and (ParentZone <> Tree.TopZone) then
  begin
    if Insert then
      TempSize := ParentZone.VisibleSize
    else
      TempSize := ParentZone.LimitSize;

    ParentZone.InsertOrRemove(TempSize, Insert, Hide);
  end;
  if ParentZone = nil then Exit;

  if ParentZone.Orientation <> TCnVCDockTree(Tree).DockSiteOrient then
  begin
    if (TCnVCDockZone(ParentZone.ChildZones).ZoneSizeStyle in [zssMaximum, zssMinimum]) then//or
    begin
      { 如果ParentZone中有一个停靠客户是最大化状态,就调用ResetZoneSizeStyle函数重新设置ZoneSizeStyle }
      if Insert then
      begin
        ZoneSizeStyle := zssMinimum;
        TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssMaximum, nil);
      end
      else
      begin
        if ZoneSizeStyle = zssMinimum then
          TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssMaximum, self)
        else if ZoneSizeStyle = zssMaximum then
        begin
          TCnVCDockTree(Tree).ResetZoneSizeStyle(ParentZone, zssNormal, self);
          TCnVCDockZone(ParentZone).DoSetChildSizeStyle(zssNormal);
        end;
      end;
      Exit;
    end;

    case TCnVCDockTree(Tree).DockSiteOrient of
      doVertical:   TempSize := Tree.DockSite.Height;
      doHorizontal: TempSize := Tree.DockSite.Width;
    else
      raise Exception.Create('');
    end;

    if DockSize > TempSize - (ParentZone.VisibleChildCount-1) * TCnVCDockTree(Tree).MinSize then
      DockSize := TempSize - (ParentZone.VisibleChildCount-1) * TCnVCDockTree(Tree).MinSize;

    BorderWidth := TCnVCDockTree(Tree).BorderWidth;

    TempSize := ParentZone.HeightWidth[ParentZone.Orientation] + BorderWidth;

    if DockSize = 0 then
      DockSize := TempSize div 2;

    if BeforeClosestVisibleZone = nil then
    begin
      { 要插入的节点是在父节点的最前面 }
      PrevShift := 0;
      NextShift := (2 * Integer(Insert) - 1) * (DockSize + BorderWidth);

      ZoneLimit := Integer(Insert) * (DockSize + BorderWidth);
      if ParentZone.VisibleChildCount = 2 then
        NextSibling.ZoneLimit := TempSize;
    end else if AfterClosestVisibleZone = nil then
    begin
      { 要插入的节点是在父节点的最后面 }
      PrevShift := (2 * Integer(Insert) - 1) * (DockSize + BorderWidth);
      NextShift := 0;
      begin
        if ParentZone.ChildCount = 2 then
          PrevSibling.ZoneLimit := TempSize - Integer(Insert) * PrevShift
        else
          PrevSibling.ZoneLimit := PrevSibling.ZoneLimit - PrevShift;
      end;
      ZoneLimit := TempSize;

    end else
    begin
      { 要插入的节点是在父节点的中间 }
      PrevShift := (2 * Integer(Insert) - 1) * Round((PrevSibling.ZoneLimit) * (DockSize + BorderWidth) / (TempSize - Integer(not Insert) * (DockSize + BorderWidth)));
      NextShift := (2 * Integer(Insert) - 1) * DockSize - PrevShift;
      PrevSibling.ZoneLimit := PrevSibling.ZoneLimit - PrevShift;
      ZoneLimit := Integer(Insert) * (DockSize + BorderWidth) + PrevSibling.ZoneLimit;
    end;

    { 如果新节点有上一个兄弟节点 }
    if PrevShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        { 当遍历到新节点的上一个兄弟节点的时候，就停止遍历 }
        ReplacementZone := PrevSibling;
        try
          if PrevSibling.ZoneLimit + PrevShift <> 0 then
            ScaleBy := PrevSibling.ZoneLimit / (PrevSibling.ZoneLimit + PrevShift)
          else ScaleBy := 1;
          ShiftScaleOrient := ParentZone.Orientation;
          if ScaleBy <> 1 then
            ForEachAt(ParentZone.ChildZones, ScaleZone, tskForward);
        finally
          ReplacementZone := nil;
        end;
      end;
      { 对PrevSibling的ZoneLimit进行调整 }
      if PrevSibling.LimitSize < TCnVCDockTree(Tree).MinSize then
        PrevSibling.ZoneLimit := PrevSibling.LimitBegin + TCnVCDockTree(Tree).MinSize;
    end;


    { 如果新节点有下一个兄弟节点 }
    if NextShift <> 0 then
    begin
      with TCnVCDockTree(Tree) do
      begin
        if TempSize - ZoneLimit + NextShift <> 0 then
          ScaleBy := (TempSize - ZoneLimit) / (TempSize - ZoneLimit + NextShift)
        else ScaleBy := 1;
        ParentLimit := TempSize;
        ShiftScaleOrient := ParentZone.Orientation;
        if ScaleBy <> 1 then
          ForEachAt(NextSibling, ScaleSiblingZone, tskForward);
      end;
    end;

    ParentZone.Update;

  end else
  begin
    with TCnVCDockTree(Tree) do
    begin
      { 如果是插入操作 }
      if Insert then
      begin
        { 如果兄弟节点是nil，就要重新调整DockSite的宽度和高度 }
        if AfterClosestVisibleZone = nil then
        begin
          ZoneLimit := LimitBegin + DockHeightWidth[DockSiteOrient];// + DockSize;
          DockSiteSize := ZoneLimit;
        end else if BeforeClosestVisibleZone = nil then
        begin
          { 新的节点的ZoneLimit就是她上面的控件的LRDockWidth加上BorderWidth }
          ZoneLimit := DockHeightWidth[DockSiteOrient] + BorderWidth;// + DockSize;
          { 需要调整的偏移量就是新的节点的ZoneLimit }
          ShiftBy := ZoneLimit;
          { 设置偏移方向为垂直的 }
          ShiftScaleOrient := DockSiteOrient;
          { 开始偏移量调整 }
          ForEachAt(AfterClosestVisibleZone, ShiftZone, tskForward);
          { 重新给DockSite的宽度或者高度赋值 }
          DockSiteSize := DockSiteSize + ZoneLimit;
        end;
        { 调用GetClientAlignControl触发DockServer的GetClientAlignSize事件 }
        TCnDockPanel(DockSite).DockServer.GetClientAlignControl(DockSite.Align);
      end else
      { 否则就是删除操作 }
      begin
        { 如果父节点的停靠方向和DockSite的停靠方向相同的话，就进行相应的比例缩放 }
        ZoneLimit := LimitBegin;
        ShiftBy := -DockSize;
        ShiftScaleOrient := DockSiteOrient;
        { 如果Zone的前一个兄弟存在，就从Zone的前一个兄弟开始调整 }
        if PrevSibling <> nil then
          DockSiteSize := DockSiteSize - DockSize - 5
        { 否者，如果Zone的后一个兄弟存在，就从Zone的后一个兄弟开始调整 }
        else if NextSibling <> nil then
        begin
          ForEachAt(NextSibling, ShiftZone, tskForward);
          { 下面开始调整DockSite的宽度和高度 }
          DockSiteSize := DockSiteSize - DockSize;
        end;
      end;
    end;
  end;
  if Insert then
    Visibled := True;
end;

procedure TCnVCDockZone.SetZoneSize(Size: Integer; Show: Boolean);
begin
  inherited SetZoneSize(Size, Show);
end;

{ TCnVCDragDockObject }

constructor TCnVCDragDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FrameWidth := DefaultFrameWidth;

  FDockOverFrameWidth := DefaultDockOverFrameWidth;

  FDockOverBrush := TBrush.Create;
  SetDefaultBrushStyle;

  CurrState := dsDragEnter;
  OldState := CurrState;
end;

destructor TCnVCDragDockObject.Destroy;
begin
  FDockOverBrush.Free;
  inherited;
end;

function TCnVCDragDockObject.DragFindWindow(const Pos: TPoint): HWND;
begin
  Result := 0;
end;

procedure TCnVCDragDockObject.GetBrush_PenSize_DrawRect(
      var ABrush: TBrush; var PenSize: Integer; var DrawRect: TRect; Erase: Boolean);

var  DockOver: Boolean;

  { 获得画布的句柄和框架宽度 }
  procedure GetBrushAndFrameWidth;
  begin
    if DockOver then
    begin
      PenSize := FDockOverFrameWidth;
      ABrush := FDockOverBrush;
    end else
    begin
      PenSize := FrameWidth;
      ABrush := Brush;
    end;
  end;

begin
              { DockOver开始的时候 }                                                   { 当直接把Control拖动到临近的停靠服务器的时候 }
  DockOver := ((OldState = dsDragEnter) and (CurrState = dsDragMove) and (not Erase or (OldTarget <> nil)))
              { DockOver途中的时候 }
              or ((OldState = dsDragMove) and (CurrState = dsDragMove))
              { DockOver结束的时候 }
              or ((OldState = dsDragMove) and (CurrState = dsDragLeave) and Erase);
              { Control本身就已经是停靠的 }

  GetBrushAndFrameWidth;

  if (OldState = dsDragMove) and (CurrState = dsDragLeave) then
  begin
    { 如果DockOver结束的话，就设置OldState为dsDragEnter，使上面的条件不成立 }
    OldState := dsDragEnter;
    OldTarget := nil;
  end else OldTarget := DragTarget;

  if Erase then DrawRect := EraseDockRect
  else DrawRect := DockRect;

end;

function TCnVCDragDockObject.GetDropCtl: TControl;
begin
  Result := DropOnControl;
end;

procedure TCnVCDragDockObject.SetCurrState(const Value: TDragState);
begin
  FCurrState := Value;
end;

procedure TCnVCDragDockObject.SetDefaultBrushStyle;
begin
  FDockOverBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  FDockOverBrush.Style := bsSolid;
end;

procedure TCnVCDragDockObject.SetOldState(const Value: TDragState);
begin
  FOldState := Value;
end;

{ TCnVCDockSplitter }

constructor TCnVCDockSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldSize := MinSize;
end;

function TCnVCDockSplitter.DoCanResize(var NewSize: Integer): Boolean;
var DockPanel: TCnDockPanel;
  Limit, MinSize: Integer;
begin
  Result := inherited DoCanResize(NewSize);
  if (Result) and (FOldSize > NewSize) then
  begin
    DockPanel := DockServer.DockPanel[Integer(Align) - 1];
    Limit := DockPanel.CnDockManager.GetDockClientLimit(Cn_GetControlOrient(DockPanel),
      Align in [alLeft, alTop]);
    MinSize := DockPanel.CnDockManager.MinSize;

    if DockPanel.Align in [alLeft, alTop] then
    begin
      if NewSize < Limit + MinSize then
        Result := False;
    end else
    begin
      if NewSize < Cn_GetControlSize(DockPanel) - Limit + MinSize then
        Result := False;
    end;
  end;
  if Result then
    FOldSize := NewSize;
end;

procedure TCnVCDockSplitter.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var DockPanel: TCnDockPanel;
begin
  DockPanel := DockServer.DockPanel[Integer(Align) - 1];
  DockPanel.CnDockManager.BeginResizeDockSite;
  try
    inherited MouseUp(Button, Shift, X, Y);
  finally
    DockPanel.CnDockManager.EndResizeDockSite;
  end;
end;

procedure TCnVCDockSplitter.Paint;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  Inc(Rect.Right, 2);
  case Align of
    alLeft:
    begin
      InflateRect(Rect, 0, 2);
    end;
    alRight:
    begin
      OffsetRect(Rect, -1, 0);
      InflateRect(Rect, 0, 2);
    end;
    alTop:
    begin
      Inc(Rect.Bottom, 2);
      InflateRect(Rect, 2, 0);
    end;
    alBottom:
    begin
      Dec(Rect.Top, 2);
      InflateRect(Rect, 2, 1);
    end;
  end;
  Canvas.Brush.Color := Color;
  DrawFrameControl(Canvas.Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_ADJUSTRECT);
end;

{ TCnVCConjoinServerOption }

procedure TCnVCConjoinServerOption.Assign(Source: TPersistent);
begin
  if Source is TCnVCConjoinServerOption then
  begin
    FBorderWidth := TCnVCConjoinServerOption(Source).BorderWidth;
  end;
  inherited;
end;

constructor TCnVCConjoinServerOption.Create(ADockStyle: TCnBasicDockStyle);
begin
  inherited;
  BorderWidth := 4;
end;

destructor TCnVCConjoinServerOption.Destroy;
begin
  inherited;

end;

procedure TCnVCConjoinServerOption.ResetDockControlOption;
begin
  inherited;

end;

procedure TCnVCConjoinServerOption.SetBorderWidth(const Value: Integer);
begin
  FBorderWidth := Value;
end;

end.

