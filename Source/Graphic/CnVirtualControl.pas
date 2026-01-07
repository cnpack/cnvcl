{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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

unit CnVirtualControl;

interface

{$I CnPack.inc}

uses
  Windows, Messages, Classes, Controls, Graphics, SysUtils, ExtCtrls;

type
  TCnVirtualPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; Index:
    Integer; const R: TRect; Selected: Boolean) of object;

  TCnVirtualClickEvent = procedure(Sender: TObject; Index: Integer) of object;

  TCnVirtualBuildEvent = procedure(Sender: TObject; Container: TWinControl) of object;

  TCnVirtualBindItemEvent = procedure(Sender: TObject; Index: Integer; ItemType:
    Integer; Container: TWinControl; Selected: Boolean) of object;

  TCnVirtualActionEvent = procedure(Sender: TObject; Index: Integer) of object;

  TCnVirtualMeasureEvent = procedure(Sender: TObject; Index: Integer; var Height:
    Integer) of object;

  TCnVirtualGetTypeEvent = procedure(Sender: TObject; Index: Integer; var
    ItemType: Integer) of object;

  TCnVirtualBuildTemplateEvent = procedure(Sender: TObject; ItemType: Integer;
    Container: TWinControl) of object;

  TCnVirtualItemHost = class(TPanel)
  private
    FIndex: Integer;
    FOwnerCtrl: TComponent;
    FTypeId: Integer;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  public
    property Index: Integer read FIndex write FIndex;
    property OwnerCtrl: TComponent read FOwnerCtrl write FOwnerCtrl;
    property TypeId: Integer read FTypeId write FTypeId;
  end;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnVirtualControlList = class(TCustomControl)
  private
    FItemCount: Integer;
    FItemHeight: Integer;
    FItemSpacing: Integer;
    FScrollPos: Integer;
    FSelectedIndex: Integer;
    FOnItemPaint: TCnVirtualPaintEvent;
    FOnItemClick: TCnVirtualClickEvent;
    FUseItemControls: Boolean;
    FOnBuildItemControls: TCnVirtualBuildEvent;
    FOnBindItem: TCnVirtualBindItemEvent;
    FOnItemAction: TCnVirtualActionEvent;
    FOnMeasureItem: TCnVirtualMeasureEvent;
    FOnGetItemType: TCnVirtualGetTypeEvent;
    FOnBuildItemTemplate: TCnVirtualBuildTemplateEvent;
    FHosts: TList;
    FBufferItems: Integer;
    FHeights: array of Integer;
    FPrefix: array of Integer;
    FTotalHeight: Integer;
    procedure SetItemCount(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure SetItemSpacing(Value: Integer);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetScrollPos(Value: Integer);
    function RowPixels: Integer;
    function MaxScroll: Integer;
    procedure UpdateScrollRange;
    function IndexAt(Y: Integer): Integer;
    function ItemRectAt(Index: Integer; OffsetY: Integer): TRect;
    procedure EnsureHosts;
    procedure ClearHosts;
    procedure LayoutHosts;
    procedure RebuildHosts;
    procedure MeasureAll;
    procedure BuildPrefix;
    function HeightOfIndex(Index: Integer): Integer;
    function TopOfIndex(Index: Integer): Integer;
    function BottomOfIndex(Index: Integer): Integer;
    function FindStartIndex(PosY: Integer): Integer;
    function FindIndexByPos(PosY: Integer): Integer;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnsureVisible(Index: Integer);
    procedure ItemClicked(Index: Integer);
    procedure ItemAction(Index: Integer);
  published
    property Align;
    property Anchors;
    property Color;
    property Font;
    property TabStop;
    property Enabled;
    property Visible;
    property ItemCount: Integer read FItemCount write SetItemCount default 0;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 24;
    property ItemSpacing: Integer read FItemSpacing write SetItemSpacing default 0;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default -1;
    property OnItemPaint: TCnVirtualPaintEvent read FOnItemPaint write FOnItemPaint;
    property OnItemClick: TCnVirtualClickEvent read FOnItemClick write FOnItemClick;
    property UseItemControls: Boolean read FUseItemControls write
      FUseItemControls default False;
    property OnBuildItemControls: TCnVirtualBuildEvent read FOnBuildItemControls
      write FOnBuildItemControls;
    property OnBindItem: TCnVirtualBindItemEvent read FOnBindItem write FOnBindItem;
    property OnItemAction: TCnVirtualActionEvent read FOnItemAction write FOnItemAction;
    property OnMeasureItem: TCnVirtualMeasureEvent read FOnMeasureItem write
      FOnMeasureItem;
    property OnGetItemType: TCnVirtualGetTypeEvent read FOnGetItemType write
      FOnGetItemType;
    property OnBuildItemTemplate: TCnVirtualBuildTemplateEvent read
      FOnBuildItemTemplate write FOnBuildItemTemplate;
  end;

implementation

procedure TCnVirtualItemHost.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (OwnerCtrl is TCnVirtualControlList) then
    TCnVirtualControlList(OwnerCtrl).ItemClicked(Index);
end;

constructor TCnVirtualControlList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 150;
  DoubleBuffered := True;
  ControlStyle := ControlStyle + [csOpaque];
  Color := clWindow;
  TabStop := True;
  FItemCount := 0;
  FItemHeight := 24;
  FItemSpacing := 0;
  FScrollPos := 0;
  FSelectedIndex := -1;
  FUseItemControls := False;
  FHosts := TList.Create;
  FBufferItems := 2;
  SetLength(FHeights, 0);
  SetLength(FPrefix, 0);
  FTotalHeight := 0;
end;

destructor TCnVirtualControlList.Destroy;
begin
  ClearHosts;
  FHosts.Free;
  inherited Destroy;
end;

procedure TCnVirtualControlList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_VSCROLL;
end;

function TCnVirtualControlList.RowPixels: Integer;
begin
  Result := FItemHeight + FItemSpacing;
end;

procedure TCnVirtualControlList.MeasureAll;
var
  I, H: Integer;
begin
  SetLength(FHeights, FItemCount);

  for I := 0 to FItemCount - 1 do
  begin
    H := FItemHeight;
    if Assigned(FOnMeasureItem) then
      FOnMeasureItem(Self, I, H);
    FHeights[I] := H;
  end;

  BuildPrefix;
end;

procedure TCnVirtualControlList.BuildPrefix;
var
  I: Integer;
begin
  SetLength(FPrefix, FItemCount);
  FTotalHeight := 0;

  for I := 0 to FItemCount - 1 do
  begin
    if I = 0 then
      FPrefix[I] := FHeights[I]
    else
      FPrefix[I] := FPrefix[I - 1] + FHeights[I];
  end;

  if FItemCount > 0 then
    FTotalHeight := FPrefix[FItemCount - 1] + FItemCount * FItemSpacing
  else
    FTotalHeight := 0;
end;

function TCnVirtualControlList.HeightOfIndex(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < Length(FHeights)) then
    Result := FHeights[Index]
  else
    Result := FItemHeight;
end;

function TCnVirtualControlList.TopOfIndex(Index: Integer): Integer;
begin
  if Index <= 0 then
    Result := 0
  else
    Result := FPrefix[Index - 1] + Index * FItemSpacing;
end;

function TCnVirtualControlList.BottomOfIndex(Index: Integer): Integer;
begin
  Result := TopOfIndex(Index) + HeightOfIndex(Index);
end;

function TCnVirtualControlList.FindStartIndex(PosY: Integer): Integer;
var
  L, R, Mid, Top, Bottom: Integer;
begin
  if FItemCount = 0 then
  begin
    Result := -1;
    Exit;
  end;

  L := 0;
  R := FItemCount - 1;

  while L <= R do
  begin
    Mid := (L + R) shr 1;
    Top := TopOfIndex(Mid);
    Bottom := Top + HeightOfIndex(Mid);
    if Bottom <= PosY then
      L := Mid + 1
    else if Top > PosY then
      R := Mid - 1
    else
    begin
      Result := Mid;
      Exit;
    end;
  end;

  if L >= FItemCount then
    Result := FItemCount - 1
  else
    Result := L;
end;

function TCnVirtualControlList.FindIndexByPos(PosY: Integer): Integer;
var
  Idx, Top, Bottom: Integer;
begin
  Idx := FindStartIndex(PosY);
  if Idx = -1 then
  begin
    Result := -1;
    Exit;
  end;

  Top := TopOfIndex(Idx);
  Bottom := Top + HeightOfIndex(Idx);
  if (PosY >= Top) and (PosY < Bottom) then
    Result := Idx
  else
    Result := -1;
end;

function TCnVirtualControlList.MaxScroll: Integer;
begin
  if FTotalHeight > ClientHeight then
    Result := FTotalHeight - ClientHeight
  else
    Result := 0;
end;

procedure TCnVirtualControlList.UpdateScrollRange;
var
  SI: TScrollInfo;
begin
  SI.cbSize := SizeOf(SI);
  SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
  SI.nMin := 0;
  SI.nMax := MaxScroll;
  SI.nPage := ClientHeight;
  SI.nPos := FScrollPos;
  SetScrollInfo(Handle, SB_VERT, SI, True);
end;

procedure TCnVirtualControlList.SetScrollPos(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value > MaxScroll then
    Value := MaxScroll;

  if FScrollPos <> Value then
  begin
    FScrollPos := Value;
    UpdateScrollRange;
    if FUseItemControls then
      LayoutHosts
    else
      Invalidate;
  end;
end;

procedure TCnVirtualControlList.Resize;
begin
  inherited Resize;
  UpdateScrollRange;
  if FUseItemControls then
    RebuildHosts;
end;

procedure TCnVirtualControlList.SetItemCount(Value: Integer);
begin
  if Value < 0 then
    Value := 0;

  if FItemCount <> Value then
  begin
    FItemCount := Value;
    if FSelectedIndex >= FItemCount then
      FSelectedIndex := -1;
    MeasureAll;
    UpdateScrollRange;
    if FUseItemControls then
      RebuildHosts
    else
      Invalidate;
  end;
end;

procedure TCnVirtualControlList.SetItemHeight(Value: Integer);
begin
  if Value < 1 then
    Value := 1;

  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    if not Assigned(FOnMeasureItem) then
      MeasureAll
    else
      BuildPrefix;
    UpdateScrollRange;
    if FUseItemControls then
      RebuildHosts
    else
      Invalidate;
  end;
end;

procedure TCnVirtualControlList.SetItemSpacing(Value: Integer);
begin
  if Value < 0 then
    Value := 0;

  if FItemSpacing <> Value then
  begin
    FItemSpacing := Value;
    BuildPrefix;
    UpdateScrollRange;
    if FUseItemControls then
      RebuildHosts
    else
      Invalidate;
  end;
end;

procedure TCnVirtualControlList.SetSelectedIndex(Value: Integer);
begin
  if (Value < -1) then
    Value := -1;
  if (Value >= FItemCount) then
    Value := -1;

  if FSelectedIndex <> Value then
  begin
    FSelectedIndex := Value;
    if FUseItemControls then
      LayoutHosts
    else
      Invalidate;
  end;
end;

function TCnVirtualControlList.IndexAt(Y: Integer): Integer;
begin
  Result := FindIndexByPos(FScrollPos + Y);
end;

function TCnVirtualControlList.ItemRectAt(Index: Integer; OffsetY: Integer): TRect;
var
  TopY: Integer;
begin
  TopY := TopOfIndex(Index) - FScrollPos + OffsetY;
  Result.Left := 0;
  Result.Top := TopY;
  Result.Right := ClientWidth;
  Result.Bottom := TopY + HeightOfIndex(Index);
end;

procedure TCnVirtualControlList.EnsureHosts;
var
  NeedCount, I: Integer;
  Host: TCnVirtualItemHost;
begin
  if not FUseItemControls then
    Exit;

  NeedCount := (ClientHeight div RowPixels) + 1 + FBufferItems;
  if NeedCount < 0 then
    NeedCount := 0;

  while FHosts.Count < NeedCount do
  begin
    Host := TCnVirtualItemHost.Create(Self);
    Host.Parent := Self;
    Host.OwnerCtrl := Self;
    Host.TypeId := -1;
    Host.Align := alNone;
    Host.Left := 0;
    Host.Top := 0;
    Host.Width := ClientWidth;
    Host.Height := FItemHeight;
    Host.Visible := False;
    Host.BevelOuter := bvNone;
    Host.BevelInner := bvNone;
    // Host.ParentBackground := False;
    Host.Color := Color;
    if Assigned(FOnBuildItemControls) then
      FOnBuildItemControls(Self, Host);
    FHosts.Add(Host);
  end;

  for I := 0 to FHosts.Count - 1 do
    TCnVirtualItemHost(FHosts[I]).Width := ClientWidth;

  while FHosts.Count > NeedCount do
  begin
    TCnVirtualItemHost(FHosts.Last).Free;
    FHosts.Delete(FHosts.Count - 1);
  end;
end;

procedure TCnVirtualControlList.ClearHosts;
var
  I: Integer;
begin
  for I := 0 to FHosts.Count - 1 do
    TObject(FHosts[I]).Free;
  FHosts.Clear;
end;

procedure TCnVirtualControlList.LayoutHosts;
var
  StartIndex, I, Index, Y, H, ItemType: Integer;
  Host: TCnVirtualItemHost;
begin
  if not FUseItemControls then
    Exit;

  EnsureHosts;
  StartIndex := FindStartIndex(FScrollPos);
  Y := TopOfIndex(StartIndex) - FScrollPos;

  for I := 0 to FHosts.Count - 1 do
  begin
    Index := StartIndex + I;
    Host := TCnVirtualItemHost(FHosts[I]);

    if (Index >= 0) and (Index < FItemCount) then
    begin
      Host.Index := Index;
      H := HeightOfIndex(Index);
      Host.SetBounds(0, Y, ClientWidth, H);
      Host.Visible := True;
      ItemType := 0;
      if Assigned(FOnGetItemType) then
        FOnGetItemType(Self, Index, ItemType);

      if Host.TypeId <> ItemType then
      begin
        while Host.ControlCount > 0 do
          Host.Controls[0].Free;
        if Assigned(FOnBuildItemTemplate) then
          FOnBuildItemTemplate(Self, ItemType, Host)
        else if Assigned(FOnBuildItemControls) then
          FOnBuildItemControls(Self, Host);
        Host.TypeId := ItemType;
      end;

      if Assigned(FOnBindItem) then
        FOnBindItem(Self, Index, ItemType, Host, Index = FSelectedIndex);
      Y := Y + H + FItemSpacing;
    end
    else
      Host.Visible := False;
  end;
end;

procedure TCnVirtualControlList.RebuildHosts;
begin
  if not FUseItemControls then
    Exit;

  ClearHosts;
  EnsureHosts;
  LayoutHosts;
end;

procedure TCnVirtualControlList.Paint;
var
  StartIndex, I, Y: Integer;
  R: TRect;
  Selected: Boolean;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  if FItemCount = 0 then
    Exit;
  if FUseItemControls then
  begin
    LayoutHosts;
    Exit;
  end;

  StartIndex := FindStartIndex(FScrollPos);
  Y := TopOfIndex(StartIndex) - FScrollPos;
  I := StartIndex;
  while (Y < ClientHeight) and (I < FItemCount) do
  begin
    R := Rect(0, Y, ClientWidth, Y + HeightOfIndex(I));
    Selected := I = FSelectedIndex;

    if Assigned(FOnItemPaint) then
      FOnItemPaint(Self, Canvas, I, R, Selected)
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(R);
      if Selected then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.FillRect(R);
        Canvas.Font.Color := clHighlightText;
      end
      else
        Canvas.Font.Color := Font.Color;
      Canvas.TextRect(R, R.Left + 6, R.Top + (HeightOfIndex(I) - Canvas.TextHeight
        ('W')) div 2, IntToStr(I));
    end;

    Inc(Y, HeightOfIndex(I) + FItemSpacing);
    Inc(I);
  end;
end;

procedure TCnVirtualControlList.WMVScroll(var Message: TWMVScroll);
var
  SI: TScrollInfo;
begin
  case Message.ScrollCode of
    SB_TOP:
      SetScrollPos(0);
    SB_BOTTOM:
      SetScrollPos(MaxScroll);
    SB_LINEUP:
      SetScrollPos(FScrollPos - RowPixels);
    SB_LINEDOWN:
      SetScrollPos(FScrollPos + RowPixels);
    SB_PAGEUP:
      SetScrollPos(FScrollPos - ClientHeight);
    SB_PAGEDOWN:
      SetScrollPos(FScrollPos + ClientHeight);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      begin
        SI.cbSize := SizeOf(SI);
        SI.fMask := SIF_TRACKPOS;
        if GetScrollInfo(Handle, SB_VERT, SI) then
          SetScrollPos(SI.nTrackPos)
        else
          SetScrollPos(Message.Pos);
      end;
  end;
end;

procedure TCnVirtualControlList.WMMouseWheel(var Message: TMessage);
var
  Delta: SmallInt;
begin
  Delta := SmallInt((Message.WParam shr 16) and $FFFF);
  if Delta > 0 then
    SetScrollPos(FScrollPos - RowPixels)
  else if Delta < 0 then
    SetScrollPos(FScrollPos + RowPixels);
  Message.Result := 1;
end;

procedure TCnVirtualControlList.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    Idx := IndexAt(Y);
    if Idx <> -1 then
    begin
      ItemClicked(Idx);
    end;
  end;
  SetFocus;
end;

procedure TCnVirtualControlList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_UP:
      if FSelectedIndex > 0 then
        SelectedIndex := FSelectedIndex - 1
      else
        SelectedIndex := 0;
    VK_DOWN:
      if FSelectedIndex < FItemCount - 1 then
        SelectedIndex := FSelectedIndex + 1;
    VK_HOME:
      SelectedIndex := 0;
    VK_END:
      SelectedIndex := FItemCount - 1;
    VK_PRIOR:
      SetScrollPos(FScrollPos - ClientHeight);
    VK_NEXT:
      SetScrollPos(FScrollPos + ClientHeight);
  end;
  if FSelectedIndex <> -1 then
    EnsureVisible(FSelectedIndex);
end;

procedure TCnVirtualControlList.EnsureVisible(Index: Integer);
var
  TopY, BottomY: Integer;
begin
  if (Index < 0) or (Index >= FItemCount) then
    Exit;

  TopY := TopOfIndex(Index);
  BottomY := TopY + HeightOfIndex(Index);
  if TopY < FScrollPos then
    SetScrollPos(TopY)
  else if BottomY > FScrollPos + ClientHeight then
    SetScrollPos(BottomY - ClientHeight);
end;

procedure TCnVirtualControlList.ItemClicked(Index: Integer);
begin
  SetSelectedIndex(Index);
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Index);
end;

procedure TCnVirtualControlList.ItemAction(Index: Integer);
begin
  if Assigned(FOnItemAction) then
    FOnItemAction(Self, Index);
end;

end.

