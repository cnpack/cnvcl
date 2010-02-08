{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2010 CnPack 开发组                       }
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

unit CnDragResizer;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：DragResizer 组件实现单元
* 单元作者：匿名
* 移    植：刘啸 (liuxiao@cnpack.org)
* 备    注：能在运行期关联一可视化组件，使其具有拖动与改变大小的设计能力
* 开发平台：PWinXP SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.05.28
*               移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

const
  CN_GRID_DEF_INTERVAL = 4;

type
  TCnDragResizer = class;
  TMover = class;

  TMovingEvent = procedure(Sender: TCnDragResizer; var NewLeft,
    NewTop: Integer) of object;

  TSizingEvent = procedure(Sender: TCnDragResizer; var NewLeft, NewTop, NewWidth,
    NewHeight: Integer) of object;

  TCnDragResizer = class(TComponent)
  protected
    FActive: Boolean;
    FControl: TControl;
    FSizers: TList;
    FGroupMovers : TList;
    FGroup: TWinControl;
    FGridX: Integer;
    FGridY: Integer;
    FOnSized: TNotifyEvent;
    FOnSizing: TSizingEvent;
    FOnMoved: TNotifyEvent;
    FOnMoving: TMovingEvent;
    FSizing: Boolean;
    FMoving: Boolean;
    FOrigSize: TRect;
    FNewSize: TRect;
    FDownX: Integer;
    FDownY: Integer;
    FAllowSize: Boolean;
    FAllowMove: Boolean;
    FKeepInParent: Boolean;
    FShowBounds: Boolean;
    FOneMover: TMover;
    FCurMover: TMover;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetActive(b: Boolean);
    procedure SetControl(c: TControl);
    procedure SetGroup(p: TWinControl);
    procedure CreateSizers;
    procedure CheckSizers;
    procedure ShowSizers;
    procedure HideSizers;
    procedure SizerDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SizerUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SizerMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MoverDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MoverUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MoverMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DrawSizeRect(Rect: TRect);
    procedure Calc_Size_Rect(SizerNum, dx, dy: Integer);
    procedure DoSizingEvent;
    procedure Calc_Move_Rect(dx, dy: Integer);
    procedure DoMovingEvent;
    procedure Constrain_Size;
    procedure Constrain_Move;
    procedure MoverKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoSizeMove(var Key: Word; Shift: TShiftState; dx, dy: Integer);
    procedure CreateGroupMovers;
    procedure CreateOneMover(m: TMover; c: TControl);
    function FindMoverByBuddy(c: TControl): TMover;
    
    property ResizeGroup: TWinControl read FGroup write SetGroup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default True;
    {* 是否使能}
    property Control: TControl read FControl write SetControl;
    {* 关联的控件}
    property GridX: Integer read FGridX write FGridX default CN_GRID_DEF_INTERVAL;
    {* X 方向拖动的步长}
    property GridY: Integer read FGridY write FGridY default CN_GRID_DEF_INTERVAL;
    {* Y 方向拖动的步长}
    property AllowSize: Boolean read FAllowSize write FAllowSize default True;
    {* 是否允许改变大小}
    property AllowMove: Boolean read FAllowMove write FAllowMove default True;
    {* 是否允许拖动}
    property KeepInParent: Boolean read FKeepInParent write FKeepInParent default True;
    {* 是否限制在其Parent内}
    property ShowBounds: Boolean read FShowBounds write FShowBounds;
    {* 是否拖动时显示边框}

    property OnSized: TNotifyEvent read FOnSized write FOnSized;
    {* 所关联控件改变尺寸后触发}
    property OnSizing: TSizingEvent read FOnSizing write FOnSizing;
    {* 所关联控件改变尺寸时触发}
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    {* 所关联控件拖动后触发}
    property OnMoving: TMovingEvent read FOnMoving write FOnMoving;
    {* 所关联控件拖动时触发}
  end;

  TInvisWin = class(TPanel)
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMDLGCode(var Message: TMessage); message WM_GETDLGCODE;
  public
    property  OnKeyDown;
  end;

  TMover = class(TInvisWin)
  public
    Buddy: TControl;
    procedure Show;
  end;

implementation

const
  SIZE    = 6;
  HALFSIZE = SIZE div 2;

type
  TSizer = class(TPanel)
  end;

procedure TInvisWin.WndProc(var Message: TMessage);
var
  ps : TPaintStruct;
begin
  case Message.Msg of
    WM_ERASEBKGND: Message.Result := 1;
    WM_PAINT: begin
      BeginPaint(Handle, ps);
      EndPaint(Handle, ps);
      Message.Result := 1;
    end;
  else
    inherited WndProc(Message);
  end;
end;

procedure TInvisWin.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TInvisWin.WMDLGCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTALLKEYS;
end;

// TMover

procedure TMover.Show;
begin
  Assert(Buddy <> nil);
  BoundsRect := Buddy.BoundsRect;
  Parent    := Buddy.Parent;
  Visible   := True;
  BringToFront;
end;

// TCnDragResizer

constructor TCnDragResizer.Create(AOwner: TComponent);
begin
  inherited;
  FActive    := True;
  FKeepInParent := True;
  FGridX     := CN_GRID_DEF_INTERVAL;
  FGridY     := CN_GRID_DEF_INTERVAL;
  FAllowSize  := True;
  FAllowMove  := True;
  FGroupMovers  := TList.Create;
  FSizers     := TList.Create;

  FOneMover := TMover.Create(Self);
  CreateOneMover(FOneMover, nil);

  CreateSizers;
end;

destructor TCnDragResizer.Destroy;
begin
  FGroupMovers.Free;
  FSizers.Free;
  FSizers := nil;
  inherited;
end;

procedure TCnDragResizer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if csDestroying in ComponentState then
    Exit;
  if (AComponent = FControl) and (Operation = opRemove) then
    FControl := nil;
end;

procedure TCnDragResizer.SetActive(b: Boolean);
begin
  if b <> FActive then
  begin
    FActive := b;
    CheckSizers;
  end;
end;

procedure TCnDragResizer.SetControl(c: TControl);
begin
  if c <> FControl then
  begin
    if c <> nil then
    begin
      if ResizeGroup <> nil then
      begin
        Assert(c.Parent = ResizeGroup, 'FControl is not in ResizeGroup!');
        FCurMover := FindMoverByBuddy(c);
      end else begin
        FCurMover := FOneMover;
        FCurMover.Buddy := c;
      end;
      FCurMover.Show;
    end;

    FControl := c;
    CheckSizers;
  end;
end;

procedure TCnDragResizer.SetGroup(p: TWinControl);
begin
  if p <> FGroup then
  begin
    FGroup := p;
    CreateGroupMovers;
  end;
end;

procedure TCnDragResizer.CreateGroupMovers;
var
  i : Integer;
  m : TMover;
  c : TControl;
begin
  if csDesigning in ComponentState then
    Exit;

  // Clear out the old Movers
  for i := 0 to FGroupMovers.Count - 1 do
    TObject(FGroupMovers[i]).Free;
  FGroupMovers.Clear;

  if ResizeGroup <> nil then
  begin
    for i := 0 to ResizeGroup.ControlCount-1 do
    begin
      c := ResizeGroup.Controls[i];
      if (c is TMover) or (c is TSizer) then
        Continue;

      m := TMover.Create(Self);
      CreateOneMover(m, c);
      FGroupMovers.Add(m);
      m.Show;
    end;
  end;
end;

procedure TCnDragResizer.CreateSizers;
var
  i : Integer;
  p : TSizer;
begin
  if csDesigning in ComponentState then
    Exit;

  for i := 0 to 7 do
  begin
    p := TSizer.Create(Self);
    FSizers.Add(p);

    p.BevelOuter  := bvNone;
    p.Width      := SIZE;
    p.Height     := SIZE;
    p.Color      := clBlack;
    p.Caption    := '';
    p.Tag       := i;
    p.OnMouseDown  := SizerDown;
    p.OnMouseUp   := SizerUp;
    p.OnMouseMove  := SizerMove;
    p.TabStop    := False;

    case i of
      0, 7  : p.Cursor := crSizeNWSE;
      2, 5  : p.Cursor := crSizeNESW;
      1, 6  : p.Cursor := crSizeNS;
      3, 4  : p.Cursor := crSizeWE;
    end;
  end;
end;

procedure TCnDragResizer.CreateOneMover(m: TMover; c: TControl);
begin
  m.OnMouseDown := MoverDown;
  m.OnMouseUp  := MoverUp;
  m.OnMouseMove := MoverMove;
  m.TabStop    := True;
  m.OnKeyDown  := MoverKeyDown;
  m.Buddy     := c;
end;

procedure TCnDragResizer.CheckSizers;
begin
  if (FControl <> nil) and Active and (not (csDesigning in ComponentState)) then
    ShowSizers
  else
    HideSizers;
end;

procedure TCnDragResizer.ShowSizers;
var
  i : Integer;
  p : TPanel;
  c : TControl;
begin
  c := FControl;
  Assert(c <> nil);

  for i := 0 to 7 do
  begin
    p := TPanel(FSizers[i]);
    case i of
      0, 1, 2 : p.Top := c.Top - HALFSIZE;
      3,   4 : p.Top := c.Top + c.Height div 2 - HALFSIZE;
      5, 6, 7 : p.Top := c.Top + c.Height - HALFSIZE;
    end;

    case i of
      0, 3, 5 : p.Left := c.Left - HALFSIZE;
      1,   6 : p.Left := c.Left + c.Width div 2 - HALFSIZE;
      2, 4, 7 : p.Left := c.Left + c.Width - HALFSIZE;
    end;
  end;

  Assert(FCurMover<>nil);
  FCurMover.Show;

  for i := 0 to FSizers.Count - 1 do
  begin
    p := TPanel(FSizers[i]);
    p.Parent  := c.Parent;
    p.Visible := True;
    p.BringToFront;
  end;

  if FCurMover.HandleAllocated and FCurMover.CanFocus then
    FCurMover.SetFocus;
end;

procedure TCnDragResizer.HideSizers;
var
  i : Integer;
  p : TPanel;
begin
  for i := 0 to FSizers.Count - 1 do
  begin
    p := TPanel(FSizers[i]);
    p.Visible := False;
    p.Update;
  end;
  FOneMover.Visible := False;
end;

procedure TCnDragResizer.SizerDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSizing  := True;
  FDownX  := X;
  FDownY  := Y;
  HideSizers;
  FControl.Parent.Update;
  FControl.Update;
  FOrigSize := FControl.BoundsRect;
  FNewSize  := FOrigSize;
  DrawSizeRect(FNewSize);
end;

procedure DoSwap(DoSwap: Boolean; var a, b: Integer);
var
  t : Integer;
begin
  if DoSwap then
  begin
    t := a;
    a := b;
    b := t;
  end;
end;

procedure TCnDragResizer.SizerUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FNewSize.Right < FNewSize.Left then
    DoSwap(True, FNewSize.Right, FNewSize.Left);
  if FNewSize.Bottom < FNewSize.Top then
    DoSwap(True, FNewSize.Bottom, FNewSize.Top);

  FSizing := False;
  DrawSizeRect(FNewSize);
  FControl.Invalidate;
  FControl.BoundsRect := FNewSize;
  ShowSizers;
  if Assigned(FOnSized) then
    FOnSized(Self);
end;

procedure TCnDragResizer.SizerMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FSizing then
  begin
    DrawSizeRect(FNewSize);

    if AllowSize then
    begin
      Calc_Size_Rect((Sender as TSizer).Tag, X - FDownX, Y - FDownY);
      DoSizingEvent;
    end;

    DrawSizeRect(FNewSize);
    if FShowBounds then
      FControl.BoundsRect := FNewSize;
  end;
end;

procedure TCnDragResizer.DoSizingEvent;
var
  tmpWid, tmpHgt  : Integer;
begin
  tmpWid := FNewSize.Right - FNewSize.Left;
  tmpHgt := FNewSize.Bottom - FNewSize.Top;
  if Assigned(FOnSizing) then
    FOnSizing(Self, FNewSize.Left, FNewSize.Top, tmpWid, tmpHgt);
  FNewSize.Right  := FNewSize.Left + tmpWid;
  FNewSize.Bottom := FNewSize.Top + tmpHgt;
end;

procedure GetNonClientOffset(h: THandle; var nx, ny: Integer);
var
  p : TPoint;
  R : TRect;
begin
  p := Point(0, 0);
  Windows.ClientToScreen(h, p);
  Windows.GetWindowRect(h, R);
  nx := p.x - R.Left;
  ny := p.y - R.Top;
end;

procedure TCnDragResizer.DrawSizeRect(Rect: TRect);
var
  h      : THandle;
  dc     : THandle;
  c      : TCanvas;
  nx, ny  : Integer;
  OldPen  : TPen;
  OldBrush : TBrush;
begin
  if not FShowBounds then
    Exit;

  h  := (FControl.Parent as TWinControl).Handle;
  GetNonClientOffset(h, nx, ny);
  dc := GetWindowDC(h);
  try
    c := TCanvas.Create;
    c.Handle := dc;

    OldPen := TPen.Create;
    OldPen.Assign(c.Pen);
    OldBrush := TBrush.Create;
    OldBrush.Assign(c.Brush);

    c.Pen.Width := 2;
    c.Pen.Mode  := pmXOR;
    c.Pen.Color := clWhite;
    c.Brush.Style := bsClear;
    c.Rectangle(Rect.Left + nx, Rect.Top + ny, Rect.Right + nx, Rect.Bottom + ny);

    c.Pen.Assign(OldPen);
    OldPen.Free;
    c.Brush.Assign(OldBrush);
    OldBrush.Free;

    c.Handle := 0;
    c.Free;
  finally
    ReleaseDC(h, dc);
  end;
end;

procedure TCnDragResizer.Calc_Size_Rect(SizerNum, dx, dy: Integer);
begin
  dx := (dx div GridX) * GridX;
  dy := (dy div GridY) * GridY;

  case SizerNum of
    0, 1, 2 : FNewSize.Top   := FOrigSize.Top + dy;
    5, 6, 7 : FNewSize.Bottom := FOrigSize.Bottom + dy;
  end;

  case SizerNum of
    0, 3, 5 : FNewSize.Left  := FOrigSize.Left + dx;
    2, 4, 7 : FNewSize.Right  := FOrigSize.Right + dx;
  end;

  if KeepInParent then
    Constrain_Size;
end;

procedure TCnDragResizer.MoverDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCurMover := Sender as TMover;
  FControl := FCurMover.Buddy;
  Assert(FControl<>nil);
  FControl.BringToFront;
  FCurMover.BringToFront;

  FMoving := True;
  FDownX := X;
  FDownY := Y;
  HideSizers;
  FControl.Parent.Update;
  FControl.Update;
  FOrigSize := FControl.BoundsRect;
  FNewSize  := FOrigSize;
  DrawSizeRect(FNewSize);
end;

procedure TCnDragResizer.MoverUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMoving := False;
  FControl.BoundsRect := FNewSize;
  FCurMover.Invalidate;
  FControl.Refresh;
  DrawSizeRect(FNewSize);
  ShowSizers;
  if Assigned(FOnMoved) then
    FOnMoved(Self);
end;

procedure TCnDragResizer.Calc_Move_Rect(dx, dy: Integer);
begin
  FNewSize := FOrigSize;
  dx := (dx div GridX) * GridX;
  dy := (dy div GridY) * GridY;
  OffsetRect(FNewSize, dx, dy);
  if KeepInParent then
    Constrain_Move;
end;

procedure TCnDragResizer.DoMovingEvent;
var
  tmpWid, tmpHgt : Integer;
begin
  tmpWid := FNewSize.Right - FNewSize.Left;
  tmpHgt := FNewSize.Bottom - FNewSize.Top;
  if Assigned(FOnMoving) then
    FOnMoving(Self, FNewSize.Left, FNewSize.Top);
  FNewSize.Right := FNewSize.Left + tmpWid;
  FNewSize.Bottom := FNewSize.Top + tmpHgt;
end;

procedure TCnDragResizer.MoverMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  if FMoving then
  begin
    DrawSizeRect(FNewSize);

    if AllowMove then
    begin
      dx := X - FDownX;
      dy := Y - FDownY;
      Calc_Move_Rect(dx, dy);
      DoMovingEvent;
    end;

    DrawSizeRect(FNewSize);
    if FShowBounds then
      FControl.BoundsRect := FNewSize;
  end;
end;

procedure TCnDragResizer.Constrain_Size;
var
  p : TWinControl;
begin
  p := FControl.Parent;

  with FNewSize do
  begin
    if Left < 0 then
      Left := 0;
    if Top < 0 then
      Top := 0;
    if Right > p.ClientWidth then
      Right := p.ClientWidth;
    if Bottom > p.ClientHeight then
      Bottom := p.ClientHeight;

    if Right < Left + GridX then
      Right := Left + GridX;
    if Bottom < Top + GridY then
      Bottom := Top + GridY;
  end;
end;

procedure TCnDragResizer.Constrain_Move;
begin
  if FNewSize.Left < 0 then
    OffsetRect(FNewSize, -FNewSize.Left, 0);

  if FNewSize.Top < 0 then
    OffsetRect(FNewSize, 0, -FNewSize.Top);

  if FNewSize.Right > FControl.Parent.ClientWidth then
    OffsetRect(FNewSize, FControl.Parent.ClientWidth - FNewSize.Right, 0);

  if FNewSize.Bottom > FControl.Parent.ClientHeight then
    OffsetRect(FNewSize, 0, FControl.Parent.ClientHeight - FNewSize.Bottom);
end;

procedure TCnDragResizer.MoverKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Active then
  begin
    case Key of
      VK_LEFT  : DoSizeMove(Key, Shift, -GridX,  0);
      VK_RIGHT : DoSizeMove(Key, Shift,  GridX,  0);
      VK_UP   : DoSizeMove(Key, Shift,  0, -GridY);
      VK_DOWN  : DoSizeMove(Key, Shift,  0,  GridY);
    end;
  end;
end;

procedure TCnDragResizer.DoSizeMove(var Key: Word; Shift: TShiftState; dx, dy: Integer);
begin
  if (ssCtrl in Shift) or (ssShift in Shift) then
  begin
    Key := 0;
    FNewSize := FControl.BoundsRect;

    if (ssCtrl in Shift) and AllowMove then
    begin
      OffsetRect(FNewSize, dx, dy);
      if KeepInParent then
        Constrain_Move;
      DoMovingEvent;
    end;

    if (ssShift in Shift) and AllowSize then
    begin
      FNewSize.Right  := FNewSize.Right + dx;
      FNewSize.Bottom := FNewSize.Bottom + dy;
      if KeepInParent then
       Constrain_Size;
      DoSizingEvent;
    end;

    FControl.BoundsRect := FNewSize;
    ShowSizers;
  end;
end;

function TCnDragResizer.FindMoverByBuddy(c: TControl): TMover;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to FGroupMovers.Count - 1 do
    if TMover(FGroupMovers[i]).Buddy = c then
      Result := FGroupMovers[i];
  Assert(Result <> nil);
end;

end.