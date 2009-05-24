{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2007 CnPack 开发组                       }
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

unit CnSkinStdCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, Graphics, IniFiles,
  Forms, ExtCtrls, CnSkinTheme;

type
  TUnderline = (ulNone, ulHot, ulAlways);

  TCnSkinGroupBox = class(TGroupBox)
  private
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TCnSkinLabel = class(TLabel)
  private
    FHotColor: TColor;
    FHot: Boolean;
    FUnderline: TUnderline;
    procedure SetUnderline(Value: TUnderline);
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HotColor: TColor read FHotColor write FHotColor;
    property Underline: TUnderline read FUnderline write SetUnderline;
  end;

  TCnSkinEdit = class(TEdit)
  private
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TCnSkinMemo = class(TMemo)
  private
    FDownButton: TScrollBarButton;
    FOverButton: TScrollBarButton;
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHittest(var Message: TWMNCHittest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TButtonState = (bsDefault, bsOver, bsDown, bsDisabled);

  TCnSkinComboBox = class(TComboBox)
  private
    FButtonState: TButtonState;
    procedure SetButtonState(Value: TButtonState);
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCnSkinButton = class(TButton)
  private
    FButtonState: TButtonState;
    procedure SetButtonState(Value: TButtonState);
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TCnSkinCheckBox = class(TCheckBox)
  private
    FButtonState: TButtonState;
    procedure SetButtonState(Value: TButtonState);
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TCnSkinRadioButton = class(TRadioButton)
  private
    FButtonState: TButtonState;
    procedure SetButtonState(Value: TButtonState);
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TCnSkinListBox = class(TListBox)
  private
    FDownButton: TScrollBarButton;
    FOverButton: TScrollBarButton;
    FScrollBarVisible: Boolean;
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHittest(var Message: TWMNCHittest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCnSkinScrollBar = class(TScrollBar)
  private
    FOverButton: TScrollBarButton;
    FDownButton: TScrollBarButton;
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHittest(var Message: TWMNCHittest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp(var Message: TWMNCLButtonUp); message WM_NCLBUTTONUP;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TCnSkinStaticText = class(TStaticText)
  private
    FHotColor: TColor;
    FHot: Boolean;
    FUnderline: TUnderline;
    procedure SetUnderline(Value: TUnderline);  
    procedure CMThemeChange(var Message: TMessage); message CM_THEMECHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;    
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HotColor: TColor read FHotColor write FHotColor;
    property Underline: TUnderline read FUnderline write SetUnderline;
  end;

//procedure ResetSkin(SkinPath: string; IniFile: TIniFile);

implementation

uses
  Consts;

procedure DrawGroupBox(Control: TGroupBox);
var
  Canvas: TControlCanvas;
  R: TRect;
begin
  Canvas := TControlCanvas.Create;
  Canvas.Control := Control;
  Canvas.Lock;
  try
    Canvas.Brush := Control.Brush;
    Canvas.Pen.Color := CnSkinThemes.CurrentSkin.ShadowColor;
    Canvas.Font := Control.Font;
    Canvas.FillRect(Control.ClientRect);
    R := Control.ClientRect;
    R.Top := Canvas.TextHeight(Control.Caption) div 2;
    Canvas.Rectangle(R);
    R.Top := 0;
    R.Left := 8;
    DrawText(Canvas.Handle, PChar(Control.Caption), -1, R, DT_LEFT or DT_TOP);
  finally
    Canvas.Unlock;
    Canvas.Free;
  end;
end;

procedure DrawBorder(Control: TWinControl);
var
  Canvas: TCanvas;
  RC, RW: TRect;
begin
  Canvas := TCanvas.Create;
  Canvas.Handle := GetWindowDC(Control.Handle);
  try
    Windows.GetClientRect(Control.Handle, RC);
    GetWindowRect(Control.Handle, RW);
    MapWindowPoints(0, Control.Handle, RW, 2);
    OffsetRect(RC, -RW.Left, -RW.Top);
    ExcludeClipRect(Canvas.Handle, RC.Left, RC.Top, RC.Right, RC.Bottom);
    RC := Rect(0, 0, Control.Width, Control.Height);
    Canvas.Pen.Color := CnSkinThemes.CurrentSkin.ShadowColor;
    Canvas.Brush := Control.Brush;
    Canvas.Rectangle(RC);
  finally
    ReleaseDC(Control.Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure DrawCombo(Control: TComboBox; State: TButtonState);
var
  Canvas: TControlCanvas;
  H: Integer;
  SrcR, DestR: TRect;
begin
  Canvas := TControlCanvas.Create;
  Canvas.Control := Control;
  try
    Canvas.Brush := Control.Brush;
    Canvas.Pen.Color := CnSkinThemes.CurrentSkin.ShadowColor;
    Canvas.Rectangle(Control.ClientRect);
    DestR.Top := 1;
    DestR.Bottom := Control.Height -1;
    DestR.Right := Control.Width -1;
    DestR.Left := DestR.Right - GetSystemMetrics(SM_CXHTHUMB) - 1;
    H := CnSkinThemes.CurrentSkin.ComboBmp.Height div 6;
    SrcR := Rect(0, 0, CnSkinThemes.CurrentSkin.ComboBmp.Width, H);
    OffsetRect(SrcR, 0, Ord(State) * H);
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ComboBmp.Canvas, SrcR);
    SrcR := Rect(0, 0, CnSkinThemes.CurrentSkin.ComboBmp.Width, H);
    if State = bsDisabled then
      OffsetRect(SrcR, 0, 5 * H) else
      OffsetRect(SrcR, 0, 4 * H);
    Inc(DestR.Left, (DestR.Right - DestR.Left - CnSkinThemes.CurrentSkin.ComboBmp.Width) div 2);
    DestR.Right := DestR.Left + CnSkinThemes.CurrentSkin.ComboBmp.Width;
    Canvas.Brush.Style := bsClear;
    Canvas.BrushCopy(DestR, CnSkinThemes.CurrentSkin.ComboBmp, SrcR, clFuchsia);
  finally
    Canvas.Free;
  end;
end;

procedure DrawButton(Control: TButton; State: TButtonState);
var
  Canvas: TControlCanvas;
  SrcY, W, H: Integer;
  SrcR, DestR: TRect;
begin
  Canvas := TControlCanvas.Create;
  Canvas.Control := Control;
  Canvas.Lock;
  try
    W := CnSkinThemes.CurrentSkin.ButtonBmp.Width div 3;
    H := CnSkinThemes.CurrentSkin.ButtonBmp.Height div 5;
    if (GetFocus = Control.Handle) and (State = bsDefault) then
      SrcY := CnSkinThemes.CurrentSkin.ButtonBmp.Height - H
    else
      SrcY := Ord(State) * H;

    SrcR := Rect(0, SrcY, W, SrcY + H);
    DestR := Rect(0, 0, W, Control.Height);
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ButtonBmp.Canvas, SrcR);
    OffsetRect(SrcR, W, 0);
    DestR.Left := W;
    DestR.Right := Control.Width - W;
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ButtonBmp.Canvas, SrcR);
    SrcR.Right := CnSkinThemes.CurrentSkin.ButtonBmp.Width;
    SrcR.Left := SrcR.Right - W;
    DestR.Left := DestR.Right;
    DestR.Right := Control.Width;
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ButtonBmp.Canvas, SrcR);
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Control.Font;
    if not Control.Enabled then
      Canvas.Font.Color := clGray;

    DestR.Left := 0;
    DrawText(Canvas.Handle, PChar(Control.Caption), -1, DestR,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  finally
    Canvas.Unlock;
    Canvas.Free;
  end;
end;

procedure DrawCheckBox(Control: TCheckBox; State: TButtonState);
var
  Canvas: TControlCanvas;
  SrcR, DestR: TRect;
  W: Integer;
begin
  Canvas := TControlCanvas.Create;
  Canvas.Control := Control;
  Canvas.Lock;
  try
    W := CnSkinThemes.CurrentSkin.CheckBmp.Width;
    SrcR := Rect(0, 0, W, W);
    DestR := SrcR;
    OffsetRect(DestR, 0, (Control.Height - W) div 2);
    OffsetRect(SrcR, 0, (Ord(State) + Ord(Control.State) * 4) * W);
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.CheckBmp.Canvas, SrcR);
  finally
    Canvas.Unlock;
    Canvas.Free;
  end;
end;

procedure DrawRadio(Control: TRadioButton; State: TButtonState);
var
  Canvas: TControlCanvas;
  SrcR, DestR: TRect;
  W: Integer;
begin
  Canvas := TControlCanvas.Create;
  Canvas.Control := Control;
  Canvas.Lock;
  try
    W := CnSkinThemes.CurrentSkin.RadioBmp.Width;
    SrcR := Rect(0, 0, W, W);
    DestR := SrcR;
    OffsetRect(DestR, 0, (Control.Height - W) div 2);
    OffsetRect(SrcR, 0, (Ord(State) + Ord(Control.Checked) * 4) * W);
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.RadioBmp.Canvas, SrcR);
  finally
    Canvas.Unlock;
    Canvas.Free;
  end;
end;

procedure DrawScrollBar2(Canvas: TCanvas; R: TRect; I1, I2: Integer;
  Down: TScrollBarButton; Kind: TScrollBarKind; Enabled: Boolean);
const
  Colors: array[Boolean] of TColor = (clGray, clBlack);
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
var
  DestR: TRect;
  DestX, DestY, DestW: Integer;
begin
  Canvas.Brush.Color := CnSkinThemes.CurrentSkin.FaceColor;
  Canvas.FillRect(R);
  Canvas.Pen.Color := Colors[Enabled];
  Canvas.Brush.Color := Colors[Enabled];
  if Kind = sbHorizontal then
  begin
    DestW := R.Bottom - R.Top;
    DestR := R;
    DestR.Right := DestR.Left + DestW;
    DrawEdge(Canvas.Handle, DestR, DownStyles[Down = sbLeft], BF_RECT);
    DestX := (DestW - 4) div 2 + R.Left;
    DestY := (DestW - 6) div 2 + R.Top;
    Canvas.Polygon([Point(DestX, DestY + 3), Point(DestX + 3, DestY + 6),
      Point(DestX + 3, DestY), Point(DestX, DestY + 3)]);
    OffsetRect(DestR, R.Right - R.Left - DestW, 0);
    DestX := R.Right + R.Left - DestX -1;
    Canvas.Polygon([Point(DestX, DestY + 3), Point(DestX - 3, DestY + 6),
      Point(DestX - 3, DestY), Point(DestX, DestY + 3)]);
    DrawEdge(Canvas.Handle, DestR, DownStyles[Down = sbRight], BF_RECT);
    DestR.Left := I1;
    DestR.Right := DestR.Left + I2;
    if Enabled then
      DrawEdge(Canvas.Handle, DestR, DownStyles[Down = sbButton], BF_RECT);
  end
  else
  begin
    DestW := R.Right - R.Left;
    DestR := R;
    DestR.Bottom := DestR.Top + DestW;
    DrawEdge(Canvas.Handle, DestR, DownStyles[Down = sbUp], BF_RECT);
    DestX := (DestW - 6) div 2 + R.Left;
    DestY := (DestW - 4) div 2 + R.Top;
    Canvas.Polygon([Point(DestX, DestY + 3), Point(DestX + 6, DestY + 3),
      Point(DestX + 3, DestY), Point(DestX, DestY + 3)]);
    OffsetRect(DestR, 0, R.Bottom - R.Top - DestW);
    DestY := R.Bottom - R.Top - DestY -1;
    Canvas.Polygon([Point(DestX, DestY - 3), Point(DestX + 6, DestY - 3),
      Point(DestX + 3, DestY), Point(DestX, DestY - 3)]);
    DrawEdge(Canvas.Handle, DestR, DownStyles[Down = sbDown], BF_RECT);
    DestR.Top := I1;
    DestR.Bottom := DestR.Top + I2;
    if Enabled then
      DrawEdge(Canvas.Handle, DestR, DownStyles[Down = sbButton], BF_RECT);
  end;
end;

procedure DrawMemo(Control: TMemo; Over, Down: TScrollBarButton);
var
  Canvas: TCanvas;
  R, R2: TRect;
  I, I1, I2: Integer;
  Enabled: Boolean;

  procedure DrawMemoScrollBar(Kind: TScrollBarKind);
  begin
    R2 := R;
    if Kind = sbHorizontal then
    begin
      Inc(R2.Left);
      R2.Top := R2.Bottom - SBSIZE;
      if Control.ScrollBars = ssBoth then
        Dec(R2.Right, SBSIZE);
      I1 := R2.Left + SBSIZE;
      I := R2.Right - R2.Left;
      case Over of
        sbButton: Over := sbNone;
        sbButtonH: Over := sbButton;
      end;
      case Down of
        sbButton: Down := sbNone;
        sbButtonH: Down := sbButton;
      end;
    end else
    begin
      Inc(R2.Top);
      R2.Left := R2.Right - SBSIZE;
      if Control.ScrollBars = ssBoth then
        Dec(R2.Bottom, SBSIZE);
      I1 := R2.Top + SBSIZE;
      I := R2.Bottom - R2.Top;
    end;
    Enabled := Control.Enabled;
    if Enabled then
      Enabled := CnGetScrollInfo(Control, I, I1, I2, Kind);
    CnDrawScrollBar(Canvas, R2, I1, I2, Over, Down, Kind, Enabled);
  end;

begin
  Canvas := TCanvas.Create;
  Canvas.Handle := GetWindowDC(Control.Handle);
  try
    R := Rect(0, 0, Control.Width, Control.Height);
    Canvas.Pen.Color := CnSkinThemes.CurrentSkin.ShadowColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R);
    Dec(R.Right);
    Dec(R.Bottom);
    Canvas.Font := Control.Font;
    case Control.ScrollBars of
      ssBoth:
      begin
        R2 := R;
        R2.Left := R2.Right - SBSIZE;
        R2.Top := R2.Bottom - SBSIZE;
        Canvas.Brush.Color := CnSkinThemes.CurrentSkin.FaceColor;
        Canvas.FillRect(R2);
        DrawMemoScrollBar(sbVertical);
        DrawMemoScrollBar(sbHorizontal);        
      end;
      ssVertical: DrawMemoScrollBar(sbHorizontal);
      ssHorizontal: DrawMemoScrollBar(sbVertical);
    end;
  finally
    ReleaseDC(Control.Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure ScrollMemo(Control: TMemo; Count: Integer; Kind: TScrollBarKind);
begin
  if Kind = sbVertical then
    SendMessage(Control.Handle, EM_LINESCROLL, 0, Count)
  else
    SendMessage(Control.Handle, EM_LINESCROLL, Count, 0);
end;

procedure DrawListBox(Control: TListBox; Over, Down: TScrollBarButton);
var
  Canvas: TCanvas;
  R: TRect;
  H, I1, I2: Integer;
begin
  Canvas := TCanvas.Create;
  Canvas.Handle := GetWindowDC(Control.Handle);
  try
    R := Rect(0, 0, Control.Width, Control.Height);
    Canvas.Pen.Color := CnSkinThemes.CurrentSkin.ShadowColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R);
    H := Control.Height - 2;
    I1 := SBSIZE + 1;
    if CnGetScrollInfo(Control, H, I1, I2, sbVertical) then
    begin
      Dec(R.Right);
      Dec(R.Bottom);
      Inc(R.Top);
      R.Left := R.Right - SBSIZE;
      CnDrawScrollBar(Canvas, R, I1, I2, Over, Down, sbVertical, Control.Enabled);
    end;
  finally
    ReleaseDC(Control.Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure DrawScrollBar(Control: TScrollBar; Over, Down: TScrollBarButton);
var
  I, I1, I2: Integer;
  Canvas: TCanvas;
  R: TRect;
begin
  with Control do
  begin
    if Kind = sbHorizontal then
    begin
      I := Width - Height - Height;
      I2 := Height;
    end else
    begin
      I := Height - Width - Width;
      I2 := Width;
    end;
    if PageSize > 0 then
    begin
      I2 :=  I * PageSize div Max;
      if I2 < 10 then I2 := 10;
    end;
    if Kind = sbHorizontal then
      I1 := (I - I2) * Position div Max + Height
    else
      I1 := (I - I2) * Position div Max + Width;
      
    R := Rect(0, 0, Width, Height);
    Canvas := TCanvas.Create;
    Canvas.Handle := GetWindowDC(Handle);
    try
      CnDrawScrollBar(Canvas, R, I1, I2, Over, Down, Kind, Enabled);
    finally
      ReleaseDC(Handle, Canvas.Handle);
      Canvas.Free;
    end;
  end;
end;

{TCnSkinGroupBox}

constructor TCnSkinGroupBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
end;

destructor TCnSkinGroupBox.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinGroupBox.CMThemeChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSkinGroupBox.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if CnSkinThemes.Active then
    DrawGroupBox(Self);
end;

{TCnSkinLabel}

constructor TCnSkinLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
  FHotColor := clBlue;
end;

destructor TCnSkinLabel.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinLabel.SetUnderline(Value: TUnderline);
begin
  if Value <> FUnderline then
  begin
    FUnderline := Value;
    Repaint;
  end;
end;

procedure TCnSkinLabel.CMThemeChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSkinLabel.CMFontChanged(var Message: TMessage);
begin
  if not FHot then inherited;
end;

procedure TCnSkinLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHot := True;
    Repaint;
  end;
end;

procedure TCnSkinLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHot := False;
    Repaint;
  end;
end;

procedure TCnSkinLabel.Paint;
var
  SaveColor: TColor;
  X, W: Integer;
begin
  if FHot then
  begin
    SaveColor := Font.Color;
    Font.Color :=  FHotColor;
  end
  else
    SaveColor := clBlack;
    
  inherited;

  if (FUnderline = ulAlways) or ((FUnderline = ulHot) and FHot) then
  begin
    X := 0;
    W := Canvas.TextWidth(Caption);
    case Alignment of
      taCenter: X := (Width - W) div 2;
      taRightJustify: X := Width - W;
    end;
    Canvas.Pen.Color := Font.Color;
    Canvas.MoveTo(X, Height -1 );
    Canvas.LineTo(X + W, Height - 1);
  end;

  if FHot then
    Font.Color := SaveColor;
end;

{TCnSkinEdit}

constructor TCnSkinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
end;

destructor TCnSkinEdit.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinEdit.CMThemeChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSkinEdit.WMNCPaint(var Message: TMessage);
begin
  if CnSkinThemes.Active then
    DrawBorder(Self)
  else
    inherited;
end;

{TCnSkinMemo}

constructor TCnSkinMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
  FDownButton := sbNone;
  FOverButton := sbNone;
end;

destructor TCnSkinMemo.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinMemo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCnSkinMemo.CMThemeChange(var Message: TMessage);
begin
  RecreateWnd;
end;

procedure TCnSkinMemo.CMMouseLeave(var Message: TMessage);
begin
  if FOverButton <> sbNone then
  begin
    FOverButton := sbNone;
    FDownButton := sbNone;
    DrawMemo(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinMemo.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  if CnSkinThemes.Active then
  begin
    with Message.CalcSize_Params^ do
    begin
      Inc(rgrc[0].Left);
      Inc(rgrc[0].Top);
      Dec(rgrc[0].Right);
      if ScrollBars in [ssBoth, ssVertical] then
        Dec(rgrc[0].Right, SBSIZE);
      Dec(rgrc[0].Bottom);
      if ScrollBars in [ssBoth, ssHorizontal] then
        Dec(rgrc[0].Bottom, SBSIZE);
    end;
  end else
    inherited;
end;

procedure TCnSkinMemo.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  Pt: TPoint;
  Kind: TScrollBarKind;
  I, Count, Pos, CharWidth: Integer;
begin
  inherited;
  if (CnSkinThemes.Active) and (ScrollBars <> ssNone) then
  begin
    FDownButton := FOverButton;
    if FDownButton < sbButton then
    begin
      Pos := 0;
      Kind := sbVertical;
      I := 0;

      Pt := ScreenToClient(Point(Message.XCursor, Message.YCursor));
      if (Pt.X > Width - SBSIZE - 1) and (ScrollBars <> ssHorizontal) then
      begin
        I := Height - 2;
        if ScrollBars = ssBoth then Dec(I, SBSIZE);
        Kind := sbVertical;
        Pos := Pt.Y;
      end else
      if (Pt.Y > Height - SBSIZE - 1) and (ScrollBars <> ssVertical) then
      begin
        I := Width - 2;
        if ScrollBars = ssBoth then Dec(I, SBSIZE);
        Kind := sbHorizontal;
        Pos := Pt.X;
      end;
      Count := CnGetScrollCount(Self, FDownButton, I, Pos, Kind);
      if Count <> 0 then
      begin
        if FDownButton <> sbNone then
        begin
          ScrollMemo(Self, Count, Kind);
          SetTimer(Handle, 1000, 100, nil);
        end else
          if Kind = sbHorizontal then
          begin
            Pos := GetScrollPos(Handle, Ord(Kind));
            if Count > 0 then
            begin
              ScrollMemo(Self, 1, Kind);
              CharWidth := GetScrollPos(Handle, Ord(Kind)) - Pos;
              Dec(Count, CharWidth);
            end else
            begin
              ScrollMemo(Self, -1, Kind);
              CharWidth := Pos - GetScrollPos(Handle, Ord(Kind));
              Inc(Count, CharWidth);
            end;
            ScrollMemo(Self, Count div CharWidth, Kind);
          end else
            ScrollMemo(Self, Count, Kind);
      end else
        DrawMemo(Self, FOverButton, FDownButton);
    end else
      DrawMemo(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinMemo.WMNCLButtonUp(var Message: TWMNCLButtonUp);
begin
  inherited;
  if FDownButton <> sbNone then
  begin
    FDownButton := sbNone;
    DrawMemo(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinMemo.WMNCHittest(var Message: TWMNCHittest);
var
  Pt: TPoint;
  Button: TScrollBarButton;
  I, I1, I2, Pos, Count, CharWidth: Integer;
  Kind: TScrollBarKind;
begin
  inherited;
  if (not CnSkinThemes.Active) or (ScrollBars = ssNone) then Exit;
  Pt := ScreenToClient(Point(Message.XPos, Message.YPos));
  Button := sbNone;
  if (Pt.X > Width - SBSIZE - 1) and (ScrollBars <> ssHorizontal) then
  begin
    Kind := sbVertical;
    I := Height - 2;
    if ScrollBars = ssBoth then Dec(I, SBSIZE);
    if FDownButton = sbButton then
    begin
      Count := CnGetScrollCount(Self, FDownButton, I, Pt.Y, Kind);
      if Count <> 0 then
        ScrollMemo(Self, Count, Kind);
    end else
      if Pt.Y < SBSIZE + 1 then Button := sbUp else
        if Pt.Y > I - SBSIZE then Button := sbDown else
        begin
          I1 := SBSIZE + 1;
          if CnGetScrollInfo(Self, I, I1, I2, Kind) and (Pt.Y > I1) and
            (Pt.Y < I1 + I2) then
            Button := sbButton;
        end;
  end else
  if (Pt.Y > Height - SBSIZE - 1) and (ScrollBars <> ssVertical) then
  begin
    Kind := sbHorizontal;
    I := Width - 2;
    if ScrollBars = ssBoth then Dec(I, SBSIZE);
    if FDownButton = sbButtonH then
    begin
      Count := CnGetScrollCount(Self, sbButton, I, Pt.X, Kind);
      if Count <> 0 then
      begin
        Pos := GetScrollPos(Handle, 0);
        if Count > 0 then
        begin
          ScrollMemo(Self, 1, Kind);
          CharWidth := GetScrollPos(Handle, Ord(Kind)) - Pos;
          Dec(Count, CharWidth);
        end else
        begin
          ScrollMemo(Self, -1, Kind);
          CharWidth := Pos - GetScrollPos(Handle, Ord(Kind));
          Inc(Count, CharWidth);
        end;
        ScrollMemo(Self, Count div CharWidth, Kind);
      end;
    end else
      if Pt.X < SBSIZE + 1 then Button := sbLeft else
        if Pt.X > I - SBSIZE then Button := sbRight else
        begin
          I1 := SBSIZE + 1;
          if CnGetScrollInfo(Self, I, I1, I2, Kind) and (Pt.X > I1) and
            (Pt.X < I1 + I2) then Button := sbButtonH;
        end;
  end else
    Exit;
  Message.Result := HTBORDER;
  if Button <> FOverButton then
  begin
    FOverButton := Button;
    DrawMemo(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinMemo.WMNCPaint(var Message: TMessage);
begin
  if CnSkinThemes.Active then
    DrawMemo(Self, FOverButton, FDownButton) else inherited;
end;

procedure TCnSkinMemo.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if CnSkinThemes.Active then
    DrawMemo(Self, FOverButton, FDownButton);
end;

procedure TCnSkinMemo.WMTimer(var Message: TWMTimer);
var
  Kind: TScrollBarKind;
  Count: Integer;
begin
  if FDownButton in [sbUp, sbDown] then
    Kind := sbVertical else Kind := sbHorizontal;
  Count := CnGetScrollCount(Self, FDownButton, -1, -1, Kind);
  if (FDownButton <> sbNone) and (Count <> 0) then
    ScrollMemo(Self, Count, Kind) else
    KillTimer(Handle, Message.TimerID);
end;

{TCnSkinCombo}

constructor TCnSkinComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
end;

destructor TCnSkinComboBox.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCnSkinComboBox.SetButtonState(Value: TButtonState);
begin
  if (Value = bsOver) and (FButtonState = bsDown) and (GetKeyState(VK_LBUTTON) < 0) then Exit;
  if (Value <> FButtonState) then
  begin
    FButtonState := Value;
    Repaint;
  end;
end;

procedure TCnSkinComboBox.CMThemeChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSkinComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCnSkinComboBox.CMMouseLeave(var Message: TMessage);
begin
  SetButtonState(bsDefault);
end;

procedure TCnSkinComboBox.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if CnSkinThemes.Active then
    if Enabled then
      DrawCombo(Self, FButtonState) else
      DrawCombo(Self, bsDisabled);
end;

procedure TCnSkinComboBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FButtonState <> bsDefault) then
    SetButtonState(bsDown);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCnSkinComboBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (X > Width - GetSystemMetrics(SM_CXHTHUMB) -4) and (X < Width -2) then
    SetButtonState(bsOver) else SetButtonState(bsDefault);
  inherited MouseMove(Shift, X, Y);
end;

procedure TCnSkinComboBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FButtonState <> bsDefault) then
     SetButtonState(bsOver);
  inherited MouseUp(Button, Shift, X, Y);
end;

{TCnSkinButton}

constructor TCnSkinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
end;

destructor TCnSkinButton.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCnSkinButton.SetButtonState(Value: TButtonState);
begin
  if (Value = bsOver) and (FButtonState = bsDown) and (GetKeyState(VK_LBUTTON) < 0) then Exit;
  if (Value <> FButtonState) then
  begin
    FButtonState := Value;
    Repaint;
  end;
end;

procedure TCnSkinButton.CMThemeChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSkinButton.CMMouseEnter(var Message: TMessage);
begin
  SetButtonState(bsOver);
end;

procedure TCnSkinButton.CMMouseLeave(var Message: TMessage);
begin
  SetButtonState(bsDefault);
end;

procedure TCnSkinButton.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if CnSkinThemes.Active then
    if Enabled then
      DrawButton(Self, FButtonState) else
      DrawButton(Self, bsDisabled);
end;

procedure TCnSkinButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then SetButtonState(bsDown);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCnSkinButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FButtonState <> bsDefault) then
     SetButtonState(bsOver);
  inherited MouseUp(Button, Shift, X, Y);
end;

{TCnSkinCheck}

constructor TCnSkinCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
end;

destructor TCnSkinCheckBox.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCnSkinCheckBox.SetButtonState(Value: TButtonState);
begin
  if (Value = bsOver) and (FButtonState = bsDown) and (GetKeyState(VK_LBUTTON) < 0) then Exit;
  if (Value <> FButtonState) then
  begin
    FButtonState := Value;
    Repaint;
  end;
end;

procedure TCnSkinCheckBox.CMThemeChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSkinCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCnSkinCheckBox.CMMouseEnter(var Message: TMessage);
begin
  SetButtonState(bsOver);
end;

procedure TCnSkinCheckBox.CMMouseLeave(var Message: TMessage);
begin
  SetButtonState(bsDefault);
end;

procedure TCnSkinCheckBox.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if CnSkinThemes.Active then
    if Enabled then
      DrawCheckBox(Self, FButtonState) else
      DrawCheckBox(Self, bsDisabled);
end;

procedure TCnSkinCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then SetButtonState(bsDown);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCnSkinCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FButtonState <> bsDefault) then
     SetButtonState(bsOver);
  inherited MouseUp(Button, Shift, X, Y);
end;

{TCnSkinRadio}

constructor TCnSkinRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
end;

destructor TCnSkinRadioButton.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCnSkinRadioButton.SetButtonState(Value: TButtonState);
begin
  if (Value = bsOver) and (FButtonState = bsDown) and (GetKeyState(VK_LBUTTON) < 0) then Exit;
  if (Value <> FButtonState) then
  begin
    FButtonState := Value;
    Repaint;
  end;
end;

procedure TCnSkinRadioButton.CMThemeChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSkinRadioButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCnSkinRadioButton.CMMouseEnter(var Message: TMessage);
begin
  SetButtonState(bsOver);
end;

procedure TCnSkinRadioButton.CMMouseLeave(var Message: TMessage);
begin
  SetButtonState(bsDefault);
end;

procedure TCnSkinRadioButton.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if CnSkinThemes.Active then
    if Enabled then
      DrawRadio(Self, FButtonState) else
      DrawRadio(Self, bsDisabled);
end;

procedure TCnSkinRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then SetButtonState(bsDown);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCnSkinRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  if (Button = mbLeft) and (FButtonState <> bsDefault) then
  begin
    SetButtonState(bsOver);
    for I := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[I] is TCnSkinRadioButton then
        Parent.Controls[I].Repaint;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

{TCnSkinListBox}

constructor TCnSkinListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
  FOverButton := sbNone;
  FDownButton := sbNone;
  FScrollBarVisible := False;
end;

destructor TCnSkinListBox.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCnSkinListBox.CMThemeChange(var Message: TMessage);
begin
  RecreateWnd;
end;

procedure TCnSkinListBox.CMMouseLeave(var Message: TMessage);
begin
  if FOverButton <> sbNone then
  begin
    FOverButton := sbNone;
    FDownButton := sbNone;
    DrawListBox(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinListBox.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  if CnSkinThemes.Active then
  begin
    with Message.CalcSize_Params^ do
    begin
      Inc(rgrc[0].Left);
      Inc(rgrc[0].Top);
      Dec(rgrc[0].Right);
      if FScrollBarVisible then Dec(rgrc[0].Right, SBSIZE);
      Dec(rgrc[0].Bottom)
    end;
  end else inherited;
end;

procedure TCnSkinListBox.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  Pt: TPoint;
  Count: Integer;
begin
  inherited;
  if FScrollBarVisible then
  begin
    FDownButton := FOverButton;
    if FDownButton <> sbButton then
    begin
      Pt := ScreenToClient(Point(Message.XCursor, Message.YCursor));
      Count := CnGetScrollCount(Self, FDownButton, Height - 2, Pt.Y, sbVertical);
      if Count <> 0 then
      begin
        TopIndex := TopIndex + Count;
        SetTimer(Handle, 1000, 100, nil);
      end else
        DrawListBox(Self, FOverButton, FDownButton);
    end else
      DrawListBox(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinListBox.WMNCLButtonUp(var Message: TWMNCLButtonUp);
begin
  inherited;
  if FScrollBarVisible and (FDownButton <> sbNone) then
  begin
    FDownButton := sbNone;
    DrawListBox(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinListBox.WMNCHittest(var Message: TWMNCHittest);
var
  Pt: TPoint;
  Button: TScrollBarButton;
  Count, I1, I2: Integer;
begin
  inherited;
  if not FScrollBarVisible then Exit;
  Pt := ScreenToClient(Point(Message.XPos, Message.YPos));
  if Pt.X > Width - SBSIZE - 1 then
  begin
    Message.Result := HTBORDER;
    if FDownButton = sbButton then
    begin
      Count := CnGetScrollCount(Self, FDownButton, Height - 2, Pt.Y, sbVertical);
      if Count <> 0 then
        TopIndex := TopIndex + Count;
    end else
    begin
      Button := sbNone;
      if Pt.Y < SBSIZE + 1 then Button := sbUp else
        if Pt.Y > Height - SBSIZE - 1 then Button := sbDown else
        begin
          I1 := SBSIZE + 1;
          if CnGetScrollInfo(Self, Height - 2, I1, I2, sbVertical) and
            (Pt.Y > I1) and (Pt.Y < I1 + I2) then Button := sbButton;
        end;
      if Button <> FOverButton then
      begin
        FOverButton := Button;
        DrawListBox(Self, FOverButton, FDownButton);
      end;
    end;
  end;
end;

procedure TCnSkinListBox.WMNCPaint(var Message: TMessage);
begin
  if CnSkinThemes.Active then
    DrawListBox(Self, FoverButton, FDownButton) else
    inherited;
end;

procedure TCnSkinListBox.WMPaint(var Message: TWMPaint);
var
  SV: Boolean;
begin
  inherited;
  if CnSkinThemes.Active then
  begin
    SV := Items.Count > ClientHeight div ItemHeight;
    if SV <> FScrollBarVisible then
    begin
      FScrollBarVisible := SV;
      RecreateWnd;
    end;
    if SV then DrawListBox(Self, FDownButton, FOverButton);
  end;
end;

procedure TCnSkinListBox.WMTimer(var Message: TWMTimer);
var
  Count: Integer;
begin
  Count := CnGetScrollCount(Self, FDownButton, -1, -1, sbVertical);
  if (FDownButton <> sbNone) and (Count <> 0) then
    TopIndex := TopIndex + Count else
    KillTimer(Handle, Message.TimerID);
end;

{TCnSkinScrollBar}

constructor TCnSkinScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
  FOverButton := sbNone;
  FDownButton := sbNone;
end;

destructor TCnSkinScrollBar.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinScrollBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TCnSkinScrollBar.Change;
begin
  inherited;
  DrawScrollBar(Self, FOverButton, FDownButton);
end;

procedure TCnSkinScrollBar.CMThemeChange(var Message: TMessage);
begin
  RecreateWnd;
end;

procedure TCnSkinScrollBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  DrawScrollBar(Self, FOverButton, FDownButton);
end;

procedure TCnSkinScrollBar.CMMouseLeave(var Message: TMessage);
begin
  if FOverButton <> sbNone then
  begin
    FOverButton := sbNone;
    DrawScrollBar(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinScrollBar.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  if CnSkinThemes.Active then
  begin
    with Message.CalcSize_Params^ do
    begin
      Inc(rgrc[0].Top, Height);
      Inc(rgrc[0].Left, Width);
    end;
  end else inherited;
end;

procedure TCnSkinScrollBar.WMNCLButtonDown(var Message: TWMNCLButtonDown);
var
  Pt: TPoint;
begin
  inherited;
  if CnSkinThemes.Active then
  begin
    FDownButton := FOverButton;
    case FDownButton of
      sbUp, sbLeft:
        if Position > Min then
        begin
          Position := Position - 1;
          SetTimer(Handle, 1000, 100, nil);
        end else
          DrawScrollBar(Self, FOverButton, FDownButton);

      sbDown, sbRight:
        if Position < Max then
        begin
          Position := Position + 1;
          SetTimer(Handle, 1000, 100, nil);
        end else
          DrawScrollBar(Self, FOverButton, FDownButton);

      sbNone:
      begin
        Pt := Parent.ScreenToClient(Point(Message.XCursor, Message.YCursor));
        if Kind = sbHorizontal then
          Position := (Pt.X - Left - Height) * Max div (Width - Height - Height)
        else
          Position := (Pt.Y - Top - Width) * Max div (Height - Width - Width);
      end;
    else
      DrawScrollBar(Self, FOverButton, FDownButton);
    end;
  end;
end;

procedure TCnSkinScrollBar.WMNCLButtonUp(var Message: TWMNCLButtonUp);
begin
  inherited;
  if FDownButton <> sbNone then
  begin
    FDownButton := sbNone;
    DrawScrollBar(Self, FOverButton, FDownButton);
  end;
end;

procedure TCnSkinScrollBar.WMNCHittest(var Message: TWMNCHittest);
var
  Pt: TPoint;
  Button: TScrollBarButton;
  I, I1, I2: Integer;
begin
  inherited;
  if CnSkinThemes.Active then
  begin
    Pt := Parent.ScreenToClient(Point(Message.XPos, Message.YPos));
    Button := sbNone;
    if Kind = sbHorizontal then
    begin
      Dec(Pt.X, Left);
      if FDownButton = sbButton then
        Position := (Pt.X - Height) * Max div (Width - Height - Height) else
        if Pt.X < Height then Button := sbLeft else
          if Pt.X > Width - Height then Button := sbRight else
          begin
            I := Width - Height - Height;
            if PageSize > 0 then
            begin
               I2 := I * PageSize div Max;
               if I2 < 10 then I2 := 10;
            end else
              I2 := Height;
            I1 := (I - I2) * Position div Max + Height;
            if (Pt.X > I1) and (Pt.X < I1 + I2) then
              Button := sbButton;
          end;
    end else
    begin
      Dec(Pt.Y, Top);
      if FDownButton = sbButton then
        Position := (Pt.Y - Width) * Max div (Height - Width - Width) else
        if Pt.Y < Width then Button := sbUp else
          if Pt.Y > Height - Width then Button := sbDown else
          begin
            I := Height - Width - Width;
            if PageSize > 0 then
            begin
              I2 := I * PageSize div Max;
              if I2 < 10 then I2 := 10;
            end else
              I2 := Width;
            I1 := (I - I2) * Position div Max + Width;
            if (Pt.Y > I1) and (Pt.Y < I1 + I2) then
              Button := sbButton;
          end;
    end;
    if Button <> FOverButton then
    begin
      FOverButton := Button;
      DrawScrollBar(Self, FOverButton, FDownButton);
    end;
    Message.Result := HTBORDER;
  end;
end;

procedure TCnSkinScrollBar.WMNCPaint(var Message: TMessage);
begin
  if CnSkinThemes.Active then
    DrawScrollBar(Self, FOverButton, FDownButton) else inherited;
end;

procedure TCnSkinScrollBar.WMTimer(var Message: TWMTimer);
begin
  case FDownButton of
    sbUp, sbLeft:
      if Position > Min then Position := Position - 1 else
        KillTimer(Handle, Message.TimerID);

    sbDown, sbRight:
      if Position < Max then Position := Position + 1 else
        KillTimer(Handle, Message.TimerID);

    else
      KillTimer(Handle, Message.TimerID);
  end;
end;

{TCnSkinStatic}

constructor TCnSkinStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnSkinThemes.Controls.Add(Self);
  FHotColor := clBlue;
end;

destructor TCnSkinStaticText.Destroy;
begin
  CnSkinThemes.Controls.Remove(Self);
  inherited Destroy;
end;


procedure TCnSkinStaticText.SetUnderline(Value: TUnderline);
begin
  if Value <> FUnderline then
  begin
    FUnderline := Value;
    Repaint;
  end;
end;

procedure TCnSkinStaticText.CMThemeChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSkinStaticText.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHot := True;
    Repaint;
  end;
end;

procedure TCnSkinStaticText.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FHot := False;
    Repaint;
  end;
end;

procedure TCnSkinStaticText.WMPaint(var Message: TWMPaint);
const
  Flags: array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  X, W: Integer;
  Canvas: TControlCanvas;
  R: TRect;
begin
  inherited;
  Canvas := TControlCanvas.Create;
  Canvas.Control := Self;
  try
    Canvas.Font := Font;
    Canvas.Brush.Style := bsClear;
    if FHot then  Canvas.Font.Color := FHotColor;
    R := ClientRect;
    DrawText(Canvas.Handle, PChar(Caption), -1, R, Flags[Alignment]);
    if (FUnderline = ulAlways) or ((FUnderline = ulHot) and FHot) then
    begin
      X := 0;
      W := Canvas.TextWidth(Caption);
      case Alignment of
        taCenter: X := (Width - W) div 2;
        taRightJustify: X := Width - W;
      end;
      Canvas.Pen.Color := Canvas.Font.Color;
      Canvas.MoveTo(X, Height - 1);
      Canvas.LineTo(X + W, Height - 1);
    end;  
  finally
    Canvas.Free;
  end;
end;

end.
