{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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

unit CnMemo;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：有行号显示功能的 Memo
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：该单元当前仅为内部参考测试用
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：PWinXP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2015.07.26 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Classes, Messages, Controls, Graphics, StdCtrls, ExtCtrls,
  Dialogs;

type
  TCnMemo = class(TMemo)
  private
    FOriginMargin: Integer;
    FLineHeight: Integer;
    FLineNumberRegionWidth: Integer;
    FVisibleLineStart: Integer;        // 可见区域顶部的行号，1 开始
    FVisibleLineEnd: Integer;          // 可见区域底部的行号，1 开始
    FVislbleLineDigitCount: Integer;
    FLineGutter: TCustomPanel; // Impelmentation is TCnLineGutter

    FHighlightLine: Boolean;
    FShowLineNumber: Boolean;
    FLineNumberRightMargin: Integer;
    FLineNumberLeftMargin: Integer;
    FHighlightBkColor: TColor;
    FLineNumberBkColor: TColor;
    FLineNumberColor: TColor;
    FHighlightNumber: Boolean;
    FHighlightNumberColor: TColor;
    procedure SetHighlightBkColor(const Value: TColor);
    procedure SetHighlightLine(const Value: Boolean);
    procedure SetLineNumberBkColor(const Value: TColor);
    procedure SetLineNumberColor(const Value: TColor);
    procedure SetLineNumberRightMargin(const Value: Integer);
    procedure SetLineNumberLeftMargin(const Value: Integer);
    procedure SetShowLineNumber(const Value: Boolean);
    procedure SetHighlightNumber(const Value: Boolean);
    procedure SetHighlightNumberColor(const Value: TColor);

    procedure CalcLineDigits;
    function CalcLineHeight: Integer;
    procedure LinePaint(Sender: TObject);
    procedure UpdateMargins; // 计算并设置 Memo 内的边距
    procedure UpdateMemoEditRect;
    procedure InvalidateGutter;
  protected
    procedure CreateWnd; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure Change; override;
    procedure WndProc(var Message: TMessage); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FirstVisibleLine: Integer;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  published
    property ShowLineNumber: Boolean read FShowLineNumber write SetShowLineNumber;
    {* 是否显示左边的行号区域}
    property HighlightNumber: Boolean read FHighlightNumber write SetHighlightNumber;
    {* 是否高亮显示行号区域中的当前行号}
    property LineNumberColor: TColor read FLineNumberColor write SetLineNumberColor;
    {* 行号区域的行号文字显示颜色}
    property LineNumberBkColor: TColor read FLineNumberBkColor write SetLineNumberBkColor;
    {* 行号区域的背景色}
    property HighlightNumberColor: TColor read FHighlightNumberColor write SetHighlightNumberColor;
    {* 行号区域当前行的文字显示颜色}
    property LineNumberLeftMargin: Integer read FLineNumberLeftMargin write SetLineNumberLeftMargin;
    {* 行号区域左边距}
    property LineNumberRightMargin: Integer read FLineNumberRightMargin write SetLineNumberRightMargin;
    {* 行号区域右边距}
    property HighlightLine: Boolean read FHighlightLine write SetHighlightLine;
    {* 是否高亮当前行背景，未实现}
    property HighlightBkColor: TColor read FHighlightBkColor write SetHighlightBkColor;
    {* 高亮当前行的背景色，未实现}
  end;

implementation

const
  csDefaultLineNumberBkColor = clBtnface;
  csDefaultLineNumberHighlightColor = clRed;
  csDefaultLineNumberColor = clBtnText;

  csInternalMargin = 1;
  csDefaultMargin = 5;

type
  TCnLineGutter = class(TCustomPanel)
  private
    FOnPaint: TNotifyEvent;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure Paint; override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

{ TCnMemo }

constructor TCnMemo.Create(AOwner: TComponent);
begin
  inherited;
  FShowLineNumber := False;
  FHighlightNumber := True;
  FLineNumberRightMargin := csDefaultMargin;
  FLinenumberLeftMargin := csDefaultMargin;

  FLineNumberColor := csDefaultLineNumberColor;
  FLineNumberBkColor := csDefaultLineNumberBkColor;
  FHighlightNumberColor := csDefaultLineNumberHighlightColor;

  FLineGutter := TCnLineGutter.Create(Self);
  TCnLineGutter(FLineGutter).OnPaint := LinePaint;
end;

destructor TCnMemo.Destroy;
begin
  inherited;

end;

procedure TCnMemo.SetHighlightBkColor(const Value: TColor);
begin
  if FHighlightBkColor <> Value then
  begin
    FHighlightBkColor := Value;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.SetHighlightLine(const Value: Boolean);
begin
  if FHighlightLine then
  begin
    FHighlightLine := Value;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.SetHighlightNumber(const Value: Boolean);
begin
  if FHighlightNumber <> Value then
  begin
    FHighlightNumber := Value;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.SetHighlightNumberColor(const Value: TColor);
begin
  if FHighlightNumberColor <> Value then
  begin
    FHighlightNumberColor := Value;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.SetLineNumberBkColor(const Value: TColor);
begin
  if FLineNumberBkColor <> Value then
  begin
    FLineNumberBkColor := Value;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.SetLineNumberColor(const Value: TColor);
begin
  if FLineNumberColor <> Value then
  begin
    FLineNumberColor := Value;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.SetLineNumberRightMargin(const Value: Integer);
begin
  if FLineNumberRightMargin <> Value then
  begin
    FLineNumberRightMargin := Value;
    UpdateMargins;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.SetLineNumberLeftMargin(const Value: Integer);
begin
  if FLineNumberLeftMargin <> Value then
  begin
    FLineNumberLeftMargin := Value;
    UpdateMargins;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.SetShowLineNumber(const Value: Boolean);
begin
  if FShowLineNumber <> Value then
  begin
    FShowLineNumber := Value;
    CalcLineDigits;
    UpdateMargins;
    FLineGutter.Visible := Value;
    Invalidate;
    InvalidateGutter;
  end;
end;

procedure TCnMemo.Change;
begin
  CalcLineDigits;
  InvalidateGutter;
  inherited;
end;

procedure TCnMemo.CalcLineDigits;
var
  DigitCount: Integer;
begin
  if HandleAllocated and FShowLineNumber then
  begin
    if FLineHeight = 0 then
      FLineHeight := CalcLineHeight;

    FVisibleLineStart := SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0) + 1;
    FVisibleLineEnd := ((ClientRect.Bottom - ClientRect.Top) div FLineHeight) + FVisibleLineStart;
    if FVisibleLineEnd > Lines.Count then
      FVisibleLineEnd := Lines.Count;

    DigitCount := Length(IntToStr(FVisibleLineEnd));
    if DigitCount <> FVislbleLineDigitCount then
    begin
      FVislbleLineDigitCount := DigitCount;
      UpdateMargins;
    end;
  end;
end;

procedure TCnMemo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FLineHeight := CalcLineHeight;
  UpdateMargins;
  Invalidate;
end;

function TCnMemo.FirstVisibleLine: Integer;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0)
  else
    Result := -1;
end;

procedure TCnMemo.WMMouseWheel(var Message: TMessage);
begin
  inherited;
  CalcLineDigits;
  InvalidateGutter;
end;

procedure TCnMemo.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  CalcLineDigits;
  InvalidateGutter;
end;

procedure TCnMemo.UpdateMargins;
var
  LineWidth: Integer;
  ACanvas: TControlCanvas;
  S: string;
begin
  if not ShowLineNumber then
  begin
    FLineNumberRegionWidth := 0;
    UpdateMemoEditRect;
    Exit;
  end;

  if not HandleAllocated then
    Exit;

  if FVislbleLineDigitCount > 0 then
  begin
    SetLength(S, FVislbleLineDigitCount);
    FillChar(S[1], Length(S), Ord('9'));

    ACanvas := TControlCanvas.Create;
    ACanvas.Control := Self;
    ACanvas.Font := Font;
    LineWidth := ACanvas.TextWidth(S);
    ACanvas.Free;
  end
  else
    LineWidth := 0;

  FLineNumberRegionWidth := FLineNumberLeftMargin + FLineNumberRightMargin + LineWidth;
  FLineGutter.Width := FLineNumberRegionWidth;
  UpdateMemoEditRect;
end;

function TCnMemo.CalcLineHeight: Integer;
const
  csAlphaText = 'abcdefghijklmnopqrstuvwxyz';
var
  LogFont: TLogFont;
  DC: HDC;
  SaveFont: HFONT;
  AHandle: THandle;
  TM: TEXTMETRIC;
  ASize: TSize;
begin
  Result := 0;
  if GetObject(Font.Handle, SizeOf(LogFont), @LogFont) <> 0 then
  begin
    DC := CreateCompatibleDC(0);
    SaveFont := 0;
    try
      AHandle := CreateFontIndirect(LogFont);
      AHandle := SelectObject(DC, AHandle);
      if SaveFont = 0 then
        SaveFont := AHandle
      else if AHandle <> 0 then
        DeleteObject(AHandle);

      GetTextMetrics(DC, TM);
      GetTextExtentPoint(DC, csAlphaText, Length(csAlphaText), ASize);

      // 取文本高度
      if TM.tmHeight + TM.tmExternalLeading > Result then
        Result := TM.tmHeight + TM.tmExternalLeading;
      if ASize.cy > Result then
        Result := ASize.cy;
    finally
      SaveFont := SelectObject(DC, SaveFont);
      if SaveFont <> 0 then
        DeleteObject(SaveFont);
      DeleteDC(DC);
    end;
  end;
end;

procedure TCnMemo.WndProc(var Message: TMessage);
var
  Canvas: TControlCanvas;
  I: Integer;
  X, Y: Integer;
  R: TRect;
begin
  if Message.Msg = WM_PAINT then
  begin
    if FHighlightLine then
    begin
      Canvas := TControlCanvas.Create;
      Canvas.Control := Self;
      Canvas.Brush.Color := FHighlightBkColor;

      Canvas.Free;
    end;
    // InvalidateGutter;
  end
  else if (Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST) then
  begin
    CalcLineDigits;
    FLineGutter.Invalidate;
  end;
//  else if (Message.Msg = EM_SCROLL) or (Message.Msg = EM_LINESCROLL) or (Message.Msg = EM_SCROLLCARET) then
//  begin
//    case Message.Msg of
//      EM_SCROLL: ShowMessage('SCROLL');
//      EM_LINESCROLL: ShowMessage('EM_LINESCROLL');
//      EM_SCROLLCARET: ShowMessage('EM_SCROLLCARET');
//    end;
//  end;   // 拦截不着

  inherited WndProc(Message);
end;

procedure TCnMemo.LinePaint(Sender: TObject);
const
  INTERNAL_TOPMARGIN = 5;
var
  Canvas: TControlCanvas;
  I, Line, Cur: Integer;
  S: string;
  X, Y: Integer;
  R: TRect;
begin
  if FShowLineNumber then
  begin
    Canvas := TControlCanvas.Create;
    Canvas.Control := Self;
    Canvas.Font := Font;

    Canvas.Brush.Color := FLineNumberBkColor;
    Canvas.Brush.Style := bsSolid;

    R := Rect(0, 0, FLineNumberRegionWidth, ClientHeight);
    Canvas.FillRect(R);

    if FHighlightNumber then
      Cur := CaretPos.y + 1
    else
      Cur := -1;

    for I := FVisibleLineStart to FVisibleLineEnd do
    begin
      Line := I - FVisibleLineStart;
      R := Rect(0, FLineHeight * Line + INTERNAL_TOPMARGIN, FLineNumberRegionWidth - FLineNumberRightMargin,
        FLineHeight * (Line + 1) + INTERNAL_TOPMARGIN);
      S := IntToStr(I);
      if I = Cur then
        Canvas.Font.Color := FHighlightNumberColor
      else
        Canvas.Font.Color := FLineNumberColor;

      DrawText(Canvas.Handle, PChar(S), -1, R, DT_RIGHT or DT_BOTTOM);
    end;

    Canvas.Free;
  end;
end;

procedure TCnMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  CalcLineDigits;
  InvalidateGutter;
end;

procedure TCnMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  CalcLineDigits;
  InvalidateGutter;
end;

procedure TCnMemo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  UpdateMargins;
  UpdateMemoEditRect;
  if FLineGutter <> nil then
    FLineGutter.SetBounds(0, 0, FLineNumberRegionWidth, Height);
end;

procedure TCnMemo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FLineGutter <> nil then
  begin
    if AParent <> nil then
    begin
      FLineGutter.Parent := Self;
      FLineGutter.BringToFront;
    end;
  end;
end;

procedure TCnMemo.UpdateMemoEditRect;
var
  R: TRect;
begin
  if not HandleAllocated then
    Exit;

  // SendMessage(Handle, EM_GETRECT, 0, LongInt(@R));
  R.Top := csDefaultMargin - csInternalMargin;
  R.Right := ClientWidth;
  R.Bottom := ClientHeight;

  if FShowLineNumber then
  begin
    // SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN,
    //  MakeLParam(FLineNumberRegionWidth + csInternalMargin, 0))
    R.Left := FLineNumberRegionWidth + csInternalMargin;
  end
  else
  begin
    // SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN, 0);
    R.Left := csInternalMargin;
  end;

  SendMessage(Handle, EM_SETRECT, 0, LongInt(@R));
end;

procedure TCnMemo.CreateWnd;
begin
  inherited;
  FOriginMargin := SendMessage(Handle, EM_GETMARGINS, 0, 0);

  // TODO: 挂接以得到 SCROLL 事件通知
end;

procedure TCnMemo.InvalidateGutter;
begin
  if FLineGutter <> nil then
    if FLineGutter.Visible then
      FLineGutter.Invalidate;
end;

{ TCnLineGutter }

constructor TCnLineGutter.Create(AOwner: TComponent);
begin
  inherited;
//  DoubleBuffered := True;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  BevelWidth := 1;
  Caption := '';
end;

destructor TCnLineGutter.Destroy;
begin
  inherited;

end;

procedure TCnLineGutter.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

end.
