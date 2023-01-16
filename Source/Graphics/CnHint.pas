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

unit CnHint;
{* |<PRE>
================================================================================
* 软件名称：CnPack 界面组件包
* 单元名称：CnHint 控件单元
* 单元作者：
* 备    注：部分参考自网上佚名代码
* 开发平台：PWinXP + Delphi 7.0
* 兼容测试：PWin9X/2000/XP + Delphi 7.0
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.01.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Graphics, Classes, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Math;

const
  CN_MSG_HINT_NOTIFY = WM_USER + $0357;

type
  TCnHint = class;

  TVAlignment = (vtaTopJustify, vtaBottomJustify, vtaCenter);

  THintStyle = (hsNormal, hsBalloonHint, hsAuto);

  THintPosition = (hpUpLeft, hpUpRight, hpDownLeft, hpDownRight);

  THintBeforeEvent = procedure(AHint: TCnHint; Rect: TRect; Text: string) of object;

  THintOwnerDraw = procedure(AHint: TCnHint; Canvas: TCanvas; Rect: TRect; Text: string) of object;

  THintMeasureRect = procedure(AHint: TCnHint; var Rect: TRect; Text: string) of object;

  TCnHint = class(TComponent)
  {* 控制所有 Hint 风格的控制组件}
  private
    FAlignment: TAlignment;
    FBackColor: TColor;
    FOnBeforeHint: THintBeforeEvent;
    FBorderColor: TColor;
    FHintPosition: THintPosition;
    FFont: TFont;
    FGlyph: TBitmap;
    FHintStyle: THintStyle;
    FOnMeasureRect: THintMeasureRect;
    FOnOwnerDraw: THintOwnerDraw;
    FTitle: string;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure GlyphChange(Sender: TObject);
    procedure SetFont(const Value: TFont);
    procedure SetGlyph(const Value: TBitmap);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure UpdateHintWindowFont;
  published
    property Alignment: TAlignment read FAlignment write FAlignment default taLeftJustify;
    property BackColor: TColor read FBackColor write FBackColor default $F0A07D;
    property BorderColor: TColor read FBorderColor write FBorderColor default clWhite;
    property HintPosition: THintPosition read FHintPosition write FHintPosition default hpDownRight;
    property Font: TFont read FFont write SetFont;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property HintStyle: THintStyle read FHintStyle write FHintStyle;
    property Title: string read FTitle write FTitle;
    property OnBeforeHint: THintBeforeEvent read FOnBeforeHint write FOnBeforeHint;
    property OnMeasureRect: THintMeasureRect read FOnMeasureRect write FOnMeasureRect;
    property OnOwnerDraw: THintOwnerDraw read FOnOwnerDraw write FOnOwnerDraw;
  end;

  TCnInternalHintWindow = class(THintWindow)
  {* 实际用来显示的 HintWindow，不能作为组件注册到组件板上}
  private
    FTimer: TTimer;
    FBitmap: TBitmap;
    FLastActive: Cardinal;
    FHint: TCnHint;
    FHintPosition: THintPosition;
    FModified: Boolean;
    FGlyph: TBitmap;
    FUpdating: Boolean;
    FAlignment: TAlignment;
    FHintStyle: THintStyle;
    FOnCancelHint: TNotifyEvent;
    FFirstLineAsTitle: Boolean;
    procedure GlyphChange(Sender: TObject);
    function FindCnHint: TCnHint;
    function GetTextRect(ACanvas: TCanvas; Text: string; R: TRect;
      HAlign: TAlignment; VAlign: TVAlignment): TRect;
    procedure DrawHintText(Canvas: TCanvas; R: TRect; const AText: string;
      IsBalloonHint: Boolean);
    function GetHintPosition(WorkRect: TRect; AWidth, AHeight: Integer;
      Pos: TPoint; IsBalloonHint: Boolean): THintPosition;
    procedure SetPosition(const Value: THintPosition);
    procedure SetGlyph(const Value: TBitmap);
    procedure HintNotify(var message: TMessage); message CN_MSG_HINT_NOTIFY;
    procedure HintTimer(Sender: TObject);
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoCancelHint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintFromPos(const HintPos: TPoint; const AHint: string;
      const ATitle: string = ''; HidePause: Integer = 0);
    property HintPosition: THintPosition read FHintPosition write SetPosition;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property HintStyle: THintStyle read FHintStyle write FHintStyle;
    property OnCancelHint: TNotifyEvent read FOnCancelHint write FOnCancelHint;
  end;

  TCnHintWindow = class(TComponent)
  {* 封装一 CnInternalHintWindow 的组件}
  private
    FHintWindow: TCnInternalHintWindow;
    FOnCancelHint: TNotifyEvent;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetPosition(const Value: THintPosition);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetHintStyle(const Value: THintStyle);
    function GetAlignment: TAlignment;
    function GetGlyph: TBitmap;
    function GetHintStyle: THintStyle;
    function GetHintPosition: THintPosition;
  protected
    procedure HintWindowCancelHint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ActivateHint(const AHint: string; const ATitle: string);
    procedure ActivateHintFromPos(const HintPos: TPoint; const AHint: string;
      const ATitle: string = ''; HidePause: Integer = 5000);
    procedure ReleaseHandle;
  published
    property HintPosition: THintPosition read GetHintPosition write SetPosition;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property HintStyle: THintStyle read GetHintStyle write SetHintStyle;
    property OnCancelHint: TNotifyEvent read FOnCancelHint write FOnCancelHint;
  end;

implementation

var
  FCnHints: TList = nil;
  FCnHintWindows: TList = nil;

// 获得 Hint 显示区的具体尺寸
function GetHintRect(ACanvas: TCanvas; const Text: string;
  GlyphWidth, GlyphHeight: Integer; IsBalloonHint, FirstLineAsTitle: Boolean): TRect;
var
  Lines: TStrings;
  I, H, W: Integer;
  Len: Integer;
  Empty: Boolean;
  Added: Boolean;
  First: Boolean;
  OldStyles: TFontStyles;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    H := 0;
    W := 0;
    Empty := GlyphWidth <= 0;
    Added := False;
    First := True;
    OldStyles := ACanvas.Font.Style;

    for I := 0 to Lines.Count - 1 do // 挨个计算每行文字的高度与宽度
    begin
      if FirstLineAsTitle and (I = 0) then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsBold]
      else
        ACanvas.Font.Style := OldStyles;

      Len := ACanvas.TextWidth(Lines[I]);
      if not Empty then
      begin
        if (H <= GlyphHeight) and First then
        begin
          Inc(Len, GlyphWidth + 6); // 有图片的话，宽度加图片宽度加边缘
          First := False;
        end;
        if not Added and (H + 10 >= GlyphHeight) then
        begin
          Added := True;
          Inc(H, 6);
        end;
      end;
      H := H + ACanvas.TextHeight(Lines[I]);
      if W < Len then
        W := Len;
    end;
    if H < GlyphHeight then
      H := GlyphHeight;

    if IsBalloonHint then // 加上把手尖角
    begin
      Inc(H, 18);
      Inc(W, 12);
      if H < 35 then
        H := 35;
      if W < 50 then
        W := 50;
    end
    else
    begin
      Inc(H, 8);
      Inc(W, 12);
    end;
    Result := Classes.Rect(0, 0, W, H);
  finally
    ACanvas.Font.Style := OldStyles;
    Lines.Free;
  end;
end;

function CreateRegion(Mask: TBitmap; TransparentColor: TColor): HRGN;
var
  Dc: HDC;
  Rgn: HRGN;
  X, Y: Integer;
  P: TPoint;
  Line: Boolean;
  color: TColor;
begin
  Dc := Mask.Canvas.Handle;
  BeginPath(Dc);
  for X := 0 to Mask.Width - 1 do
  begin
    Line := False;
    for Y := 0 to Mask.Height - 1 do
    begin
      Color := Mask.Canvas.Pixels[x, y];
      if Color <> TransparentColor then
      begin
        if not Line then
        begin
          Line := True;
          P.x := X;
          P.y := Y;
        end;
      end;
      if (Color = TransparentColor) or (Y = Mask.Height - 1) then
      begin
        if Line then
        begin
          Line := False;
          MoveToEx(Dc, P.x, P.y, nil);
          LineTo(Dc, P.x, Y);
          LineTo(Dc, P.x + 1, Y);
          LineTo(Dc, P.x + 1, P.y);
          CloseFigure(Dc);
        end;
      end;
    end;
  end;
  EndPath(Dc);
  Rgn := PathToRegion(Dc);
  Result := Rgn;
end;

{ TCnInternalHintWindow }

procedure TCnInternalHintWindow.ActivateHint(Rect: TRect; const AHint: string);
begin
  ActivateHintFromPos(Mouse.CursorPos, AHint);
end;

function GetCursorHeightMargin: Integer;
var
  IconInfo: TIconInfo;
  BitmapInfoSize, BitmapBitsSize, ImageSize: DWORD;
  Bitmap: PBitmapInfoHeader;
  Bits: Pointer;
  BytesPerScanline: Integer;

{$IFDEF WIN64}
  function FindScanline(Source: Pointer; MaxLen: Cardinal; Value: Cardinal): Cardinal;
  var
    I: Integer;
    V: Byte;
    P: PByte;
  begin
    //  Pascal Impl.
    Result := MaxLen;
    V := Byte(Value);
    P := PByte(Source);
    for I := MaxLen downto 0 do
    begin
      if P^ = V then
      begin
        Result := I;
        Exit;
      end;
      Inc(P);
    end;
  end;
{$ELSE}
  function FindScanline(Source: Pointer; MaxLen: Cardinal; Value: Cardinal): Cardinal; assembler;
  asm                // EAX              EDX               ECX
        PUSH    ECX
        MOV     ECX, EDX     // MaxLen  -> ECX
        MOV     EDX, EDI
        MOV     EDI, EAX     // Source  -> EDI
        POP     EAX          // Pattern -> EAX
        REPE    SCASB
        MOV     EAX, ECX
        MOV     EDI, EDX
  end;
{$ENDIF}

begin
  Result := GetSystemMetrics(SM_CYCURSOR);
  if GetIconInfo(GetCursor, IconInfo) then
    try
      GetDIBSizes(IconInfo.hbmMask, BitmapInfoSize, BitmapBitsSize);
      Bitmap := AllocMem(DWORD(BitmapInfoSize) + BitmapBitsSize);
      try
        Bits := Pointer(DWORD(Bitmap) + BitmapInfoSize);

        if GetDIB(IconInfo.hbmMask, 0, Bitmap^, Bits^) and (Bitmap^.biBitCount = 1) then
        begin
          with Bitmap^do
          begin
            BytesPerScanline := ((biWidth * biBitCount + 31) and not 31) div 8;
            ImageSize := biWidth * BytesPerScanline;
            Bits := Pointer(DWORD(Bits) + BitmapBitsSize - ImageSize); 
            Result := FindScanline(Bits, ImageSize, $FF);
            if (Result = 0) and (biHeight >= 2 * biWidth) then
              Result := FindScanline(Pointer(DWORD(Bits) - ImageSize), ImageSize, $00);
            Result := Result div BytesPerScanline;
          end;
          Dec(Result, IconInfo.yHotSpot);
        end;
      finally
        FreeMem(Bitmap, BitmapInfoSize + BitmapBitsSize);
      end;
    finally
      if IconInfo.hbmColor <> 0 then
        DeleteObject(IconInfo.hbmColor);
      if IconInfo.hbmMask <> 0 then
        DeleteObject(IconInfo.hbmMask);
    end;
end;

procedure TCnInternalHintWindow.ActivateHintFromPos(const HintPos: TPoint;
  const AHint: string; const ATitle: string; HidePause: Integer);
var
  PS: array[0..2] of TPoint;
  H: HRGN;
  Pos: TPoint;
  R, SaveRect: TRect;
  Posi: THintPosition;
  intW, intH: Integer;
  AWidth, AHeight: Integer;
  Rgn: HRGN;
  cR, cG, cB: Byte;
  Rect: TRect;
  IsBalloonHint: Boolean;
  AHintStyle: THintStyle;
  WorkArea: TRect;
  PT: PPoint;
  I: Integer;

  procedure GetHintRgn;
  var
    TempR: TRect;
    OffY: Integer;
  begin
    R.Right := R.Right - R.Left;
    R.Left := 0;
    R.Bottom := R.Bottom - R.Top;
    R.Top := 0;
    intW := R.Right;
    intH := R.Bottom;
    Pos := HintPos;

    SystemParametersInfo(SPI_GETWORKAREA, 0, @TempR, 0);
    Posi := GetHintPosition(TempR, intW, intH, Pos, IsBalloonHint);
    if IsBalloonHint then
    begin
      case Posi of
        hpUpLeft:
          begin
            OffY := GetCursorHeightMargin;
            Pos.Y := Pos.Y - 6;
            R.Left := Pos.X - (R.Right - 5);
            R.Top := Pos.Y - R.Bottom - OffY;
            R.Right := R.Right + R.Left;
            R.Bottom := R.Top + R.Bottom;
            SaveRect := R;
            PS[0] := Point(R.Right - 16, R.Bottom - 1);
            PS[1] := Point(R.Right - 16, R.Bottom + 15);
            PS[2] := Point(R.Right - 16 - 17, R.Bottom - 2);
            Rect := Classes.Rect(R.Left, R.Top, R.Right, R.Bottom + 16);
          end;
        hpUpRight:
          begin
            OffY := GetCursorHeightMargin;
            Pos.Y := Pos.Y - 6;
            R.Left := Pos.X - 25;
            R.Top := Pos.Y - R.Bottom - OffY;
            R.Right := R.Right + R.Left;
            R.Bottom := R.Top + R.Bottom;
            SaveRect := R;
            PS[0] := Point(R.Left + 16, R.Bottom - 1);
            PS[1] := Point(R.Left + 16, R.Bottom + 16);
            PS[2] := Point(R.Left + 16 + 17, R.Bottom - 1);
            Rect := Classes.Rect(R.Left, R.Top, R.Right, R.Bottom + 16);
          end;
        hpDownLeft:
          begin
            OffY := GetCursorHeightMargin + 10;
            R.Left := Pos.X - (R.Right - 16);
            R.Top := Pos.Y + OffY;
            R.Right := R.Right + R.Left;
            R.Bottom := R.Top + R.Bottom;
            SaveRect := R;
            PS[0] := Point(R.Right - 16, R.Top);
            PS[1] := Point(R.Right - 16, R.Top - 16);
            PS[2] := Point(R.Right - 16 - 17, R.Top + 1);
            Rect := Classes.Rect(R.Left, R.Top - 16, R.Right, R.Bottom);
          end;
        hpDownRight:
          begin
            OffY := GetCursorHeightMargin + 10;
            R.Left := Pos.X - 16;
            R.Top := Pos.Y + OffY;
            R.Right := R.Right + R.Left;
            R.Bottom := R.Top + R.Bottom;
            SaveRect := R;
            PS[0] := Point(R.Left + 16, R.Top);
            PS[1] := Point(R.Left + 16, R.Top - 16);
            PS[2] := Point(R.Left + 16 + 17, R.Top + 1);
            Rect := Classes.Rect(R.Left, R.Top - 16, R.Right, R.Bottom);
          end;

      end;
    end
    else
    begin
      case Posi of
        hpUpLeft:
          begin
            Pos.Y := Pos.Y - 15;
            R.Left := Pos.X - (R.Right + 10);
            R.Top := Pos.Y - R.Bottom - 0;
            R.Right := R.Right + R.Left;
            R.Bottom := R.Top + R.Bottom;
            SaveRect := R;
            Rect := Classes.Rect(R.Left, R.Top, R.Right, R.Bottom);
          end;
        hpUpRight:
          begin
            Pos.Y := Pos.Y - 15;
            R.Left := Pos.X - 10;
            R.Top := Pos.Y - R.Bottom - 0;
            R.Right := R.Right + R.Left;
            R.Bottom := R.Top + R.Bottom;
            SaveRect := R;
            Rect := Classes.Rect(R.Left, R.Top, R.Right, R.Bottom);
          end;
        hpDownLeft:
          begin
            OffY := GetCursorHeightMargin;
            Pos.Y := Pos.Y + OffY;
            R.Left := Pos.X - (R.Right - 0);
            R.Top := Pos.Y + 0;
            R.Right := R.Right + R.Left;
            R.Bottom := R.Top + R.Bottom;
            SaveRect := R;
            Rect := Classes.Rect(R.Left, R.Top, R.Right, R.Bottom);
          end;
        hpDownRight:
          begin
            OffY := GetCursorHeightMargin;
            Pos.Y := Pos.Y + OffY;
            R.Left := Pos.X - 0;
            R.Top := Pos.Y + 0;
            R.Right := R.Right + R.Left;
            R.Bottom := R.Top + R.Bottom;
            SaveRect := R;
            Rect := Classes.Rect(R.Left, R.Top - 0, R.Right, R.Bottom);
          end;
      end;
    end;
  end;

begin
  if FBitmap <> nil then
  begin
    FBitmap.Width := 0;
    FBitmap.Height := 0;
  end
  else
    FBitmap := TBitmap.Create;

  try
    if FHint <> nil then
      if Owner <> nil then
      begin
        for I := 0 to Owner.ComponentCount - 1 do
          if Owner.Components[I] is TCnHint then
          begin
            FHint := Owner.Components[I] as TCnHint;
            Break;
          end;
      end;
      
    if FHint = nil then
      FHint := FindCnHint;

    if FHint = nil then
    begin
      AHintStyle := FHintStyle;
      Canvas.Font.Color := clBlack;
    end
    else
    begin
      AHintStyle := FHint.HintStyle;
      if FGlyph.Empty then
        FGlyph.Assign(FHint.FGlyph);
      Canvas.Font.Assign(FHint.Font);
    end;

    IsBalloonHint := (AHintStyle = hsBalloonHint) or
      ((AHintStyle = hsAuto) and ((ATitle <> '') or ((FHint <> nil) and (FHint.Title <> ''))));

    AWidth := 0;
    AHeight := 0;
    Caption := AHint;

    FFirstLineAsTitle := False;
    if ATitle <> '' then
    begin
      Caption := ATitle + #13#10 + Caption;
      FFirstLineAsTitle := True;
    end
    else
    begin
      if (FHint <> nil) then
        if FHint.Title <> '' then
        begin
          Caption := FHint.Title + #13#10 + Caption;
          FFirstLineAsTitle := True;
        end;
    end;
    // 只有显式指定了 Title、或 FHint.Title 不为空时，
    // 才设置 FFirstLineAsTitle 为 True; 并且 Title 的黑体效果要有图片时才能看出来

    if (FGlyph <> nil) and (FGlyph.Width > 0) and (FGlyph.Height > 0) then
    begin
      AWidth := FGlyph.Width;
      AHeight := FGlyph.Height;
    end;

    R := GetHintRect(Canvas, Caption, AWidth, AHeight, IsBalloonHint, FFirstLineAsTitle);
    GetHintRgn;
    case Posi of
      hpUpLeft:
        begin
          if (FHint <> nil) and (Assigned(FHint.FOnMeasureRect)) then
          begin
            FHint.FOnMeasureRect(FHint, R, Caption);
            OffSetRect(R, SaveRect.Right - R.Right, SaveRect.Bottom - R.Bottom);
            GetHintRgn;
          end;
        end;
      hpUpRight:
        begin
          if (FHint <> nil) and (Assigned(FHint.FOnMeasureRect)) then
          begin
            FHint.FOnMeasureRect(FHint, R, Caption);
            OffSetRect(R, 0, SaveRect.Bottom - R.Bottom);
            GetHintRgn;
          end;
        end;
      hpDownLeft:
        begin
          if (FHint <> nil) and (Assigned(FHint.FOnMeasureRect)) then
          begin
            FHint.FOnMeasureRect(FHint, R, Caption);
            OffSetRect(R, SaveRect.Right - R.Right, SaveRect.Top - R.Top);
            GetHintRgn;
          end;
        end;
      hpDownRight:
        begin
          if (FHint <> nil) and (Assigned(FHint.FOnMeasureRect)) then
          begin
            FHint.FOnMeasureRect(FHint, R, Caption);
            OffSetRect(R, 0, SaveRect.Top - R.Top);
            GetHintRgn;
          end;
        end;
    end;

    if IsBalloonHint then
    begin
      PS[0].X := PS[0].X - Rect.Left;
      PS[0].Y := PS[0].Y - Rect.Top;
      PS[1].X := PS[1].X - Rect.Left;
      PS[1].Y := PS[1].Y - Rect.Top;
      PS[2].X := PS[2].X - Rect.Left;
      PS[2].Y := PS[2].Y - Rect.Top;
    end;
    
    R.Left := R.Left - Rect.Left;
    R.Top := R.Top - Rect.Top;
    R.Right := R.Right - Rect.Left;
    R.Bottom := R.Bottom - Rect.Top;
    FBitmap.Width := Rect.Right - Rect.Left;
    FBitmap.Height := Rect.Bottom - Rect.Top + 1;

    if FHint = nil then
    begin
      cR := GetRValue(ColorToRGB(clBlack)) + 1;
      cG := GetGValue(ColorToRGB(clBlack)) + 2;
      cB := GetBValue(ColorToRGB(clBlack)) + 3;
      FBitmap.Canvas.Brush.Color := cB shl 16 or cG shl 8 or cR;
      FBitmap.Canvas.FillRect(Classes.Rect(0, 0, FBitmap.Width, FBitmap.Height));
      FBitmap.Canvas.Brush.Color := clInfoBk;
      FBitmap.Canvas.Pen.Color := clBlack;
    end
    else
    begin
      cR := GetRValue(ColorToRGB(FHint.BorderColor)) + 1;
      cG := GetGValue(ColorToRGB(FHint.BorderColor)) + 2;
      cB := GetBValue(ColorToRGB(FHint.BorderColor)) + 3;
      FBitmap.Canvas.Brush.Color := cB shl 16 or cG shl 8 or cR;
      FBitmap.Canvas.FillRect(Classes.Rect(0, 0, FBitmap.Width, FBitmap.Height));
      FBitmap.Canvas.Brush.Color := FHint.BackColor;
      FBitmap.Canvas.Pen.Color := FHint.BorderColor;
    end;

    if IsBalloonHint then
    begin
      FBitmap.Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 15, 20);
      H := CreatePolygonRgn(PS, 3, WINDING);
      FillRgn(FBitmap.Canvas.Handle, H, FBitmap.Canvas.Brush.Handle);
      DeleteObject(H);
      FBitmap.Canvas.MoveTo(PS[0].X, PS[0].Y);
      FBitmap.Canvas.LineTo(PS[1].X, PS[1].Y);
      FBitmap.Canvas.LineTo(PS[2].X, PS[2].Y);
    end
    else
    begin
      FBitmap.Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      FBitmap.Canvas.Pen.Color := RGB(212, 208, 200);
      FBitmap.Canvas.MoveTo(R.Left, R.Bottom - 1);
      FBitmap.Canvas.LineTo(R.Left, R.Top);
      FBitmap.Canvas.LineTo(R.Right - 1, R.Top);
    end;

    FBitmap.Canvas.Brush.Style := bsClear;
    FBitmap.Canvas.Font.Assign(Canvas.Font);

    if (FHint <> nil) and Assigned(FHint.FOnBeforeHint) then
      FHint.FOnBeforeHint(FHint, R, Caption);

    if (FHint <> nil) and Assigned(FHint.FOnOwnerDraw) then
      FHint.FOnOwnerDraw(FHint, FBitmap.Canvas, R, Caption)
    else
      DrawHintText(FBitmap.Canvas, R, Caption, IsBalloonHint);

    if IsBalloonHint then
      Rgn := CreateRegion(FBitmap, FBitmap.Canvas.Pixels[0, 0])
    else
      Rgn := CreateRegion(FBitmap, FBitmap.Canvas.Pixels[0, 0] - 1);

    SetWindowRgn(Handle, Rgn, True);
    SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0);

    if Rect.Left < WorkArea.Left then
      OffsetRect(Rect, WorkArea.Left - Rect.Left, 0);
    if Rect.Top < WorkArea.Top then
      OffsetRect(Rect, 0, WorkArea.Top - Rect.Top);
    if Rect.Right > WorkArea.Right then
      OffsetRect(Rect, WorkArea.Right - Rect.Right, 0);
    if Rect.Bottom > WorkArea.Bottom then
      OffsetRect(Rect, 0, WorkArea.Bottom - Rect.Bottom);

    FTimer.Enabled := False;
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top,
      Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, SWP_NOACTIVATE);
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
    Invalidate;

    if HidePause = 0 then
      HidePause := Application.HintHidePause;

    if HidePause > 0 then
    begin
      FTimer.Interval := HidePause;
      FTimer.Enabled := True;
      New(PT);
      PT^ := HintPos;
      PostMessage(Handle, CN_MSG_HINT_NOTIFY, HidePause, Integer(PT));
    end;
  finally
    FLastActive := GetTickCount;
  end;
end;

constructor TCnInternalHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  with Canvas do
  begin
    Brush.Style := bsClear;
  end;
  FModified := False;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChange;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := Application.HintHidePause;
  FTimer.OnTimer := HintTimer;
  FTimer.Enabled := False;

  FAlignment := taLeftJustify;
  FHintStyle := hsAuto;

  if FCnHintWindows = nil then
    FCnHintWindows := TList.Create;
  FCnHintWindows.Add(Self);
end;

procedure TCnInternalHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style - WS_BORDER;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

destructor TCnInternalHintWindow.Destroy;
begin
  Hide;
  FTimer.Free;
  if FCnHintWindows <> nil then
    FCnHintWindows.Remove(Self);
  if FBitmap <> nil then
    FBitmap.Free;
  FGlyph.OnChange := nil;
  FGlyph.Free;
  inherited;
end;

procedure TCnInternalHintWindow.DrawHintText(Canvas: TCanvas; R: TRect;
  const AText: string; IsBalloonHint: Boolean);
var
  I, T, L: Integer;
  ARect, tR: TRect;
  Lines: TStringList;
  Align: TAlignment;
  Empty: Boolean;
  Index: Integer;
  TopText: string;
  Delta: Integer;
  OldStyles: TFontStyles;
begin
  if FHint <> nil then
    Align := FHint.Alignment
  else
    Align := FAlignment;

  ARect := R;
  InflateRect(ARect, -6, 0);
  T := 0;
  Lines := TStringList.Create;
  try
    R := ARect;
    Lines.Text := AText;
    Empty := (FGlyph = nil) or (FGlyph.Width <= 0) or (FGlyph.Height <= 0);
    if not Empty then
    begin
      L := 6 + FGlyph.Width;
      Canvas.Draw(R.Left, R.Top + 6, FGlyph);
      if not IsBalloonHint then
        Delta := 5
      else
        Delta := 10;

      T := Delta;
      Index := -1;
      TopText := '';
      for I := 0 to Lines.Count - 1 do
      begin
        TopText := TopText + Lines[I] + #13#10;
        if (T >= FGlyph.Height) then // T 记录文字累计的高度
        begin
          Index := I - 1;
          Delete(TopText, Length(TopText) - 1, 2);
          Inc(T, Canvas.TextHeight(Lines[I]));
          Break;
        end;
        Inc(T, Canvas.TextHeight(Lines[I]));
      end;

      if Index = -1 then
        Index := Lines.Count - 1;
      tR := GetTextRect(Canvas, TopText, Rect(R.Left + L, R.Top + Delta, R.Right, R.Top + T), Align, vtaTopJustify);
      T := 0;

      OldStyles := Canvas.Font.Style;
      for I := 0 to Index do // 画图片旁边的文字
      begin
        if FFirstLineAsTitle and (I = 0) then // 有图片、并且第一行是黑体时，画黑体字
          Canvas.Font.Style := Canvas.Font.Style + [fsBold]
        else
          Canvas.Font.Style := OldStyles;

        case Align of
          taLeftJustify:
            Canvas.TextRect(R, tR.Left, tR.Top + T, Lines[I]);
          taCenter:
            begin
              Canvas.TextRect(R, tR.Left + (tR.Right - tR.Left - Canvas.TextWidth(Lines[I])) div 2, tR.Top + T, Lines[I]);
            end;
          taRightJustify:
            begin
              Canvas.TextRect(R, tR.Right - Canvas.TextWidth(Lines[I]), tR.Top + T, Lines[I]);
            end;
        end;
        Inc(T, Canvas.TextHeight(Lines[I]));
      end;

      Canvas.Font.Style := OldStyles; // 确保恢复旧字体

      Inc(T, 6);
      L := 0;
      TopText := '';
      for I := Index + 1 to Lines.Count - 1 do
        TopText := TopText + Lines[I] + #13#10;
      Delete(TopText, Length(TopText) - 1, 2);
      tR := GetTextRect(Canvas, TopText, Rect(R.Left + L, R.Top + T + 6, R.Right, R.Bottom), Align, vtaTopJustify);
      T := 0;

      for I := Index + 1 to Lines.Count - 1 do // 画图片下面的文字
      begin
        case Align of
          taLeftJustify:
            Canvas.TextRect(R, tR.Left, tR.Top + T, Lines[I]);
          taCenter:
            begin
              Canvas.TextRect(R, tR.Left + (tR.Right - tR.Left - Canvas.TextWidth(Lines[I])) div 2, tR.Top + T, Lines[I]);
            end;
          taRightJustify:
            begin
              Canvas.TextRect(R, tR.Right - Canvas.TextWidth(Lines[I]), tR.Top + T, Lines[I]);
            end;

        end;
        Inc(T, Canvas.TextHeight(Lines[I]));
      end;
    end
    else // 无图片，不画黑体的 Title，按平常画
    begin
      tR := GetTextRect(Canvas, Lines.Text, R, Align, vtaCenter);
      for I := 0 to Lines.Count - 1 do
      begin
        case Align of
          taLeftJustify:
            Canvas.TextRect(R, tR.Left, tR.Top + T, Lines[I]);
          taCenter:
            begin
              Canvas.TextRect(R, tR.Left + (tR.Right - tR.Left - Canvas.TextWidth(Lines[I])) div 2, tR.Top + T, Lines[I]);
            end;
          taRightJustify:
            begin
              Canvas.TextRect(R, tR.Right - Canvas.TextWidth(Lines[I]), tR.Top + T, Lines[I]);
            end;
        end;
        Inc(T, Canvas.TextHeight(Lines[I]));
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TCnInternalHintWindow.FindCnHint: TCnHint;
begin
  Result := nil;
  if FCnHints <> nil then
    if FCnHints.Count > 0 then
      Result := TCnHint(FCnHints[0]);
end;

function TCnInternalHintWindow.GetHintPosition(WorkRect: TRect; AWidth, AHeight:
  Integer; Pos: TPoint; IsBalloonHint: Boolean): THintPosition;
var
  R: TRect;
  B: Boolean;
  Delta: Integer;
begin
  R := WorkRect;
  InflateRect(R, -10, -10);
  B := False;

  if IsBalloonHint then
    Delta := 16
  else
    Delta := 0;

  if not FModified and (FHint <> nil) then
    Result := FHint.HintPosition
  else
    Result := FHintPosition;

  case Result of
    hpUpLeft:
      B := ((AHeight + Delta - (Pos.Y - R.Top)) < 0) and ((AWidth - Delta) <= (Pos.X - R.Left));
    hpUpRight:
      B := ((AHeight + Delta - (Pos.Y - R.Top)) < 0) and ((AWidth - Delta + Pos.X) < R.Right);
    hpDownLeft:
      B := ((AHeight + Delta - (R.Bottom - Pos.Y)) < 0) and ((AWidth - Delta) <= (Pos.X - R.Left));
    hpDownRight:
      B := ((AHeight + Delta - (R.Bottom - Pos.Y)) < 0) and ((AWidth - Delta + Pos.X) < R.Right);
  end;

  if B then
    Exit;

  if (AHeight + Delta - (Pos.Y - R.Top)) < 0 then
  begin
    if (AWidth - Delta + Pos.X) < R.Right then
      Result := hpUpRight
    else
      Result := hpUpLeft;
  end
  else
  begin
    if (AWidth - Delta + Pos.X) < R.Right then
      Result := hpDownRight
    else
      Result := hpDownLeft;
  end;
end;

function TCnInternalHintWindow.GetTextRect(ACanvas: TCanvas; Text: string; R: TRect;
  HAlign: TAlignment; VAlign: TVAlignment): TRect;
var
  I, Len: Integer;
  Lines: TStrings;
  str: string;
  tR: TRect;
  intW, intH: Integer;
  OldStyles: TFontStyles;
begin
  Result := Rect(R.Left, R.Top, R.Left, R.Top);
  tR := Result;
  Lines := TStringList.Create;
  try
    Lines.Text := Text;
    OldStyles := ACanvas.Font.Style;
    for I := 0 to Lines.Count - 1 do
    begin
      str := Lines[I];
      if str <> '' then
      begin
        // 首行、无图片、并且有 Title 时，才按黑体来
        if FFirstLineAsTitle and (I = 0) and not Glyph.Empty then
        begin
          ACanvas.Font.Style := [fsBold]
        end
        else
          ACanvas.Font.Style := OldStyles;

        Len := ACanvas.TextWidth(str);
        if (tR.Right - tR.Left) < Len then
          tR.Right := tR.Left + Len;
        tR.Bottom := tR.Bottom + ACanvas.TextHeight(str);
      end;
    end;
    intW := tR.Right - tR.Left;
    intH := tR.Bottom - tR.Top;
    case HAlign of
      taLeftJustify:
        tR.Left := R.Left;
      taCenter:
        tR.Left := R.Left + Max((R.Right - R.Left - (tR.Right - tR.Left)) div 2, 0);
      taRightJustify:
        tR.Left := R.Right - (tR.Right - tR.Left);
    end;
    case VAlign of
      vtaTopJustify:
        tR.Top := R.Top + 3;
      vtaBottomJustify:
        tR.Top := R.Bottom - (tR.Bottom - tR.Top) - 1;
      vtaCenter:
        tR.Top := R.Top + Max((R.Bottom - R.Top - (tR.Bottom - tR.Top)) div 2, 0);

    end;
    tR.Right := tR.Left + intW;
    tR.Bottom := tR.Top + intH;
    Result := tR;
  finally
    Lines.Free;
    ACanvas.Font.Style := OldStyles;
  end;
end;

procedure TCnInternalHintWindow.GlyphChange(Sender: TObject);
begin
  if FUpdating then
    Exit;

  FUpdating := True;
  try
    if (FGlyph <> nil) and not Glyph.Transparent then
    begin
      Glyph.TransparentColor := Glyph.Canvas.Pixels[0, 0];
      Glyph.Transparent := True;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TCnInternalHintWindow.HintNotify(var message: TMessage);
var
  AMsg: MSG;
  HintControl: TControl;
  P: TPoint;
  PT: PPoint;
begin
  PT := PPoint(Pointer(message.LParam));
  P := PT^;
  HintControl := FindDragTarget(P, True);
  Dispose(PT);

  while (GetMessage(AMsg, 0, 0, 0)) do
  begin
    if (AMsg.message = WM_QUIT) then
    begin
      PostQuitMessage(0);
      Break;
    end
    else if ((AMsg.message >= WM_KEYFIRST) and (AMsg.message <= WM_KEYLAST))
      or ((AMsg.message = CM_ACTIVATE) or (AMsg.message = CM_DEACTIVATE)) or
      (AMsg.message = CM_APPKEYDOWN) or (AMsg.message = CM_APPSYSCOMMAND) or
      (AMsg.message = WM_COMMAND) or ((AMsg.message > WM_MOUSEMOVE) and
      (AMsg.message <= WM_MOUSELAST)) or (AMsg.message = WM_NCMOUSEMOVE) then
    begin
      PostMessage(AMsg.hwnd, AMsg.message, AMsg.wParam, AMsg.lParam);
      Break;
    end
    else if (AMsg.message = WM_MOUSEMOVE) and (HintControl <> FindDragTarget(Mouse.CursorPos, True)) then
    begin
      PostMessage(AMsg.hwnd, AMsg.message, AMsg.wParam, AMsg.lParam);
      Break;
    end
    else
    begin
      TranslateMessage(AMsg);
      DispatchMessage(AMsg);
    end;
  end;

//  DoCancelHint;
end;

procedure TCnInternalHintWindow.Paint;
begin
  BitBlt(Canvas.Handle, 0, 0, FBitmap.Width, FBitmap.Height,
    FBitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TCnInternalHintWindow.SetPosition(const Value: THintPosition);
begin
  if FHintPosition <> Value then
  begin
    FHintPosition := Value;
    FModified := True;
  end;
end;

procedure TCnInternalHintWindow.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TCnInternalHintWindow.DoCancelHint;
begin
  if Assigned(FOnCancelHint) then
    FOnCancelHint(Self);
end;

procedure TCnInternalHintWindow.HintTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  DoCancelHint;
  ReleaseHandle;
end;

{ TCnHint }

procedure TCnHint.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Application.ShowHint := not Application.ShowHint;
  Application.ShowHint := not Application.ShowHint;
  UpdateHintWindowFont;
end;

constructor TCnHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackColor := clInfoBk; //$F0A07D;
  FBorderColor := clBlack;
  FHintPosition := hpDownRight;
  FAlignment := taLeftJustify;

  FFont := TFont.Create;
  FFont.Color := clBlack;
  FGlyph := TBitmap.Create;

  FGlyph.OnChange := GlyphChange;

  if not (csDesigning in ComponentState) then
  begin
    HintWindowClass := TCnInternalHintWindow;
    Application.ShowHint := not Application.ShowHint;
    Application.ShowHint := not Application.ShowHint;
    UpdateHintWindowFont;
  end;

  if FCnHints = nil then
    FCnHints := TList.Create;
  FCnHints.Add(Self);
end;

destructor TCnHint.Destroy;
begin
  if FCnHints <> nil then
    FCnHints.Remove(Self);
  FFont.Free;
  FGlyph.OnChange := nil;
  FGlyph.Free;
  inherited;
end;

procedure TCnHint.GlyphChange(Sender: TObject);
begin
  if FGlyph <> nil then
  begin
    Glyph.TransparentColor := Glyph.Canvas.Pixels[0, 0];
    Glyph.Transparent := True;
  end;
end;

procedure TCnHint.Loaded;
begin
  if not (csDesigning in ComponentState) then
  begin
    inherited Loaded;
    HintWindowClass := TCnInternalHintWindow;
    Application.ShowHint := not Application.ShowHint;
    Application.ShowHint := not Application.ShowHint;
    UpdateHintWindowFont;
  end;
end;

procedure TCnHint.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Application.ShowHint := not Application.ShowHint;
  Application.ShowHint := not Application.ShowHint;
  UpdateHintWindowFont;
end;

procedure TCnHint.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TCnHint.UpdateHintWindowFont;
var
  I: Integer;
begin
  if FCnHintWindows <> nil then
    if FCnHintWindows.Count > 0 then
      for I := 0 to FCnHintWindows.Count - 1 do
        if Application.Components[I] is TCnInternalHintWindow then
          TCnInternalHintWindow(FCnHintWindows[I]).Canvas.Font.Assign(FFont);
end;

{ TCnHintWindow }

procedure TCnHintWindow.ActivateHint(const AHint: string; const ATitle: string);
begin
  FHintWindow.ActivateHintFromPos(Mouse.CursorPos, AHint, ATitle);
end;

procedure TCnHintWindow.ActivateHintFromPos(const HintPos: TPoint;
  const AHint, ATitle: string; HidePause: Integer);
begin
  FHintWindow.ActivateHintFromPos(HintPos, AHint, ATitle, HidePause);
end;

constructor TCnHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  FHintWindow := TCnInternalHintWindow.Create(Self);
  FHintWindow.OnCancelHint := HintWindowCancelHint;
end;

destructor TCnHintWindow.Destroy;
begin
  FHintWindow.Free;
  inherited;
end;

function TCnHintWindow.GetAlignment: TAlignment;
begin
  Result := FHintWindow.Alignment;
end;

function TCnHintWindow.GetGlyph: TBitmap;
begin
  Result := FHintWindow.Glyph;
end;

function TCnHintWindow.GetHintPosition: THintPosition;
begin
  Result := FHintWindow.HintPosition;
end;

function TCnHintWindow.GetHintStyle: THintStyle;
begin
  Result := FHintWindow.HintStyle;
end;

procedure TCnHintWindow.HintWindowCancelHint(Sender: TObject);
begin
  if Assigned(FOnCancelHint) then
    FOnCancelHint(FHintWindow);
end;

procedure TCnHintWindow.ReleaseHandle;
begin
  FHintWindow.ReleaseHandle;
end;

procedure TCnHintWindow.SetAlignment(const Value: TAlignment);
begin
  FHintWindow.Alignment := Value;
end;

procedure TCnHintWindow.SetGlyph(const Value: TBitmap);
begin
  FHintWindow.Glyph.Assign(Value);
end;

procedure TCnHintWindow.SetHintStyle(const Value: THintStyle);
begin
  FHintWindow.HintStyle := Value;
end;

procedure TCnHintWindow.SetPosition(const Value: THintPosition);
begin
  FHintWindow.HintPosition := Value;
end;

initialization

finalization
  FreeAndNil(FCnHints);
  FreeAndNil(FCnHintWindows);
  
end.

