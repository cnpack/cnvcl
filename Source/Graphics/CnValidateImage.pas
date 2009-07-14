{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

unit CnValidateImage;
{* |<PRE>
================================================================================
* 软件名称：CnPack IDE 专家包
* 单元名称：验证码生成图像单元
* 单元作者：二娃(鸟哥) QQ: 78493244
* 备    注：部分代码取自 名士:QQ517165547, wjf0334@163.com
* 开发平台：PWinXPPro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该窗体中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.10.31 V1.0
*               移植单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Classes, Controls, Graphics, Forms, Messages, ExtCtrls;

type
  TCnValidateImage = class(TPaintBox)
  private
    FPicture: TPicture;
    FTransparent: Boolean;
    FDrawing: Boolean;
    FCaseSensitive: Boolean;
    FFontRandomSeed: Integer;
    FnoiseCount: Integer;
    FValidateCount: Integer;
    FValueLength: Integer;
    FValue: string;
    function GetCanvas: TCanvas;
    procedure PictureChanged(Sender: TObject);
    procedure SetFontRandomSeed(const Value: Integer);
    procedure SetnoiseCount(const Value: Integer);
    procedure SetValueLength(const Value: Integer);
    procedure SetTransparent(Value: Boolean);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DestRect: TRect;
    function DoPaletteChange: Boolean;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RandomValue;
    function ValidateInput(const aDest: string): Boolean;
    property Canvas: TCanvas read GetCanvas;
    property Picture: TPicture read FPicture;
    property Value: string read FValue;
    property ValidateCount: Integer read FValidateCount;
  published
    property NoiseCount: Integer read FNoiseCount write SetNoiseCount default 100;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property FontRandomSeed: Integer read FFontRandomSeed write SetFontRandomSeed default 6;
    property ValueLength: Integer read FValueLength write SetValueLength default 4;
  end;

implementation

{$IFDEF DELPHI11_UP}
uses
  UxTheme, DwmApi;
{$ENDIF}

{ TCnValidateImage }

constructor TCnValidateImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable {$IFDEF DELPHI11_UP}, csPannable{$ENDIF}];
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  Height := 21;
  Width := 50;
  FValueLength := 4;
  FNoiseCount := 100;
  FValidateCount := 0;
  FCaseSensitive := False;
  FontRandomSeed :=6;
  RandomValue;
end;

destructor TCnValidateImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TCnValidateImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette;
end;

function TCnValidateImage.DestRect: TRect;
var
  w, h: Integer;
begin
  w := ClientWidth;
  h := ClientHeight;

  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;
end;

procedure TCnValidateImage.Paint;

  procedure DoBufferedPaint(Canvas: TCanvas);
{$IFDEF DELPHI11_UP}
  var
    MemDC: HDC;
    Rect: TRect;
    PaintBuffer: HPAINTBUFFER;
{$ENDIF}
  begin
{$IFDEF DELPHI11_UP}
    Rect := DestRect;
    PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      Canvas.Handle := MemDC;
      Canvas.StretchDraw(DestRect, Picture.Graphic);
      BufferedPaintMakeOpaque(PaintBuffer, @Rect);
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
{$ENDIF}
  end;

var
  Save: Boolean;
  PaintOnGlass: Boolean;
{$IFDEF DELPHI11_UP}
  LForm: TCustomForm;
{$ENDIF}
begin
  if csDesigning in ComponentState then
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  Save := FDrawing;
  FDrawing := True;
  try
{$IFDEF DELPHI11_UP}
    PaintOnGlass := DwmCompositionEnabled and not (csDesigning in ComponentState);;
    if PaintOnGlass then
    begin
      LForm := GetParentForm(Self);
      PaintOnGlass := (LForm <> nil) and LForm.GlassFrame.FrameExtended and
        LForm.GlassFrame.IntersectsControl(Self);
    end;
{$ELSE}
    PaintOnGlass := False;
{$ENDIF}
    if PaintOnGlass then
      DoBufferedPaint(inherited Canvas)
    else
      with inherited Canvas do
        StretchDraw(DestRect, Picture.Graphic);

    if Assigned(OnPaint) then
      OnPaint(Self);
  finally
    FDrawing := Save;
  end;
end;

function TCnValidateImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := Picture.Graphic;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
    (Tmp.PaletteModified) then
  begin
    if (Tmp.Palette = 0) then
      Tmp.PaletteModified := False
    else
    begin
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
      begin
        if FDrawing then
          ParentForm.Perform(wm_QueryNewPalette, 0, 0)
        else
          PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
        Result := True;
        Tmp.PaletteModified := False;
      end;
    end;
  end;
end;

function TCnValidateImage.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if Picture.Graphic = nil then
  begin
    Bitmap := TBitmap.create;
    try
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      Picture.Graphic := Bitmap;
    finally
      Bitmap.Free;
    end;
  end;
  Result := TBitmap(Picture.Graphic).Canvas;
end;

procedure TCnValidateImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    PictureChanged(Self);
  end;
end;

procedure TCnValidateImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  D : TRect;
begin
  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
    SetBounds(Left, Top, Picture.Width, Picture.Height);
  G := Picture.Graphic;

  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := FTransparent;

    D := DestRect;
    if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
       (D.Right >= Width) and (D.Bottom >= Height) then
      ControlStyle := ControlStyle + [csOpaque]
    else
      ControlStyle := ControlStyle - [csOpaque];

    if DoPaletteChange and FDrawing then
      Update;
  end
  else
    ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then
    Invalidate;
end;

function TCnValidateImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and
    (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height;
  end;
end;

procedure TCnValidateImage.Click;
begin
  inherited;
  RandomValue;
end;

procedure TCnValidateImage.RandomValue;
var
  I: Integer;
  vPoint: TPoint;
  vHoz, vVert: Integer;
  aTempBitmap: TBitmap;
  aFontStyle: TFontStyles;
const
  arrStr: string = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';//58
begin
  FValue := '';
  Randomize;
  for I := 1 to FValueLength do
    FValue := FValue + arrStr[Random(57) + 1];

  vHoz := 2;
  vVert := 2;
  aTempBitmap := TBitmap.Create;

  with aTempBitmap do
  try
    Canvas.Font.Name := Screen.Fonts[10];
    Canvas.Font.Style := [fsBold, fsItalic];
    Width := Round(FValueLength * (Canvas.TextWidth('Wg') / 2 + 4) + vHoz * (FValueLength + 1));
    Height := Canvas.TextHeight('g') + 5 + vVert * 2;

    for I := 0 to FNoiseCount - 1 do
    begin
      vPoint.X := Random(Width);
      vPoint.Y := Random(Height);
      Canvas.Pixels[vPoint.X,vPoint.Y]:=
        RGB(Random(256) and $C0, Random(256) and $C0, Random(256) and $C0);
    end;

    for I := 1 to FValueLength do
    begin
      Canvas.Font.Size := Random(FFontRandomSeed) + 9;
      Canvas.Font.Color := RGB(Random(256) and $C0, Random(256) and
        $C0, Random(256) and $C0);

      aFontStyle := [];
      if Random(2) = 1 then
        aFontStyle := aFontStyle + [fsBold];
      if Random(2) = 1 then
        aFontStyle := aFontStyle + [fsItalic];
      Canvas.Font.Style := aFontStyle;

      vPoint.X := Random(4) + vHoz;
      vPoint.Y := Random(5);
      Canvas.TextOut(vPoint.X, vPoint.Y, FValue[I]);
      vHoz := vPoint.X + Canvas.TextWidth(FValue[I]);
    end;
    FPicture.Assign(aTempBitmap);
  finally
    Free;
  end;
end;

procedure TCnValidateImage.SetFontRandomSeed(const Value: Integer);
begin
  if Value < 0 then
    FFontRandomSeed := 0
  else
  if Value > 10 then
    FFontRandomSeed := 10
  else
    FFontRandomSeed := Value;
  RandomValue;
end;

procedure TCnValidateImage.SetNoiseCount(const Value: Integer);
begin
  if Value < 0 then
    FNoiseCount :=0
  else if Value > 1000 then
    FNoiseCount := 1000
  else
    FNoiseCount := Value;
  RandomValue;
end;

procedure TCnValidateImage.SetValueLength(const Value: Integer);
begin
  if Value < 1 then
    FValueLength := 1
  else
    FValueLength := Value;
  RandomValue;
end;

function TCnValidateImage.ValidateInput(const aDest: string): Boolean;
begin
  if FCaseSensitive then
    Result := aDest = FValue
  else 
    Result := UpperCase(aDest) = UpperCase(FValue);

  if Result then
    FValidateCount := 0
  else
  begin
    RandomValue;
    Inc(FValidateCount);
  end;
end;

end.
