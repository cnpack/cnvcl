{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnQRImage;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：二维码显示单元
* 单元作者：CnPack 开发组
* 备    注：本单元使用 CnQRCode 单元中的 TCnQREncoder 实现 VCL/FPX 下的二维码图形绘制，
*           暂不支持 FMX 组件。
*           另外实现了将 VCL/FMX/FPC 的位图转换为二维码矩阵数据供辨识的函数。
*           注意 FMX 相关功能需要定义 ENABLE_FMX
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.05.15 V1.1
*               增加将 VCL/FMX/FPC 的位图等转为二维码灰度矩阵的功能供解码用
*           2026.01.13 V1.0
*               创建单元，在 AI 帮助下实现编码并能扫描成功
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// 如果要在 FMX 中使用针对 FMX 的 Bitmap 的解码功能，请工程中或下面定义 ENABLE_FMX
// {$DEFINE ENABLE_FMX}

{$IFNDEF SUPPORT_FMX}
  {$UNDEF ENABLE_FMX}
{$ENDIF}

uses
  SysUtils, Classes, {$IFDEF FPC} LCLIntf, LCLType, FPImage, {$ELSE} Windows, {$ENDIF}
  {$IFNDEF ENABLE_FMX} Graphics, {$ENDIF} Controls, ExtCtrls,
  {$IFDEF ENABLE_FMX} Vcl.Graphics, UITypes,
  {$IFDEF FMX_HAS_GRAPHICS} FMX.Graphics, {$ELSE} FMX.Types, {$ENDIF} {$ENDIF} CnQRCode;

type
{$IFNDEF ENABLE_FMX}
{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnQRCodeImage = class(TGraphicControl)
  {* 二维码绘制类}
  private
    FEncoder: TCnQREncoder;
    FIcon: TIcon;
    FCellSize: Integer;
    FForeColor: TColor;
    FIconSize: Integer;
    FIconMargin: Integer;
    function GetQRErrorRecoveryLevel: TCnErrorRecoveryLevel;
    procedure SetQRErrorRecoveryLevel(const Value: TCnErrorRecoveryLevel);
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetIcon(const Value: TIcon);
    procedure SetCellSize(const Value: Integer);
    procedure SetForeColor(const Value: TColor);
    procedure SetQRWideCharMode(const Value: TCnQRWideCharMode);
    function GetFQRWideCharMode: TCnQRWideCharMode;
    procedure SetIconMargin(const Value: Integer);
    procedure SetIconSize(const Value: Integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: string; Border: Integer = 4);

  published
    property Color;
    {* 二维码背景色}
    property Text: string read GetText write SetText;
    {* 显示的字符串}
    property QRWideCharMode: TCnQRWideCharMode read GetFQRWideCharMode write
      SetQRWideCharMode;
    {* 宽字符编码模式，默认 Utf8}
    property QRErrorRecoveryLevel: TCnErrorRecoveryLevel read
      GetQRErrorRecoveryLevel write SetQRErrorRecoveryLevel;
    {* 二维码纠错等级}
    property Icon: TIcon read FIcon write SetIcon;
    {* 绘制在中间的图标}

    property CellSize: Integer read FCellSize write SetCellSize;
    {* 二维码每个模块大小，0 表示自动适应}
    property ForeColor: TColor read FForeColor write SetForeColor default clBlack;
    {* 二维码中的黑色颜色}
    property IconSize: Integer read FIconSize write SetIconSize;
    {* 中央的图标尺寸，默认 32 像素}
    property IconMargin: Integer read FIconMargin write SetIconMargin;
    {* 中央图标边缘的空隙}
  end;

{$ENDIF}

{$IFDEF ENABLE_FMX}
  // 如果引用了 FMX，会造成 TBitmap 混乱。
  // 需要显式指定 TBitmap 是 VCL 的 Graphics，同时兼容 FPC 的 TBitmap，FMX 的则用全称
  TBitmap = Vcl.Graphics.TBitmap;
{$IFDEF FMX_HAS_GRAPHICS}
  TCnFMXBitmap = FMX.Graphics.TBitmap;
  TCnBitmapData =  FMX.Graphics.TBitmapData;
{$ELSE}
  TCnFMXBitmap = FMX.Types.TBitmap;
  TCnBitmapData = FMX.Types.TBitmapData;
{$ENDIF}
{$ENDIF}

function CnBitmapToGrayImage(const ABitmap: TBitmap): TCnQRData;
{* 将 VCL/FPC 的 TBitmap 转换为二维码专用的 TCnQRData。
   灰度转换（灰度公式: 0.299R+0.587G+0.114B）}

{$IFDEF ENABLE_FMX}

function CnFMXBitmapToGrayImage(const ABitmap: TCnFMXBitmap): TCnQRData;
{* 将 FMX 的 TBitmap 转换为二维码专用的 TCnQRData。
   灰度转换（灰度公式: 0.299R+0.587G+0.114B）}

{$ENDIF}

{$IFDEF FPC}

function CnFPCImageToGrayImage(Image: TFPCustomImage): TCnQRData;
{* 将 FPC 的 TFPCustomImage 转换为二维码专用的 TCnQRData。
   灰度转换（灰度公式: 0.299R+0.587G+0.114B）}

{$ENDIF}

function CnDecodeQRImageFile(const FileName: string): string;
{* 从图片文件中解码二维码文本（VCL/FPC），使用 TPicture 加载文件并解码}

{$IFDEF ENABLE_FMX}

function CnFMXDecodeQRImageFile(const FileName: string): string;
{* 从图片文件中解码二维码文本（FMX），使用 FMX.Graphics.TBitmap 加载文件并解码}

{$ENDIF}

{$IFDEF FPC}

function CnFPCDecodeQRImageFile(const FileName: string): string;
{* 从图片文件中解码二维码文本（FPC），使用 TFPCustomImage 加载文件并解码}

{$ENDIF}

implementation

// 将 VCL 的 TBitmap 转换为二维码专用的 TCnQRData
function CnBitmapToGrayImage(const ABitmap: TBitmap): TCnQRData;
var
  X, Y, Width, Height: Integer;
  P: PByteArray;
  R, G, B: Byte;
{$IFDEF FPC}
  TempBmp: TBitmap;
{$ENDIF}
begin
  Width := ABitmap.Width;
  Height := ABitmap.Height;
  SetLength(Result, Width, Height);
  if (Width <= 0) or (Height <= 0) then Exit;

{$IFDEF FPC}
  if ABitmap.PixelFormat = pf24bit then
  begin
    for Y := 0 to Height - 1 do
    begin
      P := ABitmap.ScanLine[Y];
      for X := 0 to Width - 1 do
      begin
        B := P[X * 3];
        G := P[X * 3 + 1];
        R := P[X * 3 + 2];
        Result[X, Y] := (R * 299 + G * 587 + B * 114) div 1000;
      end;
    end;
  end
  else
  begin
    TempBmp := TBitmap.Create;
    try
      TempBmp.PixelFormat := pf24bit;
      TempBmp.SetSize(Width, Height);
      TempBmp.Canvas.Draw(0, 0, ABitmap);

      for Y := 0 to Height - 1 do
      begin
        P := TempBmp.ScanLine[Y];
        for X := 0 to Width - 1 do
        begin
          B := P[X * 3];
          G := P[X * 3 + 1];
          R := P[X * 3 + 2];
          Result[X, Y] := (R * 299 + G * 587 + B * 114) div 1000;
        end;
      end;
    finally
      TempBmp.Free;
    end;
  end;
{$ELSE}
  ABitmap.PixelFormat := pf24bit;
  for Y := 0 to Height - 1 do
  begin
    P := ABitmap.ScanLine[Y];
    for X := 0 to Width - 1 do
    begin
      // ScanLine 返回 BGR 顺序
      B := P[X * 3];
      G := P[X * 3 + 1];
      R := P[X * 3 + 2];
      Result[X, Y] := (R * 299 + G * 587 + B * 114) div 1000;
    end;
  end;
{$ENDIF}
end;

{$IFDEF ENABLE_FMX}

function CnFMXBitmapToGrayImage(const ABitmap: TCnFMXBitmap): TCnQRData;
var
  X, Y, W, H: Integer;
{$IFDEF FMX_HAS_GRAPHICS}
  Data: FMX.Graphics.TBitmapData;
  Res: Boolean;
{$ENDIF}
  Pixel: TAlphaColor;
  R, G, B: Byte;
begin
  W := ABitmap.Width;
  H := ABitmap.Height;
  SetLength(Result, W, H);
  if (W <= 0) or (H <= 0) then Exit;

{$IFDEF FMX_HAS_GRAPHICS}
  // XE5 及以上能 map
{$IFDEF DELPHIXE6_UP}
  // XE6 及以上改名了
  Res := ABitmap.Map(TMapAccess.Read, Data);
{$ELSE}
  Res := ABitmap.Map(TMapAccess.maRead, Data);
{$ENDIF}
  if Res then
  begin
    try
      for Y := 0 to H - 1 do
      begin
        for X := 0 to W - 1 do
        begin
          Pixel := Data.GetPixel(X, Y);
          R := (Pixel shr 16) and $FF;
          G := (Pixel shr 8) and $FF;
          B := Pixel and $FF;
          Result[X, Y] := (R * 299 + G * 587 + B * 114) div 1000;
        end;
      end;
    finally
      ABitmap.Unmap(Data);
    end;
  end;
{$ELSE}
  // TODO: 没 Map，直接访问 ScanLine
{$ENDIF}
end;

{$ENDIF}

{$IFDEF FPC}

function CnFPCImageToGrayImage(Image: TFPCustomImage): TCnQRData;
var
  X, Y: Integer;
  Pixel: TFPColor;
  R, G, B: Byte;
begin
  SetLength(Result, Image.Width, Image.Height);

  for Y := 0 to Image.Height - 1 do
  begin
    for X := 0 to Image.Width - 1 do
    begin
      Pixel := Image.Colors[X, Y];  // 注意 TFPColor 是 0 到 65536
      R := Pixel.Red div 256;
      G := Pixel.Green div 256;
      B := Pixel.Blue div 256;
      Result[X, Y] := (R * 299 + G * 587 + B * 114) div 1000;
    end;
  end;
end;

{$ENDIF}

function CnDecodeQRImageFile(const FileName: string): string;
var
  Pic: TPicture;
  GrayData: TCnQRData;
begin
  Pic := TPicture.Create;
  try
    Pic.LoadFromFile(FileName);
    GrayData := CnBitmapToGrayImage(Pic.Bitmap);
    Result := CnQRDecodeFromGrayImage(GrayData);
  finally
    Pic.Free;
  end;
end;

{$IFDEF ENABLE_FMX}

function CnFMXDecodeQRImageFile(const FileName: string): string;
var
  Bmp: FMX.Graphics.TBitmap;
  GrayData: TCnQRData;
begin
  Bmp := FMX.Graphics.TBitmap.Create;
  try
    Bmp.LoadFromFile(FileName);
    GrayData := CnFMXBitmapToGrayImage(Bmp);
    Result := CnQRDecodeFromGrayImage(GrayData);
  finally
    Bmp.Free;
  end;
end;

{$ENDIF}

{$IFDEF FPC}

function CnFPCDecodeQRImageFile(const FileName: string): string;
var
  Pic: TPicture;
  GrayData: TCnQRData;
begin
  Pic := TPicture.Create;
  try
    Pic.LoadFromFile(FileName);
    GrayData := CnBitmapToGrayImage(Pic.Bitmap);
    Result := CnQRDecodeFromGrayImage(GrayData);
  finally
    Pic.Free;
  end;
end;

{$ENDIF}

{$IFNDEF ENABLE_FMX}

{ TCnQRCodeImage }

constructor TCnQRCodeImage.Create(AOwner: TComponent);
begin
  inherited;
  FEncoder := TCnQREncoder.Create;
  FIcon := TIcon.Create;
  FForeColor := clBlack;
  Color := clWhite;
  FIconSize := 32;
  FIconMargin := 2;
end;

destructor TCnQRCodeImage.Destroy;
begin
  FIcon.Free;
  FEncoder.Free;
  inherited;
end;

procedure TCnQRCodeImage.SaveToFile(const FileName: string; Border: Integer = 4);
var
  CS, QRWidth, QRHeight, QRLeft, QRTop, I, J: Integer;
  Edge: Integer;
  Bmp: TBitmap;
  ImgW, ImgH: Integer;
begin
  QRWidth := FEncoder.QRSize + Border * 2;
  QRHeight := FEncoder.QRSize + Border * 2;
  CS := CellSize;
  if CS <= 0 then
  begin
    Edge := Width;
    if Height < Edge then
      Edge := Height;
    CS := Edge div (FEncoder.QRSize + Border * 2);
    if CS <= 0 then
      CS := 1;
  end;
  ImgW := QRWidth * CS;
  ImgH := QRHeight * CS;
  QRLeft := 0;
  QRTop := 0;
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := ImgW;
    Bmp.Height := ImgH;
    Bmp.Canvas.Brush.Style := bsSolid;
    Bmp.Canvas.Brush.Color := Color;
    Bmp.Canvas.FillRect(Rect(0, 0, ImgW, ImgH));
    Bmp.Canvas.Brush.Color := ForeColor;
    Bmp.Canvas.Pen.Style := psClear;
    for I := 0 to FEncoder.QRSize - 1 do
    begin
      for J := 0 to FEncoder.QRSize - 1 do
      begin
        if FEncoder.QRData[I, J] = 1 then
          Bmp.Canvas.FillRect(Rect(
              QRLeft + (I + Border) * CS,
              QRTop + (J + Border) * CS,
              QRLeft + (I + Border + 1) * CS,
              QRTop + (J + Border + 1) * CS
            ));
      end;
    end;
    Bmp.SaveToFile(FileName);
  finally
    Bmp.Free;
  end;
end;

function TCnQRCodeImage.GetQRErrorRecoveryLevel: TCnErrorRecoveryLevel;
begin
  Result := FEncoder.QRErrorRecoveryLevel;
end;

function TCnQRCodeImage.GetText: string;
begin
  Result := FEncoder.Text;
end;

function TCnQRCodeImage.GetFQRWideCharMode: TCnQRWideCharMode;
begin
  Result := FEncoder.QRWideCharMode;
end;

procedure TCnQRCodeImage.Paint;
var
  CS, QL, QT, Edge, I, J: Integer;
  QRWidth, QRHeight: Integer;
  QRLeft, QRTop: Integer;
  ISZ, WH: Integer;
  R: TRect;
  TmpBmp: TBitmap;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if csDesigning in ComponentState then
  begin
    with Canvas do
    begin
      Pen.Style := psSolid;
      Brush.Style := bsSolid;
      Rectangle(0, 0, Width, Height);
    end;
  end;

  // 计算二维码实际绘制区域（包含静区）
  QRWidth := FEncoder.QRSize + 8;  // 4 模块静区 * 2 = 8
  QRHeight := FEncoder.QRSize + 8;

  // 确定绘制位置，画二维码
  CS := CellSize;
  if CS <= 0 then
  begin
    // 根据内容宽高与尺寸宽高，计算合适的 CellSize 以及正方形位置
    Edge := Width;
    if Height < Edge then
      Edge := Height;

    // 静区，按 (QRSize + 8) 计算单元大小
    CS := Edge div (FEncoder.QRSize + 8);
    if CS <= 0 then
      CS := 1;
  end;

  // 计算绘制位置（居中）
  QRLeft := (Width - QRWidth * CS) div 2;
  QRTop := (Height - QRHeight * CS) div 2;

  // 绘制白色背景（静区）
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(QRLeft, QRTop,
      QRLeft + QRWidth * CS,
      QRTop + QRHeight * CS));

  // 绘制二维码模块
  Canvas.Brush.Color := ForeColor;
  Canvas.Pen.Style := psClear;

  for I := 0 to FEncoder.QRSize - 1 do
  begin
    for J := 0 to FEncoder.QRSize - 1 do
    begin
      if FEncoder.QRData[I, J] = 1 then
      begin
        Canvas.FillRect(Rect(
            QRLeft + (I + 4) * CS,      // +4 表示4模块静区
            QRTop + (J + 4) * CS,
            QRLeft + (I + 4 + 1) * CS,
            QRTop + (J + 4 + 1) * CS
          ));
      end;
    end;
  end;

  // 绘制 Icon
  if not FIcon.Empty then
  begin
    if FIconSize = 0 then
      ISZ := FIcon.Width
    else
      ISZ := FIconSize;

    WH := ISZ + 2 * FIconMargin;
    if FIconMargin > 0 then
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := clNone;
      R := Rect((Width - WH) div 2, (Height - WH) div 2, (Width + WH) div 2, (Height + WH) div 2);
      Canvas.FillRect(R);
    end;

    QL := (Width - ISZ) div 2;
    QT := (Height - ISZ) div 2;

    TmpBmp := TBitmap.Create;
    try
      TmpBmp.PixelFormat := pf24bit;
      TmpBmp.Width := FIcon.Width;
      TmpBmp.Height := FIcon.Height;
      TmpBmp.Canvas.Brush.Style := bsSolid;
      TmpBmp.Canvas.Brush.Color := Color;
      TmpBmp.Canvas.FillRect(Rect(0, 0, TmpBmp.Width, TmpBmp.Height));
      TmpBmp.Canvas.Draw(0, 0, FIcon);
      Canvas.StretchDraw(Rect(QL, QT, QL + ISZ, QT + ISZ), TmpBmp);
    finally
      TmpBmp.Free;
    end;
  end;
end;

procedure TCnQRCodeImage.SetCellSize(const Value: Integer);
begin
  if FCellSize <> Value then
  begin
    FCellSize := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetForeColor(const Value: TColor);
begin
  if FForeColor <> Value then
  begin
    FForeColor := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetIcon(const Value: TIcon);
begin
  if Value <> nil then
  begin
    FIcon.Assign(Value);
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetQRErrorRecoveryLevel(const Value: TCnErrorRecoveryLevel);
begin
  if FEncoder.QRErrorRecoveryLevel <> Value then
  begin
    FEncoder.QRErrorRecoveryLevel := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetText(const Value: string);
begin
  if FEncoder.Text <> Value then
  begin
    FEncoder.Text := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetQRWideCharMode(const Value: TCnQRWideCharMode);
begin
  if FEncoder.QRWideCharMode <> Value then
  begin
    FEncoder.QRWideCharMode := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetIconMargin(const Value: Integer);
begin
  if FIconMargin <> Value then
  begin
    if FIconMargin < 0 then
      FIconMargin := 0;

    FIconMargin := Value;
    Invalidate;
  end;
end;

procedure TCnQRCodeImage.SetIconSize(const Value: Integer);
begin
  if FIconSize <> Value then
  begin
    if (FIconSize < 16) and (FIconSize <> 0) then
      FIconSize := 16;

    FIconSize := Value;
    Invalidate;
  end;
end;

{$ENDIF}

end.

