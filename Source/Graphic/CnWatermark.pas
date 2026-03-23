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

unit CnWatermark;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元作者：CnPack 开发组
* 备    注：提供不可见的频域数字水印嵌入与提取功能，基本上能一定程度对抗
*           JPG压缩、剪切、旋转、横竖缩放、色彩变换等。
* 开发平台：PWinXP + Delphi 7.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.03.23 V1.0
*               完善功能
*           2026.02.03 V1.0
*               增加 DFT 图片水印模式，抗旋转/缩放/裁剪。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF} SysUtils, Graphics, Controls,
  CnMatrix, CnDFT, CnNative, Math, CnClasses, CnComplex;

type
  ECnWatermarkException = class(Exception);
  {* 水印相关异常}

  TCnWatermarkStrength = (wsLow, wsMedium, wsHigh, wsCustom);
  {* 水印强度预设}

  TCnWatermarkMode = (wmText, wmImage);
  {* 水印模式}

  TCnWatermarkProgressEvent = procedure(Sender: TObject; Percent: Integer) of object;
  {* 进度事件}

  TCnWatermarkImageEvent = procedure(Sender: TObject; Image: TBitmap) of object;
  {* 图像相关事件}

  TCnWatermark = class(TCnComponent)
  {* 数字水印嵌入与提取控件}
  private
    FText: string;
    FWatermarkImage: TBitmap;
    FMode: TCnWatermarkMode;
    FStrength: Double;
    FStrengthLevel: TCnWatermarkStrength;
    FOnProgress: TCnWatermarkProgressEvent;
    FOnWatermarkImageReady: TCnWatermarkImageEvent;
    FFont: TFont;
    FMargin: Integer;
    procedure SetStrengthLevel(const Value: TCnWatermarkStrength);
    procedure SetStrength(const Value: Double);
    procedure SetWatermarkImage(const Value: TBitmap);
    procedure SetFont(const Value: TFont);
    procedure SetMargin(const Value: Integer);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure DoProgress(Percent: Integer); dynamic;
    procedure EmbedText(Source, Dest: TBitmap);
    procedure EmbedImage(Source, Dest: TBitmap);

    procedure DoWatermarkImageReady(Image: TBitmap); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {* 嵌入水印
       @param Source 源图像，必须是有效图像。处理时会强制转换为 24位。
       @param Dest 目标图像。如果为 nil，则直接修改 Source。
    *}
    procedure Embed(Source: TBitmap; Dest: TBitmap = nil);

    {* 提取水印 (文字模式)
       @param Source 源图像
       @return 返回提取出的原始比特流组装成的字符串
    *}
    function ExtractText(Source: TBitmap): string;

    {* 提取水印 (图像模式)
       @param Source 源图像
       @return 返回提取出的频域幅值谱图像，需肉眼验证
    *}
    function ExtractImage(Source: TBitmap): TBitmap;

    {* 验证水印 (仅文字模式)
       @param Source 源图像
       @param TextToVerify 要验证的水印文本
       @return 返回置信度 (0.0 - 1.0)
    *}
    function Verify(Source: TBitmap; const TextToVerify: string): Double;

  published
    property Mode: TCnWatermarkMode read FMode write FMode default wmText;
    property Text: string read FText write FText;
    property WatermarkImage: TBitmap read FWatermarkImage write SetWatermarkImage;
    property StrengthLevel: TCnWatermarkStrength read FStrengthLevel write SetStrengthLevel default wsMedium;
    property Strength: Double read FStrength write SetStrength;
    property Font: TFont read FFont write SetFont;
    property Margin: Integer read FMargin write SetMargin default 2;

    property OnProgress: TCnWatermarkProgressEvent read FOnProgress write FOnProgress;
    property OnWatermarkImageReady: TCnWatermarkImageEvent read FOnWatermarkImageReady write FOnWatermarkImageReady;
  end;

implementation

resourcestring
  SCnErrorWatermarkNot24Bit = 'Image Pixel Format Must Be 24-Bit.';

const
  WATERMARK_CELL_SIZE = 8;
  STRENGTH_LOW = 15.0;
  STRENGTH_MEDIUM = 30.0;
  STRENGTH_HIGH = 50.0;

  DFT_BLOCK_SIZE = 256; // DFT 模式分块大小，需为 2 的幂

{ TCnWatermark }

constructor TCnWatermark.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrengthLevel := wsMedium;
  FStrength := STRENGTH_MEDIUM;
  FMode := wmText;
  FWatermarkImage := TBitmap.Create;
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Size := 28;
  FFont.Color := clBlack;
  FFont.Style := [fsBold];
  FMargin := 2;
end;

destructor TCnWatermark.Destroy;
begin
  FFont.Free;
  FWatermarkImage.Free;
  inherited Destroy;
end;

procedure TCnWatermark.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := 'CnWatermark';
  Author := 'CnPack Team';
  Email := 'master@cnpack.org';
  Comment := 'Invisible Digital Watermark Component (DCT Text / DFT Image)';
end;

procedure TCnWatermark.DoProgress(Percent: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Percent);
end;

procedure TCnWatermark.SetStrength(const Value: Double);
begin
  FStrength := Value;
  if (Abs(FStrength - STRENGTH_LOW) < 0.001) then
    FStrengthLevel := wsLow
  else if (Abs(FStrength - STRENGTH_MEDIUM) < 0.001) then
    FStrengthLevel := wsMedium
  else if (Abs(FStrength - STRENGTH_HIGH) < 0.001) then
    FStrengthLevel := wsHigh
  else
    FStrengthLevel := wsCustom;
end;

procedure TCnWatermark.SetStrengthLevel(const Value: TCnWatermarkStrength);
begin
  FStrengthLevel := Value;
  case FStrengthLevel of
    wsLow: FStrength := STRENGTH_LOW;
    wsMedium: FStrength := STRENGTH_MEDIUM;
    wsHigh: FStrength := STRENGTH_HIGH;
  end;
end;

procedure TCnWatermark.SetWatermarkImage(const Value: TBitmap);
begin
  FWatermarkImage.Assign(Value);
end;

procedure TCnWatermark.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;
procedure TCnWatermark.SetMargin(const Value: Integer);
begin
  if FMargin <> Value then
    FMargin := Value;
end;

procedure TCnWatermark.Embed(Source: TBitmap; Dest: TBitmap);
begin
  if (Source = nil) or (Source.Empty) then Exit;

  if FMode = wmText then
    EmbedText(Source, Dest)
  else
    EmbedImage(Source, Dest);
end;

procedure TCnWatermark.EmbedText(Source, Dest: TBitmap);
var
  TargetBmp: TBitmap;
  R, C, X, Y, W, H, TotalBlocks, ProcessedBlocks: Integer;
  M, T, MDCT, MIDCT: TCnFloatMatrix;
  P: PByte;
  BitIndex: Integer;
  CurrentBit: Boolean;
  T1, T2: Double;
  BitValue: Integer;

  function GetWatermarkBit(Idx: Integer): Boolean;
  var
    CharIdx, BitPos: Integer;
    Ch: Byte;
  begin
    if Length(FText) = 0 then
    begin
      Result := False;
      Exit;
    end;
    // Infinite loop over the string
    CharIdx := (Idx div 8) mod Length(FText);
    BitPos := 7 - (Idx mod 8); // MSB first
    Ch := Ord(FText[CharIdx + 1]);
    Result := ((Ch shr BitPos) and 1) = 1;
  end;

begin
  if Length(FText) = 0 then Exit;

  if Dest = nil then
    TargetBmp := Source
  else
  begin
    TargetBmp := Dest;
    TargetBmp.Assign(Source);
  end;

  TargetBmp.PixelFormat := pf24bit;

  W := TargetBmp.Width div WATERMARK_CELL_SIZE;
  H := TargetBmp.Height div WATERMARK_CELL_SIZE;
  TotalBlocks := W * H;
  if TotalBlocks = 0 then Exit;

  M := TCnFloatMatrix.Create(WATERMARK_CELL_SIZE, WATERMARK_CELL_SIZE);
  T := TCnFloatMatrix.Create(WATERMARK_CELL_SIZE, WATERMARK_CELL_SIZE);
  MDCT := TCnFloatMatrix.Create;
  MIDCT := TCnFloatMatrix.Create;

  try
    CnGenerateDCT2Matrix(MDCT, WATERMARK_CELL_SIZE);
    CnMatrixTranspose(MDCT, MIDCT);

    BitIndex := 0;
    ProcessedBlocks := 0;

    for R := 0 to H - 1 do
    begin
      for C := 0 to W - 1 do
      begin
        // Read Block (Blue channel)
        for Y := 0 to WATERMARK_CELL_SIZE - 1 do
        begin
          P := TargetBmp.ScanLine[R * WATERMARK_CELL_SIZE + Y];
          Inc(P, C * WATERMARK_CELL_SIZE * 3);
          for X := 0 to WATERMARK_CELL_SIZE - 1 do
          begin
            M[X, Y] := P^;
            Inc(P, 3);
          end;
        end;

        // Forward DCT
        CnDCT2(M, T, MDCT, MIDCT);

        // Modulate coefficients (0,1) and (1,0)
        T1 := T[0, 1];
        T2 := T[1, 0];

        CurrentBit := GetWatermarkBit(BitIndex);
        Inc(BitIndex);

        if CurrentBit then // Embed '1'
        begin
          if T1 <= T2 + FStrength then
            T[0, 1] := T2 + FStrength + 1;
        end
        else // Embed '0'
        begin
          if T2 <= T1 + FStrength then
            T[1, 0] := T1 + FStrength + 1;
        end;

        // Inverse DCT
        CnIDCT2(T, M, MDCT, MIDCT);

        // Write Back
        for Y := 0 to WATERMARK_CELL_SIZE - 1 do
        begin
          P := TargetBmp.ScanLine[R * WATERMARK_CELL_SIZE + Y];
          Inc(P, C * WATERMARK_CELL_SIZE * 3);
          for X := 0 to WATERMARK_CELL_SIZE - 1 do
          begin
            BitValue := Round(M[X, Y]);
            if BitValue < 0 then BitValue := 0;
            if BitValue > 255 then BitValue := 255;
            P^ := Byte(BitValue);
            Inc(P, 3);
          end;
        end;

        Inc(ProcessedBlocks);
        if (ProcessedBlocks mod 100 = 0) or (ProcessedBlocks = TotalBlocks) then
          DoProgress(ProcessedBlocks * 100 div TotalBlocks);
      end;
    end;
  finally
    M.Free;
    T.Free;
    MDCT.Free;
    MIDCT.Free;
  end;
end;

procedure TCnWatermark.EmbedImage(Source, Dest: TBitmap);
var
  TargetBmp: TBitmap;
  WmBmp: TBitmap;
  Data: PCnComplexArray;
  P: PByte;
  X, Y, BX, BY, W, H, NewW, NewH: Integer;
  BlockSize: Integer;
  Val: Double;
  TotalBlocks, ProcessedBlocks: Integer;
  TempBmp: TBitmap;
  UseBmp: TBitmap;
  TextW, TextH: Integer;
  DestRect: TRect;
  W_mag: array of array of Double;
  su1, sv1, su2, sv2: Integer;
  dx, dy: Integer;
  Intensity: Byte;
  Mag, R_val, I_val: Double;
  Scale: Double;
begin
  TempBmp := nil;
  if FWatermarkImage.Empty then
  begin
    if FText = '' then Exit;
    TempBmp := TBitmap.Create;
    TempBmp.Canvas.Font.Assign(FFont);
    TextW := TempBmp.Canvas.TextWidth(FText);
    TextH := TempBmp.Canvas.TextHeight(FText);

    // Ensure minimum size
    if TextW < 8 then TextW := 8;
    if TextH < 8 then TextH := 8;

    TempBmp.Width := TextW + FMargin * 2;
    TempBmp.Height := TextH + FMargin * 2;

    TempBmp.Canvas.Brush.Color := clWhite;
    TempBmp.Canvas.FillRect(Rect(0, 0, TempBmp.Width, TempBmp.Height));

    TempBmp.Canvas.Font.Assign(FFont); // Re-assign in case resize affected it
    // Draw text at (Margin, Margin)
    TempBmp.Canvas.TextOut(FMargin, FMargin, FText);

    UseBmp := TempBmp;
  end
  else
  begin
    UseBmp := FWatermarkImage;
  end;

  DoWatermarkImageReady(UseBmp);

  BlockSize := DFT_BLOCK_SIZE;

  SetLength(W_mag, BlockSize, BlockSize);
  for Y := 0 to BlockSize - 1 do
    for X := 0 to BlockSize - 1 do
      W_mag[Y, X] := 0;

  if Dest = nil then
    TargetBmp := Source
  else
  begin
    TargetBmp := Dest;
    TargetBmp.Assign(Source);
  end;

  TargetBmp.PixelFormat := pf24bit;

  // 准备水印图: 缩放到 DFT 块的一半大小 (中频区域)
  WmBmp := TBitmap.Create;
  try
    WmBmp.PixelFormat := pf24bit;
    WmBmp.Width := BlockSize div 4; // 64 for BlockSize=256
    WmBmp.Height := BlockSize div 4;
    // 使用最近邻插值保持黑白图锐利
    WmBmp.Canvas.Brush.Color := clWhite;
    WmBmp.Canvas.FillRect(Rect(0, 0, WmBmp.Width, WmBmp.Height));

    // Calculate proportional rect
    Scale := 1.0;
    if (UseBmp.Width > 0) and (UseBmp.Height > 0) then
    begin
       if (UseBmp.Width / UseBmp.Height) > 1.0 then
         Scale := WmBmp.Width / UseBmp.Width
       else
         Scale := WmBmp.Height / UseBmp.Height;
    end;

    NewW := Round(UseBmp.Width * Scale);
    NewH := Round(UseBmp.Height * Scale);
    DestRect := Rect(
      (WmBmp.Width - NewW) div 2,
      (WmBmp.Height - NewH) div 2,
      (WmBmp.Width - NewW) div 2 + NewW,
      (WmBmp.Height - NewH) div 2 + NewH
    );

    WmBmp.Canvas.StretchDraw(DestRect, UseBmp);

    // W_mag
    for Y := 0 to WmBmp.Height - 1 do
    begin
      P := WmBmp.ScanLine[Y];
      for X := 0 to WmBmp.Width - 1 do
      begin
        // Convert to grayscale
        Intensity := Round(0.114 * P^ + 0.587 * PByteArray(P)^[1] + 0.299 * PByteArray(P)^[2]);
        Inc(P, 3);

        if Intensity < 128 then // Black/dark pixels form the watermark
        begin
          // To achieve spatial amplitude of A pixels, we need Mag = A * N^2 / 2
          // FStrength is 15-50. Let's map 30 to amplitude ~3.0
          // Val = 3.0 * 65536 / 2 = 98304
          // 30 * X = 98304 => X = 3276
          Val := (255 - Intensity) / 255.0 * FStrength * (BlockSize * BlockSize / 20.0);
          dy := Y - (WmBmp.Height div 2);
          dx := X - (WmBmp.Width div 2);

          // First copy (Bottom-Right in shifted coords)
          su1 := (BlockSize * 3) div 4 + dx;
          sv1 := (BlockSize * 3) div 4 + dy;
          if (su1 >= 0) and (su1 < BlockSize) and (sv1 >= 0) and (sv1 < BlockSize) then
            W_mag[sv1, su1] := W_mag[sv1, su1] + Val;

          // Second copy (Top-Left in shifted coords) to maintain conjugate symmetry
          su2 := BlockSize div 4 - dx;
          sv2 := BlockSize div 4 - dy;
          if (su2 >= 0) and (su2 < BlockSize) and (sv2 >= 0) and (sv2 < BlockSize) then
            W_mag[sv2, su2] := W_mag[sv2, su2] + Val;
        end;
      end;
    end;

    W := TargetBmp.Width;
    H := TargetBmp.Height;
    TotalBlocks := (W div BlockSize) * (H div BlockSize);
    ProcessedBlocks := 0;

    GetMem(Data, BlockSize * BlockSize * SizeOf(TCnComplexNumber));
    try
      for BY := 0 to (H div BlockSize) - 1 do
      begin
        for BX := 0 to (W div BlockSize) - 1 do
        begin
          // 1. Read block (Green channel)
          for Y := 0 to BlockSize - 1 do
          begin
            P := TargetBmp.ScanLine[BY * BlockSize + Y];
            Inc(P, (BX * BlockSize) * 3);
            for X := 0 to BlockSize - 1 do
            begin
              Data^[Y * BlockSize + X].R := PByteArray(P)^[1]; // Green channel
              Data^[Y * BlockSize + X].I := 0;
              Inc(P, 3);
            end;
          end;

          // 2. FFT
          CnFFT2(Data, BlockSize, BlockSize);

          // 3. Modify Magnitude
          for Y := 0 to BlockSize - 1 do
          begin
            for X := 0 to BlockSize - 1 do
            begin
              // su, sv are shifted coordinates
              su1 := (X + BlockSize div 2) mod BlockSize;
              sv1 := (Y + BlockSize div 2) mod BlockSize;

              if W_mag[sv1, su1] > 0 then
              begin
                R_val := Data^[Y * BlockSize + X].R;
                I_val := Data^[Y * BlockSize + X].I;
                Mag := Sqrt(Sqr(R_val) + Sqr(I_val));
                if Mag > 0 then
                begin
                  Data^[Y * BlockSize + X].R := R_val * (Mag + W_mag[sv1, su1]) / Mag;
                  Data^[Y * BlockSize + X].I := I_val * (Mag + W_mag[sv1, su1]) / Mag;
                end
                else
                begin
                  Data^[Y * BlockSize + X].R := W_mag[sv1, su1];
                  Data^[Y * BlockSize + X].I := 0;
                end;
              end;
            end;
          end;

          // 4. IFFT
          CnIFFT2(Data, BlockSize, BlockSize);

          // 5. 写回
          for Y := 0 to BlockSize - 1 do
          begin
            P := TargetBmp.ScanLine[BY * BlockSize + Y];
            Inc(P, (BX * BlockSize) * 3);
            for X := 0 to BlockSize - 1 do
            begin
              Val := Data^[Y * BlockSize + X].R;
              // Clip
              if Val < 0 then Val := 0;
              if Val > 255 then Val := 255;
              PByteArray(P)^[1] := Round(Val);
              Inc(P, 3);
            end;
          end;

          Inc(ProcessedBlocks);
          if TotalBlocks > 0 then
            DoProgress(ProcessedBlocks * 100 div TotalBlocks);
        end;
      end;
    finally
      FreeMem(Data);
    end;
  finally
    WmBmp.Free;
    if TempBmp <> nil then TempBmp.Free;
    SetLength(W_mag, 0, 0);
  end;
end;

function TCnWatermark.ExtractText(Source: TBitmap): string;
var
  W, H, R, C, X, Y: Integer;
  M, T, MDCT, MIDCT: TCnFloatMatrix;
  P: PByte;
  T1, T2: Double;
  Bits: TList;
  ByteVal: Byte;
begin
  Result := '';
  if (Source = nil) or (Source.Empty) then Exit;

  // Ensure pf24bit
  if Source.PixelFormat <> pf24bit then
    raise ECnWatermarkException.Create(SCnErrorWatermarkNot24Bit);

  W := Source.Width div WATERMARK_CELL_SIZE;
  H := Source.Height div WATERMARK_CELL_SIZE;
  if (W = 0) or (H = 0) then
    Exit;

  M := TCnFloatMatrix.Create(WATERMARK_CELL_SIZE, WATERMARK_CELL_SIZE);
  T := TCnFloatMatrix.Create(WATERMARK_CELL_SIZE, WATERMARK_CELL_SIZE);
  MDCT := TCnFloatMatrix.Create;
  MIDCT := TCnFloatMatrix.Create;
  Bits := TList.Create;

  try
    CnGenerateDCT2Matrix(MDCT, WATERMARK_CELL_SIZE);
    CnMatrixTranspose(MDCT, MIDCT);

    for R := 0 to H - 1 do
    begin
      for C := 0 to W - 1 do
      begin
        for Y := 0 to WATERMARK_CELL_SIZE - 1 do
        begin
          if Source.PixelFormat = pf24bit then
          begin
            P := Source.ScanLine[R * WATERMARK_CELL_SIZE + Y];
            Inc(P, C * WATERMARK_CELL_SIZE * 3);
            for X := 0 to WATERMARK_CELL_SIZE - 1 do
            begin
              M[X, Y] := P^;
              Inc(P, 3);
            end;
          end
          else
          begin
             // Slow fallback for non-24bit
             for X := 0 to WATERMARK_CELL_SIZE - 1 do
            M[X, Y] := Source.Canvas.Pixels[C * 8 + X, R * 8 + Y] and $FF;
          end;
        end;

        CnDCT2(M, T, MDCT, MIDCT);

        T1 := T[0, 1];
        T2 := T[1, 0];

        if T1 > T2 then
          Bits.Add(Pointer(1))
        else
          Bits.Add(Pointer(0));
      end;
    end;

    for R := 0 to (Bits.Count div 8) - 1 do
    begin
      ByteVal := 0;
      for C := 0 to 7 do
      begin
        if Bits[R * 8 + C] <> nil then
          ByteVal := ByteVal or (1 shl (7 - C));
      end;
      if ByteVal <> 0 then
        Result := Result + Chr(ByteVal);
    end;

  finally
    Bits.Free;
    M.Free;
    T.Free;
    MDCT.Free;
    MIDCT.Free;
  end;
end;

function TCnWatermark.ExtractImage(Source: TBitmap): TBitmap;
var
  BlockSize: Integer;
  Data: PCnComplexArray;
  P: PByte;
  X, Y, BX, BY, W, H: Integer;
  Mag: Double;
  MaxMag, MinMag: Double;
  Val: Byte;
  Spectrum: TBitmap;
  SumMag: array of array of Double;
  BlocksCount: Integer;
  Dist: Double;
begin
  Result := nil;
  if (Source = nil) or (Source.Empty) then
    Exit;

  BlockSize := DFT_BLOCK_SIZE;
  if (Source.Width < BlockSize) or (Source.Height < BlockSize) then
    Exit;

  Spectrum := TBitmap.Create;
  Spectrum.PixelFormat := pf24bit;
  Spectrum.Width := BlockSize;
  Spectrum.Height := BlockSize;

  SetLength(SumMag, BlockSize, BlockSize);
  for Y := 0 to BlockSize - 1 do
  begin
    for X := 0 to BlockSize - 1 do
      SumMag[Y, X] := 0;
  end;

  GetMem(Data, BlockSize * BlockSize * SizeOf(TCnComplexNumber));
  try
    W := Source.Width;
    H := Source.Height;
    BlocksCount := 0;

    // Grid over the image, taking overlapping blocks for better averaging
    for BY := 0 to (H - BlockSize) div (BlockSize div 2) do
    begin
      for BX := 0 to (W - BlockSize) div (BlockSize div 2) do
      begin
        // 1. Read Green channel
        for Y := 0 to BlockSize - 1 do
        begin
          if Source.PixelFormat = pf24bit then
          begin
            P := Source.ScanLine[BY * (BlockSize div 2) + Y];
            Inc(P, (BX * (BlockSize div 2)) * 3);
            for X := 0 to BlockSize - 1 do
            begin
              Data^[Y * BlockSize + X].R := PByteArray(P)^[1];
              Data^[Y * BlockSize + X].I := 0;
              Inc(P, 3);
            end;
          end
          else
          begin
            // Fallback
            for X := 0 to BlockSize - 1 do
            begin
              Data^[Y * BlockSize + X].R := Source.Canvas.Pixels[BX * (BlockSize div 2) + X, BY * (BlockSize div 2) + Y] and $FF;
              Data^[Y * BlockSize + X].I := 0;
            end;
          end;
        end;

        // 2. FFT
        CnFFT2(Data, BlockSize, BlockSize);

        // 3. Accumulate magnitude
        for Y := 0 to BlockSize - 1 do
        begin
          for X := 0 to BlockSize - 1 do
          begin
            Mag := Sqrt(Sqr(Data^[Y * BlockSize + X].R) + Sqr(Data^[Y * BlockSize + X].I));
            SumMag[Y, X] := SumMag[Y, X] + Mag;
          end;
        end;
        Inc(BlocksCount);
      end;
    end;

    if BlocksCount = 0 then
      Exit;

    MaxMag := -1.0;
    MinMag := 1.0e20;

    // Average and Log scale
    for Y := 0 to BlockSize - 1 do
    begin
      for X := 0 to BlockSize - 1 do
      begin
        Mag := SumMag[Y, X] / BlocksCount;
        Mag := Log10(1 + Mag);
        SumMag[Y, X] := Mag;

        // Calculate Min/Max for visualization
        // Ignore low frequencies (radius < 30 in unshifted coords) to avoid DC domination
        if X > BlockSize div 2 then
          BX := BlockSize - X
        else
          BX := X;
        if Y > BlockSize div 2 then
          BY := BlockSize - Y
        else
          BY := Y;

        Dist := Sqrt(Sqr(BX) + Sqr(BY));
        if Dist > 30 then
        begin
          if Mag > MaxMag then MaxMag := Mag;
          if Mag < MinMag then MinMag := Mag;
        end;
      end;
    end;

    if MaxMag <= MinMag then
      MaxMag := MinMag + 1.0;

    // Render Spectrum with FFTShift
    for Y := 0 to BlockSize - 1 do
    begin
      P := Spectrum.ScanLine[Y];
      for X := 0 to BlockSize - 1 do
      begin
        // Shifted coordinates
        Mag := SumMag[(Y + BlockSize div 2) mod BlockSize, (X + BlockSize div 2) mod BlockSize];

        Val := Round(((Mag - MinMag) / (MaxMag - MinMag)) * 255);
        if Val > 255 then Val := 255;
        if Val < 0 then Val := 0;

        P^ := Val;
        Inc(P);
        P^ := Val;
        Inc(P);
        P^ := Val;
        Inc(P);
      end;
    end;

    Result := Spectrum;
  finally
    FreeMem(Data);
    SetLength(SumMag, 0, 0);
  end;
end;

function TCnWatermark.Verify(Source: TBitmap; const TextToVerify: string): Double;
var
  Extracted: string;
  LenPattern, LenExt, MatchCount, I, j: Integer;
  Found: Boolean;
begin
  Result := 0.0;
  // Verify only supported in Text Mode currently
  if FMode <> wmText then Exit;

  if TextToVerify = '' then Exit;

  Extracted := ExtractText(Source);
  LenExt := Length(Extracted);
  LenPattern := Length(TextToVerify);

  if LenExt < LenPattern then Exit;

  MatchCount := 0;
  for I := 1 to LenExt - LenPattern + 1 do
  begin
    Found := True;
    for j := 0 to LenPattern - 1 do
    begin
      if Extracted[I + j] <> TextToVerify[j + 1] then
      begin
        Found := False;
        Break;
      end;
    end;
    if Found then Inc(MatchCount);
  end;

  if LenPattern > 0 then
    Result := MatchCount * LenPattern / LenExt;

  if Result > 1.0 then
    Result := 1.0;
end;

procedure TCnWatermark.DoWatermarkImageReady(Image: TBitmap);
begin
  if Assigned(FOnWatermarkImageReady) then
    FOnWatermarkImageReady(Self, Image);
end;

end.
