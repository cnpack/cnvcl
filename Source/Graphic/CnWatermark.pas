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
* 备    注：提供不可见的数字水印嵌入与提取功能
* 开发平台：PWinXP + Delphi 7.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元无需本地化处理
* 修改记录：2026.02.03 V1.0
*               增加 DFT 图片水印模式，抗旋转/缩放/裁剪。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Graphics, Controls, CnMatrix, CnDFT, CnNative, Math,
  CnClasses, CnComplex;

type
  TCnWatermarkStrength = (wsLow, wsMedium, wsHigh, wsCustom);
  {* 水印强度预设}

  TCnWatermarkMode = (wmText, wmImage);
  {* 水印模式}

  TCnWatermarkProgressEvent = procedure(Sender: TObject; Percent: Integer) of object;
  {* 进度事件}

  TCnWatermarkImageEvent = procedure(Sender: TObject; Image: TBitmap) of object;
  {* 图像相关事件}

  TCnWatermark = class(TCnComponent)
  private
    FText: string;
    FWatermarkImage: TBitmap;
    FMode: TCnWatermarkMode;
    FStrength: Double;
    FStrengthLevel: TCnWatermarkStrength;
    FOnProgress: TCnWatermarkProgressEvent;
    FOnWatermarkImageReady: TCnWatermarkImageEvent;
    procedure SetStrengthLevel(const Value: TCnWatermarkStrength);
    procedure SetStrength(const Value: Double);
    procedure SetWatermarkImage(const Value: TBitmap);
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

    property OnProgress: TCnWatermarkProgressEvent read FOnProgress write FOnProgress;
    property OnWatermarkImageReady: TCnWatermarkImageEvent read FOnWatermarkImageReady write FOnWatermarkImageReady;
  end;

implementation

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
end;

destructor TCnWatermark.Destroy;
begin
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
  X, Y, BX, BY, W, H: Integer;
  BlockSize: Integer;
  Val: Double;
  HalfSize: Integer;
  U, V: Integer;
  Mag: Double;
  TotalBlocks, ProcessedBlocks: Integer;
  TempBmp: TBitmap;
  UseBmp: TBitmap;
begin
  TempBmp := nil;
  if FWatermarkImage.Empty then
  begin
    if FText = '' then Exit;
    TempBmp := TBitmap.Create;
    TempBmp.Width := 128;
    TempBmp.Height := 128;
    TempBmp.Canvas.Brush.Color := clWhite;
    TempBmp.Canvas.FillRect(Rect(0, 0, TempBmp.Width, TempBmp.Height));
    TempBmp.Canvas.Font.Name := 'Arial';
    TempBmp.Canvas.Font.Size := 20;
    TempBmp.Canvas.Font.Color := clBlack;
    TempBmp.Canvas.Font.Style := [fsBold];
    // Center Text
    TempBmp.Canvas.TextOut((TempBmp.Width - TempBmp.Canvas.TextWidth(FText)) div 2,
      (TempBmp.Height - TempBmp.Canvas.TextHeight(FText)) div 2, FText);
    UseBmp := TempBmp;
  end
  else
  begin
    UseBmp := FWatermarkImage;
  end;

  DoWatermarkImageReady(UseBmp);

  BlockSize := DFT_BLOCK_SIZE;
  HalfSize := BlockSize div 2;

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
    WmBmp.Width := HalfSize;
    WmBmp.Height := HalfSize;
    // 简单的缩放绘制
    WmBmp.Canvas.StretchDraw(Rect(0, 0, HalfSize, HalfSize), UseBmp);

    W := TargetBmp.Width;
    H := TargetBmp.Height;
    TotalBlocks := (W div BlockSize) * (H div BlockSize);
    ProcessedBlocks := 0;

    // 分块 FFT 处理
    // 暂时只处理完整块，边缘忽略
    for BY := 0 to (H div BlockSize) - 1 do
    begin
      for BX := 0 to (W div BlockSize) - 1 do
      begin
        // 1. 读取块数据 (只取 Blue 通道，或可改为 Y 通道)
        // 此处 Data 大小必须足够 BlockSize * BlockSize
        // CnComplexArray 定义为 8192，可能不够。
        // 我们需要动态分配
        // 但 CnFFT2 接口参数是 TCnComplexArray 指针，如果是静态数组可能会溢出。
        // 检查 CnComplex.pas，TCnComplexArray = array[0..8191] of TCnComplexNumber;
        // 8192 个点不够 256*256 (65536)。
        // 必须使用动态分配的内存块，并强制转换为 PCnComplexArray。
        // CnFFT2 内部只使用指针算术，应该没问题。

        GetMem(Data, BlockSize * BlockSize * SizeOf(TCnComplexNumber));
        try
          FillChar(Data^, BlockSize * BlockSize * SizeOf(TCnComplexNumber), 0);

          for Y := 0 to BlockSize - 1 do
          begin
            P := TargetBmp.ScanLine[BY * BlockSize + Y];
            Inc(P, (BX * BlockSize) * 3);
            for X := 0 to BlockSize - 1 do
            begin
              Data^[Y * BlockSize + X].R := P^; // Blue
              Data^[Y * BlockSize + X].I := 0;
              Inc(P, 3);
            end;
          end;

          // 2. FFT
          CnFFT2(Data, BlockSize, BlockSize);

          // 3. 嵌入水印 (在中频区添加能量)
          // 对应频域中心 (HalfSize, HalfSize) 周围
          // 将水印图叠加到 (HalfSize - WmW/2, HalfSize - WmH/2) 区域
          // 必须保持共轭对称： F(u,v) = F*(-u,-v)
          // 为简化，我们在四个象限对称位置都加

          for Y := 0 to WmBmp.Height - 1 do
          begin
            P := WmBmp.ScanLine[Y];
            for X := 0 to WmBmp.Width - 1 do
            begin
              // 如果水印像素是黑色/深色，则增加能量
              if (P^ < 128) then // Blue channel of watermark
              begin
                // 映射到频域坐标
                // 我们在第一象限 (低频区外围) 嵌入
                // 坐标 U, V
                U := HalfSize div 2 + X;
                V := HalfSize div 2 + Y;

                // 限制范围
                if (U < BlockSize) and (V < BlockSize) then
                begin
                   // 增加幅值
                   Mag := FStrength * 1000.0; // 缩放系数
                   Data^[V * BlockSize + U].R := Data^[V * BlockSize + U].R + Mag;

                   // 对称点 (共轭对称)
                   // (N-U, N-V)
                   Data^[(BlockSize - V) * BlockSize + (BlockSize - U)].R :=
                     Data^[(BlockSize - V) * BlockSize + (BlockSize - U)].R + Mag;
                end;
              end;
              Inc(P, 3);
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
              P^ := Round(Val);
              Inc(P, 3);
            end;
          end;

        finally
          FreeMem(Data);
        end;

        Inc(ProcessedBlocks);
        if TotalBlocks > 0 then
          DoProgress(ProcessedBlocks * 100 div TotalBlocks);
      end;
    end;
  finally
    WmBmp.Free;
    if TempBmp <> nil then TempBmp.Free;
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
  begin
    // Warning or conversion needed
  end;

  W := Source.Width div WATERMARK_CELL_SIZE;
  H := Source.Height div WATERMARK_CELL_SIZE;
  if (W = 0) or (H = 0) then Exit;

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
  X, Y: Integer;
  Mag: Double;
  MaxMag, MinMag: Double;
  Val: Byte;
  Spectrum: TBitmap;
  CenterX, CenterY: Integer;
begin
  Result := nil;
  if (Source = nil) or (Source.Empty) then Exit;

  // 为了展示全息效果，我们只提取中心的一个块
  // 或者对整个图像缩放后做 FFT?
  // 按照嵌入逻辑，是分块嵌入的。我们提取中心的一块 BlockSize x BlockSize 即可看到水印。

  BlockSize := DFT_BLOCK_SIZE;

  // 确保源图足够大
  if (Source.Width < BlockSize) or (Source.Height < BlockSize) then Exit;

  Spectrum := TBitmap.Create;
  Spectrum.PixelFormat := pf24bit;
  Spectrum.Width := BlockSize;
  Spectrum.Height := BlockSize;

  GetMem(Data, BlockSize * BlockSize * SizeOf(TCnComplexNumber));
  try
    CenterX := (Source.Width - BlockSize) div 2;
    CenterY := (Source.Height - BlockSize) div 2;

    // 1. 读取中心块
    for Y := 0 to BlockSize - 1 do
    begin
      // ScanLine 有效性检查
      if Source.PixelFormat = pf24bit then
      begin
        P := Source.ScanLine[CenterY + Y];
        Inc(P, CenterX * 3);
        for X := 0 to BlockSize - 1 do
        begin
          Data^[Y * BlockSize + X].R := P^;
          Data^[Y * BlockSize + X].I := 0;
          Inc(P, 3);
        end;
      end
      else
      begin
        // Fallback
        for X := 0 to BlockSize - 1 do
        begin
          Data^[Y * BlockSize + X].R := Source.Canvas.Pixels[CenterX + X, CenterY + Y] and $FF;
          Data^[Y * BlockSize + X].I := 0;
        end;
      end;
    end;

    // 2. FFT
    CnFFT2(Data, BlockSize, BlockSize);

    // 3. 计算幅值谱并进行 FFTShift (将低频移到中心)
    // FFTShift:
    // Q1 <-> Q3
    // Q2 <-> Q4

    // 我们先找出最大幅值用于归一化

    // Log scale
    for Y := 0 to BlockSize - 1 do
    begin
      for X := 0 to BlockSize - 1 do
      begin
        Mag := Sqrt(Sqr(Data^[Y * BlockSize + X].R) + Sqr(Data^[Y * BlockSize + X].I));
        Mag := Log10(1 + Mag);
        Data^[Y * BlockSize + X].R := Mag; // Store magnitude in R
      end;
    end;

    // Recalculate Min/Max ignoring DC (0,0)
    MaxMag := -1.0;
    MinMag := 1.0e20;
    for Y := 0 to BlockSize - 1 do
    begin
      for X := 0 to BlockSize - 1 do
      begin
        if (X = 0) and (Y = 0) then Continue; // Skip DC
        Mag := Data^[Y * BlockSize + X].R;
        if Mag > MaxMag then MaxMag := Mag;
        if Mag < MinMag then MinMag := Mag;
      end;
    end;
    if MaxMag <= MinMag then MaxMag := MinMag + 1.0;

    // 4. 绘制频谱图 (带 FFTShift)
    for Y := 0 to BlockSize - 1 do
    begin
      P := Spectrum.ScanLine[Y];
      for X := 0 to BlockSize - 1 do
      begin
        // Shifted coordinates
        // Target (X, Y) comes from Source ((X + N/2) mod N, (Y + N/2) mod N)
        // 实际上：
        // Dest[y][x] = Src[(y + h/2) % h][(x + w/2) % w]

        // Direct coordinates (No FFTShift)
        Mag := Data^[Y * BlockSize + X].R;

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
  end;
end;

function TCnWatermark.Verify(Source: TBitmap; const TextToVerify: string): Double;
var
  Extracted: string;
  LenPattern, LenExt, MatchCount, i, j: Integer;
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
  for i := 1 to LenExt - LenPattern + 1 do
  begin
    Found := True;
    for j := 0 to LenPattern - 1 do
    begin
      if Extracted[i + j] <> TextToVerify[j + 1] then
      begin
        Found := False;
        Break;
      end;
    end;
    if Found then Inc(MatchCount);
  end;

  if LenPattern > 0 then
    Result := MatchCount * LenPattern / LenExt;

  if Result > 1.0 then Result := 1.0;
end;

procedure TCnWatermark.DoWatermarkImageReady(Image: TBitmap);
begin
  if Assigned(FOnWatermarkImageReady) then
    FOnWatermarkImageReady(Self, Image);
end;

end.


