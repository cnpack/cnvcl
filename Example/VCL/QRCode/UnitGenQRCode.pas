unit UnitGenQRCode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, CnCommon,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, CnNative, CnQRImage, CnQRCode;

type
  TFormQRTest = class(TForm)
    lblText: TLabel;
    btnShowQRImage: TButton;
    edtQRText: TEdit;
    lblIconSize: TLabel;
    edtIconSize: TEdit;
    lblIconMargin: TLabel;
    edtIconMargin: TEdit;
    lblBackColor: TLabel;
    shpBackColor: TShape;
    lblForeColor: TLabel;
    shpForeColor: TShape;
    dlgColor: TColorDialog;
    btnTestDecode: TButton;
    btnTestDecodeImage: TButton;
    mmoDecodeResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnShowQRImageClick(Sender: TObject);
    procedure shpBackColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shpForeColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnTestDecodeClick(Sender: TObject);
    procedure btnTestDecodeImageClick(Sender: TObject);
  private
    FQRImage: TCnQRCodeImage;
  public

  end;

var
  FormQRTest: TFormQRTest;

implementation

{$R *.dfm}

procedure TFormQRTest.FormCreate(Sender: TObject);
begin
  FQRImage := TCnQRCodeImage.Create(Self);
  FQRImage.Parent := Self;
  FQRImage.Left := 10;
  FQRImage.Top := 10;
  FQRImage.Width := 450;
  FQRImage.Height := 450;
  FQRImage.Anchors := [akLeft, akTop, akBottom, akRight];
  FQRImage.Text := 'CnPack Sample QR Code 以及汉字';
  FQRImage.QRErrorRecoveryLevel := erlM;
  FQRImage.CellSize := 10;
  FQRImage.Icon := Application.Icon;
end;

procedure TFormQRTest.btnShowQRImageClick(Sender: TObject);
begin
  FQRImage.Text := edtQRText.Text;
  FQRImage.IconSize := StrToIntDef(edtIconSize.Text, 32);
  FQRImage.IconMargin := StrToIntDef(edtIconMargin.Text, 2);
  FQRImage.Color := shpBackColor.Brush.Color;
  FQRImage.ForeColor := shpForeColor.Brush.Color;
end;

procedure TFormQRTest.shpBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color := shpBackColor.Brush.Color;
  if dlgColor.Execute then
    shpBackColor.Brush.Color := dlgColor.Color;
end;

procedure TFormQRTest.shpForeColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  dlgColor.Color := shpForeColor.Brush.Color;
  if dlgColor.Execute then
    shpForeColor.Brush.Color := dlgColor.Color;
end;

procedure TFormQRTest.btnTestDecodeClick(Sender: TObject);
var
  Encoder: TCnQREncoder;
  DecodedText: string;
  TestText: string;
begin
  mmoDecodeResult.Clear;
  TestText := 'CnPack QRCode Decode Test 123!@#';

  // Step 1: 用编码器生成二维码矩阵
  Encoder := TCnQREncoder.Create;
  try
    Encoder.Text := TestText;
    Encoder.QRErrorRecoveryLevel := erlM;

    // Step 2: 调用 CnQRDecodeFromMatrix 解码
    try
      DecodedText := CnQRDecodeFromMatrix(Encoder.QRData);
      mmoDecodeResult.Lines.Add('=== 解码测试结果 ===');
      mmoDecodeResult.Lines.Add('');
      mmoDecodeResult.Lines.Add('原始文本: ' + TestText);
      mmoDecodeResult.Lines.Add('解码文本: ' + DecodedText);
      mmoDecodeResult.Lines.Add('');
      if DecodedText = TestText then
        mmoDecodeResult.Lines.Add('测试结果: 编解码一致，通过!')
      else
        mmoDecodeResult.Lines.Add('测试结果: 编解码不一致，失败!');
    except
      on E: Exception do
      begin
        mmoDecodeResult.Lines.Add('=== 解码异常 ===');
        mmoDecodeResult.Lines.Add('错误信息: ' + E.Message);
      end;
    end;
  finally
    Encoder.Free;
  end;
end;

procedure TFormQRTest.btnTestDecodeImageClick(Sender: TObject);
var
  Encoder: TCnQREncoder;
  Bitmap: TBitmap;
  GrayImage: TCnQRGrayImage;
  DecodedText: string;
  Size, X, Y, CellSize, Margin: Integer;
  Binarized: TCnQRData;
  BW, BH: Integer;
  LineStr: string;
  TLFP: TCnQRFinderPattern;
  TRFP: TCnQRFinderPattern;
  BLFP: TCnQRFinderPattern;
  ModuleSize: Double;
  Dimension: Integer;
  SrcPoints, DstPoints: array[0..3] of TCnQRPointF;
  Transform: TCnQRPerspectiveTransform;
  QRData: TCnQRData;
begin
  mmoDecodeResult.Clear;

  // 同步当前编辑框文本到 FQRImage
  btnShowQRImageClick(nil);

  // 用编码器生成矩阵并手动渲染为位图（Delphi 5 兼容，不依赖 PaintTo）
  Encoder := TCnQREncoder.Create;
  try
    Encoder.Text := edtQRText.Text;
    Encoder.QRErrorRecoveryLevel := erlM;

    Size := Encoder.QRSize;
    CellSize := 8;
    Margin := 4;

    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := Size * CellSize + Margin * 2;
      Bitmap.Height := Size * CellSize + Margin * 2;
      Bitmap.PixelFormat := pf24bit;

      // 白色背景
      Bitmap.Canvas.Brush.Color := clWhite;
      Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

      // 绘制二维码黑色模块
      Bitmap.Canvas.Brush.Color := clBlack;
      for X := 0 to Size - 1 do
        for Y := 0 to Size - 1 do
          if Encoder.QRData[X, Y] = 1 then
            Bitmap.Canvas.FillRect(Rect(
              X * CellSize + Margin, Y * CellSize + Margin,
              (X + 1) * CellSize + Margin, (Y + 1) * CellSize + Margin));

      // VCL TBitmap → TCnQRGrayImage
      GrayImage := CnBitmapToGrayImage(Bitmap);

      // ---- 调试：独立二值化并打印 ----
      Binarized := CnQRBinarize(GrayImage, BW, BH);
      mmoDecodeResult.Lines.Add('=== 调试: 二值化矩阵 (左上角 21x21) ===');
      mmoDecodeResult.Lines.Add(Format('矩阵尺寸: %d x %d', [BW, BH]));
      mmoDecodeResult.Lines.Add('');
      // 仅输出左上角 21 行（寻像图案所在区域）
      for Y := 0 to 20 do
      begin
        LineStr := '';
        for X := 0 to 20 do
        begin
          if Binarized[X, Y] = 1 then
            LineStr := LineStr + '##'
          else
            LineStr := LineStr + '  ';
        end;
        mmoDecodeResult.Lines.Add(Format('%2d: %s', [Y, LineStr]));
      end;
      mmoDecodeResult.Lines.Add('');

      // ---- 独立测试寻像图案检测 ----
      if CnQRFindFinderPatterns(Binarized, BW, BH, TLFP, TRFP, BLFP) then
      begin
        mmoDecodeResult.Lines.Add(Format('寻像图案: TL(%.1f,%.1f) TR(%.1f,%.1f) BL(%.1f,%.1f)',
          [TLFP.X, TLFP.Y, TRFP.X, TRFP.Y, BLFP.X, BLFP.Y]));
      end
      else
        mmoDecodeResult.Lines.Add('寻像图案: 未找到!');

      // ---- 调试：对比采样网格与原始编码矩阵 ----
      if CnQRFindFinderPatterns(Binarized, BW, BH, TLFP, TRFP, BLFP) then
      begin
        ModuleSize := CnQRCalcModuleSize(TLFP, TRFP, BLFP);
        Dimension := CnQRCalcDimension(TLFP, TRFP, BLFP, ModuleSize);
        mmoDecodeResult.Lines.Add(Format('Dimension=%d ModSize=%.1f', [Dimension, ModuleSize]));

        SrcPoints[0].X := TLFP.X;   SrcPoints[0].Y := TLFP.Y;
        SrcPoints[1].X := TRFP.X;   SrcPoints[1].Y := TRFP.Y;
        SrcPoints[2].X := BLFP.X;   SrcPoints[2].Y := BLFP.Y;
        SrcPoints[3].X := TRFP.X - TLFP.X + BLFP.X;
        SrcPoints[3].Y := TRFP.Y - TLFP.Y + BLFP.Y;

        DstPoints[0].X := 3.5;                    DstPoints[0].Y := 3.5;
        DstPoints[1].X := Dimension - 3.5;        DstPoints[1].Y := 3.5;
        DstPoints[2].X := 3.5;                    DstPoints[2].Y := Dimension - 3.5;
        DstPoints[3].X := Dimension - 3.5;        DstPoints[3].Y := Dimension - 3.5;

        Transform := CnQRCalcPerspectiveTransform(DstPoints, SrcPoints);
        QRData := CnQRSampleGrid(Binarized, BW, BH, Transform, Dimension);
        mmoDecodeResult.Lines.Add('=== 采样网格 0-8行 0-24列 ===');
        for Y := 0 to 8 do
        begin
          LineStr := Format('%2d:', [Y]);
          for X := 0 to 24 do
          begin
            if (X < Length(QRData)) and (Y < Length(QRData[X])) then
            begin
              if QRData[X, Y] = 1 then LineStr := LineStr + '#'
              else LineStr := LineStr + '.';
            end;
          end;
          // 对比原始编码矩阵
          LineStr := LineStr + '  |  ';
          for X := 0 to 24 do
          begin
            if (X < Length(Encoder.QRData)) and (Y < Length(Encoder.QRData[X])) then
            begin
              if Encoder.QRData[X, Y] = 1 then LineStr := LineStr + '#'
              else LineStr := LineStr + '.';
            end;
          end;
          mmoDecodeResult.Lines.Add(LineStr);
        end;
        SetLength(QRData, 0);
      end;

      // ---- 端到端解码 ----
      mmoDecodeResult.Lines.Add('');
      try
        DecodedText := CnQRDecodeFromGrayImage(GrayImage);
        mmoDecodeResult.Lines.Add('=== 图像解码测试结果 ===');
        mmoDecodeResult.Lines.Add('');
        mmoDecodeResult.Lines.Add('原始文本: ' + Encoder.Text);
        mmoDecodeResult.Lines.Add('解码文本: ' + DecodedText);
        mmoDecodeResult.Lines.Add('');
        if DecodedText = Encoder.Text then
          mmoDecodeResult.Lines.Add('测试结果: 图像编解码一致，通过!')
        else
          mmoDecodeResult.Lines.Add('测试结果: 图像编解码不一致，失败!');
      except
        on E: Exception do
        begin
          mmoDecodeResult.Lines.Add('=== 图像解码异常 ===');
          mmoDecodeResult.Lines.Add('错误信息: ' + E.Message);
        end;
      end;

      SetLength(Binarized, 0);
    finally
      Bitmap.Free;
    end;
  finally
    Encoder.Free;
  end;
end;

end.

