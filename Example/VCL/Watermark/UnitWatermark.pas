unit UnitWatermark;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, jpeg,
  StdCtrls, ExtCtrls, ComCtrls, CnNative, CnMatrix, CnDFT, CnWatermark;

type
  TFormWatermark = class(TForm)
    dlgOpen1: TOpenDialog;
    PageControl1: TPageControl;
    tsRaw: TTabSheet;
    img1: TImage;
    btnOpenFile: TButton;
    btnEmbed: TButton;
    btnExtract: TButton;
    edtWatermark: TEdit;
    memLog: TMemo;
    tsComponent: TTabSheet;
    img2: TImage;
    lblStrength: TLabel;
    btnCompLoad: TButton;
    edtCompText: TEdit;
    btnCompEmbed: TButton;
    btnCompExtract: TButton;
    btnCompVerify: TButton;
    memCompLog: TMemo;
    pbProgress: TProgressBar;
    cbbStrength: TComboBox;
    rbTextMode: TRadioButton;
    rbImageMode: TRadioButton;
    gbEmbed: TGroupBox;
    gbExtract: TGroupBox;
	btnLoadExtractImg: TButton;
    btnLoadWatermark: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEmbedClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnCompLoadClick(Sender: TObject);
    procedure btnCompEmbedClick(Sender: TObject);
    procedure btnCompExtractClick(Sender: TObject);
    procedure btnCompVerifyClick(Sender: TObject);
    procedure btnLoadWatermarkClick(Sender: TObject);
	procedure btnLoadExtractImgClick(Sender: TObject);
  private
    FJpg: TJPEGImage;
    FBmp: TBitmap;

    FCompExtractBmp: TBitmap;
    FCompExtractJpg: TJPEGImage;
    // For Component Demo
    FCompBmp: TBitmap;
    FCompJpg: TJPEGImage;
    FCnWatermark: TCnWatermark;
    FWatermarkImg: TBitmap;

    procedure Log(const Msg: string);
    procedure CompLog(const Msg: string);
    function ProcessWatermark(Embed: Boolean): Boolean;
    procedure OnWatermarkProgress(Sender: TObject; Percent: Integer);
  public

  end;

var
  FormWatermark: TFormWatermark;

implementation

uses
  CnDebug;

const
  WATERMARK_CELL_SIZE = 8;
  WATERMARK_STRENGTH = 30.0;

{$R *.DFM}

{$R-}

procedure TFormWatermark.FormCreate(Sender: TObject);
begin
  FJpg := TJPEGImage.Create;
  FBmp := TBitmap.Create;

  FCompBmp := TBitmap.Create;
  FCompJpg := TJPEGImage.Create;
  FWatermarkImg := TBitmap.Create;
  FCompExtractBmp := TBitmap.Create;
  FCompExtractJpg := TJPEGImage.Create;

  FCnWatermark := TCnWatermark.Create(Self);
  FCnWatermark.OnProgress := OnWatermarkProgress;

  cbbStrength.ItemIndex := 1; // Medium
end;

procedure TFormWatermark.FormDestroy(Sender: TObject);
begin
  FBmp.Free;
  FJpg.Free;
  FCompBmp.Free;
  FCompJpg.Free;
  FWatermarkImg.Free;
  FCompExtractBmp.Free;
  FCompExtractJpg.Free;
  // FCnWatermark is owned by Self, so it frees automatically
end;

procedure TFormWatermark.Log(const Msg: string);
begin
  memLog.Lines.Add(Msg);
end;

procedure TFormWatermark.CompLog(const Msg: string);
begin
  memCompLog.Lines.Add(Msg);
end;

procedure TFormWatermark.btnOpenFileClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
  begin
    FJpg.LoadFromFile(dlgOpen1.FileName);
    FBmp.Assign(FJpg);
    // Force 24-bit for consistent processing (BGR)
    FBmp.PixelFormat := pf24bit;

    img1.Picture.Assign(FBmp);
    Log('Loaded: ' + dlgOpen1.FileName);
    Log(Format('Size: %d x %d', [FBmp.Width, FBmp.Height]));
  end;
end;

function TFormWatermark.ProcessWatermark(Embed: Boolean): Boolean;
var
  R, C, X, Y, W, H: Integer;
  M, T, MDCT, MIDCT: TCnFloatMatrix;
  P: PByte;
  WatermarkStr: string;
  BitIndex: Integer;
  CurrentBit: Boolean;
  Bits: TList;
  BitValue: Integer;
  ByteVal: Byte;
  ExtractedStr: string;
  T1, T2: Double;

  function GetWatermarkBit(Idx: Integer): Boolean;
  var
    CharIdx, BitPos: Integer;
    Ch: Byte;
  begin
    if Length(WatermarkStr) = 0 then
    begin
      Result := False;
      Exit;
    end;

    // Infinite loop over the string
    CharIdx := (Idx div 8) mod Length(WatermarkStr);
    BitPos := 7 - (Idx mod 8); // MSB first
    Ch := Ord(WatermarkStr[CharIdx + 1]);
    Result := ((Ch shr BitPos) and 1) = 1;
  end;

begin
  Result := False;
  if FBmp.Empty then
  begin
    Log('Please open an image first.');
    Exit;
  end;

  W := FBmp.Width div WATERMARK_CELL_SIZE;
  H := FBmp.Height div WATERMARK_CELL_SIZE;

  M := TCnFloatMatrix.Create(WATERMARK_CELL_SIZE, WATERMARK_CELL_SIZE);
  T := TCnFloatMatrix.Create(WATERMARK_CELL_SIZE, WATERMARK_CELL_SIZE);
  MDCT := TCnFloatMatrix.Create;
  MIDCT := TCnFloatMatrix.Create;

  Bits := nil;
  if not Embed then
    Bits := TList.Create;

  try
    CnGenerateDCT2Matrix(MDCT, WATERMARK_CELL_SIZE);
    CnMatrixTranspose(MDCT, MIDCT);

    WatermarkStr := edtWatermark.Text;
    // Append a delimiter to make it easier to read in raw dump
    if Embed and (WatermarkStr <> '') then
       WatermarkStr := WatermarkStr + '   ';

    BitIndex := 0;

    for R := 0 to H - 1 do
    begin
      for C := 0 to W - 1 do
      begin
        // Read Block (Blue channel only for least visual impact)
        // Blue is usually P^ in pf24bit (BGR)
        for Y := 0 to WATERMARK_CELL_SIZE - 1 do
        begin
          P := FBmp.ScanLine[R * WATERMARK_CELL_SIZE + Y];
          Inc(P, C * WATERMARK_CELL_SIZE * 3);
          for X := 0 to WATERMARK_CELL_SIZE - 1 do
          begin
            M[X, Y] := P^;
            Inc(P, 3);
          end;
        end;

        // Forward DCT
        CnDCT2(M, T, MDCT, MIDCT);

        if Embed then
        begin
          CurrentBit := GetWatermarkBit(BitIndex);
          Inc(BitIndex);

          T1 := T[0, 1]; // Low freq Vertical
          T2 := T[1, 0]; // Low freq Horizontal

          if CurrentBit then
          begin
            // Embed '1': Ensure T1 > T2 + Strength
            if T1 <= T2 + WATERMARK_STRENGTH then
              T[0, 1] := T2 + WATERMARK_STRENGTH + 1;
          end
          else
          begin
            // Embed '0': Ensure T2 > T1 + Strength
            if T2 <= T1 + WATERMARK_STRENGTH then
              T[1, 0] := T1 + WATERMARK_STRENGTH + 1;
          end;

          // Inverse DCT
          CnIDCT2(T, M, MDCT, MIDCT);

          // Write Back to Blue channel
          for Y := 0 to WATERMARK_CELL_SIZE - 1 do
          begin
            P := FBmp.ScanLine[R * WATERMARK_CELL_SIZE + Y];
            Inc(P, C * WATERMARK_CELL_SIZE * 3);
            for X := 0 to WATERMARK_CELL_SIZE - 1 do
            begin
              BitValue := Round(M[X, Y]);
              // Clamp
              if BitValue < 0 then BitValue := 0;
              if BitValue > 255 then BitValue := 255;
              P^ := Byte(BitValue);
              Inc(P, 3);
            end;
          end;
        end
        else // Extract
        begin
           T1 := T[0, 1];
           T2 := T[1, 0];

           if T1 > T2 then
             Bits.Add(Pointer(1))
           else
             Bits.Add(Pointer(0));
        end;
      end;
    end;

    if Embed then
    begin
      img1.Picture.Assign(FBmp);
      Log('Watermark embedded. Total bits: ' + IntToStr(BitIndex));
      Log('Used Coefficients: (0,1) & (1,0) of Blue Channel.');
    end
    else
    begin
      // Reconstruct string
      ExtractedStr := '';

      // Decode every 8 bits
      for R := 0 to (Bits.Count div 8) - 1 do
      begin
        ByteVal := 0;
        for C := 0 to 7 do
        begin
          if Bits[R * 8 + C] <> nil then
            ByteVal := ByteVal or (1 shl (7 - C));
        end;
        // Filter non-printable chars for cleaner log
        if (ByteVal >= 32) and (ByteVal <= 126) then
          ExtractedStr := ExtractedStr + Chr(ByteVal)
        else
          ExtractedStr := ExtractedStr + '.';
      end;

      Log('Extracted Raw Data (First 200 chars):');
      Log(Copy(ExtractedStr, 1, 200));
      Log('Look for repeating pattern: "' + edtWatermark.Text + '"');
    end;

    Result := True;
  finally
    if Assigned(Bits) then Bits.Free;
    M.Free;
    T.Free;
    MDCT.Free;
    MIDCT.Free;
  end;
end;

procedure TFormWatermark.btnEmbedClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  Jpg: TJPEGImage;
begin
  if not ProcessWatermark(True) then
    Exit;

  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Filter := 'JPEG Image|*.jpg|Bitmap|*.bmp';
    SaveDlg.DefaultExt := 'jpg';
    if SaveDlg.Execute then
    begin
      if LowerCase(ExtractFileExt(SaveDlg.FileName)) = '.jpg' then
      begin
        Jpg := TJPEGImage.Create;
        try
          Jpg.Assign(FBmp);
          Jpg.CompressionQuality := 100; // High quality
          Jpg.SaveToFile(SaveDlg.FileName);
        finally
          Jpg.Free;
        end;
      end
      else
        FBmp.SaveToFile(SaveDlg.FileName);
      Log('Saved to ' + SaveDlg.FileName);
    end;
  finally
    SaveDlg.Free;
  end;
end;

procedure TFormWatermark.btnExtractClick(Sender: TObject);
begin
  ProcessWatermark(False);
end;

// =============================================================================
// Component Demo Event Handlers
// =============================================================================

procedure TFormWatermark.btnCompLoadClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
  begin
    FCompJpg.LoadFromFile(dlgOpen1.FileName);
    FCompBmp.Assign(FCompJpg);
    FCompBmp.PixelFormat := pf24bit;

    img2.Picture.Assign(FCompBmp);
    CompLog('Loaded: ' + dlgOpen1.FileName);
    CompLog(Format('Size: %d x %d', [FCompBmp.Width, FCompBmp.Height]));

    // Clear Progress
    pbProgress.Position := 0;
  end;
end;

procedure TFormWatermark.OnWatermarkProgress(Sender: TObject; Percent: Integer);
begin
  pbProgress.Position := Percent;
  Application.ProcessMessages;
end;

procedure TFormWatermark.btnCompEmbedClick(Sender: TObject);
var
  SaveDlg: TSaveDialog;
  Jpg: TJPEGImage;
begin
  if FCompBmp.Empty then
  begin
    CompLog('Please open an image first.');
    Exit;
  end;

  FCnWatermark.Text := edtCompText.Text;

  if rbImageMode.Checked then
  begin
    FCnWatermark.Mode := wmImage;
    if FWatermarkImg.Empty then
    begin
      CompLog('Error: Watermark image not loaded for Image Mode.');
      Exit;
    end;
    FCnWatermark.WatermarkImage := FWatermarkImg;
  end
  else
    FCnWatermark.Mode := wmText;

  case cbbStrength.ItemIndex of
    0: FCnWatermark.StrengthLevel := wsLow;
    1: FCnWatermark.StrengthLevel := wsMedium;
    2: FCnWatermark.StrengthLevel := wsHigh;
  end;

  CompLog('Embedding watermark...');
  pbProgress.Position := 0;

  // Embed directly into FCompBmp
  FCnWatermark.Embed(FCompBmp);

  img2.Picture.Assign(FCompBmp);
  CompLog('Done.');

  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Filter := 'JPEG Image|*.jpg|Bitmap|*.bmp';
    SaveDlg.DefaultExt := 'jpg';
    if SaveDlg.Execute then
    begin
      if LowerCase(ExtractFileExt(SaveDlg.FileName)) = '.jpg' then
      begin
        Jpg := TJPEGImage.Create;
        try
          Jpg.Assign(FCompBmp);
          Jpg.CompressionQuality := 95;
          Jpg.SaveToFile(SaveDlg.FileName);
        finally
          Jpg.Free;
        end;
      end
      else
        FCompBmp.SaveToFile(SaveDlg.FileName);
      CompLog('Saved to ' + SaveDlg.FileName);
    end;
  finally
    SaveDlg.Free;
  end;
end;

procedure TFormWatermark.btnLoadExtractImgClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
  begin
    FCompExtractJpg.LoadFromFile(dlgOpen1.FileName);
    FCompExtractBmp.Assign(FCompExtractJpg);
    FCompExtractBmp.PixelFormat := pf24bit;

    // Optional: Show loaded image somewhere?
    // For now just log
    CompLog('Target Image Loaded: ' + dlgOpen1.FileName);
    CompLog(Format('Size: %d x %d', [FCompExtractBmp.Width, FCompExtractBmp.Height]));
  end;
end;

procedure TFormWatermark.btnCompExtractClick(Sender: TObject);
var
  Res: string;
  ResBmp: TBitmap;
  SubForm: TForm;
  SubImg: TImage;
begin
  if FCompExtractBmp.Empty then
  begin
    CompLog('Please open a target image (in Extract Group) first.');
    Exit;
  end;

  CompLog('Extracting watermark...');

  if rbImageMode.Checked then
  begin
    FCnWatermark.Mode := wmImage;
    ResBmp := FCnWatermark.ExtractImage(FCompExtractBmp);
    if ResBmp <> nil then
    begin
      CompLog('Image Extracted. Showing in new window...');
      SubForm := TForm.Create(Self);
      SubForm.Caption := 'Extracted Watermark (Spectrogram)';
      SubForm.Width := ResBmp.Width + 40;
      SubForm.Height := ResBmp.Height + 60;
      SubForm.Position := poScreenCenter;

      SubImg := TImage.Create(SubForm);
      SubImg.Parent := SubForm;
      SubImg.Left := 10;
      SubImg.Top := 10;
      SubImg.AutoSize := True;
      SubImg.Picture.Assign(ResBmp);

      SubForm.Show; // Modeless
      ResBmp.Free;
    end;
  end
  else
  begin
    FCnWatermark.Mode := wmText;
    Res := FCnWatermark.ExtractText(FCompExtractBmp);
    CompLog('Raw Data (First 200 chars):');
    CompLog(Copy(Res, 1, 200));
  end;
end;

procedure TFormWatermark.btnCompVerifyClick(Sender: TObject);
var
  Confidence: Double;
begin
  if FCompExtractBmp.Empty then
  begin
    CompLog('Please open a target image (in Extract Group) first.');
    Exit;
  end;

  if rbImageMode.Checked then
  begin
    CompLog('Verify not supported for Image Mode. Please check extracted image visually.');
    Exit;
  end;

  CompLog('Verifying watermark: ' + edtCompText.Text);
  Confidence := FCnWatermark.Verify(FCompExtractBmp, edtCompText.Text);

  CompLog(Format('Confidence: %.2f%%', [Confidence * 100]));
  if Confidence > 0.8 then
    CompLog('Result: HIGH MATCH')
  else if Confidence > 0.5 then
    CompLog('Result: PROBABLE MATCH')
  else
    CompLog('Result: NO MATCH');
end;

procedure TFormWatermark.btnLoadWatermarkClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
  begin
    try
      FWatermarkImg.LoadFromFile(dlgOpen1.FileName);
      CompLog('Watermark Image Loaded: ' + dlgOpen1.FileName);
      rbImageMode.Checked := True;
    except
      on E: Exception do
        CompLog('Error loading watermark image: ' + E.Message);
    end;
  end;
end;

end.
