unit UnitGenQRCode;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, CnCommon,
  Controls, Forms, Dialogs, ExtDlgs, StdCtrls, ExtCtrls, CnNative, CnQRImage, CnQRCode;

type

  { TFormQRTest }

  TFormQRTest = class(TForm)
    btnDecodeImage1: TButton;
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
    btnDecodeImage: TButton;
    dlgOpenPicture: TOpenPictureDialog;
    lblDecodeResult: TLabel;
    mmoResult: TMemo;
    imgDebug: TImage;
    lblImageInfo: TLabel;
    procedure btnDecodeImage1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnShowQRImageClick(Sender: TObject);
    procedure shpBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shpForeColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnDecodeImageClick(Sender: TObject);
  private
    FQRImage: TCnQRCodeImage;
  public

  end;

var
  FormQRTest: TFormQRTest;

implementation

{$R *.lfm}

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

procedure TFormQRTest.btnDecodeImage1Click(Sender: TObject);
begin
  if dlgOpenPicture.Execute then
    mmoResult.Lines.Add(CnFPCDecodeQRImageFile(dlgOpenPicture.FileName));
end;

procedure TFormQRTest.btnShowQRImageClick(Sender: TObject);
begin
  FQRImage.Text := edtQRText.Text;
  FQRImage.IconSize := StrToIntDef(edtIconSize.Text, 32);
  FQRImage.IconMargin := StrToIntDef(edtIconMargin.Text, 2);
  FQRImage.Color := shpBackColor.Brush.Color;
  FQRImage.ForeColor := shpForeColor.Brush.Color;
  // Hide debug image, show QR code
  imgDebug.Visible := False;
  FQRImage.Visible := True;
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

function ByteToHex(B: Byte): string;
begin
  Result := IntToHex(B, 2);
end;

function StrToHex(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + IntToHex(Ord(S[I]), 2);
  end;
end;

function Utf8StrToHex(const S: AnsiString): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + IntToHex(Ord(S[I]), 2);
  end;
end;

procedure TFormQRTest.btnDecodeImageClick(Sender: TObject);
var
  Pic: TPicture;
  Bmp: TBitmap;
  QRData: TCnQRData;
  DecodedText: string;
  P: PByteArray;
  FirstPixelR, FirstPixelG, FirstPixelB: Byte;
  InfoLine: string;
  DiagText: string;
begin
  if not dlgOpenPicture.Execute then
    Exit;

  Pic := TPicture.Create;
  Bmp := TBitmap.Create;
  try
    Pic.LoadFromFile(dlgOpenPicture.FileName);

    // Bmp.Assign(Pic.Graphic) in Lazarus does NOT correctly copy JPEG pixel data.
    // Use Canvas.Draw to rasterize the JPEG/PNG/BMP properly.
    Bmp.SetSize(Pic.Graphic.Width, Pic.Graphic.Height);
    Bmp.Canvas.Draw(0, 0, Pic.Graphic);

    // Show the real rasterized bitmap on debug image
    imgDebug.Picture.Assign(Bmp);
//    imgDebug.Visible := True;
//    FQRImage.Visible := False;

    // Convert to gray — sets pf24bit internally for ScanLine access
    QRData := CnBitmapToGrayImage(Bmp);

    // Build info AFTER conversion (Bmp is now pf24bit)
    InfoLine := Format('TBitmap: %dx%d pf=%d',
      [Bmp.Width, Bmp.Height, Ord(Bmp.PixelFormat)]);
    if (Bmp.Width > 0) and (Bmp.Height > 0) then
    begin
      P := Bmp.ScanLine[0];
      FirstPixelB := P[0];
      FirstPixelG := P[1];
      FirstPixelR := P[2];
      InfoLine := InfoLine + Format(' pixel[0]=(R%d G%d B%d)',
        [FirstPixelR, FirstPixelG, FirstPixelB]);
    end;
    if (Length(QRData) > 0) and (Length(QRData[0]) > 0) then
      InfoLine := InfoLine + Format(' QRData[0,0]=%d', [QRData[0,0]]);
    lblImageInfo.Caption := InfoLine;

    DecodedText := CnQRDecodeFromGrayImage(QRData);

    // Diagnostic: show decoded text + hex dump
    DiagText := 'Decoded: ' + DecodedText;
    DiagText := DiagText + #13#10 + 'Hex (AnsiString bytes): ' + StrToHex(DecodedText);
    DiagText := DiagText + #13#10 + 'Length: ' + IntToStr(Length(DecodedText)) + ' chars';
    mmoResult.Text := DiagText;
  except
    on E: Exception do
      mmoResult.Text := 'Decode Failed: ' + E.Message;
  end;
  Bmp.Free;
  Pic.Free;
end;

end.

