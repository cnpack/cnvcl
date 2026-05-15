unit UnitQRCode;

interface

uses
  SysUtils, Classes, System.Types, System.UITypes,
  FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.ExtCtrls, FMX.Edit, FMX.Memo, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation, FMX.Objects,
  CnQRCode, CnQRImage;

type
  TFormQRTest = class(TForm)
    lblText: TLabel;
    btnShowQRImage: TButton;
    edtQRText: TEdit;
    Image1: TImage;
    btnTestDecode: TButton;
    btnTestDecodeImage: TButton;
    btnOpenFileDecode: TButton;
    mmoDecodeResult: TMemo;
    dlgOpenPic: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnShowQRImageClick(Sender: TObject);
    procedure btnTestDecodeClick(Sender: TObject);
    procedure btnTestDecodeImageClick(Sender: TObject);
    procedure btnOpenFileDecodeClick(Sender: TObject);
  private
    FEncoder: TCnQREncoder;
    procedure PaintQRCode;
  public
    destructor Destroy; override;
  end;

var
  FormQRTest: TFormQRTest;

implementation

{$R *.fmx}

destructor TFormQRTest.Destroy;
begin
  FEncoder.Free;
  inherited;
end;

procedure TFormQRTest.FormCreate(Sender: TObject);
begin
  FEncoder := TCnQREncoder.Create;
  FEncoder.Text := 'CnPack Sample QR Code 以及汉字';
  FEncoder.QRErrorRecoveryLevel := erlM;
  PaintQRCode;
end;

procedure TFormQRTest.PaintQRCode;
var
  Bmp: FMX.Graphics.TBitmap;
  Size, CellSize, Margin, X, Y, ImgSize: Integer;
begin
  Size := FEncoder.QRSize;
  CellSize := 10;
  Margin := 4;
  ImgSize := Size * CellSize + Margin * 2;

  Bmp := FMX.Graphics.TBitmap.Create(ImgSize, ImgSize);
  try
    Bmp.Clear(TAlphaColorRec.White);

    Bmp.Canvas.BeginScene;
    try
      Bmp.Canvas.Fill.Color := TAlphaColorRec.Black;
      for Y := 0 to Size - 1 do
        for X := 0 to Size - 1 do
          if FEncoder.QRData[X, Y] = 1 then
            Bmp.Canvas.FillRect(
              RectF(X * CellSize + Margin, Y * CellSize + Margin,
                    (X + 1) * CellSize + Margin, (Y + 1) * CellSize + Margin),
              0, 0, AllCorners, $FF);
    finally
      Bmp.Canvas.EndScene;
    end;

    Image1.Bitmap := Bmp;
  finally
    Bmp.Free;
  end;
end;

procedure TFormQRTest.btnShowQRImageClick(Sender: TObject);
begin
  FEncoder.Text := edtQRText.Text;
  FEncoder.QRErrorRecoveryLevel := erlM;
  PaintQRCode;
end;

procedure TFormQRTest.btnTestDecodeClick(Sender: TObject);
var
  DecodedText, TestText: string;
begin
  mmoDecodeResult.Lines.Clear;
  TestText := 'CnPack QRCode Decode Test 123!@#';

  FEncoder.Text := TestText;
  FEncoder.QRErrorRecoveryLevel := erlM;

  try
    DecodedText := CnQRDecodeFromMatrix(FEncoder.QRData);
    mmoDecodeResult.Lines.Add('=== Matrix Decode Test ===');
    mmoDecodeResult.Lines.Add('');
    mmoDecodeResult.Lines.Add('Original: ' + TestText);
    mmoDecodeResult.Lines.Add('Decoded:  ' + DecodedText);
    mmoDecodeResult.Lines.Add('');
    if DecodedText = TestText then
      mmoDecodeResult.Lines.Add('Result: Match, Pass!')
    else
      mmoDecodeResult.Lines.Add('Result: Mismatch, Fail!');
  except
    on E: Exception do
      mmoDecodeResult.Lines.Add('Decode error: ' + E.Message);
  end;
end;

procedure TFormQRTest.btnTestDecodeImageClick(Sender: TObject);
var
  Bmp: FMX.Graphics.TBitmap;
  GrayImage: TCnQRData;
  DecodedText: string;
  Size, X, Y, CellSize, Margin, ImgSize: Integer;
begin
  mmoDecodeResult.Lines.Clear;

  FEncoder.Text := edtQRText.Text;
  FEncoder.QRErrorRecoveryLevel := erlM;

  Size := FEncoder.QRSize;
  CellSize := 8;
  Margin := 4;
  ImgSize := Size * CellSize + Margin * 2;

  Bmp := FMX.Graphics.TBitmap.Create(ImgSize, ImgSize);
  try
    Bmp.Clear(TAlphaColorRec.White);

    Bmp.Canvas.BeginScene;
    try
      Bmp.Canvas.Fill.Color := TAlphaColorRec.Black;
      for X := 0 to Size - 1 do
        for Y := 0 to Size - 1 do
          if FEncoder.QRData[X, Y] = 1 then
            Bmp.Canvas.FillRect(
              RectF(X * CellSize + Margin, Y * CellSize + Margin,
                    (X + 1) * CellSize + Margin, (Y + 1) * CellSize + Margin),
              0, 0, AllCorners, $FF);
    finally
      Bmp.Canvas.EndScene;
    end;

    GrayImage := CnFMXBitmapToGrayImage(Bmp);

    mmoDecodeResult.Lines.Add('=== Image Decode Test ===');
    mmoDecodeResult.Lines.Add(Format('Image size: %d x %d', [Length(GrayImage), Length(GrayImage[0])]));
    mmoDecodeResult.Lines.Add('');

    try
      DecodedText := CnQRDecodeFromGrayImage(GrayImage);
      mmoDecodeResult.Lines.Add('Original: ' + FEncoder.Text);
      mmoDecodeResult.Lines.Add('Decoded:  ' + DecodedText);
      mmoDecodeResult.Lines.Add('');
      if DecodedText = FEncoder.Text then
        mmoDecodeResult.Lines.Add('Result: Image decode match, Pass!')
      else
        mmoDecodeResult.Lines.Add('Result: Image decode mismatch, Fail!');
    except
      on E: Exception do
        mmoDecodeResult.Lines.Add('Decode error: ' + E.Message);
    end;
  finally
    Bmp.Free;
  end;
end;

procedure TFormQRTest.btnOpenFileDecodeClick(Sender: TObject);
var
  Bmp: FMX.Graphics.TBitmap;
  GrayImage: TCnQRData;
  DecodedText: string;
begin
  mmoDecodeResult.Lines.Clear;

  if not dlgOpenPic.Execute then
    Exit;

  Bmp := FMX.Graphics.TBitmap.Create;
  try
    Bmp.LoadFromFile(dlgOpenPic.FileName);
    GrayImage := CnFMXBitmapToGrayImage(Bmp);
    try
      DecodedText := CnQRDecodeFromGrayImage(GrayImage);
      mmoDecodeResult.Lines.Add('=== File Image Decode ===');
      mmoDecodeResult.Lines.Add('');
      mmoDecodeResult.Lines.Add('File: ' + ExtractFileName(dlgOpenPic.FileName));
      mmoDecodeResult.Lines.Add('Size: ' + IntToStr(Bmp.Width) + 'x' + IntToStr(Bmp.Height));
      mmoDecodeResult.Lines.Add('');
      mmoDecodeResult.Lines.Add('Decoded: ' + DecodedText);
    except
      on E: Exception do
        mmoDecodeResult.Lines.Add('Decode error: ' + E.Message);
    end;
  finally
    Bmp.Free;
  end;
end;

end.