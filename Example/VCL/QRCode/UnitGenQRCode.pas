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
    mmoDecodeResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnShowQRImageClick(Sender: TObject);
    procedure shpBackColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure shpForeColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnTestDecodeClick(Sender: TObject);
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

end.

