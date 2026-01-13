unit UnitGenQRCode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, CnCommon,
  Controls, Forms, Dialogs, StdCtrls, CnNative, CnQRCode;

type
  TFormQRTest = class(TForm)
    lblText: TLabel;
    btnShowQRImage: TButton;
    edtQRText: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnShowQRImageClick(Sender: TObject);
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
  FQRImage.Text := 'CnPack Sample QR Code.';
  FQRImage.QRErrorRecoveryLevel := erlM;
  FQRImage.CellSize := 10;
  FQRImage.Icon := Application.Icon;
end;

procedure TFormQRTest.btnShowQRImageClick(Sender: TObject);
begin
  FQRImage.Text := edtQRText.Text;
end;

end.

