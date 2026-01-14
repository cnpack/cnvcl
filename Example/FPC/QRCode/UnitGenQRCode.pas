unit UnitGenQRCode;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, CnCommon,
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
    procedure FormCreate(Sender: TObject);
    procedure btnShowQRImageClick(Sender: TObject);
    procedure shpBackColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure shpForeColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

end.

