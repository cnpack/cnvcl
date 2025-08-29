unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, {$IFNDEF VER130} Variants, {$ENDIF} CnEdit;

type
  TFrmMain = class(TForm)
    edtNormalText: TCnEdit;
    lblNormalText: TLabel;
    edtIntegerText: TCnEdit;
    lblIntegerText: TLabel;
    lblFloatText: TLabel;
    edtFloatText: TCnEdit;
    lblIdentText: TLabel;
    edtIdentText: TCnEdit;
    lblIntegerText2: TLabel;
    edtIntegerText2: TCnEdit;
    lblFloatText2: TLabel;
    edtFloatText2: TCnEdit;
    btnGetValue: TButton;
    edt1: TCnEdit;
    lbl1: TLabel;
    procedure btnGetValueClick(Sender: TObject);
    procedure EditorExit(Sender: TObject);
    procedure edt1ButtonClick(Sender: TObject);
  private
    FLastEditor: TCnEdit;
  public

  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

procedure TFrmMain.btnGetValueClick(Sender: TObject);
var
  Value: Variant;
begin
  if not Assigned(FLastEditor) then Exit;
  Value := FLastEditor.Value;
  if VarType(Value) = varString then // uses Variants to support D6/7
    ShowMessage(Value)
  else
    ShowMessage(FloatToStr(Value));
end;

procedure TFrmMain.EditorExit(Sender: TObject);
begin
  if Sender is TCnEdit then
    FLastEditor := Sender as TCnEdit;
end;

procedure TFrmMain.edt1ButtonClick(Sender: TObject);
begin
  ShowMessage('Button Clicked');
end;

end.
