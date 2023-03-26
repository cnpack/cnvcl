unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, {$IFNDEF VER130} Variants, {$ENDIF} CnEdit;

type
  TfrmMain = class(TForm)
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
    { Private declarations }
    FLastEditor: TCnEdit;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnGetValueClick(Sender: TObject);
var
  value: Variant;
begin
  if not Assigned(FLastEditor) then Exit;
  value := FLastEditor.Value;
  if VarType(value) = varString then // uses Variants to support D6/7
    ShowMessage(value)
  else
    ShowMessage(FloatToStr(value));
end;

procedure TfrmMain.EditorExit(Sender: TObject);
begin
  if Sender is TCnEdit then
    FLastEditor := Sender as TCnEdit;
end;

procedure TfrmMain.edt1ButtonClick(Sender: TObject);
begin
  ShowMessage('Button Clicked');
end;

end.
