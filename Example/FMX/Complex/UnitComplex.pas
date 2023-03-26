unit UnitComplex;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnComplex, FMX.Edit, FMX.Types, FMX.Controls.Presentation;

type
  TFormComplex = class(TForm)
    grpComplex: TGroupBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    edtComplexAR: TEdit;
    edtComplexAI: TEdit;
    edtComplexBR: TEdit;
    edtComplexBI: TEdit;
    btnAdd: TButton;
    btnSub: TButton;
    btnMul: TButton;
    btnDiv: TButton;
    edtComplexResult: TEdit;
    procedure btnAddClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnMulClick(Sender: TObject);
    procedure btnDivClick(Sender: TObject);
  private
    FC1, FC2, FCR: TCnComplexNumber;
  public
    procedure SetComplexValue;
    procedure ShowComplexValue;
  end;

var
  FormComplex: TFormComplex;

implementation

{$R *.fmx}

procedure TFormComplex.SetComplexValue;
begin
  ComplexNumberSetValue(FC1, edtComplexAR.Text, edtComplexAI.Text);
  ComplexNumberSetValue(FC2, edtComplexBR.Text, edtComplexBI.Text);
end;

procedure TFormComplex.ShowComplexValue;
begin
  edtComplexResult.Text := ComplexNumberToString(FCR);
end;

procedure TFormComplex.btnAddClick(Sender: TObject);
begin
  SetComplexValue;
  ComplexNumberAdd(FCR, FC1, FC2);
  ShowComplexValue;
end;

procedure TFormComplex.btnSubClick(Sender: TObject);
begin
  SetComplexValue;
  ComplexNumberSub(FCR, FC1, FC2);
  ShowComplexValue;
end;

procedure TFormComplex.btnMulClick(Sender: TObject);
begin
  SetComplexValue;
  ComplexNumberMul(FCR, FC1, FC2);
  ShowComplexValue;
end;

procedure TFormComplex.btnDivClick(Sender: TObject);
begin
  SetComplexValue;
  ComplexNumberDiv(FCR, FC1, FC2);
  ShowComplexValue;
end;

end.
