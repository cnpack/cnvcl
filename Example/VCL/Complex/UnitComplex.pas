unit UnitComplex;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnComplex;

type
  TFormComplex = class(TForm)
    grpComplex: TGroupBox;
    edtComplexAR: TEdit;
    lbl1: TLabel;
    edtComplexAI: TEdit;
    lbl2: TLabel;
    edtComplexBR: TEdit;
    lbl3: TLabel;
    edtComplexBI: TEdit;
    lbl4: TLabel;
    btnAdd: TButton;
    btnSub: TButton;
    btnMul: TButton;
    btnDiv: TButton;
    lbl5: TLabel;
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

{$R *.DFM}

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
