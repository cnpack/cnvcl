unit UnitComplex;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnComplex;

type
  TFormComplex = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
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
    btnAbsolute: TButton;
    btnArgument: TButton;
    btnSqrt: TButton;
    grpBigComplexDecimal: TGroupBox;
    lbl6: TLabel;
    lbl7: TLabel;
    edtBigDecAR: TEdit;
    edtBigDecAI: TEdit;
    lbl8: TLabel;
    edtBigDecBR: TEdit;
    edtBigDecBI: TEdit;
    lbl9: TLabel;
    edtBigDecResult: TEdit;
    btnBigDecAdd: TButton;
    btnBigDecSub: TButton;
    btnBigDecMul: TButton;
    btnBigDecDiv: TButton;
    btnBigDecAbsolute: TButton;
    btnBigDecArgument: TButton;
    btnBigDecConjugate: TButton;
    btnBigDecNegate: TButton;
    btnBigDecSetZero: TButton;
    btnBigDecSetOne: TButton;
    btnBigDecSetI: TButton;
    edtBigDecDetail: TEdit;
    procedure btnAddClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnMulClick(Sender: TObject);
    procedure btnDivClick(Sender: TObject);
    procedure btnAbsoluteClick(Sender: TObject);
    procedure btnArgumentClick(Sender: TObject);
    procedure btnSqrtClick(Sender: TObject);
    procedure btnBigDecAddClick(Sender: TObject);
    procedure btnBigDecSubClick(Sender: TObject);
    procedure btnBigDecMulClick(Sender: TObject);
    procedure btnBigDecDivClick(Sender: TObject);
    procedure btnBigDecAbsoluteClick(Sender: TObject);
    procedure btnBigDecArgumentClick(Sender: TObject);
    procedure btnBigDecConjugateClick(Sender: TObject);
    procedure btnBigDecNegateClick(Sender: TObject);
    procedure btnBigDecSetZeroClick(Sender: TObject);
    procedure btnBigDecSetOneClick(Sender: TObject);
    procedure btnBigDecSetIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FC1, FC2, FCR: TCnComplexNumber;
    FBigDec1, FBigDec2, FBigDecResult: TCnBigComplexDecimal;
    procedure SetComplexValue;
    procedure ShowComplexValue;
    procedure SetBigDecValue;
    procedure ShowBigDecValue;
  public

  end;

var
  FormComplex: TFormComplex;

implementation

{$R *.DFM}

procedure TFormComplex.FormCreate(Sender: TObject);
begin
  // Initialize BigComplexDecimal objects
  FBigDec1 := TCnBigComplexDecimal.Create;
  FBigDec2 := TCnBigComplexDecimal.Create;
  FBigDecResult := TCnBigComplexDecimal.Create;

  // Set default values for BigComplexDecimal
  edtBigDecAR.Text := '1.5';
  edtBigDecAI.Text := '2.5';
  edtBigDecBR.Text := '3.0';
  edtBigDecBI.Text := '1.0';
end;

procedure TFormComplex.FormDestroy(Sender: TObject);
begin
  // Free BigComplexDecimal objects
  FBigDec1.Free;
  FBigDec2.Free;
  FBigDecResult.Free;
end;

procedure TFormComplex.SetComplexValue;
begin
  ComplexNumberSetValue(FC1, edtComplexAR.Text, edtComplexAI.Text);
  ComplexNumberSetValue(FC2, edtComplexBR.Text, edtComplexBI.Text);
end;

procedure TFormComplex.ShowComplexValue;
begin
  edtComplexResult.Text := ComplexNumberToString(FCR);
end;

procedure TFormComplex.SetBigDecValue;
begin
  FBigDec1.SetValue(edtBigDecAR.Text, edtBigDecAI.Text);
  FBigDec2.SetValue(edtBigDecBR.Text, edtBigDecBI.Text);
end;

procedure TFormComplex.ShowBigDecValue;
begin
  edtBigDecResult.Text := FBigDecResult.ToString;
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

procedure TFormComplex.btnAbsoluteClick(Sender: TObject);
var
  R: Extended;
begin
  SetComplexValue;
  R := ComplexNumberAbsoluteValue(FC1);
  ShowMessage(FloatToStr(R));
end;

procedure TFormComplex.btnArgumentClick(Sender: TObject);
var
  R: Extended;
begin
  SetComplexValue;
  R := ComplexNumberArgument(FC1);
  ShowMessage(FloatToStr(R / PI) + ' ¦Ð');
end;

procedure TFormComplex.btnSqrtClick(Sender: TObject);
begin
  SetComplexValue;
  ComplexNumberSqrt(FCR, FC1);
  ShowComplexValue;
end;

// TCnBigComplexDecimal operations
procedure TFormComplex.btnBigDecAddClick(Sender: TObject);
begin
  SetBigDecValue;
  BigComplexDecimalAdd(FBigDecResult, FBigDec1, FBigDec2);
  ShowBigDecValue;
  edtBigDecDetail.Text := 'Addition operation completed';
end;

procedure TFormComplex.btnBigDecSubClick(Sender: TObject);
begin
  SetBigDecValue;
  BigComplexDecimalSub(FBigDecResult, FBigDec1, FBigDec2);
  ShowBigDecValue;
  edtBigDecDetail.Text := 'Subtraction operation completed';
end;

procedure TFormComplex.btnBigDecMulClick(Sender: TObject);
begin
  SetBigDecValue;
  BigComplexDecimalMul(FBigDecResult, FBigDec1, FBigDec2);
  ShowBigDecValue;
  edtBigDecDetail.Text := 'Multiplication operation completed';
end;

procedure TFormComplex.btnBigDecDivClick(Sender: TObject);
begin
  try
    SetBigDecValue;
    BigComplexDecimalDiv(FBigDecResult, FBigDec1, FBigDec2);
    ShowBigDecValue;
    edtBigDecDetail.Text := 'Division operation completed';
  except
    on E: EZeroDivide do
    begin
      edtBigDecResult.Text := 'Error: Division by zero';
      edtBigDecDetail.Text := E.Message;
    end;
  end;
end;

procedure TFormComplex.btnBigDecAbsoluteClick(Sender: TObject);
var
  AbsValue: Extended;
begin
  SetBigDecValue;
  AbsValue := FBigDec1.AbsoluteValue;
  edtBigDecResult.Text := 'Absolute: ' + FloatToStr(AbsValue);
  edtBigDecDetail.Text := 'Absolute value calculated';
end;

procedure TFormComplex.btnBigDecArgumentClick(Sender: TObject);
var
  ArgValue: Extended;
begin
  SetBigDecValue;
  ArgValue := FBigDec1.Argument;
  edtBigDecResult.Text := 'Argument: ' + FloatToStr(ArgValue) + ' radians';
  edtBigDecDetail.Text := 'Argument: ' + FloatToStr(ArgValue * 180 / Pi) + ' degrees';
end;

procedure TFormComplex.btnBigDecConjugateClick(Sender: TObject);
begin
  SetBigDecValue;
  FBigDec1.Conjugate;
  edtBigDecAR.Text := FBigDec1.R.ToString;
  edtBigDecAI.Text := FBigDec1.I.ToString;
  BigComplexDecimalCopy(FBigDecResult, FBigDec1);
  ShowBigDecValue;
  edtBigDecDetail.Text := 'Conjugate operation completed';
end;

procedure TFormComplex.btnBigDecNegateClick(Sender: TObject);
begin
  SetBigDecValue;
  FBigDec1.Negate;
  edtBigDecAR.Text := FBigDec1.R.ToString;
  edtBigDecAI.Text := FBigDec1.I.ToString;
  BigComplexDecimalCopy(FBigDecResult, FBigDec1);
  ShowBigDecValue;
  edtBigDecDetail.Text := 'Negate operation completed';
end;

procedure TFormComplex.btnBigDecSetZeroClick(Sender: TObject);
begin
  FBigDec1.SetZero;
  edtBigDecAR.Text := FBigDec1.R.ToString;
  edtBigDecAI.Text := FBigDec1.I.ToString;
  BigComplexDecimalCopy(FBigDecResult, FBigDec1);
  ShowBigDecValue;
  edtBigDecDetail.Text := 'Set to zero';
end;

procedure TFormComplex.btnBigDecSetOneClick(Sender: TObject);
begin
  FBigDec1.SetOne;
  edtBigDecAR.Text := FBigDec1.R.ToString;
  edtBigDecAI.Text := FBigDec1.I.ToString;
  BigComplexDecimalCopy(FBigDecResult, FBigDec1);
  ShowBigDecValue;
  edtBigDecDetail.Text := 'Set to one';
end;

procedure TFormComplex.btnBigDecSetIClick(Sender: TObject);
begin
  FBigDec1.SetI;
  edtBigDecAR.Text := FBigDec1.R.ToString;
  edtBigDecAI.Text := FBigDec1.I.ToString;
  BigComplexDecimalCopy(FBigDecResult, FBigDec1);
  ShowBigDecValue;
  edtBigDecDetail.Text := 'Set to i';
end;

end.
