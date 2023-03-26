unit UnitBigRational;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, CnBigRational;

type
  TFormRational = class(TForm)
    grpBR1: TGroupBox;
    edtBR1N: TEdit;
    bvl1: TBevel;
    edtBR1D: TEdit;
    btnSet1: TButton;
    btnSet2: TButton;
    btnSet3: TButton;
    btnReduce: TButton;
    grpRN2: TGroupBox;
    bvlRN2: TBevel;
    edtBR2N: TEdit;
    edtBR2D: TEdit;
    btnAdd: TButton;
    btnSub: TButton;
    btnMul: TButton;
    btnDiv: TButton;
    btnRN2SetValue: TButton;
    lblFloat: TLabel;
    edtExtended: TEdit;
    btnSetExtended: TButton;
    btnSetString: TButton;
    btnToDec: TButton;
    btnAddInt: TButton;
    btnSubInt: TButton;
    btnMulInt: TButton;
    btnDivInt: TButton;
    edtInt: TEdit;
    btnBNAdd: TButton;
    btnBNSub: TButton;
    btnBNMul: TButton;
    btnBNDiv: TButton;
    btnCompare: TButton;
    btnCompareInt: TButton;
    btnCompareBN: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSet1Click(Sender: TObject);
    procedure btnSet2Click(Sender: TObject);
    procedure btnSet3Click(Sender: TObject);
    procedure btnReduceClick(Sender: TObject);
    procedure btnRN2SetValueClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnMulClick(Sender: TObject);
    procedure btnDivClick(Sender: TObject);
    procedure btnSetExtendedClick(Sender: TObject);
    procedure btnSetStringClick(Sender: TObject);
    procedure btnToDecClick(Sender: TObject);
    procedure btnAddIntClick(Sender: TObject);
    procedure btnSubIntClick(Sender: TObject);
    procedure btnMulIntClick(Sender: TObject);
    procedure btnDivIntClick(Sender: TObject);
    procedure btnBNAddClick(Sender: TObject);
    procedure btnBNSubClick(Sender: TObject);
    procedure btnBNMulClick(Sender: TObject);
    procedure btnBNDivClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure btnCompareIntClick(Sender: TObject);
    procedure btnCompareBNClick(Sender: TObject);
  private
    FR1, FR2, FR: TCnBigRational;
  public
    { Public declarations }
  end;

var
  FormRational: TFormRational;

implementation

uses
  CnBigNumber;

{$R *.DFM}

procedure TFormRational.FormCreate(Sender: TObject);
begin
  FR1 := TCnBigRational.Create;
  FR2 := TCnBigRational.Create;
  FR := TCnBigRational.Create;
end;

procedure TFormRational.FormDestroy(Sender: TObject);
begin
  FR.Free;
  FR2.Free;
  FR1.Free;
end;

procedure TFormRational.btnSet1Click(Sender: TObject);
begin
  FR1.SetString(edtBR1N.Text + '/' + edtBR1D.Text);
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnSet2Click(Sender: TObject);
begin
  FR1.SetValue(edtBR1N.Text, edtBR1D.Text);
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnSet3Click(Sender: TObject);
begin
  FR1.SetIntValue(StrToInt(edtBR1N.Text));
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnReduceClick(Sender: TObject);
begin
  FR1.SetValue(edtBR1N.Text, edtBR1D.Text);
  FR1.Reduce;
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnRN2SetValueClick(Sender: TObject);
begin
  FR2.SetValue(edtBR2N.Text, edtBR2D.Text);
  ShowMessage(FR2.ToString);
end;

procedure TFormRational.btnAddClick(Sender: TObject);
begin
  BigRationalNumberAdd(FR, FR1, FR2);
  ShowMessage(FR.ToString);
end;

procedure TFormRational.btnSubClick(Sender: TObject);
begin
  BigRationalNumberSub(FR, FR1, FR2);
  ShowMessage(FR.ToString);
end;

procedure TFormRational.btnMulClick(Sender: TObject);
begin
  BigRationalNumberMul(FR, FR1, FR2);
  ShowMessage(FR.ToString);
end;

procedure TFormRational.btnDivClick(Sender: TObject);
begin
  BigRationalNumberDiv(FR, FR1, FR2);
  ShowMessage(FR.ToString);
end;

procedure TFormRational.btnSetExtendedClick(Sender: TObject);
var
  F: Extended;
begin
  F := StrToFloat(edtExtended.Text);
  FR1.SetFloat(F);
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnSetStringClick(Sender: TObject);
begin
  FR1.SetString(edtExtended.Text);
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnToDecClick(Sender: TObject);
begin
  ShowMessage(FR2.ToDec(100));
end;

procedure TFormRational.btnAddIntClick(Sender: TObject);
begin
  FR1.Add(StrToInt(edtInt.Text));
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnSubIntClick(Sender: TObject);
begin
  FR1.Sub(StrToInt(edtInt.Text));
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnMulIntClick(Sender: TObject);
begin
  FR1.Mul(StrToInt(edtInt.Text));
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnDivIntClick(Sender: TObject);
begin
  FR1.Divide(StrToInt(edtInt.Text));
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnBNAddClick(Sender: TObject);
var
  B: TCnBigNumber;
begin
  B := TCnBigNumber.FromDec(edtInt.Text);
  FR1.Add(B);
  B.Free;
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnBNSubClick(Sender: TObject);
var
  B: TCnBigNumber;
begin
  B := TCnBigNumber.FromDec(edtInt.Text);
  FR1.Sub(B);
  B.Free;
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnBNMulClick(Sender: TObject);
var
  B: TCnBigNumber;
begin
  B := TCnBigNumber.FromDec(edtInt.Text);
  FR1.Mul(B);
  B.Free;
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnBNDivClick(Sender: TObject);
var
  B: TCnBigNumber;
begin
  B := TCnBigNumber.FromDec(edtInt.Text);
  FR1.Divide(B);
  B.Free;
  ShowMessage(FR1.ToString);
end;

procedure TFormRational.btnCompareClick(Sender: TObject);
begin
  ShowMessage(IntToStr(BigRationalNumberCompare(FR1, FR2)));
end;

procedure TFormRational.btnCompareIntClick(Sender: TObject);
begin
  ShowMessage(IntToStr(BigRationalNumberCompare(FR1, StrToInt(edtInt.Text))));
end;

procedure TFormRational.btnCompareBNClick(Sender: TObject);
//var
//  B: TCnBigNumber;
begin
//  B := TCnBigNumber.FromDec(edtInt.Text);
//  ShowMessage(IntToStr(CnBigRationalNumberCompare(FR1, B)));
//  B.Free;
  ShowMessage('NOT Implemented.');
end;

end.
