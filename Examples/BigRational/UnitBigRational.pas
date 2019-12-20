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
  private
    FR1, FR2, FR: TCnBigRationalNumber;
  public
    { Public declarations }
  end;

var
  FormRational: TFormRational;

implementation

{$R *.DFM}

procedure TFormRational.FormCreate(Sender: TObject);
begin
  FR1 := TCnBigRationalNumber.Create;
  FR2 := TCnBigRationalNumber.Create;
  FR := TCnBigRationalNumber.Create;
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
  CnBigRationalNumberAdd(FR1, FR2, FR);
  ShowMessage(FR.ToString);
end;

procedure TFormRational.btnSubClick(Sender: TObject);
begin
  CnBigRationalNumberSub(FR1, FR2, FR);
  ShowMessage(FR.ToString);
end;

procedure TFormRational.btnMulClick(Sender: TObject);
begin
  CnBigRationalNumberMul(FR1, FR2, FR);
  ShowMessage(FR.ToString);
end;

procedure TFormRational.btnDivClick(Sender: TObject);
begin
  CnBigRationalNumberDiv(FR1, FR2, FR);
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

end.
