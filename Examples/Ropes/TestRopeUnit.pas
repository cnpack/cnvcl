unit TestRopeUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnStrings, StdCtrls, CnRopes, ComCtrls;

type
  TTestRopeForm = class(TForm)
    grpFastPos: TGroupBox;
    edtPattern: TEdit;
    edtStr: TEdit;
    btnSearch: TButton;
    grpRopes: TGroupBox;
    mmoRope: TMemo;
    edtAppend: TEdit;
    btnAppend: TButton;
    mmoResult: TMemo;
    btnTrim: TButton;
    btnEqual: TButton;
    btnReverse: TButton;
    udStart: TUpDown;
    edtStart: TEdit;
    udEnd: TUpDown;
    edtEnd: TEdit;
    lbl1: TLabel;
    btnDelete: TButton;
    btnSubStr: TButton;
    btnPos: TButton;
    edtInsert: TEdit;
    edtInsertPos: TEdit;
    udInsertPos: TUpDown;
    btnInsert: TButton;
    btnDup: TButton;
    procedure btnSearchClick(Sender: TObject);
    procedure mmoRopeChange(Sender: TObject);
    procedure btnAppendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnTrimClick(Sender: TObject);
    procedure btnEqualClick(Sender: TObject);
    procedure btnReverseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSubStrClick(Sender: TObject);
    procedure btnPosClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDupClick(Sender: TObject);
  private
    { Private declarations }
    FRope: ICnRope;
    procedure ShowResult(Rope: ICnRope);
  public
    { Public declarations }
  end;

var
  TestRopeForm: TTestRopeForm;

implementation

{$R *.DFM}

procedure TTestRopeForm.btnSearchClick(Sender: TObject);
var
  R: Integer;
begin
  R := FastPosition(PChar(edtStr.Text), PChar(edtPattern.Text));
  ShowMessage('Search Result: ' + IntToStr(R));
end;

procedure TTestRopeForm.mmoRopeChange(Sender: TObject);
begin
  FRope := CreateRope(mmoRope.Lines.Text);
end;

procedure TTestRopeForm.ShowResult(Rope: ICnRope);
begin
  mmoResult.Lines.Text := Rope.ToString;
end;

procedure TTestRopeForm.btnAppendClick(Sender: TObject);
begin
  ShowResult(FRope.Append(edtAppend.Text));
end;

procedure TTestRopeForm.FormCreate(Sender: TObject);
begin
  FRope := CreateRope(mmoRope.Lines.Text);
end;

procedure TTestRopeForm.btnTrimClick(Sender: TObject);
begin
  ShowResult(FRope.Trim);
end;

procedure TTestRopeForm.btnEqualClick(Sender: TObject);
begin
  if FRope.Trim.EqualsStr(edtAppend.Text) then
    ShowMessage('Equal')
  else
    ShowMessage('Not Equal');
end;

procedure TTestRopeForm.btnReverseClick(Sender: TObject);
begin
  ShowResult(FRope.Reverse);
end;

procedure TTestRopeForm.btnDeleteClick(Sender: TObject);
begin
  ShowResult(FRope.Delete(udStart.Position, udEnd.Position));
end;

procedure TTestRopeForm.btnSubStrClick(Sender: TObject);
begin
  ShowResult(FRope.SubStr(udStart.Position, udEnd.Position));
end;

procedure TTestRopeForm.btnPosClick(Sender: TObject);
begin
  ShowMessage(IntToStr(FRope.Position(edtAppend.Text)));
end;

procedure TTestRopeForm.btnInsertClick(Sender: TObject);
begin
  ShowResult(FRope.Insert(edtInsert.Text, udInsertPos.Position));
end;

procedure TTestRopeForm.btnDupClick(Sender: TObject);
begin
  ShowResult(FRope.Duplicate);
end;

end.
