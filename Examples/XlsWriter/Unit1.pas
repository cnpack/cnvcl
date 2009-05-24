unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids;

type
  TForm1 = class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure InitGrid;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  CnXlsWriter;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  op: TCnXlsWriter;
  i, j: Integer;
begin
  op := TCnXlsWriter.Create;
  try
    for i := 1 to StringGrid1.ColCount do
      for j := 1 to StringGrid1.RowCount do
        op.Cells[i - 1, j - 1] := StringGrid1.Cells[i, j];
  finally
    op.ADOCompatible := CheckBox1.Checked;
    op.SaveToXls('a.xls');
    op.Free;
  end;
end;

procedure TForm1.InitGrid;
var
  i: Integer;
begin
  with StringGrid1 do
  begin
    for i := 1 to ColCount do
    begin
      Cells[i, 0] := Char(Ord('A') + i - 1);
    end;
    for i := 1 to RowCount do
    begin
      Cells[0, i] := IntToStr(i);
    end;
    Cells[3,1] := '12345';
    Cells[4,2] := 'www.cnpack.org';
    Cells[1,3] := '10e3';
    Cells[5,4] := '123.456';
    Cells[1,4] := 'CnWizards';
    Cells[2,5] := DateTimeToStr(Now);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitGrid;
end;

end.
