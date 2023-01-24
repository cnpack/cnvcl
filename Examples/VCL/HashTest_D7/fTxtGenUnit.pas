unit fTxtGenUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm4 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Edit2: TEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    function Count: Integer;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

function RandomStr(iMaxLen: Integer): string;
var
  i, iMax: Integer;
begin
  Result := '';
  iMax := Random(iMaxLen);
  if iMax < 6 then
    iMax := 6;
  for i := 1 to iMax do
    Result := Result + Char(Ord(' ') + Random(95));
end;

procedure TForm4.Button1Click(Sender: TObject);
var
  f: TextFile;
  i: Integer;
  s: string;
begin
  Randomize;
  AssignFile(f, Edit2.Text);
  ReWrite(f);
  for i := 1 to Count do
  begin
    s := RandomStr(16);
    Writeln(f, s);
  end;
  CloseFile(f);
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  f: TextFile;
  i: Integer;
  s: string;
begin
  Randomize;
  AssignFile(f, Edit2.Text);
  ReWrite(f);
  for i := 1 to Count do
  begin
    s := 'ShenTest' + IntToStr(i);
    Writeln(f, s);
  end;
  CloseFile(f);
end;

function TForm4.Count: Integer;
begin
  Result := StrToIntDef(Edit1.Text, 0);
end;

end.
