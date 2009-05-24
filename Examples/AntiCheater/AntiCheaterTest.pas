unit AntiCheaterTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    mmo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CnAntiCheater;

{$R *.DFM}

type
  TCheater = class(TCnAntiCheater)
  private
    FTest: Integer;
    function GetTest: Integer;
    procedure SetTest(const Value: Integer);
  published
    property Test: Integer read GetTest write SetTest;
  end;

var
  a: TCnAntiCheater;
  b: TCheater;

procedure TForm1.FormCreate(Sender: TObject);
begin
  a := TCnAntiCheater.Create;
  a.Data := 346893;
  lbl1.Caption := InttoStr(a.Data);

  b := TCheater.Create;
  b.Test := 783342;
  lbl2.Caption := InttoStr(b.Test);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  a.Free;
  b.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  a.Data := a.Data + 1;
  lbl1.Caption := InttoStr(a.Data);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  b.Test := b.Test + 1;
  lbl2.Caption := InttoStr(b.Test);
end;

{ TCheater }

function TCheater.GetTest: Integer;
begin
  if FTest <> 0 then
    Result := FTest
  else
    Result := 0;
end;

procedure TCheater.SetTest(const Value: Integer);
begin
  if FTest <> Value then
    FTest := Value;
end;

end.
 