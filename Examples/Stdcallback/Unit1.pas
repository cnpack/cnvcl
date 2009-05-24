unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TACallBack = function (a, b: Integer): Integer; stdcall;

  TForm1 = class(TForm)
    btn1: TButton;
    Button1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    Fi: Integer;
    AFunc: TACallBack;
    { Private declarations }
  public
    { Public declarations }
    function ToCallback(a, b: Integer): Integer; stdcall;

    procedure CallABack;
  end;

var
  Form1: TForm1;

implementation

uses CnCallBack;

{$R *.DFM}

{ TForm1 }

function TForm1.ToCallback(a, b: Integer): Integer;
begin
  Result := a + b + Self.Fi;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  @AFunc := StdcallMethodToCallBack(Self, @TForm1.ToCallback);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Fi := 5;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CallABack;
end;

procedure TForm1.CallABack;
begin
  if @AFunc <> nil then
    Self.Caption := IntToStr(AFunc(3, 4));
end;

end.
 