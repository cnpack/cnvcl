unit TargetMainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
const
  A: string = 'Button1ClickStr';
var
  B: string;
begin
  B := A + 'Button1Click';
end;

procedure TForm2.Button2Click(Sender: TObject);
const
  A: string = 'Button2ClickStr';
var
  B: string;
begin
  B := A + 'Button2Click';
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  _Click: TNotifyEvent;
begin
  _Click := Self.Button1Click;
  Memo1.Lines.Add(Format('Button1Click: %x', [dword(@_Click)]));
  _Click := Self.Button2Click;
  Memo1.Lines.Add(Format('Button2Click: %x', [dword(@_Click)]));
end;

end.

