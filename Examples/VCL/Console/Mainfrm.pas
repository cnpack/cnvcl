unit Mainfrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnClasses, CnConsole, StdCtrls;

type
  TForm1 = class(TForm)
    CnConsole1: TCnConsole;
    Button1: TButton;
    btnOff: TButton;
    btnrst: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btnOffClick(Sender: TObject);
    procedure btnrstClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnOffClick(Sender: TObject);
begin
  CnConsole1.Enabled := false;
end;

procedure TForm1.btnrstClick(Sender: TObject);
begin
  CnConsole1.ResetConsole;
  CnConsole1.SetTextColor(tfGreen+tfIntensity+tbRed);
  Writeln('This is green color!');
  CnConsole1.SetTextColor(tfWhite);
  Writeln('This is white color!');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s:string;
begin
  CnConsole1.Enabled := true;
  Write('Hellow world! Please input: ');
  CnConsole1.SetTextColor(tfBlue + tfIntensity);
  Readln(s);
  CnConsole1.SetTextColor(tfRed + tfIntensity);
  Write('Your input: ');
  Writeln(s);
end;

end.
