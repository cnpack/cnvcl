unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CnCameraEye;

type
  TForm1 = class(TForm)
    CnCameraEye1: TCnCameraEye;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
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

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CnCameraEye1.Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CnCameraEye1.Stop;
end;

end.
