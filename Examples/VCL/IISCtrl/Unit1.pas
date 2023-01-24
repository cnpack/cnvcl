unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnIISCtrl;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button1: TButton;
    Button2: TButton;
    CnIISCtrl1: TCnIISCtrl;
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
  if not CnIISCtrl1.CheckVirtualDir(edit1.Text) then
    CnIISCtrl1.CreateVirtualDir(edit1.Text,Edit2.Text,Edit3.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if CnIISCtrl1.CheckVirtualDir(Edit1.Text) then
    CnIISCtrl1.DeleteVirtualDir(edit1.Text);
end;

end.
