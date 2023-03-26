unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnDockFormControl, CnVIDDockStyle, CnVSNETDockStyle, Menus;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    CnDockClient1: TCnDockClient;
    procedure CnDockClient1FormShow(Sender: TObject);
    procedure CnDockClient1FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses Main;

{$R *.DFM}

procedure TForm4.CnDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

procedure TForm4.CnDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

end.
