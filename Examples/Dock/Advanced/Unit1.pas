unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnDockFormControl, Menus, ExtCtrls, ComCtrls,
  CnDelphiDockStyle, CnDockSupportClass, CnClasses;

type
  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

uses Main;


{$R *.DFM}

procedure TForm1.CnDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

procedure TForm1.CnDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

end.
