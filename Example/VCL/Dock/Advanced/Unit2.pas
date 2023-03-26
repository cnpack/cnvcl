unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnDockFormControl, Menus, StdCtrls, ComCtrls, ExtCtrls, CnDockSupportClass,
  CnClasses;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    CnDockClient1: TCnDockClient;
    Memo1: TMemo;
    procedure CnDockClient1FormHide(Sender: TObject);
    procedure CnDockClient1FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses Main;

{$R *.DFM}

procedure TForm2.CnDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

procedure TForm2.CnDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

end.
