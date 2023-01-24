unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnDockFormControl, StdCtrls, ComCtrls, Menus, ExtCtrls;

type
  TForm3 = class(TForm)
    CnDockClient1: TCnDockClient;
    Panel1: TPanel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure CnDockClient1FormShow(Sender: TObject);
    procedure CnDockClient1FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses Main;

{$R *.DFM}

procedure TForm3.FormCreate(Sender: TObject);
begin
{  RichEdit1.Lines.Clear;
  RichEdit1.Lines.Add('EnableDock = ' + BoolStr[Integer(DockClient1.EnableDock)]);
  RichEdit1.Lines.Add('TopDock = ' + BoolStr[Integer(DockClient1.TopDock)]);
  RichEdit1.Lines.Add('LeftDock = ' + BoolStr[Integer(DockClient1.LeftDock)]);
  RichEdit1.Lines.Add('BottomDock = ' + BoolStr[Integer(DockClient1.BottomDock)]);
  RichEdit1.Lines.Add('RightDock = ' + BoolStr[Integer(DockClient1.RightDock)]);
  RichEdit1.Lines.Add('EachOtherDock = ' + BoolStr[Integer(DockClient1.EachOtherDock)]);}
    Width := Height;

end;

procedure TForm3.CnDockClient1FormShow(Sender: TObject);
begin
  TMenuItem(Tag).Checked := True;
end;

procedure TForm3.CnDockClient1FormHide(Sender: TObject);
begin
  TMenuItem(Tag).Checked := False;
end;

end.
