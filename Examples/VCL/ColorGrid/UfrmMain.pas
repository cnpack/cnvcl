unit UfrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, CnColorGrid, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Label1: TLabel;
    CnColorGrid1: TCnColorGrid;
    procedure CnColorGrid1SelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
  if not CnColorGrid1.NextColorSet then
    ShowMessage('It''s the last Set');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not CnColorGrid1.PreColorSet then
    ShowMessage('It''s the first Set');
end;

procedure TForm1.CnColorGrid1SelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  Edit1.Text := IntToHex(CnColorGrid1.SelectedColor, 6);
  Edit1.Color := CnColorGrid1.SelectedColor;
end;

end.

