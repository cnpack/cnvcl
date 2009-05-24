unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CnCheckTreeView, StdCtrls;

type
  TForm1 = class(TForm)
    CnCheckTreeView1: TCnCheckTreeView;
    btnCheck: TButton;
    btnUncheck: TButton;
    btnEnable: TButton;
    btnDisable: TButton;
    chkCanDisableNode: TCheckBox;
    btnHideCheck: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCheckClick(Sender: TObject);
    procedure btnUncheckClick(Sender: TObject);
    procedure btnDisableClick(Sender: TObject);
    procedure btnEnableClick(Sender: TObject);
    procedure chkCanDisableNodeClick(Sender: TObject);
    procedure btnHideCheckClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnCheckTreeView1.Items[0].Expand(True);
  CnCheckTreeView1.CanDisableNode := True;
end;

procedure TForm1.btnCheckClick(Sender: TObject);
begin
  if CnCheckTreeView1.Selected <> nil then
    CnCheckTreeView1.Checked[CnCheckTreeView1.Selected] := True;
end;

procedure TForm1.btnUncheckClick(Sender: TObject);
begin
  if CnCheckTreeView1.Selected <> nil then
    CnCheckTreeView1.Checked[CnCheckTreeView1.Selected] := False;
end;

procedure TForm1.btnDisableClick(Sender: TObject);
begin
  if CnCheckTreeView1.Selected <> nil then
    CnCheckTreeView1.NodeEnabled[CnCheckTreeView1.Selected] := False;
end;

procedure TForm1.btnEnableClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to CnCheckTreeView1.Items.Count - 1 do
    CnCheckTreeView1.NodeEnabled[CnCheckTreeView1.Items[I]] := True;
end;

procedure TForm1.chkCanDisableNodeClick(Sender: TObject);
begin
  CnCheckTreeView1.CanDisableNode := chkCanDisableNode.Checked;
end;

procedure TForm1.btnHideCheckClick(Sender: TObject);
begin
  CnCheckTreeView1.HideCheckBox(CnCheckTreeView1.Selected);;
end;

end.
