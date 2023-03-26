unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnKeyBlocker;

type
  TForm1 = class(TForm)
    CnKeyBlocker1: TCnKeyBlocker;
    chkKeyBlocker: TCheckBox;
    grpBlocks: TGroupBox;
    chkAltEsc: TCheckBox;
    chkAltTab: TCheckBox;
    chkCAD: TCheckBox;
    chkCAE: TCheckBox;
    chkCtrlEnter: TCheckBox;
    chkCtrlEsc: TCheckBox;
    chkPower: TCheckBox;
    chkSleep: TCheckBox;
    chkWinApp: TCheckBox;
    Label1: TLabel;
    procedure chkKeyBlockerClick(Sender: TObject);
    procedure chkClick(Sender: TObject);
    procedure CnKeyBlocker1BlockKey(VirtualKey: Cardinal);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.chkKeyBlockerClick(Sender: TObject);
begin
  CnKeyBlocker1.Enabled := chkKeyBlocker.Checked;
end;

procedure TForm1.chkClick(Sender: TObject);
begin
  //CnKeyBlocker1.BlockCtrlAltDelete := chkCAD.Checked;
  CnKeyBlocker1.BlockAltTab := chkAltTab.Checked;
  CnKeyBlocker1.BlockCtrlEsc := chkCtrlEsc.Checked;
  CnKeyBlocker1.BlockAltEsc := chkAltEsc.Checked;
  CnKeyBlocker1.BlockCtrlEnter := chkCtrlEnter.Checked;
  CnKeyBlocker1.BlockSleep := chkSleep.Checked;
  CnKeyBlocker1.BlockPower := chkPower.Checked;
  CnKeyBlocker1.BlockWinApps := chkWinApp.Checked;
end;

procedure TForm1.CnKeyBlocker1BlockKey(VirtualKey: Cardinal);
begin
  Label1.Caption := Format('Key %d Blocked.', [VirtualKey]);
end;

end.
