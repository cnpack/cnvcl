unit CnDialUpDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnDialUp, StdCtrls;

type
  TFrmCnDialUpDemo = class(TForm)
    cbbConnections: TComboBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    edtUserName: TEdit;
    edtPwd: TEdit;
    btnConnect: TButton;
    btnDisConnect: TButton;
    FDialupDemo: TCnDialUp;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisConnectClick(Sender: TObject);
  private
    procedure StatEvent(Sender: TObject; MessageText: string; Error: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmCnDialUpDemo: TFrmCnDialUpDemo;

implementation

{$R *.dfm}

procedure TFrmCnDialUpDemo.FormCreate(Sender: TObject);
begin
  Application.Title := Caption;
  cbbConnections.Items.Assign(FDialupDemo.PossibleConnections);
end;

procedure TFrmCnDialUpDemo.btnConnectClick(Sender: TObject);
begin
  if cbbConnections.ItemIndex > -1 then
  begin
    try
      FDialupDemo.ConnectTo := cbbConnections.Text;
      FDialupDemo.Username := edtUserName.Text;
      FDialupDemo.Password := edtPwd.Text;
      FDialupDemo.OnStatusEvent := StatEvent;
      FDialupDemo.GoOnline;
    except
      FDialupDemo.GoOffline;
    end;
  end;  
end;

procedure TFrmCnDialUpDemo.btnDisConnectClick(Sender: TObject);
begin
  if cbbConnections.ItemIndex > -1 then
    FDialupDemo.GoOffline;
end;

procedure TFrmCnDialUpDemo.StatEvent(Sender: TObject;
  MessageText: string; Error: Boolean);
begin
  Caption := MessageText;
end;

end.
