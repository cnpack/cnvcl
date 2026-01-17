unit UDPDemoFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Spin, CnUDP;

type
  TForm1 = class(TForm)
    grp1: TGroupBox;
    lbl1: TLabel;
    mmoSend: TMemo;
    edtServer: TEdit;
    lbl2: TLabel;
    lbl3: TLabel;
    edtFile: TEdit;
    btnFile: TSpeedButton;
    lbl4: TLabel;
    btnSendText: TButton;
    btnSendFile: TButton;
    grp2: TGroupBox;
    lbl6: TLabel;
    lbl8: TLabel;
    mmoReceive: TMemo;
    btnClose: TButton;
    CnUDP1: TCnUDP;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    lblStatus: TLabel;
    chkBoardCast: TCheckBox;
    lbl5: TLabel;
    sePort: TSpinEdit;
    seBind: TSpinEdit;
    procedure btnFileClick(Sender: TObject);
    procedure seBindChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSendTextClick(Sender: TObject);
    procedure btnSendFileClick(Sender: TObject);
    procedure CnUDP1DataReceived(Sender: TComponent; Buffer: Pointer;
      Len: Integer; const FromIP: String; Port: Integer);
    procedure btnCloseClick(Sender: TObject);
  private
    procedure UpdateStatus;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.LFM}

const
  csTextFlag = #1;

procedure TForm1.FormShow(Sender: TObject);
begin
  CnUDP1.LocalPort := seBind.Value;
  UpdateStatus;
end;

procedure TForm1.UpdateStatus;
begin
  if CnUDP1.Listening then
    lblStatus.Caption := 'Listening...'
  else
    lblStatus.Caption := 'Not listening.';
  lblStatus.Caption := 'IP: ' + CnUDP1.LocalHost + ' ' + lblStatus.Caption;
end;

procedure TForm1.btnFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtFile.Text := dlgOpen.FileName;
end;

procedure TForm1.seBindChange(Sender: TObject);
begin
  CnUDP1.LocalPort := seBind.Value;
  UpdateStatus;
end;

procedure TForm1.btnSendTextClick(Sender: TObject);
var
  S: AnsiString;
begin
  // Insert a Text flag at head
  S := csTextFlag + AnsiString(mmoSend.Lines.Text);
  CnUDP1.RemoteHost := edtServer.Text;
  CnUDP1.RemotePort := sePort.Value;
  CnUDP1.SendBuffer(PAnsiChar(S), Length(S), chkBoardCast.Checked);
end;

procedure TForm1.btnSendFileClick(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  if FileExists(edtFile.Text) then
  begin
    Stream := TMemoryStream.Create;
    try
      Stream.LoadFromFile(edtFile.Text);
      CnUDP1.RemoteHost := edtServer.Text;
      CnUDP1.RemotePort := sePort.Value;
      CnUDP1.SendStream(Stream, chkBoardCast.Checked);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TForm1.CnUDP1DataReceived(Sender: TComponent; Buffer: Pointer;
  Len: Integer; const FromIP: string; Port: Integer);
var
  S: AnsiString;
  Stream: TFileStream;
begin
  if PAnsiChar(Buffer)^ = csTextFlag then
  begin
    // Is text
    SetLength(S, Len - 1);
    CopyMemory(PAnsiChar(S), PAnsiChar(Integer(Buffer) + 1), Len - 1);
    mmoReceive.Lines.Text := Format('From: %s:%d'#13#10, [FromIP, Port]) + string(S);
  end
  else
  begin
    // Is file
    if dlgSave.Execute then
    begin
      Stream := TFileStream.Create(dlgSave.FileName, fmCreate);
      try
        Stream.WriteBuffer(Buffer^, Len);
      finally
        Stream.Free;
      end;
    end;
  end;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
