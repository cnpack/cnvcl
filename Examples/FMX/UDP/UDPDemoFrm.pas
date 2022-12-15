unit UDPDemoFrm;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.SpinBox, CnUDP, FMX.Edit, FMX.Memo, FMX.Types, FMX.EditBox,
  FMX.ScrollBox, FMX.Controls.Presentation, FMX.Memo.Types;

type
  TForm1 = class(TForm)
    grp1: TGroupBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    btnFile: TSpeedButton;
    lbl4: TLabel;
    lbl5: TLabel;
    mmoSend: TMemo;
    edtServer: TEdit;
    edtFile: TEdit;
    sePort: TSpinBox;
    btnSendText: TButton;
    btnSendFile: TButton;
    chkBoardCast: TCheckBox;
    grp2: TGroupBox;
    lbl6: TLabel;
    lbl8: TLabel;
    lblStatus: TLabel;
    mmoReceive: TMemo;
    seBind: TSpinBox;
    btnClose: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure btnSendTextClick(Sender: TObject);
    procedure btnSendFileClick(Sender: TObject);
    procedure seBindChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    CnUDP1: TCnUDP;
    procedure UpdateStatus;
  public
    procedure CnUDP1DataReceived(Sender: TComponent; Buffer: Pointer;
      Len: Integer; const FromIP: string; Port: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  CnNative;

const
  csTextFlag = #1;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnUDP1 := TCnUDP.Create(Self);
  CnUDP1.OnDataReceived := CnUDP1DataReceived;
  CnUDP1.LocalPort := 5353;
  CnUDP1.UpdateBinding;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  CnUDP1.LocalPort := Trunc(seBind.Value);
  UpdateStatus;
end;

procedure TForm1.UpdateStatus;
begin
  if CnUDP1.Listening then
    lblStatus.Text := 'Listening...'
  else
    lblStatus.Text := 'Not listening.';
  lblStatus.Text := 'IP: ' + CnUDP1.LocalHost + ' ' + lblStatus.Text;
end;

procedure TForm1.btnFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtFile.Text := dlgOpen.FileName;
end;

procedure TForm1.seBindChange(Sender: TObject);
begin
  CnUDP1.LocalPort := Trunc(seBind.Value);
  UpdateStatus;
end;

procedure TForm1.btnSendTextClick(Sender: TObject);
var
  S: AnsiString;
begin
  // Insert a Text flag at head
  S := csTextFlag + AnsiString(mmoSend.Lines.Text);
  CnUDP1.RemoteHost := edtServer.Text;
  CnUDP1.RemotePort := Trunc(sePort.Value);
  CnUDP1.SendBuffer(PAnsiChar(S), Length(S), chkBoardCast.IsChecked);
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
      CnUDP1.RemotePort := Trunc(sePort.Value);
      CnUDP1.SendStream(Stream, chkBoardCast.IsChecked);
    finally
      Stream.Free;
    end;
  end;
end;

procedure TForm1.CnUDP1DataReceived(Sender: TComponent; Buffer: Pointer;
  Len: Integer; const FromIP: String; Port: Integer);
var
  S: AnsiString;
  Stream: TFileStream;
begin
  if PAnsiChar(Buffer)^ = csTextFlag then
  begin
    // Is text
    SetLength(S, Len - 1);
    Move(PAnsiChar(TCnNativeInt(Buffer) + 1)^, S[1], Len - 1);
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
