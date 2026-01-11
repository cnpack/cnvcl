unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, CnNative, CnTLS;

type
  TFormMain = class(TForm)
    grpConfig: TGroupBox;
    lblHost: TLabel;
    edtHost: TEdit;
    lblPort: TLabel;
    edtPort: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    grpData: TGroupBox;
    lblRequest: TLabel;
    memRequest: TMemo;
    btnSend: TButton;
    lblResponse: TLabel;
    memResponse: TMemo;
    lblLog: TLabel;
    memLog: TMemo;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTLSClient: TCnTLSClient;
    procedure DoTLSLog(Sender: TObject; const LogMsg: string);
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FTLSClient := TCnTLSClient.Create(Self);
  FTLSClient.OnTLSLog := DoTLSLog;
end;

procedure TFormMain.btnConnectClick(Sender: TObject);
begin
  memLog.Lines.Clear;
  memLog.Lines.Add('Connecting to ' + edtHost.Text + ':' + edtPort.Text);

  FTLSClient.RemoteHost := edtHost.Text;
  FTLSClient.RemotePort := StrToInt(edtPort.Text);
  
  try
    FTLSClient.Open;
    memLog.Lines.Add('Connected successfully');
    btnConnect.Enabled := False;
    btnDisconnect.Enabled := True;
    btnSend.Enabled := True;
  except
    on E: Exception do
    begin
      memLog.Lines.Add('Connection failed: ' + E.Message);
      Application.MessageBox(PChar('Connection failed: ' + E.Message),
        'Error', MB_ICONERROR);
    end;
  end;
end;

procedure TFormMain.btnDisconnectClick(Sender: TObject);
begin
  FTLSClient.Close;
  memLog.Lines.Add('Disconnected');
  btnConnect.Enabled := True;
  btnDisconnect.Enabled := False;
  btnSend.Enabled := False;
end;

procedure TFormMain.btnSendClick(Sender: TObject);
var
  Data: TBytes;
  RecvData: TBytes;
  Len: Integer;
begin
  memResponse.Lines.Clear;
  memLog.Lines.Add('Sending request...');
  
  try
    Data := AnsiToBytes(AnsiString(memRequest.Text));
    if FTLSClient.Send(Data[0], Length(Data)) <> Length(Data) then
      raise Exception.Create('Send failed');
    
    memLog.Lines.Add('Request sent');
    SetLength(RecvData, 8192);
    Len := FTLSClient.Recv(RecvData[0], Length(RecvData));
    
    if Len > 0 then
    begin
      SetLength(RecvData, Len);
      memResponse.Text := string(AnsiString(RecvData));
      memLog.Lines.Add(Format('Response received, length: %d', [Len]));
    end
    else
      memLog.Lines.Add('No response received');
  except
    on E: Exception do
    begin
      memLog.Lines.Add('Error: ' + E.Message);
      Application.MessageBox(PChar('Error: ' + E.Message),
        'Error', MB_ICONERROR);
    end;
  end;
end;

procedure TFormMain.DoTLSLog(Sender: TObject; const LogMsg: string);
begin
  memLog.Lines.Add('[TLS] ' + LogMsg);
  SendMessage(memLog.Handle, EM_SCROLLCARET, 0, 0);
end;

end.
