unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, CnNative, CnTLS, CnCertificateAuthority;

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
    procedure DoVerifyServerCertificate(Sender: TObject; const Host: AnsiString;
      const CertChain: TCnTLSCertificateChain; var Accepted: Boolean; var ErrorMsg: string);
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
  FTLSClient.VerifyCertificate := True;
  FTLSClient.OnVerifyServerCertificate := DoVerifyServerCertificate;
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

procedure TFormMain.DoVerifyServerCertificate(Sender: TObject; const Host: AnsiString;
  const CertChain: TCnTLSCertificateChain; var Accepted: Boolean; var ErrorMsg: string);
var
  I, J: Integer;
  Cer: TCnCertificate;
  BC: TCnBasicCertificate;
begin
  memLog.Lines.Add(Format('[VERIFY] Host=%s CertCount=%d', [string(Host), Length(CertChain)]));
  for I := 0 to High(CertChain) do
  begin
    memLog.Lines.Add(Format('[VERIFY] cert[%d] DER length=%d', [I, Length(CertChain[I])]));
    Cer := TCnCertificate.Create;
    try
      if not CnCALoadCertificateFromBytes(CertChain[I], Cer) then
      begin
        memLog.Lines.Add(Format('[VERIFY] cert[%d] parse failed', [I]));
        Continue;
      end;

      BC := Cer.BasicCertificate;
      memLog.Lines.Add(Format('[VERIFY] cert[%d] Serial=%s', [I, BC.SerialNumber]));
      memLog.Lines.Add(Format('[VERIFY] cert[%d] Subject=%s', [I, BC.Subject.ToString]));
      memLog.Lines.Add(Format('[VERIFY] cert[%d] Issuer=%s', [I, BC.Issuer.ToString]));
      memLog.Lines.Add(Format('[VERIFY] cert[%d] CN=%s', [I, BC.Subject.CommonName]));
      memLog.Lines.Add(Format('[VERIFY] cert[%d] Valid=%s ~ %s', [I,
        DateTimeToStr(BC.NotBefore.DateTime), DateTimeToStr(BC.NotAfter.DateTime)]));
      memLog.Lines.Add(Format('[VERIFY] cert[%d] SelfSigned=%s', [I, BoolToStr(Cer.IsSelfSigned, True)]));

      if (BC.StandardExtension <> nil) and (BC.StandardExtension.SubjectAltName <> nil) then
      begin
        if BC.StandardExtension.SubjectAltName.Count > 0 then
        begin
          memLog.Lines.Add(Format('[VERIFY] cert[%d] SubjectAltName:', [I]));
          for J := 0 to BC.StandardExtension.SubjectAltName.Count - 1 do
            memLog.Lines.Add('  ' + BC.StandardExtension.SubjectAltName[J]);
        end;
      end;

      if (BC.PrivateInternetExtension <> nil) then
      begin
        if BC.PrivateInternetExtension.AuthorityInformationAccessOcsp <> '' then
          memLog.Lines.Add(Format('[VERIFY] cert[%d] AIA OCSP=%s', [I, BC.PrivateInternetExtension.AuthorityInformationAccessOcsp]));
        if BC.PrivateInternetExtension.AuthorityInformationAccessCaIssuers <> '' then
          memLog.Lines.Add(Format('[VERIFY] cert[%d] AIA caIssuers=%s', [I, BC.PrivateInternetExtension.AuthorityInformationAccessCaIssuers]));
      end;
    finally
      Cer.Free;
    end;
  end;
  Accepted := True;
  ErrorMsg := '';
  SendMessage(memLog.Handle, EM_SCROLLCARET, 0, 0);
end;

end.
