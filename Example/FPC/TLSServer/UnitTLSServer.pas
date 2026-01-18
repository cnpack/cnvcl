unit UnitTLSServer;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WinSock, CnThreadingTCPServer, CnTLS, CnNative, CnSocket;

type
  TFormTLSServer = class(TForm)
    edtIP: TEdit;
    edtPort: TEdit;
    btnOpen: TButton;
    btnClose: TButton;
    mmoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    FServer: TCnTLSServer;
    procedure TLSAccept(Sender: TObject; ClientSocket: TCnClientSocket);
    procedure TLSError(Sender: TObject; ClientSocket: TCnClientSocket; Code: Integer; const Msg: string);
    procedure TLSLog(Sender: TObject; const Msg: string);
    procedure Log(const S: string);
  public
  end;

var
  FormTLSServer: TFormTLSServer;

implementation

{$R *.lfm}

procedure TFormTLSServer.FormCreate(Sender: TObject);
begin
  FServer := TCnTLSServer.Create(Self);
  FServer.OnTLSAccept := TLSAccept;
  FServer.OnTLSError := TLSError;
  FServer.OnTLSLog := TLSLog;
end;

procedure TFormTLSServer.btnOpenClick(Sender: TObject);
begin
  FServer.LocalIP := edtIP.Text;
  FServer.LocalPort := StrToIntDef(edtPort.Text, 0);
  FServer.ServerCertFile := 'server_cert.crt';
  FServer.ServerKeyFile := 'server_key.pem';
  FServer.Active := True;
  Log('Listening ' + FServer.LocalIP + ':' + IntToStr(FServer.ActualLocalPort));
end;

procedure TFormTLSServer.btnCloseClick(Sender: TObject);
begin
  FServer.Active := False;
  Log('Closed');
end;

procedure TFormTLSServer.TLSAccept(Sender: TObject; ClientSocket: TCnClientSocket);
var
  RecvBuf: array[0..2047] of Byte;
  Resp: AnsiString;
  C: Integer;
begin
  Log('Accept ' + TCnTLSServerClientSocket(ClientSocket).RemoteIP + ':' +
    IntToStr(TCnTLSServerClientSocket(ClientSocket).RemotePort));
  C := ClientSocket.Recv(RecvBuf, SizeOf(RecvBuf));
  if (C = SOCKET_ERROR) or (C = 0) then
    Exit;
  Resp := 'HTTP/1.1 404 Not Found'#13#10'Content-Type: text/plain; charset=utf-8'#13#10 +
    'Content-Length: 9'#13#10'Connection: close'#13#10#13#10'Not Found';
  ClientSocket.Send(PAnsiChar(Resp)^, Length(Resp));
end;

procedure TFormTLSServer.TLSError(Sender: TObject; ClientSocket: TCnClientSocket; Code: Integer; const Msg: string);
begin
  Log('TLS Error ' + IntToStr(Code) + ' ' + Msg);
end;

procedure TFormTLSServer.TLSLog(Sender: TObject; const Msg: string);
begin
  Log('TLS ' + Msg);
end;

procedure TFormTLSServer.Log(const S: string);
begin
  mmoLog.Lines.Add(S);
end;

end.

