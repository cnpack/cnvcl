unit UnitForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnThreadingTCPServer, CnTCPForwarder;

type
  TFormForwarder = class(TForm)
    lblIP: TLabel;
    lblPort: TLabel;
    edtLocalIP: TEdit;
    edtLocalPort: TEdit;
    btnOpen: TButton;
    lblRemoteHost: TLabel;
    lblRemotePort: TLabel;
    edtRemoteHost: TEdit;
    edtRemotePort: TEdit;
    mmoResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    FForwarder: TCnTCPForwarder;
    procedure Log(const Msg: string);
  public
    procedure TCPAccept(Sender: TObject; ClientSocket: TCnClientSocket);
    procedure TCPError(Sender: TObject; SocketError: Integer);
    procedure RemoteConnectd(Sender: TObject);
  end;

var
  FormForwarder: TFormForwarder;

implementation

{$R *.DFM}

procedure TFormForwarder.FormCreate(Sender: TObject);
begin
  FForwarder := TCnTCPForwarder.Create(Self);
  FForwarder.OnAccept := TCPAccept;
  FForwarder.OnError := TCPError;
  FForwarder.OnRemoteConnected := RemoteConnectd;
end;

procedure TFormForwarder.Log(const Msg: string);
begin
  mmoResult.Lines.Add(Msg);
end;

procedure TFormForwarder.TCPAccept(Sender: TObject;
  ClientSocket: TCnClientSocket);
begin
  Log('Client Connected: ' + ClientSocket.RemoteIP + ':' + IntToStr(ClientSocket.RemotePort));
end;

procedure TFormForwarder.TCPError(Sender: TObject; SocketError: Integer);
begin
  Log('*** Socket Error: ' + IntToStr(SocketError));
end;

procedure TFormForwarder.btnOpenClick(Sender: TObject);
begin
  if FForwarder.Active then
  begin
    FForwarder.Close;
    btnOpen.Caption := 'Forward To:';
  end
  else
  begin
    FForwarder.LocalIP := edtLocalIP.Text;
    FForwarder.LocalPort := StrToInt(edtLocalPort.Text);
    FForwarder.RemoteHost := edtRemoteHost.Text;
    FForwarder.RemotePort := StrToInt(edtRemotePort.Text);

    FForwarder.Active := True;
    if FForwarder.Listening then
      btnOpen.Caption := 'Close';
  end;
end;

procedure TFormForwarder.RemoteConnectd(Sender: TObject);
begin
  Log('Remote Connected.');
end;

end.
