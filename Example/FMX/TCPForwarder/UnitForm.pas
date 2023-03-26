unit UnitForm;

interface

uses
  SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnThreadingTCPServer, CnTCPForwarder, FMX.Edit, FMX.Memo, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormForwarder = class(TForm)
    lblIP: TLabel;
    lblPort: TLabel;
    lblRemoteHost: TLabel;
    lblRemotePort: TLabel;
    edtLocalIP: TEdit;
    edtLocalPort: TEdit;
    btnOpen: TButton;
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
    procedure ServerData(Sender: TObject; Buf: Pointer; var DataSize: Integer;
      var NewBuf: Pointer; var NewDataSize: Integer);
    procedure ClientData(Sender: TObject; Buf: Pointer; var DataSize: Integer;
      var NewBuf: Pointer; var NewDataSize: Integer);
    procedure RemoteConnectd(Sender: TObject);
  end;

var
  FormForwarder: TFormForwarder;

implementation

{$R *.fmx}

procedure TFormForwarder.ClientData(Sender: TObject; Buf: Pointer;
  var DataSize: Integer; var NewBuf: Pointer; var NewDataSize: Integer);
begin
  Log('Get Client Bytes: ' + IntToStr(DataSize));
end;

procedure TFormForwarder.FormCreate(Sender: TObject);
begin
  FForwarder := TCnTCPForwarder.Create(Self);
  FForwarder.OnAccept := TCPAccept;
  FForwarder.OnError := TCPError;
  FForwarder.OnRemoteConnected := RemoteConnectd;
  FForwarder.OnServerData := ServerData;
  FForwarder.OnClientData := ClientData;
end;

procedure TFormForwarder.Log(const Msg: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      mmoResult.Lines.Add(Msg);
    end);
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
    btnOpen.Text := 'Forward To:';
  end
  else
  begin
    FForwarder.LocalIP := edtLocalIP.Text;
    FForwarder.LocalPort := StrToInt(edtLocalPort.Text);
    FForwarder.RemoteHost := edtRemoteHost.Text;
    FForwarder.RemotePort := StrToInt(edtRemotePort.Text);

    FForwarder.Active := True;
    if FForwarder.Listening then
      btnOpen.Text := 'Close';
  end;
end;

procedure TFormForwarder.RemoteConnectd(Sender: TObject);
begin
  Log('Remote Connected.');
end;

procedure TFormForwarder.ServerData(Sender: TObject; Buf: Pointer;
  var DataSize: Integer; var NewBuf: Pointer; var NewDataSize: Integer);
begin
  Log('Get Server Bytes: ' + IntToStr(DataSize));
end;

end.
