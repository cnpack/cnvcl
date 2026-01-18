unit UnitTCPClient;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WinSock, CnTCPClient;

type
  TTCPClientRecvThread = class(TThread)
  private
    FTCP: TCnTCPClient;
  protected
    procedure Execute; override;
    property TCP: TCnTCPClient read FTCP write FTCP;
  end;

  TFormTCPClient = class(TForm)
    lblIP: TLabel;
    lblPort: TLabel;
    edtHost: TEdit;
    edtPort: TEdit;
    btnOpen: TButton;
    mmoResult: TMemo;
    mmoContent: TMemo;
    btnSend: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
    FTCP: TCnTCPClient;
    FThread: TTCPClientRecvThread;
    procedure Log(const Msg: string);
    procedure TCPConnect(Sender: TObject);
    procedure TCPDisconnect(Sender: TObject);
    procedure TCPError(Sender: TObject; SocketError: Integer);
  public
    { Public declarations }
  end;

var
  FormTCPClient: TFormTCPClient;

implementation

{$R *.lfm}

{ TFormTCPClient }

procedure TFormTCPClient.Log(const Msg: string);
begin
  mmoResult.Lines.Add(Msg);
end;

procedure TFormTCPClient.FormCreate(Sender: TObject);
begin
  FTCP := TCnTCPClient.Create(Self);
  FTCP.OnConnect := TCPConnect;
  FTCP.OnDisconnect := TCPDisconnect;
  FTCP.OnError := TCPError;
end;

procedure TFormTCPClient.TCPConnect(Sender: TObject);
begin
  Log('Connected.');
  btnOpen.Caption := 'Disconnect';

  FThread := TTCPClientRecvThread.Create(True);
  FThread.TCP := FTCP;
  FThread.FreeOnTerminate := True;
  FThread.Resume;
end;

procedure TFormTCPClient.TCPDisconnect(Sender: TObject);
begin
  Log('Disconnected.');
  btnOpen.Caption := 'Connect';
  FThread.Terminate;
end;

procedure TFormTCPClient.btnOpenClick(Sender: TObject);
begin
  if FTCP.Active then
  begin
    FTCP.Active := False;
  end
  else
  begin
    FTCP.RemoteHost := edtHost.Text;
    FTCP.RemotePort := StrToInt(edtPort.Text);

    FTCP.Active := True;
  end;
end;

procedure TFormTCPClient.TCPError(Sender: TObject; SocketError: Integer);
begin
  Log('*** Socket Error: ' + IntToStr(SocketError));
end;

procedure TFormTCPClient.btnSendClick(Sender: TObject);
var
  S: string;
  Ret: Integer;
begin
  S := mmoContent.Lines.Text;
  if S <> '' then
  begin
    Ret := FTCP.Send(S[1], Length(S) * SizeOf(Char));
    if Ret <> SOCKET_ERROR then
      Log('Sent ' + IntToStr(Ret) + ' Bytes.')
    else
      Log('Sent Error.');
  end;
end;

{ TTCPClientRecvThread }

procedure TTCPClientRecvThread.Execute;
var
  Buf: array[0..1023] of Byte;
  Ret: Integer;
begin
  while not Terminated do
  begin
    if not FTCP.Active then
      Exit;

    Ret := FTCP.Recv(Buf, SizeOf(Buf) - 1);
    if Ret = 0 then
    begin
      FTCP.Close;
      Exit;
    end
    else if Ret <> SOCKET_ERROR then
      FormTCPClient.Log('Get ' + IntToStr(Ret) + ' Bytes.')
    else
      FormTCPClient.Log('Get Error.');

    Sleep(0);
  end;
end;

end.
