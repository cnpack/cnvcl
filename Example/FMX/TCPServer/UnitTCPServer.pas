unit UnitTCPServer;

interface

uses
  SysUtils, Classes, CnNetwork,
  {$IFDEF MSWINDOWS} Windows, Messages, WinSock, {$ENDIF}
  FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnThreadingTCPServer, FMX.Edit, FMX.Memo, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormTCPServer = class(TForm)
    lblIP: TLabel;
    lblPort: TLabel;
    edtIP: TEdit;
    edtPort: TEdit;
    btnOpen: TButton;
    mmoResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    FTCP: TCnThreadingTCPServer;
    procedure Log(const Msg: string);
  public
    procedure TCPAccept(Sender: TObject; ClientSocket: TCnClientSocket);
    procedure TCPError(Sender: TObject; SocketError: Integer);
  end;

var
  FormTCPServer: TFormTCPServer;

implementation

{$R *.fmx}

uses
  CnSocket;

procedure TFormTCPServer.btnOpenClick(Sender: TObject);
begin
  if FTCP.Active then
  begin
    FTCP.Active := False;
    btnOpen.Text := 'Open';
  end
  else
  begin
    FTCP.LocalIP := edtIP.Text;
    FTCP.LocalPort := StrToInt(edtPort.Text);

    FTCP.Active := True;
    if FTCP.Listening then
    begin
      btnOpen.Text := 'Close';
      Log('Listening at Port: ' + IntToStr(FTCP.ActualLocalPort));
    end;
  end;
end;

procedure TFormTCPServer.FormCreate(Sender: TObject);
begin
  FTCP := TCnThreadingTCPServer.Create(Self);
  FTCP.OnAccept := TCPAccept;
  FTCP.OnError := TCPError;
end;

procedure TFormTCPServer.Log(const Msg: string);
begin
  mmoResult.Lines.Add(Msg);
end;

procedure TFormTCPServer.TCPAccept(Sender: TObject; ClientSocket: TCnClientSocket);
var
  C: Integer;
  RecvBuf: array[0..1023] of Byte;
  SendBuf: array[0..1023] of Byte;
begin
  Log('Connected: ' + ClientSocket.RemoteIP + ':' + IntToStr(ClientSocket.RemotePort));
  // 演示服务端收、发
  C := ClientSocket.Recv(RecvBuf, SizeOf(RecvBuf) - 1);
  if C = SOCKET_ERROR then
    Exit;

  Log('Get ' + IntToStr(C) + ' Bytes.');
  SendBuf[0] := Ord('A');
  SendBuf[1] := Ord('B');
  C := ClientSocket.Send(SendBuf, 2);
  if C = SOCKET_ERROR then
    Exit;

  Log('Send ' + IntToStr(C) + ' Bytes.');

  //退出事件处理函数则断开了
end;

procedure TFormTCPServer.TCPError(Sender: TObject; SocketError: Integer);
begin
  Log('*** Socket Error: ' + IntToStr(SocketError));
end;

end.
