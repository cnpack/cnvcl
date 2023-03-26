unit UnitSocks5;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnTCPClient, CnThreadingTCPServer, ExtCtrls, CnNetwork, WinSock;

type
  TFormSocks5 = class(TForm)
    lblLocalIP: TLabel;
    lblProxyPort: TLabel;
    edtLocalIP: TEdit;
    edtPort: TEdit;
    btnProxy: TButton;
    lstClients: TListBox;
    tmrClient: TTimer;
    mmoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure tmrClientTimer(Sender: TObject);
    procedure btnProxyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure DumpClientStatus;
    procedure Log(const Msg: string);
    procedure Socks5ShutdownClient(Sender: TObject);
  public
    procedure Socks5Accept(Sender: TObject; ClientSocket: TCnClientSocket);
    procedure Socks5Error(Sender: TObject; SocketError: Integer);
  end;

var
  FormSocks5: TFormSocks5;

implementation

var
  FTCPServer: TCnThreadingTCPServer = nil;

{$R *.DFM}

procedure TFormSocks5.DumpClientStatus;
var
  I: Integer;
  Client: TCnClientSocket;
begin
  lstClients.Clear;
  if FTCPServer.ClientCount = 0 then
    lstClients.Items.Add('<No Client.>')
  else if FTCPServer.Active then
  for I := 0 to FTCPServer.ClientCount - 1 do
  begin
    Client := FTCPServer.Clients[I];
    lstClients.Items.Add(Format('%s:%d In %d, Out %d Bytes.', [Client.RemoteIP,
      Client.RemotePort, Client.BytesReceived, Client.BytesSent]));
  end;
end;

procedure TFormSocks5.FormCreate(Sender: TObject);
begin
  FTCPServer := TCnThreadingTCPServer.Create(Self);
  FTCPServer.OnAccept := Socks5Accept;
  FTCPServer.OnError := Socks5Error;
  FTCPServer.OnShutdownClient := Socks5ShutdownClient;
end;

procedure TFormSocks5.tmrClientTimer(Sender: TObject);
begin
  if FTCPServer.Active then
    DumpClientStatus;
end;

procedure TFormSocks5.btnProxyClick(Sender: TObject);
begin
  if FTCPServer.Active then
  begin
    FTCPServer.Active := False;
    btnProxy.Caption := 'Proxy !';
    lstClients.Clear;
  end
  else
  begin
    FTCPServer.LocalIP := edtLocalIP.Text;
    FTCPServer.LocalPort := StrToInt(edtPort.Text);

    FTCPServer.Active := True;
    btnProxy.Caption := 'Close';
    mmoLog.Clear;
  end;
end;

const
  VALID_METHODS = [0, 1, 2];

procedure TFormSocks5.Socks5Accept(Sender: TObject;
  ClientSocket: TCnClientSocket);
var
  Buf: array[0..8191] of Byte;
  Ret: Integer;
  NegReq: PCnSocksNegotiationRequest;
  CmdReq: PCnSocksRequest;
  Resp: TCnSocksResponse;
  I, RemPort: Integer;
  M, RemHost: string;
  Version, AuthMethod: Byte;
  RemoteSocket: TSocket;
  SockAddr: TSockAddr;
  ReadFds: TFDSet;
begin
  Log('*** New Client Connected: ' + ClientSocket.RemoteIP + ':' + IntToStr(ClientSocket.RemotePort));
  Ret := ClientSocket.Recv(Buf, SizeOf(Buf));
  if Ret <= 0 then
    Exit;

  // 客户端握手包
  NegReq := PCnSocksNegotiationRequest(@Buf[0]);
  if not (NegReq^.Version in [4, 5]) then
  begin
    Log('Error Version: ' + IntToStr(NegReq^.Version));
    Exit;
  end;

  Version := NegReq^.Version;

  // 打印客户端接受的认证方法
  Log('Socks Request: Client Method Count: ' + IntToStr(NegReq^.Method));
  M := '';
  for I := 1 to NegReq^.Method do
    M := M + ' ' + IntToStr(NegReq^.Methods[I]);
  Log('Socks Request: Methods: ' + M);

  // 判断并挑选一个能接受的认证方法
  AuthMethod := $FF;
  for I := 1 to NegReq^.Method do
  begin
    if NegReq^.Methods[I] in VALID_METHODS then
    begin
      AuthMethod := NegReq^.Methods[I];
      Break;
    end;
  end;

  if AuthMethod = $FF then // 无服务端支持的则退出
    Exit;

  // 发认证包，目前只支持无密码认证
  Buf[0] := Version;
  Buf[1] := 0; // AuthMethod;
  Ret := ClientSocket.Send(Buf, 2);
  if Ret <= 0 then
    Exit;

  // 收 Command 包
  Ret := ClientSocket.Recv(Buf, SizeOf(Buf));
  if Ret <= 0 then
    Exit;

  CmdReq := PCnSocksRequest(@Buf[0]);
  if CmdReq^.Version <> 5 then
    Exit;

  case CmdReq^.Command of
    CN_SOCKS_CMD_CONNECT:
      begin
        RemHost := CnGetSocksRequestDestinationAddress(CmdReq);
        RemPort := CnGetSocksRequestDestinationPort(CmdReq);
        Log('To Connect to: ' + RemHost + ':' + IntToStr(RemPort));

        Resp.Version := 5;
        Resp.Reserved := 0;
        Resp.AddressType := CN_SOCKS_ADDRESS_TYPE_IPV4;
        Resp.BindAddress.IpV4Address := 0;

        // 分配地址为 0 的随机端口，然后客户端忽略之，用本连接开始后续通讯
        RemoteSocket := WinSock.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
        if RemoteSocket = INVALID_SOCKET then
        begin
          Resp.Reply := CN_SOCKS_REPLY_GENERAL_FAILURE;
          ClientSocket.Send(Resp, Ret);
          Exit;
        end;
        ClientSocket.Tag := TObject(RemoteSocket); // 存起来

        // 连接目标主机
        SockAddr.sin_family := AF_INET;
        SockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(TCnTCPClient.LookupHostAddr(RemHost))));
        SockAddr.sin_port := ntohs(RemPort);
        if WinSock.connect(RemoteSocket, SockAddr, SizeOf(SockAddr)) <> 0 then
        begin
          Resp.Reply := CN_SOCKS_REPLY_CONNECTION_REFUSED;  // 目标主机连接失败
          ClientSocket.Send(Resp, Ret);
          Exit;
        end;

        // 连接成功，发送回应
        Resp.Reply := CN_SOCKS_REPLY_SUCCESS;
        Ret := CnSetSocksResponseBindPort(@Resp, 1080);
        if ClientSocket.Send(Resp, Ret) <= 0 then
          Exit;

        Log('Send Reply with Random Allocated Forwarder Placeholder and Forwarding Data.');

        while True do
        begin
          // SELECT 等俩 Socket 上的消息，准备读来写去
          FD_ZERO(ReadFds);
          FD_SET(ClientSocket.Socket, ReadFds);
          FD_SET(RemoteSocket, ReadFds);
          Ret := (WinSock.select(0, @ReadFds, nil, nil, nil));
          if Ret <= 0 then
            Exit;

          if FD_ISSET(ClientSocket.Socket, ReadFds) then // 客户端有数据来
          begin
            Ret := ClientSocket.Recv(Buf, SizeOf(Buf));
            if Ret <= 0 then
              Exit;

            Ret := WinSock.send(RemoteSocket, Buf, Ret, 0); // 发到服务端
            if Ret <= 0 then
              Exit;
          end;

          if FD_ISSET(RemoteSocket, ReadFds) then // 服务端有数据来
          begin
            Ret := WinSock.recv(RemoteSocket, Buf, SizeOf(Buf), 0);
            if Ret <= 0 then
              Exit;

            Ret := ClientSocket.Send(Buf, Ret); // 发到客户端
            if Ret <= 0 then
              Exit;
          end;
          Sleep(0);
        end;
      end;
  else
    Exit;
  end;
end;

procedure TFormSocks5.Socks5Error(Sender: TObject; SocketError: Integer);
begin
  Log('*** Socket Error: ' + IntToStr(SocketError));
end;

procedure TFormSocks5.Log(const Msg: string);
begin
  mmoLog.Lines.Add(Msg);
end;

procedure TFormSocks5.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tmrClient.Enabled := False;
end;

procedure TFormSocks5.Socks5ShutdownClient(Sender: TObject);
var
  Sock: TSocket;
begin
  Sock := TSocket((Sender as TCnClientSocket).Tag);
  if (Sock <> 0) or (Sock <> INVALID_SOCKET) then
    closesocket(Sock);
end;

end.
