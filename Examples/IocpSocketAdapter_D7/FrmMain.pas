unit FrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, WinSock,
  IdTCPConnection, IdTCPClient, IdTCPServer,
  IdComponent, IdUDPBase, IdUDPClient, StdCtrls,
  Dialogs, CnIocpSocketAdapter, CnIocpSimpleMemPool,IdUDPServer,
  IdBaseComponent, IdSocketHandle;

type
  TForm1 = class(TForm)
    idpclnt1: TIdUDPClient;
    idpsrvr1: TIdUDPServer;
    btn1: TButton;
    Memo1: TMemo;
    IdTCPServer1: TIdTCPServer;
    IdTCPClient1: TIdTCPClient;
    btn2: TButton;
    cncpsmplmpl1: TCnIocpSimpleMemPool;
    cncpscktdptr1: TCnIocpSocketAdapter;
    procedure btn1Click(Sender: TObject);
    procedure IdTCPServer1Execute(AThread: TIdPeerThread);
    procedure cncpscktdptr1RecvEvent(Sender: TObject; Error,
      Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal;
      Param: Pointer);
    procedure cncpscktdptr1RecvFromEvent(Sender: TObject; Error,
      Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal;
      Param: Pointer; FromAddr: PPeerAddress);
    procedure cncpscktdptr1SendEvent(Sender: TObject; Error,
      Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal;
      Param: Pointer);
    procedure cncpscktdptr1SendToEvent(Sender: TObject; Error,
      Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal;
      Param: Pointer; ToAddr: PPeerAddress);
    procedure btn2Click(Sender: TObject);
    //procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//-----------------------------------------------------------------------------
// 描述: 整形IP(主机字节顺序) -> 串型IP
//-----------------------------------------------------------------------------
function IpToString(Ip: Integer): string;
type
  TIntegerRec = packed record
    b1, b2, b3, b4: Byte;
  end;
begin
  with TIntegerRec(Ip) do
  begin
    Result := IntToStr(b4) + '.' + IntToStr(b3) + '.' +
      IntToStr(b2) + '.' + IntToStr(b1);
  end;
end;

//-----------------------------------------------------------------------------
// 描述: 串型IP -> 整形IP(主机字节顺序)
//-----------------------------------------------------------------------------
function StringToIp(const S: string): Integer;
type
  TIntegerRec = packed record
    b1, b2, b3, b4: Byte;
  end;

  function Fetch(var AInput: string): String;
  var
    I: Integer;
  begin
    I := Pos('.', AInput);
    if I = 0 then
    begin
      Result := AInput;
      AInput := '';
    end else
    begin
      Result := Copy(AInput, 1, I - 1);
      AInput := Copy(AInput, I + 1, MaxInt);
    end;
  end;

var
  IpStr: string;
begin
  IpStr := S;
  with TIntegerRec(Result) do
  begin
    b4 := Byte(StrToIntDef(Fetch(IpStr), 0));
    b3 := Byte(StrToIntDef(Fetch(IpStr), 0));
    b2 := Byte(StrToIntDef(Fetch(IpStr), 0));
    b1 := Byte(StrToIntDef(Fetch(IpStr), 0));
  end;
end;


procedure TForm1.IdTCPServer1Execute(AThread: TIdPeerThread);
var
  Buf: array[0..64*1000] of byte;
begin
  AThread.Connection.Socket.Recv(Buf, 64 * 1000);
  AThread.Connection.Socket.Send(Buf, 64 * 1000);
end;

procedure TForm1.btn1Click(Sender: TObject);
const
  BufferSize = 64 * 10;
var
  HandleClient, HandleServer: TSocket;
  I: Integer;
  Peer: TPeerAddress;
  SendBuffer, RecvBuffer: PChar;
begin
  if not idpsrvr1.Active then
  begin
    idpclnt1.Active := True;
    cncpscktdptr1.AssicoateSocket(idpsrvr1.Binding.Handle);
    idpclnt1.Active := True;
    cncpscktdptr1.AssicoateSocket(idpclnt1.Binding.Handle);
  end;
  HandleClient := idpclnt1.Binding.Handle;
  HandleServer := idpsrvr1.Binding.Handle;

  Peer.Ip := StringToIp('127.0.0.1');
  Peer.Port := 9001;
  for I := 0 to 10 do
  begin
    GetMem(SendBuffer, BufferSize);
    cncpscktdptr1.SendTo(HandleClient, SendBuffer, BufferSize, @Peer, SendBuffer);
    GetMem(RecvBuffer, BufferSize);
    cncpscktdptr1.RecvFrom(HandleServer, RecvBuffer, BufferSize, RecvBuffer);
  end;
end;

procedure TForm1.btn2Click(Sender: TObject);
const
  BufferSize = 64 * 1024;
var
  Handle: TSocket;
  I: Integer;
  SendBuffer, RecvBuffer: PChar;
begin
  if not IdTCPServer1.Active then
  begin
    IdTCPServer1.Active := True;
    IdTCPClient1.Host := '127.0.0.1';
    IdTCPClient1.Port := 9000;
    IdTCPClient1.Connect;
    cncpscktdptr1.AssicoateSocket(IdTCPClient1.Socket.Binding.Handle);
  end;
  Handle := IdTCPClient1.Socket.Binding.Handle;

  for I := 0 to 100 do
  begin
    // 发出数据，完成后回调事件 SendEvent
    cncpsmplmpl1.RentMemory( Pointer(SendBuffer), (BufferSize));
    cncpscktdptr1.Send(Handle, SendBuffer, BufferSize, SendBuffer);
    // 注册接收数据，等待回调事件 RecvEvent
    //Sleep(10);   Application.ProcessMessages;
    cncpsmplmpl1.RentMemory( Pointer(RecvBuffer), (BufferSize));
    cncpscktdptr1.Recv(Handle, RecvBuffer, BufferSize, RecvBuffer);
    //Sleep(10);   Application.ProcessMessages;
    //ShowMessage('Waiting..');
  end;
end;

procedure TForm1.cncpscktdptr1RecvEvent(Sender: TObject; Error,
  Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer);
begin
  Memo1.Lines.Add(Format('RecvEvent, error=%d, trans=%d', [Error, Transferred]));
  cncpsmplmpl1.ReturnMemory( Param);
end;

procedure TForm1.cncpscktdptr1RecvFromEvent(Sender: TObject; Error,
  Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer;
  FromAddr: PPeerAddress);
begin
  Memo1.Lines.Add(Format('RecvFromEvent, error=%d, trans=%d, ip=%s, port=%d',
    [Error, Transferred, IpToString(FromAddr.Ip), FromAddr.Port]));
  FreeMem(Param);
end;

procedure TForm1.cncpscktdptr1SendEvent(Sender: TObject; Error,
  Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer);
begin
  Memo1.Lines.Add(Format('SendEvent, error=%d, trans=%d', [Error, Transferred]));
  cncpsmplmpl1.ReturnMemory( Param);
end;

procedure TForm1.cncpscktdptr1SendToEvent(Sender: TObject; Error,
  Transferred: Cardinal; Buffer: PWSABUF; BufCount: Cardinal; Param: Pointer;
  ToAddr: PPeerAddress);
begin
  FreeMem(Param);
  Memo1.Lines.Add(Format('SendToEvent, error=%d, trans=%d, ip=%s, port=%d',
    [Error, Transferred, IpToString(ToAddr.Ip), ToAddr.Port]));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if IdTCPServer1.Active then
  begin
    IdTCPClient1.Disconnect;
    IdTCPServer1.Active := False;
  end;
  if idpsrvr1.Active then
  begin
    idpsrvr1.Active := False;
  end;
end;

end.
