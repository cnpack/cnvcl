unit UnitNetDecl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, WinSock, ExtCtrls;

type
  TFormNetDecl = class(TForm)
    pgcNetDecl: TPageControl;
    tsIP: TTabSheet;
    cbbIP: TComboBox;
    lblLocal: TLabel;
    btnSniff: TButton;
    rbTCP: TRadioButton;
    rbUDP: TRadioButton;
    rbICMP: TRadioButton;
    mmoIPSniffer: TMemo;
    lblIPCount: TLabel;
    rbAll: TRadioButton;
    btnIPManual: TButton;
    btnCheckSum: TButton;
    tsSSL: TTabSheet;
    btnSSLListenStart: TButton;
    btnSSLParseTest: TButton;
    mmoSSL: TMemo;
    bvl1: TBevel;
    btnSSLClient: TButton;
    edtTLSHost: TEdit;
    edtTLSPort: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnSniffClick(Sender: TObject);
    procedure btnIPManualClick(Sender: TObject);
    procedure btnCheckSumClick(Sender: TObject);
    procedure btnSSLListenStartClick(Sender: TObject);
    procedure btnSSLParseTestClick(Sender: TObject);
    procedure btnSSLClientClick(Sender: TObject);
  private
    FRecving: Boolean;
    FRecCount: Integer;
    FSnifSock: TSocket;
    procedure UpdateButtonState;
    procedure StartSniff;
    procedure StopSniff;
    procedure ParsingPacket(Buf: Pointer; DataLen: Integer);
  public
    FSocket: TSocket;
    FAddr: TSockAddrIn;
    FTlsClientSocket: TSocket;
  end;

  TIpInfo = record
    Address: Int64;
    IP: string;
    Host: string;
  end;

  TIP_NetType = (iptNone, iptANet, iptBNet, iptCNet, iptDNet, iptENet,
    iptBroadCast, iptKeepAddr);

  TIPNotes = array[1..4] of Byte;

  TIP_Info = packed record
    IPAddress: Cardinal;                 // IP��ַ,�˴������δ洢
    SubnetMask: Cardinal;                // ��������,�˴������δ洢
    Broadcast: Cardinal;                 // �㲥��ַ,�˴������δ洢
    HostName: array[0..255] of AnsiChar; // ������
    NetType: TIP_NetType;                // IP��ַ����������
    Notes: TIPNotes;                     // IP��ַ�ĸ��ӽڵ�
    UpState: Boolean;                    // ����״̬
    Loopback: Boolean;                   // �Ƿ񻷻ص�ַ
    SupportBroadcast: Boolean;           // �Ƿ�֧�ֹ㲥
  end;

  TIPGroup = array of TIP_Info; //IP��ַ��

  sockaddr_gen = packed record
    AddressIn: sockaddr_in;
    filler: packed array[0..7] of AnsiChar;
  end;

  TINTERFACE_INFO = packed record
    iiFlags: u_long; // Interface flags
    iiAddress: sockaddr_gen; // Interface address
    iiBroadcastAddress: sockaddr_gen; // Broadcast address
    iiNetmask: sockaddr_gen; // Network mask
  end;

  TWSAIoctl = function(s: TSocket; cmd: DWORD; lpInBuffer: PByte; dwInBufferLen:
    DWORD; lpOutBuffer: PByte; dwOutBufferLen: DWORD; lpdwOutBytesReturned:
    LPDWORD; lpOverLapped: POINTER; lpOverLappedRoutine: POINTER): Integer; stdcall;

var
  FormNetDecl: TFormNetDecl;

implementation

uses
  CnNetwork, CnNative, CnSocket, CnRandom;

{$R *.DFM}

const
  WS2_32DLL = 'WS2_32.DLL';
  MAXIPNOTE = 255;
  IPJOIN = '.';
  IPADDRFORMAT = '%0:D.%1:D.%2:D.%3:D';
  SIO_GET_INTERFACE_LIST = $4004747F;
  IOC_VENDOR = $18000000;
  SIO_RCVALL = IOC_IN or IOC_VENDOR or 1;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;
  IPNOTE1 = $FF000000;
  IPNOTE2 = $00FF0000;
  IPNOTE3 = $0000FF00;
  IPNOTE4 = $000000FF;

const
  DATA_CLIENT_HELLO =
    '1603010200' + 
    '010001FC' +
    '0303' + 'FB55143D5EA1D3D75161F4F1C4D005CC5481ADE320C1A7CC2D63584EFBFB6A8A' + // 32 �ֽ� Random
    '20' + '2D028CC0B79560C101974EA1F6F16992E14C54565751FC0214FFDE2B782D52C8'+    // 32 �ֽ� Session
    '0020' + 'CACA130113021303C02BC02FC02CC030CCA9CCA8C013C014009C009D002F0035' + // 32 �ֽڵ� CipherSuites
    '01' + '00' +  // ѹ��
    '0193' + // ��չ�ܳ��ȣ������ 806 �ֽ�
    '1A1A' + '00000010000E000C02683208' +
    '687474702F312E31000A000A0008EAEA001D0017001844690005000302683200' +
    '0B00020100001700000033002B0029EAEA000100001D00207FF1FDC5590367C8' +
    '316B70BA34BF05CA810ECB47337D973EC82BF60FDE114C13001B000302000200' +
    '0500050100000000000D0012001004030804040105030805050108060601002D' +
    '00020101FF0100010000120000002B0007068A8A0304030300230000FAFA0001' +
    '00001500E0000000000000000000000000000000000000000000000000000000' +
    '0000000000000000000000000000000000000000000000000000000000000000' +
    '0000000000000000000000000000000000000000000000000000000000000000' +
    '0000000000000000000000000000000000000000000000000000000000000000' +
    '0000000000000000000000000000000000000000000000000000000000000000' +
    '0000000000000000000000000000000000000000000000000000000000000000' +
    '0000000000000000000000000000000000000000000000000000000000000000' +
    '0000000000';

var
  WSAIoctl: TWSAIoctl = nil;
  WS2_32DllHandle: THandle = 0;

procedure InitWSAIoctl;
begin
  WS2_32DllHandle := LoadLibrary(WS2_32DLL);
  if WS2_32DllHandle <> 0 then
  begin
    @WSAIoctl := GetProcAddress(WS2_32DllHandle, 'WSAIoctl');
  end;
end;

procedure FreeWSAIoctl;
begin
  if WS2_32DllHandle <> 0 then
    FreeLibrary(WS2_32DllHandle);
end;

function SetIP(aIPAddr: string; var aIP: TIpInfo): Boolean;
var
  pIPAddr: PAnsiChar;
begin
  Result := False;
  aIP.Address := INADDR_NONE;
  aIP.IP := aIPAddr;
  aIP.Host := '';
  if aIP.IP = '' then
    Exit;

  GetMem(pIPAddr, Length(aIP.IP) + 1);
  try
    ZeroMemory(pIPAddr, Length(aIP.IP) + 1);
    StrPCopy(pIPAddr, {$IFDEF UNICODE}AnsiString{$ENDIF}(aIP.IP));
    aIP.Address := inet_addr(PAnsiChar(pIPAddr)); // IPת�����޵�����
  finally
    FreeMem(pIPAddr); // �ͷ�����Ķ�̬�ڴ�
  end;
  Result := aIP.Address <> INADDR_NONE;
end;

function GetIPNotes(const aIP: string; var aResult: TIPNotes): Boolean;
var
  iPos, iNote: Integer;
  sIP: string;

  function CheckIpNote(aNote: string): Byte;
  begin
    iNote := StrToInt(aNote);
    if (iNote < 0) or (iNote > MAXIPNOTE) then
      raise Exception.Create(aNote + ' Error Range.');
    Result := iNote;
  end;

begin
  iPos := Pos(IPJOIN, aIP);
  aResult[1] := CheckIpNote(Copy(aIP, 1, iPos - 1));
  sIP := Copy(aIP, iPos + 1, 20);
  iPos := Pos(IPJOIN, sIP);
  aResult[2] := CheckIpNote(Copy(sIP, 1, iPos - 1));
  sIP := Copy(sIP, iPos + 1, 20);
  iPos := Pos(IPJOIN, sIP);
  aResult[3] := CheckIpNote(Copy(sIP, 1, iPos - 1));
  aResult[4] := CheckIpNote(Copy(sIP, iPos + 1, 20));
  Result := aResult[1] > 0;
end;

function IPTypeCheck(const aIP: string): TIP_NetType;
var
  FNotes: TIPNotes;
begin
  Result := iptNone;
  if GetIPNotes(aIP, FNotes) then
  begin
    case FNotes[1] of
      1..126:
        Result := iptANet;
      127:
        Result := iptKeepAddr;
      128..191:
        Result := iptBNet;
      192..223:
        Result := iptCNet;
      224..239:
        Result := iptDNet;
      240..255:
        Result := iptENet;
    else
      Result := iptNone;
    end;
  end;
end;

function IPToInt(const aIP: string): Cardinal;
var
  FNotes: TIPNotes;
begin
  Result := 0;
  if IPTypeCheck(aIP) = iptNone then
  begin
    //raise Exception.Create(SCnErrorAddress);
    Exit;
  end;
  if GetIPNotes(aIP, FNotes) then
  begin
    Result := Result or FNotes[1] shl 24 or FNotes[2] shl 16 or FNotes[3] shl 8
      or FNotes[4];
  end;
end;

function IntToIP(const aIP: Cardinal): string;
var
  FNotes: TIPNotes;
begin
  FNotes[1] := aIP and IPNOTE1 shr 24;
  FNotes[2] := aIP and IPNOTE2 shr 16;
  FNotes[3] := aIP and IPNOTE3 shr 8;
  FNotes[4] := aIP and IPNOTE4;
  Result := Format(IPADDRFORMAT, [FNotes[1], FNotes[2], FNotes[3], FNotes[4]]);
end;

function EnumLocalIP(var aLocalIP: TIPGroup): Integer;
var
  skLocal: TSocket;
  iIP: Integer;
  PtrA: pointer;
  BytesReturned, SetFlags: u_long;
  pAddrInet: Sockaddr_IN;
  Buffer: array[0..20] of TINTERFACE_INFO;
  FWSAData: TWSAData;
begin
  Result := 0;

  WSAStartup($101, FWSAData);
  try
    skLocal := socket(AF_INET, SOCK_STREAM, 0); // Open a socket
    if (skLocal = INVALID_SOCKET) then
      Exit;

    try // Call WSAIoCtl
      PtrA := @BytesReturned;
      if (WSAIoctl(skLocal, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA,
        nil, nil) <> SOCKET_ERROR) then
      begin // If ok, find out how
        Result := BytesReturned div SizeOf(TINTERFACE_INFO);
        SetLength(aLocalIP, Result);
        for iIP := 0 to Result - 1 do // For every interface
        begin
          pAddrInet := Buffer[iIP].iiAddress.AddressIn;
          aLocalIP[iIP].IPAddress := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa
            (pAddrInet.sin_addr)));
          pAddrInet := Buffer[iIP].iiNetMask.AddressIn;
          aLocalIP[iIP].SubnetMask := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa
            (pAddrInet.sin_addr)));
          pAddrInet := Buffer[iIP].iiBroadCastAddress.AddressIn;
          aLocalIP[iIP].Broadcast := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa
            (pAddrInet.sin_addr)));
          SetFlags := Buffer[iIP].iiFlags;
          aLocalIP[iIP].UpState := (SetFlags and IFF_UP) = IFF_UP;
          aLocalIP[iIP].Loopback := (SetFlags and IFF_LOOPBACK) = IFF_LOOPBACK;
          aLocalIP[iIP].SupportBroadcast := (SetFlags and IFF_BROADCAST) = IFF_BROADCAST;
        end;
      end;
    except
      ;
    end;
    closesocket(skLocal);
  finally
    WSACleanup;
  end;
end;

procedure TFormNetDecl.FormCreate(Sender: TObject);
var
  Wsa: TWSAData;
  aLocalIP: TIPGroup;
  I: Integer;
begin
  if (WSAStartup(MakeWord(2, 2), Wsa) <> 0) then
    raise Exception.Create('WSAStartup');
  InitWSAIoctl;

  EnumLocalIP(aLocalIP);
  if Length(aLocalIP) > 0 then
  begin
    for I := 0 to Length(aLocalIP) - 1 do
      cbbIP.Items.Add(IntToIP(aLocalIP[I].IPAddress));
    cbbIP.ItemIndex := 0;
  end;
end;

procedure TFormNetDecl.btnSniffClick(Sender: TObject);
begin
  if FRecving then
  begin
    // Stop
    StopSniff;
    UpdateButtonState;
  end
  else
  begin
    // Start
    StartSniff;
    UpdateButtonState;
  end;
end;

procedure TFormNetDecl.UpdateButtonState;
begin
  if FRecving then
  begin
    btnSniff.Caption := 'Stop';
  end
  else
  begin
    btnSniff.Caption := 'Sniff';
  end;
end;

procedure TFormNetDecl.StartSniff;
var
  RecvTimeout: Integer;
  Sa: TSockAddr;
  Ret: Integer;
  Buf: array[0..65535] of AnsiChar;
  BytesRet, InBufLen: Cardinal;
  CtlBuf: array[0..1023] of AnsiChar;
  DataLen: Integer;
  ListeningIP: u_long;
begin
  if FRecving then
    Exit;

  FRecCount := 0;
  mmoIPSniffer.Clear;

  FSnifSock := socket(AF_INET, SOCK_RAW, IPPROTO_IP);
  if FSnifSock = INVALID_SOCKET then
    raise Exception.Create('Create Socket Fail.');

  RecvTimeout := 10;
  if (setsockopt(FSnifSock, SOL_SOCKET, SO_RCVTIMEO, @RecvTimeout, SizeOf(RecvTimeout))
    = SOCKET_ERROR) then
  begin
    Ret := WSAGetLastError;
    closesocket(FSnifSock);
    raise Exception.CreateFmt('Set Socketopt Fail. %d', [Ret]);
  end;

  ListeningIP := inet_addr(PAnsiChar(cbbIP.Text));

  Sa.sin_family := AF_INET;
  Sa.sin_port := htons(0);
  Sa.sin_addr.s_addr := ListeningIP;
  if bind(FSnifSock, Sa, SizeOf(Sa)) = SOCKET_ERROR then
  begin
    Ret := WSAGetLastError;
    closesocket(FSnifSock);
    raise Exception.CreateFmt('Bind IP Fail. %d', [Ret]);
  end;

  InBufLen := 1;
  if WSAIoctl(FSnifSock, SIO_RCVALL, @InBufLen, SizeOf(InBufLen), @CtlBuf[0],
    SizeOf(CtlBuf), @BytesRet, nil, nil) = SOCKET_ERROR then
  begin
    Ret := WSAGetLastError;
    closesocket(FSnifSock);
    raise Exception.CreateFmt('WSAIoctl Fail. %d', [Ret]);
  end;

  FillChar(Buf[0], SizeOf(Buf), 0);
  FRecving := True;
  UpdateButtonState;

  Application.ProcessMessages;
  try
    while FRecving do
    begin
      DataLen := recv(FSnifSock, Buf[0], SizeOf(Buf), 0);
      if (DataLen <> SOCKET_ERROR) and (DataLen > 0) then
      begin
        Inc(FRecCount);
        lblIPCount.Caption := IntToStr(FRecCount);

        // ���� IP ��
        ParsingPacket(@Buf[0], DataLen);
      end
      else if DataLen = SOCKET_ERROR then
      begin
        Ret := WSAGetLastError;
        if Ret <> 10060 then // 10060 �ǳ�ʱ�����ݣ���������
          raise Exception.CreateFmt('Recv Fail. %d', [Ret]);
      end;

      Application.ProcessMessages;
    end;
  finally
    closesocket(FSnifSock);
    UpdateButtonState;
  end;
end;

procedure TFormNetDecl.StopSniff;
begin
  if not FRecving then
    Exit;
  FRecving := False;
end;

procedure TFormNetDecl.ParsingPacket(Buf: Pointer; DataLen: Integer);
var
  PIP: PCnIPHeader;
  PTCP: PCnTCPHeader;
  PUDP: PCnUDPHeader;
  PICMP: PCnICMPHeader;
begin
  PIP := Buf;
  if not rbAll.Checked then
  begin
    if rbTCP.Checked and (PIP^.Protocol <> CN_IP_PROTOCOL_TCP) then
      Exit;
    if rbUDP.Checked and (PIP^.Protocol <> CN_IP_PROTOCOL_UDP) then
      Exit;
    if rbICMP.Checked and (PIP^.Protocol <> CN_IP_PROTOCOL_ICMP) then
      Exit;
  end;

  mmoIPSniffer.Lines.Add('=== Got an IP Packet. Length: ' + IntToStr(DataLen));
  mmoIPSniffer.Lines.Add('IP Version: ' + IntToStr(CnGetIPVersion(PIP)));
  mmoIPSniffer.Lines.Add('IP Header Length(Bytes): ' + IntToStr(SizeOf(DWORD) *
    CnGetIPHeaderLength(PIP)));
  mmoIPSniffer.Lines.Add('IP Type Of Service Precedence: ' + IntToStr(CnGetIPTypeOfServicePrecedence
    (PIP)));
  mmoIPSniffer.Lines.Add('IP Type Of Service Delay: ' + IntToStr(Integer(CnGetIPTypeOfServiceDelay
    (PIP))));
  mmoIPSniffer.Lines.Add('IP Type Of Service Throughput: ' + IntToStr(Integer(CnGetIPTypeOfServiceThroughput
    (PIP))));
  mmoIPSniffer.Lines.Add('IP Type Of Service Relibility: ' + IntToStr(Integer(CnGetIPTypeOfServiceReliability
    (PIP))));
  mmoIPSniffer.Lines.Add('IP TotalLength(Bytes): ' + IntToStr(CnGetIPTotalLength(PIP)));
  mmoIPSniffer.Lines.Add(Format('IP Identification: $%4.4x', [CnGetIPIdentification
    (PIP)]));
  mmoIPSniffer.Lines.Add('IP Fragment Offset: ' + IntToStr(CnGetIPFragmentOffset(PIP)));
  mmoIPSniffer.Lines.Add('IP Fragment Dont Flag: ' + IntToStr(Integer(CnGetIPFlagDontFragment
    (PIP))));
  mmoIPSniffer.Lines.Add('IP Fragment More Flag: ' + IntToStr(Integer(CnGetIPFlagMoreFragment
    (PIP))));
  mmoIPSniffer.Lines.Add('IP TTL: ' + IntToStr(PIP^.TTL));
  mmoIPSniffer.Lines.Add('IP Protocol: ' + IntToStr(PIP^.Protocol));
  mmoIPSniffer.Lines.Add('IP Checksum: ' + IntToStr(CnGetIPCheckSum(PIP)));
  mmoIPSniffer.Lines.Add('IP Source: ' + IntToIP(CnGetIPSourceIP(PIP)));
  mmoIPSniffer.Lines.Add('IP Destination: ' + IntToIP(CnGetIPDestIP(PIP)));

  if PIP^.Protocol = CN_IP_PROTOCOL_TCP then
  begin
    PTCP := PCnTCPHeader(Integer(PIP) + SizeOf(DWORD) * CnGetIPHeaderLength(PIP));
    mmoIPSniffer.Lines.Add('  TCP Source Port: ' + IntToStr(CnGetTCPSourcePort(PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Destination Port: ' + IntToStr(CnGetTCPDestPort(PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Seq Num: ' + IntToStr(CnGetTCPSequenceNumber(PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Ack Num: ' + IntToStr(CnGetTCPAcknowledgementNumber
      (PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Offset(Bytes): ' + IntToStr(SizeOf(DWORD) *
      CnGetTCPOffset(PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Flag URG: ' + IntToStr(Integer(CnGetTCPFlagURG(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag ACK: ' + IntToStr(Integer(CnGetTCPFlagACK(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag PSH: ' + IntToStr(Integer(CnGetTCPFlagPSH(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag RST: ' + IntToStr(Integer(CnGetTCPFlagRST(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag SYN: ' + IntToStr(Integer(CnGetTCPFlagSYN(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag FIN: ' + IntToStr(Integer(CnGetTCPFlagFIN(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Window: ' + IntToStr(Integer(CnGetTCPWindow(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP CheckSum: ' + IntToStr(Integer(CnGetTCPCheckSum
      (PTCP))));
    mmoIPSniffer.Lines.Add('  TCP UrgentPointer: ' + IntToStr(Integer(CnGetTCPUrgentPointer
      (PTCP))));
  end
  else if PIP^.Protocol = CN_IP_PROTOCOL_UDP then
  begin
    PUDP := PCnUDPHeader(Integer(PIP) + SizeOf(DWORD) * CnGetIPHeaderLength(PIP));
    mmoIPSniffer.Lines.Add('  UDP Source Port: ' + IntToStr(CnGetUDPSourcePort(PUDP)));
    mmoIPSniffer.Lines.Add('  UDP Destination Port: ' + IntToStr(CnGetUDPDestPort(PUDP)));
    mmoIPSniffer.Lines.Add('  UDP Length: ' + IntToStr(CnGetUDPLength(PUDP)));
    mmoIPSniffer.Lines.Add('  UDP CheckSum: ' + IntToStr(Integer(CnGetUDPCheckSum(PUDP))));
  end
  else if PIP^.Protocol = CN_IP_PROTOCOL_ICMP then
  begin
    PICMP := PCnICMPHeader(Integer(PIP) + SizeOf(DWORD) * CnGetIPHeaderLength(PIP));
    mmoIPSniffer.Lines.Add('  ICMP Type: ' + IntToStr(CnGetICMPType(PICMP)));
    mmoIPSniffer.Lines.Add('  ICMP Code: ' + IntToStr(CnGetICMPCode(PICMP)));
  end;
end;

procedure TFormNetDecl.btnIPManualClick(Sender: TObject);
const
  IP: array[0..47] of Byte = ($45, $00, $00, $30, $00, $00, $40, $00, $35,
    $06, $24, $98, $DC, $B5, $7C, $32, $AC, $14, $1C, $34, $00, $50, $C3, $F9,
    $5E, $56, $55, $60, $DD, $24, $1A, $2B, $70, $12, $39, $08, $B9, $8E, $00,
    $00, $02, $04, $05, $AC, $01, $01, $04, $02);
begin
  mmoIPSniffer.Clear;
  ParsingPacket(@IP[0], SizeOf(IP));
end;

procedure TFormNetDecl.btnCheckSumClick(Sender: TObject);
const
  IP_DATA: array[0..19] of Byte =
    ($45, $00, $00, $3C, $CA, $2C, $00, $00, $80, $01, $00, $00,
     $C0, $A8, $04, $FD, $C0, $A8, $04, $05);
var
  R: Word;
begin
  R := CnGetNetworkCheckSum(@IP_DATA[0], SizeOf(IP_DATA));
  ShowMessage(IntToHex(R, 2)); // �õ� E641
end;

procedure TFormNetDecl.btnSSLListenStartClick(Sender: TObject);
var
  ClientSocket: TSocket;
  ClientAddr: TSockAddrIn;
  BufLen: Integer;
  Data: array[0..1023] of Byte;
  F: TFileStream;
begin
  // �����׽���
  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    raise Exception.Create('Socket creation failed');

  // �󶨵����ض˿�
  FAddr.sin_family := AF_INET;
  FAddr.sin_port := htons(8020);
  FAddr.sin_addr.S_addr := htonl(INADDR_ANY);
  if bind(FSocket, FAddr, SizeOf(FAddr)) <> 0 then
    raise Exception.Create('Bind failed');

  // ��ʼ������������
  if listen(FSocket, SOMAXCONN) <> 0 then
    raise Exception.Create('Listen failed');

  ClientSocket := accept(FSocket, @ClientAddr, @BufLen);
  if ClientSocket = INVALID_SOCKET then
    Exit;

  try
    // �ӿͻ��˽�������
    BufLen := recv(ClientSocket, Data, SizeOf(Data), 0);
    if BufLen > 0 then
    begin
      // ������յ�������
      F := TFileStream.Create('Data.txt', fmCreate);
      F.WriteBuffer(Data[0], BufLen);
      F.Free;
    end;
  finally
    // �رտͻ����׽���
    closesocket(ClientSocket);
  end;
end;

procedure TFormNetDecl.btnSSLParseTestClick(Sender: TObject);
var
  I: Integer;
  Data, T: TBytes;
  Cp: TWords;
  P1: PCnTLSRecordLayer;
  P2: PCnTLSHandShakeHeader;
  P3: PCnTLSHandShakeClientHello;
begin
  mmoSSL.Lines.Clear;
  Data := HexToBytes(DATA_CLIENT_HELLO);
  P1 := PCnTLSRecordLayer(@Data[0]);
  mmoSSL.Lines.Add(Format('TLSRecordLayer.ContentType %d', [P1^.ContentType]));
  mmoSSL.Lines.Add(Format('TLSRecordLayer.MajorVersion %d', [P1^.MajorVersion]));
  mmoSSL.Lines.Add(Format('TLSRecordLayer.MinorVersion %d', [P1^.MinorVersion]));
  mmoSSL.Lines.Add(Format('TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(P1)]));

  P2 := PCnTLSHandShakeHeader(@(P1^.Body));
  mmoSSL.Lines.Add(Format('TLSHandShakeHeader.HandShakeType %d', [P2^.HandShakeType]));
  mmoSSL.Lines.Add(Format('TLSHandShakeHeader.Length %d', [CnGetTLSHandShakeHeaderContentLength(P2)]));

  P3 := PCnTLSHandShakeClientHello(@(P2^.Content));
  mmoSSL.Lines.Add(Format('TLSHandShakeClientHello.ProtocolVersion $%4.4x', [P3^.ProtocolVersion]));
  mmoSSL.Lines.Add(Format('TLSHandShakeClientHello.Random32: %s', [DataToHex(@P3^.Random[0], SizeOf(P3^.Random))]));
  mmoSSL.Lines.Add(Format('TLSHandShakeClientHello.SessionLength %d', [P3^.SessionLength]));
  T := CnGetTLSHandShakeClientHelloSessionId(P3);
  mmoSSL.Lines.Add(Format('TLSHandShakeClientHello.SessionId: %s', [BytesToHex(T)]));
  mmoSSL.Lines.Add(Format('TLSHandShakeClientHello.CipherSuitesLength: %d', [CnGetTLSHandShakeClientHelloCipherSuitesLength(P3)]));

  Cp := CnGetTLSHandShakeClientHelloCipherSuites(P3);
  for I := 0 to Length(Cp) - 1 do
    mmoSSL.Lines.Add(Format('TLSHandShakeClientHello.CipherSuites[%d]: %4.4x %s', [I, Cp[I], GetNameFromCipher(Cp[I])]));

  mmoSSL.Lines.Add(Format('TLSHandShakeClientHello.CompressionMethodLength: %d', [CnGetTLSHandShakeClientHelloCompressionMethodLength(P3)]));
  T := CnGetTLSHandShakeClientHelloCompressionMethod(P3);
  mmoSSL.Lines.Add(Format('TLSHandShakeClientHello.CompressionMethod: %s', [BytesToHex(T)]));
end;

procedure TFormNetDecl.btnSSLClientClick(Sender: TObject);
const
  HOST: AnsiString = 'www.cnpack.org';
var
  SockAddress: TSockAddr;
  Data: TBytes;
  Buffer: array[0..1023] of Byte;
  Ciphers: TWords;
  H: PCnTLSRecordLayer;
  B: PCnTLSHandShakeHeader;
  C: PCnTLSHandShakeClientHello;
  E: PCnTLSHandShakeExtensions;
  S: PCnTLSHandShakeServerNameIndication;
  BytesReceived: Integer;
begin
  // ���� Socket ����Ŀ�꣬�������ݣ��հ�
  FTlsClientSocket := CnNewSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FTlsClientSocket = INVALID_SOCKET then
    Exit;

  SockAddress.sin_family := AF_INET;
  SockAddress.sin_port := ntohs(StrToIntDef(edtTLSPort.Text, 443));

  SockAddress.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(edtTLSHost.Text)));
  if SOCKET_ERROR <> CnConnect(FTlsClientSocket, SockAddress, SizeOf(SockAddress)) then
  begin
    // ���� ClientData ���� Data
    // Data := HexToBytes(DATA_CLIENT_HELLO);
    FillChar(Buffer, SizeOf(Buffer), 0);

    // ��������Э���е� ClientHello ��

    // TLS ��ͷ
    H := PCnTLSRecordLayer(@Buffer[0]);
    H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H^.MajorVersion := 3;
    H^.MinorVersion := 1;
    // H^.BodyLength := 0;    // TODO: �ߴ�

    // ����Э��ͷ
    B := PCnTLSHandShakeHeader(@H^.Body[0]);
    B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_CLIENT_HELLO;
    // CnSetTLSHandShakeHeaderContentLength(B, 0); // �ߴ����� 0�������ٲ�

    // ����Э�� ClientHello ��
    C := PCnTLSHandShakeClientHello(@B^.Content[0]);

    // ���������汾��
    C^.ProtocolVersion := CN_TLS_SSL_VERSION_TLS_13;

    // ���������
    CnRandomFillBytes2(@C^.Random[0], SizeOf(C^.Random));

    // ������� Session
    CnSetTLSHandShakeClientHelloSessionId(C, CnRandomBytes(32));

    // ��� Ciphers
    SetLength(Ciphers, 4);
    Ciphers[0] := CN_CIPHER_TLS_SM4_GCM_SM3;
    Ciphers[1] := CN_CIPHER_TLS_AES_256_GCM_SHA384;
    Ciphers[2] := CN_CIPHER_TLS_AES_128_CCM_SHA256;
    Ciphers[3] := CN_CIPHER_TLS_CHACHA20_POLY1305_SHA256;

    CnSetTLSHandShakeClientHelloCipherSuites(C, Ciphers);

    // ���ѹ������
    SetLength(Data, 1);
    Data[0] := 0;
    CnSetTLSHandShakeClientHelloCompressionMethod(C, Data);

    // �� SNI ���е�һ����¼
    E := CnGetTLSHandShakeClientHelloExtensions(C);
    CnSetTLSHandShakeExtensionsExtensionType(E, CN_TLS_EXTENSIONTYPE_SERVER_NAME);
    S := PCnTLSHandShakeServerNameIndication(@E^.ExtensionData[0]);
    CnSetTLSHandShakeExtensionsExtensionDataLength(E, CnTLSHandShakeServerNameIndicationAddHost(S, 'www.cnpack.org'));

    CnSetTLSHandShakeExtensionsExtensionLength(E, CnGetTLSHandShakeExtensionsExtensionDataLength(E) + SizeOf(Word)); // Data ��һ�� Type������ Length

    // C �� ExtensionData �����ֽڵĲ��ټ���չ���ݳ��Ⱦ�������Э��� Content ��
    CnSetTLSHandShakeHeaderContentLength(B, TCnIntAddress(@E.ExtensionData[0]) - TCnIntAddress(C) + CnGetTLSHandShakeExtensionsExtensionDataLength(E));

    CnSetTLSRecordLayerBodyLength(H, CnGetTLSHandShakeHeaderContentLength(B) + 4); // 3 �ֽ� Length + 1 �ֽ�����

    // ���� ClientHello ����+ 5 �� RecordLayer �е� Type Version BodyLength �ȳ��Ⱥ�
    if CnSend(FTlsClientSocket, H^, CnGetTLSRecordLayerBodyLength(H) + 5, 0) <> SOCKET_ERROR then
    begin
      // ���ջذ�
      FillChar(Buffer, SizeOf(Buffer), 0);
      BytesReceived := CnRecv(FTlsClientSocket, Buffer[0], Length(Buffer), 0);
      if BytesReceived > 0 then
      begin
        // ���� TLS ��Ӧ
      end;
    end;
  end;
  CnCloseSocket(FTlsClientSocket);
end;

end.

