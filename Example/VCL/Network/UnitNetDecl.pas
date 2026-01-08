unit UnitNetDecl;

interface

{$I CnPack.inc}

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
    btnSSLClientBad: TButton;
    edtTLSHost: TEdit;
    edtTLSPort: TEdit;
    btnSSLClient: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSniffClick(Sender: TObject);
    procedure btnIPManualClick(Sender: TObject);
    procedure btnCheckSumClick(Sender: TObject);
    procedure btnSSLListenStartClick(Sender: TObject);
    procedure btnSSLParseTestClick(Sender: TObject);
    procedure btnSSLClientBadClick(Sender: TObject);
    procedure btnSSLClientClick(Sender: TObject);
  private
    FRecving: Boolean;
    FRecCount: Integer;
    FSnifSock: TSocket;
    procedure UpdateButtonState;
    procedure StartSniff;
    procedure StopSniff;
    procedure MySSLLog(const Str: string);
    procedure ParsingPacket(Buf: Pointer; DataLen: Integer);
    function SSLHandShake12(const Host: AnsiString; Port: Word): Boolean;
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
    IPAddress: Cardinal;                 // IP地址,此处用整形存储
    SubnetMask: Cardinal;                // 子网掩码,此处用整形存储
    Broadcast: Cardinal;                 // 广播地址,此处用整形存储
    HostName: array[0..255] of AnsiChar; // 主机名
    NetType: TIP_NetType;                // IP地址的网络类型
    Notes: TIPNotes;                     // IP地址的各子节点
    UpState: Boolean;                    // 启用状态
    Loopback: Boolean;                   // 是否环回地址
    SupportBroadcast: Boolean;           // 是否支持广播
  end;

  TIPGroup = array of TIP_Info; //IP地址组

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
  CnNetwork, CnNative, CnSocket, CnRandom, CnECC, CnSHA2, CnCertificateAuthority,
  CnMD5, CnSHA1, CnSM3, CnAEAD, CnPoly1305, CnChaCha20, CnBigNumber;

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
    '0303' + 'FB55143D5EA1D3D75161F4F1C4D005CC5481ADE320C1A7CC2D63584EFBFB6A8A' + // 32 字节 Random
    '20' + '2D028CC0B79560C101974EA1F6F16992E14C54565751FC0214FFDE2B782D52C8' +   // 32 字节 Session
    '0020' + 'CACA130113021303C02BC02FC02CC030CCA9CCA8C013C014009C009D002F0035' + // 32 字节的 CipherSuites
    '01' + '00' +  // 压缩
    '0193' + // 扩展总长度，后面的 806 字节
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

  DATA_SERVER_HELLO =
    '1603030058' +
    '02000054' +
    '0303' + '0CCF42C46A501B5A8A32F9D0C6AACC6E88E034AC163C21F4749DC64A9F847A4E' +
    '20' + '8C37281CBEC0C9C167FBC6BA27CAB6F2C134154AA716692505CAD97843D898AE' +
    'C02B'+ '00' + '000C' + '00000000000B00040300010216030308350B0008310008';

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
    aIP.Address := inet_addr(PAnsiChar(pIPAddr)); // IP转换成无点整型
  finally
    FreeMem(pIPAddr); // 释放申请的动态内存
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

        // 解析 IP 包
        ParsingPacket(@Buf[0], DataLen);
      end
      else if DataLen = SOCKET_ERROR then
      begin
        Ret := WSAGetLastError;
        if Ret <> 10060 then // 10060 是超时无数据，继续接收
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
  ShowMessage(IntToHex(R, 2)); // 得到 E641
end;

procedure TFormNetDecl.btnSSLListenStartClick(Sender: TObject);
var
  ClientSocket: TSocket;
  ClientAddr: TSockAddrIn;
  BufLen: Integer;
  Data: array[0..1023] of Byte;
  F: TFileStream;
begin
  // 创建套接字
  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    raise Exception.Create('Socket creation failed');

  // 绑定到本地端口
  FAddr.sin_family := AF_INET;
  FAddr.sin_port := htons(8020);
  FAddr.sin_addr.S_addr := htonl(INADDR_ANY);
  if bind(FSocket, FAddr, SizeOf(FAddr)) <> 0 then
    raise Exception.Create('Bind failed');

  // 开始监听连接请求
  if listen(FSocket, SOMAXCONN) <> 0 then
    raise Exception.Create('Listen failed');

  ClientSocket := accept(FSocket, @ClientAddr, @BufLen);
  if ClientSocket = INVALID_SOCKET then
    Exit;

  try
    // 从客户端接收数据
    BufLen := recv(ClientSocket, Data, SizeOf(Data), 0);
    if BufLen > 0 then
    begin
      // 处理接收到的数据
      F := TFileStream.Create('Data.txt', fmCreate);
      F.WriteBuffer(Data[0], BufLen);
      F.Free;
    end;
  finally
    // 关闭客户端套接字
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
  S1: PCnTLSRecordLayer;
  S2: PCnTLSHandShakeHeader;
  S3: PCnTLSHandShakeServerHello;
  SE: PCnTLSHandShakeExtensions;
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

  mmoSSL.Lines.Add('');
  Data := HexToBytes(DATA_SERVER_HELLO);
  S1 := PCnTLSRecordLayer(@Data[0]);
  mmoSSL.Lines.Add(Format('TLSRecordLayer.ContentType %d', [S1^.ContentType]));
  mmoSSL.Lines.Add(Format('TLSRecordLayer.MajorVersion %d', [S1^.MajorVersion]));
  mmoSSL.Lines.Add(Format('TLSRecordLayer.MinorVersion %d', [S1^.MinorVersion]));
  mmoSSL.Lines.Add(Format('TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(S1)]));

  S2 := PCnTLSHandShakeHeader(@(S1^.Body));
  mmoSSL.Lines.Add(Format('TLSHandShakeHeader.HandShakeType %d', [S2^.HandShakeType]));
  mmoSSL.Lines.Add(Format('TLSHandShakeHeader.Length %d', [CnGetTLSHandShakeHeaderContentLength(S2)]));

  S3 := PCnTLSHandShakeServerHello(@(S2^.Content));
  mmoSSL.Lines.Add(Format('TLSHandShakeServerHello.ProtocolVersion $%4.4x', [S3^.ProtocolVersion]));
  mmoSSL.Lines.Add(Format('TLSHandShakeServerHello.Random32: %s', [DataToHex(@S3^.Random[0], SizeOf(S3^.Random))]));
  mmoSSL.Lines.Add(Format('TLSHandShakeServerHello.SessionLength %d', [S3^.SessionLength]));
  T := CnGetTLSHandShakeServerHelloSessionId(S3);
  mmoSSL.Lines.Add(Format('TLSHandShakeServerHello.SessionId: %s', [BytesToHex(T)]));
  mmoSSL.Lines.Add(Format('TLSHandShakeServerHello.CipherSuite: %4.4x %s', [CnGetTLSHandShakeServerHelloCipherSuite(S3), GetNameFromCipher(CnGetTLSHandShakeServerHelloCipherSuite(S3))]));
  mmoSSL.Lines.Add(Format('TLSHandShakeServerHello.CompressionMethod: %d', [CnGetTLSHandShakeServerHelloCompressionMethod(S3)]));

  SE := CnGetTLSHandShakeServerHelloExtensions(S3);
  mmoSSL.Lines.Add(Format('TLSHandShakeServerHello.ExtensionLength: %d', [CnGetTLSHandShakeExtensionsExtensionLength(SE)]));
end;

function UInt64ToBE8(Value: TUInt64): TBytes;
var
  R: TBytes;
  I: Integer;
begin
  SetLength(R, 8);
  for I := 0 to 7 do
    R[7 - I] := Byte((Value shr (I * 8)) and $FF);
  Result := R;
end;

function NewZeroBytes(Count: Integer): TBytes;
begin
  SetLength(Result, Count);
  if Count > 0 then
    FillChar(Result[0], Count, 0);
end;

function TLSChaChaNonce(const FixedIV12: TBytes; Seq: TUInt64): TBytes;
var
  N: TBytes;
  S: TBytes;
  I: Integer;
begin
  SetLength(N, 12);
  S := UInt64ToBE8(Seq);
  for I := 0 to 3 do
    N[I] := FixedIV12[I] xor 0;
  for I := 0 to 7 do
    N[4 + I] := FixedIV12[4 + I] xor S[I];
  Result := N;
end;

function Poly1305TagTLS(AAD, C: TBytes; PolyKey: TBytes): TCnGCM128Tag;
var
  M: TBytes;
  PadLen: Integer;
  D: TCnPoly1305Digest;
  Ls: array[0..15] of Byte;
  AL, CL: TUInt64;
begin
  M := nil;
  if AAD <> nil then
  begin
    M := ConcatBytes(M, AAD);
    PadLen := (16 - (Length(AAD) mod 16)) mod 16;
    if PadLen > 0 then
      M := ConcatBytes(M, NewZeroBytes(PadLen));
  end;
  if C <> nil then
  begin
    M := ConcatBytes(M, C);
    PadLen := (16 - (Length(C) mod 16)) mod 16;
    if PadLen > 0 then
      M := ConcatBytes(M, NewZeroBytes(PadLen));
  end;
  FillChar(Ls[0], SizeOf(Ls), 0);
  AL := Length(AAD);
  CL := Length(C);
  Move(AL, Ls[0], SizeOf(TUInt64));
  Move(CL, Ls[8], SizeOf(TUInt64));
  M := ConcatBytes(M, NewBytesFromMemory(@Ls[0], SizeOf(Ls)));
  D := Poly1305Bytes(M, PolyKey);
  Move(D[0], Result[0], SizeOf(Result));
end;

function ChaCha20Poly1305EncryptTLS(Key, FixedIV12, Plain, AAD: TBytes; Seq: TUInt64; out Tag: TCnGCM128Tag): TBytes;
var
  K: TCnChaChaKey;
  N: TCnChaChaNonce;
  Nonce12, OTK, KS: TBytes;
  Stream: TCnChaChaState;
  I, J: Integer;
begin
  Result := nil;
  Nonce12 := TLSChaChaNonce(FixedIV12, Seq);
  Move(Key[0], K[0], SizeOf(TCnChaChaKey));
  Move(Nonce12[0], N[0], SizeOf(TCnChaChaNonce));
  ChaCha20Block(K, N, 0, Stream);
  SetLength(KS, 64);
  for I := 0 to 15 do
    for J := 0 to 3 do
      KS[I * 4 + J] := Byte((Stream[I] shr (J * 8)) and $FF);
  OTK := Copy(KS, 0, 32);
  SetLength(Result, Length(Plain));
  if Length(Plain) > 0 then
    Result := ChaCha20EncryptBytes(K, N, Plain);
  Tag := Poly1305TagTLS(AAD, Result, OTK);
end;

function ChaCha20Poly1305DecryptTLS(Key, FixedIV12, En, AAD: TBytes; Seq: TUInt64; InTag: TCnGCM128Tag): TBytes;
var
  K: TCnChaChaKey;
  N: TCnChaChaNonce;
  Nonce12, OTK, KS: TBytes;
  Stream: TCnChaChaState;
  I, J: Integer;
  CTag: TCnGCM128Tag;
begin
  Result := nil;
  Nonce12 := TLSChaChaNonce(FixedIV12, Seq);
  Move(Key[0], K[0], SizeOf(TCnChaChaKey));
  Move(Nonce12[0], N[0], SizeOf(TCnChaChaNonce));
  ChaCha20Block(K, N, 0, Stream);
  SetLength(KS, 64);
  for I := 0 to 15 do
    for J := 0 to 3 do
      KS[I * 4 + J] := Byte((Stream[I] shr (J * 8)) and $FF);
  OTK := Copy(KS, 0, 32);
  CTag := Poly1305TagTLS(AAD, En, OTK);
  if not CompareMem(@CTag[0], @InTag[0], SizeOf(TCnGCM128Tag)) then
    Exit;
  SetLength(Result, Length(En));
  if Length(En) > 0 then
    Result := ChaCha20DecryptBytes(K, N, En);
end;

function EccDigestBytes(Data: TBytes; DigestType: TCnEccSignDigestType): TBytes;
var
  MD5Dig: TCnMD5Digest;
  SHA1Dig: TCnSHA1Digest;
  SHA256Dig: TCnSHA256Digest;
  SM3Dig: TCnSM3Digest;
  SHA384Dig: TCnSHA384Digest;
  SHA512Dig: TCnSHA512Digest;
begin
  Result := nil;
  case DigestType of
    esdtMD5:
      begin
        MD5Dig := MD5Bytes(Data);
        Result := NewBytesFromMemory(@MD5Dig[0], SizeOf(TCnMD5Digest));
      end;
    esdtSHA1:
      begin
        SHA1Dig := SHA1Bytes(Data);
        Result := NewBytesFromMemory(@SHA1Dig[0], SizeOf(TCnSHA1Digest));
      end;
    esdtSHA256:
      begin
        SHA256Dig := SHA256Bytes(Data);
        Result := NewBytesFromMemory(@SHA256Dig[0], SizeOf(TCnSHA256Digest));
      end;
    esdtSM3:
      begin
        SM3Dig := SM3Bytes(Data);
        Result := NewBytesFromMemory(@SM3Dig[0], SizeOf(TCnSM3Digest));
      end;
    esdtSHA384:
      begin
        SHA384Dig := SHA384Bytes(Data);
        Result := NewBytesFromMemory(@SHA384Dig[0], SizeOf(TCnSHA384Digest));
      end;
    esdtSHA512:
      begin
        SHA512Dig := SHA512Bytes(Data);
        Result := NewBytesFromMemory(@SHA512Dig[0], SizeOf(TCnSHA512Digest));
      end;
  end;
end;

function EccHMacBytes(Key: TBytes; Data: TBytes; DigestType:
  TCnEccSignDigestType): TBytes;
var
  MD5Dig: TCnMD5Digest;
  SHA1Dig: TCnSHA1Digest;
  SHA256Dig: TCnSHA256Digest;
  SM3Dig: TCnSM3Digest;
  SHA384Dig: TCnSHA384Digest;
  SHA512Dig: TCnSHA512Digest;
begin
  Result := nil;
  case DigestType of
    esdtMD5:
      begin
        MD5Dig := MD5HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@MD5Dig[0], SizeOf(TCnMD5Digest));
      end;
    esdtSHA1:
      begin
        SHA1Dig := SHA1HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SHA1Dig[0], SizeOf(TCnSHA1Digest));
      end;
    esdtSHA256:
      begin
        SHA256Dig := SHA256HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SHA256Dig[0], SizeOf(TCnSHA256Digest));
      end;
    esdtSM3:
      begin
        SM3Dig := SM3HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SM3Dig[0], SizeOf(TCnSM3Digest));
      end;
    esdtSHA384:
      begin
        SHA384Dig := SHA384HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SHA384Dig[0], SizeOf(TCnSHA384Digest));
      end;
    esdtSHA512:
      begin
        SHA512Dig := SHA512HmacBytes(Key, Data);
        Result := NewBytesFromMemory(@SHA512Dig[0], SizeOf(TCnSHA512Digest));
      end;
  end;
end;

function PseudoRandomFunc(Secret: TBytes; const PLabel: AnsiString; Seed: TBytes;
  DigestType: TCnEccSignDigestType; NeedLength: Integer): TBytes;
var
  Data, Res, A: TBytes;
begin
  Data := ConcatBytes(AnsiToBytes(PLabel), Seed);
  A := EccHMacBytes(Secret, Data, DigestType);
  Res := nil;
  repeat
    Res := ConcatBytes(Res, EccHMacBytes(Secret, ConcatBytes(A, Data), DigestType));
    A := EccHMacBytes(Secret, A, DigestType);
  until Length(Res) >= NeedLength;
  Result := Copy(Res, 0, NeedLength);
end;

function ExtractEccCurveDigest(SigAlg: Word; out CurveType: TCnEccCurveType; out
  DigestType: TCnEccSignDigestType): Boolean;
begin
  Result := True;
  case SigAlg of
    CN_TLS_SIGN_ALG_ECDSA_SECP256R1_SHA256:
      begin
        CurveType := ctSecp256r1;
        DigestType := esdtSHA256;
      end;
    CN_TLS_SIGN_ALG_ECDSA_SECP384R1_SHA384:
      begin
        CurveType := ctSecp384r1;
        DigestType := esdtSHA384;
      end;
    CN_TLS_SIGN_ALG_ECDSA_SECP521R1_SHA512:
      begin
        CurveType := ctSecp521r1;
        DigestType := esdtSHA512;
      end;
  else
    Result := False;
  end;
end;

function ResolveHostIPv4(const Host: string): string;
var
{$IFDEF MSWINDOWS}
  HE: PHostEnt;
  InA: PInAddr;
{$ELSE}
{$IFDEF FPC}
  H: PHostEnt;
  Addr: PAnsiChar;
{$ENDIF}{$ENDIF}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  HE := WinSock.gethostbyname(PAnsiChar(AnsiString(Host)));
  if HE <> nil then
  begin
    InA := PInAddr(HE^.h_addr_list^);
    if InA <> nil then
      Result := string(WinSock.inet_ntoa(InA^));
  end;
{$ELSE}
{$IFDEF FPC}
  H := c_gethostbyname(PAnsiChar(AnsiString(Host)));
    if H <> nil then
  begin
    Addr := H^.h_addr_list^;
    if Addr <> nil
    then
      Result := Format('%d.%d.%d.%d', [Byte(Addr[0]), Byte(Addr[1]),
    Byte(Addr[2]), Byte(Addr[3])]);
  end;
{$ENDIF}{$ENDIF}
end;

// 以下这个 Bad 始终跑不通，废弃
procedure TFormNetDecl.btnSSLClientBadClick(Sender: TObject);
const
  HOST: AnsiString = 'www.cnpack.org';
var
  SockAddress: TSockAddr;
  Buffer: array[0..4095] of Byte;
  TotalHandShake: TBytes;
  TotalHash: TBytes;
  Ciphers: TWords;
  H: PCnTLSRecordLayer;
  B: PCnTLSHandShakeHeader;
  C: PCnTLSHandShakeClientHello;
  SNI: PCnTLSHandShakeServerNameIndication;
  RandClient, RandServer: TBytes;
  BytesReceived: Integer;
  A: PCnTLSAlertPacket;
  SessionId: TBytes;
  CompressionMethod: TBytes;

  E: PCnTLSHandShakeExtensions;
  EI: PCnTLSHandShakeExtensionItem;
  ExtensionsStart: PByte;
  CurrentExtPtr: PByte;
  TotalHandshakeLen: Cardinal;
  ExtensionsTotalLen: Word;
  ExtType: PWord;
  ExtDataLen: PWord;
  ExtData, ExtTmpData: PByte;

  T: TBytes;
  W: TWords;
  I: Integer;
  S: PCnTLSHandShakeServerHello;

  CerBytes: Cardinal;
  Cer: PCnTLSHandShakeCertificate;
  CI: PCnTLSHandShakeCertificateItem;
  SK: PCnTLSHandShakeServerKeyExchange;
  SP: PCnTLSHandShakeSignedParams;

  SigVerified: Boolean;
  SignStream: TMemoryStream;
  SignValueStream: TMemoryStream;
  CurveType: TCnEccCurveType;
  DigestType: TCnEccSignDigestType;
  Ecc: TCnEcc;
  ServerSigBytes: TBytes;
  ServerCert: TCnCertificate;
  ServerCertBytes: TBytes;
  ServerKeyBytes: TBytes;

  EccPrivKey: TCnEccPrivateKey;
  EccPubKey: TCnEccPublicKey;
  PreMasterKey: TCnEccPublicKey;
  MasterKey: TBytes;

  CK: PCnTLSHandShakeClientKeyExchange;
  CC: PCnTLSChangeCipherSpecPacket;
  VerifyData: TBytes;
  F: PCnTLSHandShakeFinished;
begin
  FTlsClientSocket := CnNewSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FTlsClientSocket = INVALID_SOCKET then
    Exit;

  SockAddress.sin_family := AF_INET;
  SockAddress.sin_port := htons(StrToIntDef(edtTLSPort.Text, 443));
  SockAddress.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(edtTLSHost.Text)));

  if SOCKET_ERROR <> CnConnect(FTlsClientSocket, SockAddress, SizeOf(SockAddress)) then
  begin
    FillChar(Buffer, SizeOf(Buffer), 0);

    // TLS 层头
    H := PCnTLSRecordLayer(@Buffer[0]);
    H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H^.MajorVersion := 3;
    H^.MinorVersion := 3;  // TLS 1.2

    // 握手协议头
    B := PCnTLSHandShakeHeader(@H^.Body[0]);
    B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_CLIENT_HELLO;

    // 握手协议 ClientHello 包
    C := PCnTLSHandShakeClientHello(@B^.Content[0]);

    // 设置版本号 - TLS 1.2
    C^.ProtocolVersion := CN_TLS_SSL_VERSION_TLS_12;

    // 生成随机数
    SetLength(RandClient, SizeOf(C^.Random));
    CnRandomFillBytes2(@RandClient[0], Length(RandClient));
    Move(RandClient[0], C^.Random[0], SizeOf(C^.Random));

    // 生成 Session ID (32 字节)
    SetLength(SessionId, 32);
    CnRandomFillBytes2(@SessionId[0], 32);

    CnSetTLSHandShakeClientHelloSessionId(C, SessionId);

    // 填充 Ciphers - 使用 TLS 1.2 兼容的密码套件
    SetLength(Ciphers, 7);
    Ciphers[0] := CN_CIPHER_ECDHE_RSA_AES128_GCM_SHA256;
    Ciphers[1] := CN_CIPHER_ECDHE_RSA_AES256_GCM_SHA384;
    Ciphers[2] := CN_CIPHER_ECDHE_ECDSA_AES128_GCM_SHA256;
    Ciphers[3] := CN_CIPHER_ECDHE_ECDSA_AES256_GCM_SHA384;
    Ciphers[4] := CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305;
    Ciphers[5] := CN_CIPHER_AES128_GCM_SHA256;
    Ciphers[6] := CN_CIPHER_TLS_SM4_GCM_SM3;
    CnSetTLSHandShakeClientHelloCipherSuites(C, Ciphers);

    // 填充压缩类型
    SetLength(CompressionMethod, 1);
    CompressionMethod[0] := 0;
    CnSetTLSHandShakeClientHelloCompressionMethod(C, CompressionMethod);

    // 获取 Extensions 的起始位置
    ExtensionsStart := PByte(C);
    Inc(ExtensionsStart, SizeOf(Word));     // ProtocolVersion
    Inc(ExtensionsStart, 32);               // Random
    Inc(ExtensionsStart, 1);                // SessionLength
    Inc(ExtensionsStart, C^.SessionLength); // SessionId
    Inc(ExtensionsStart, SizeOf(Word));     // CipherSuitesLength
    Inc(ExtensionsStart, CnGetTLSHandShakeClientHelloCipherSuitesLength(C));
    Inc(ExtensionsStart, 1);                // CompressionMethodLength
    Inc(ExtensionsStart, CnGetTLSHandShakeClientHelloCompressionMethodLength(C));

    // 开始设置扩展内容
    E := PCnTLSHandShakeExtensions(ExtensionsStart);

    // 拿第一个 ExtensionItem，设置类型
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E);
    CnSetTLSHandShakeExtensionsExtensionType(EI, CN_TLS_EXTENSIONTYPE_SERVER_NAME);

    // 拿第一个 ExtensionItem 的数据，当成一个 SNI，写一个 Host，并设置 ExtensionItem 的数据长度
    SNI := CnGetTLSHandShakeExtensionsExtensionData(EI);
    CnSetTLSHandShakeExtensionsExtensionDataLength(EI, CnTLSHandShakeServerNameIndicationAddHost(SNI, HOST));

    // 拿第二个 ExtensionItem 的数据，当成一个 Supported Groups，写入四个支持的椭圆曲线类型
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
    SetLength(W, 4);
    W[0] := CN_TLS_NAMED_GROUP_X25519;
    W[1] := CN_TLS_NAMED_GROUP_SECP256R1;
    W[2] := CN_TLS_NAMED_GROUP_SECP384R1;
    W[3] := CN_TLS_NAMED_GROUP_SECP521R1;
    CnSetTLSHandShakeSupportedGroups(EI, W); // 同时设置了扩展数据类型和扩展数据长度

    // 拿第三个 ExtensionItem 的数据，当成一个 EC Point Formats，写入一个 Uncompressed
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
    CnSetTLSHandShakeECPointFormats(EI, CN_TLS_EC_POINT_FORMATS_UNCOMPRESSED);

    // 拿第四个 ExtensionItem 的数据，当成一个 Signature Algorithms，写入六个算法
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
    SetLength(W, 6);
    W[0] := CN_TLS_SIGN_ALG_ECDSA_SECP256R1_SHA256;
    W[1] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA256;
    W[2] := CN_TLS_SIGN_ALG_ECDSA_SECP384R1_SHA384;
    W[3] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA384;
    W[4] := CN_TLS_SIGN_ALG_ECDSA_SECP521R1_SHA512;
    W[5] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA512;
    CnSetTLSHandShakeSignatureAlgorithms(EI, W);

    // 总共四个，计算扩展数据总长度并写入，注意整个扩展长度还要加上 2 字节长度
    CnSetTLSHandShakeExtensionsExtensionLengthByItemCount(E, 4);

    // 还缺两个长度
    // 计算整个握手消息的内容长度
    TotalHandshakeLen :=
      SizeOf(Word) +                                                    // ProtocolVersion
      32 +                                                              // Random
      1 +                                                               // SessionLength
      C^.SessionLength +                                                // SessionId
      SizeOf(Word) +                                                    // CipherSuitesLength
      CnGetTLSHandShakeClientHelloCipherSuitesLength(C) +               // CipherSuites
      1 +                                                               // CompressionMethodLength
      CnGetTLSHandShakeClientHelloCompressionMethodLength(C) +          // CompressionMethod
      SizeOf(Word) +                                                    // ExtensionsLength
      CnGetTLSHandShakeExtensionsExtensionLength(E);                    // Extensions 内容

    // 设置握手头的内容长度
    CnSetTLSHandShakeHeaderContentLength(B, TotalHandshakeLen);

    // 设置 TLS Record Layer 的 Body 长度，1 + 3 表示 TCnTLSHandShakeHeader 的前三个字段 1 + 1 + 2
    CnSetTLSRecordLayerBodyLength(H, 1 + 3 + TotalHandshakeLen);

    // 发送 ClientHello 包，5 表示 TCnTLSRecordLayer 中的前四个字段：3 Byte + 1 Word
    if CnSend(FTlsClientSocket, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) <> SOCKET_ERROR then
    begin
      // 保存发送包，以备最后验证
      TotalHandShake := NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H));

      mmoSSL.Lines.Add('Sent ClientHello Packet, Size: ' +
        IntToStr(5 + CnGetTLSRecordLayerBodyLength(H)));

      mmoSSL.Lines.Add('');

      // 等一会儿接收回包
      Sleep(1000);

      FillChar(Buffer, SizeOf(Buffer), 0);
      BytesReceived := CnRecv(FTlsClientSocket, Buffer[0], Length(Buffer), 0);
      if BytesReceived > SizeOf(TCnTLSRecordLayer) then
      begin
        mmoSSL.Lines.Add(Format('SSL/TLS Get Response %d', [BytesReceived]));

        H := PCnTLSRecordLayer(@Buffer[0]);
        mmoSSL.Lines.Add(Format('TLSRecordLayer.ContentType %d', [H^.ContentType]));
        mmoSSL.Lines.Add(Format('TLSRecordLayer.MajorVersion %d', [H^.MajorVersion]));
        mmoSSL.Lines.Add(Format('TLSRecordLayer.MinorVersion %d', [H^.MinorVersion]));
        mmoSSL.Lines.Add(Format('TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(H)]));

        TotalHandshakeLen := 5 + CnGetTLSRecordLayerBodyLength(H);
        case H^.ContentType of
          CN_TLS_CONTENT_TYPE_ALERT:
            begin
              A := PCnTLSAlertPacket(@(H^.Body[0]));
              mmoSSL.Lines.Add(Format('TLSAlertPacket.AlertLevel %d', [A^.AlertLevel]));
              mmoSSL.Lines.Add(Format('TLSAlertPacket.AlertDescription %d', [A^.AlertDescription]));

              case A^.AlertDescription of
                40:
                  mmoSSL.Lines.Add('Error: Handshake Failure');
                47:
                  mmoSSL.Lines.Add('Error: Illegal Parameter');
                50:
                  mmoSSL.Lines.Add('Error: Decode Error');
                70:
                  mmoSSL.Lines.Add('Error: Protocol Version');
              else
                if A^.AlertLevel = 2 then
                  mmoSSL.Lines.Add('Fatal Error!')
                else
                  mmoSSL.Lines.Add('Warning.');
              end;
            end;
          CN_TLS_CONTENT_TYPE_HANDSHAKE:
            begin
              mmoSSL.Lines.Add('Received ServerHello!');
              B := PCnTLSHandShakeHeader(@(H^.Body[0]));
              mmoSSL.Lines.Add(Format('HandShakeType: %d', [B^.HandShakeType]));

              TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H))); // 保存收包

              // 解析 ServerHello 内容
              if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO then
              begin
                S := PCnTLSHandShakeServerHello(@B^.Content[0]);
                mmoSSL.Lines.Add('=== ServerHello Details ===');
                mmoSSL.Lines.Add(Format('Protocol Version: $%4.4x',
                    [S^.ProtocolVersion]));

                RandServer := NewBytesFromMemory(@S^.Random[0], SizeOf(S^.Random));
                mmoSSL.Lines.Add(Format('Random (32 Bytes): %s', [BytesToHex(RandServer)]));

                mmoSSL.Lines.Add(Format('Session ID Length: %d', [S^.SessionLength]));
                if S^.SessionLength > 0 then
                begin
                  T := CnGetTLSHandShakeServerHelloSessionId(S);
                  mmoSSL.Lines.Add(Format('Session ID: %s', [BytesToHex(T)]));
                end
                else
                  mmoSSL.Lines.Add('Session ID: (empty - no session resumption)');

                // 获取选择的 Cipher Suite（ServerHello 中只有一个）
                ExtData := @S^.SessionId[0];
                Inc(ExtData, S^.SessionLength);

                // ServerHello 中 Cipher Suite 是 2 字节，没长度了
                mmoSSL.Lines.Add(Format('Selected Cipher Suite: $%4.4x - %s',
                    [CnGetTLSHandShakeServerHelloCipherSuite(S), GetNameFromCipher(CnGetTLSHandShakeServerHelloCipherSuite(S))]));
                Inc(ExtData, SizeOf(Word));

                // Compression Method (1 字节)
                mmoSSL.Lines.Add(Format('Compression Method: %d', [CnGetTLSHandShakeServerHelloCompressionMethod(S)]));
                Inc(ExtData);

                // Extensions (如果有)
                // 计算当前位置距离 HandShake Header Content 起始的偏移
                // 如果还有剩余数据，则表示有 Extensions
                CurrentExtPtr := PByte(S);
                Inc(CurrentExtPtr,
                  SizeOf(Word) +          // ProtocolVersion
                  32 +                    // Random
                  1 +                     // SessionLength
                  S^.SessionLength +      // SessionId
                  SizeOf(Word) +          // Cipher Suite
                  1);                     // Compression Method

                if TCnIntAddress(ExtData) < TCnIntAddress(B) + 4 +
                  CnGetTLSHandShakeHeaderContentLength(B) then
                begin
                  E := CnGetTLSHandShakeServerHelloExtensions(S);
                  // 读取 Extensions Length
                  ExtensionsTotalLen := CnGetTLSHandShakeExtensionsExtensionLength(E);
                  Inc(ExtData, SizeOf(Word));
                  mmoSSL.Lines.Add(Format('Extensions Length: %d', [ExtensionsTotalLen]));

                  if ExtensionsTotalLen > 0 then
                  begin
                    mmoSSL.Lines.Add('=== Server Extensions ===');
                    CurrentExtPtr := ExtData;

                    while TCnIntAddress(CurrentExtPtr) < TCnIntAddress(ExtData)
                      + ExtensionsTotalLen do
                    begin
                      // Extension Type
                      ExtType := PWord(CurrentExtPtr);
                      ExtType^ := UInt16NetworkToHost(ExtType^);
                      Inc(CurrentExtPtr, SizeOf(Word));

                      // Extension Data Length
                      ExtDataLen := PWord(CurrentExtPtr);
                      ExtDataLen^ := UInt16NetworkToHost(ExtDataLen^);
                      Inc(CurrentExtPtr, SizeOf(Word));

                      mmoSSL.Lines.Add(Format('Extension Type: %d, Length: %d',
                        [ExtType^, ExtDataLen^]));

                      // 根据类型解析具体内容
                      case ExtType^ of
                        CN_TLS_EXTENSIONTYPE_SERVER_NAME:
                          mmoSSL.Lines.Add('  - Server Name Indication (SNI)');
                        CN_TLS_EXTENSIONTYPE_SUPPORTED_GROUPS:
                          begin
                            mmoSSL.Lines.Add('  - Supported Groups');
                            // 服务器一般不发送此扩展，但如果发送则解析
                          end;
                        CN_TLS_EXTENSIONTYPE_EC_POINT_FORMATS:
                          begin
                            mmoSSL.Lines.Add('  - EC Point Formats');
                            if ExtDataLen^ > 0 then
                            begin
                              ExtTmpData := CurrentExtPtr;
                              mmoSSL.Lines.Add(Format('    Formats Count: %d', [ExtTmpData
                                ^]));
                              Inc(ExtTmpData);
                              for I := 0 to Integer(PByte(TCnIntAddress(CurrentExtPtr))^) - 1 do
                              begin
                                case ExtTmpData^ of
                                  0:
                                    mmoSSL.Lines.Add('      - Uncompressed');
                                  1:
                                    mmoSSL.Lines.Add('      - ANSI X9.62 Compressed Prime');
                                  2:
                                    mmoSSL.Lines.Add('      - ANSI X9.62 Compressed Char2');
                                else
                                  mmoSSL.Lines.Add(Format('      - Unknown Format: %d',
                                    [ExtTmpData^]));
                                end;
                                Inc(ExtTmpData);
                              end;
                            end;
                          end;
                        CN_TLS_EXTENSIONTYPE_SESSION_TICKET:
                          mmoSSL.Lines.Add('  - Session Ticket');
                        CN_TLS_EXTENSIONTYPE_ENCRYPT_THEN_MAC:
                          mmoSSL.Lines.Add('  - Encrypt-then-MAC');
                        CN_TLS_EXTENSIONTYPE_EXTENDED_MASTER_SECRET:
                          mmoSSL.Lines.Add('  - Extended Master Secret');
                        CN_TLS_EXTENSIONTYPE_RENEGOTIATION_INFO:
                          begin
                            mmoSSL.Lines.Add('  - Renegotiation Info');
                            if ExtDataLen^ > 0 then
                            begin
                              ExtData := CurrentExtPtr;
                              mmoSSL.Lines.Add(Format('    Renegotiated Connection Length: %d',
                                [ExtData^]));
                            end;
                          end;
                        CN_TLS_EXTENSIONTYPE_SUPPORTED_VERSIONS:
                          begin
                            mmoSSL.Lines.Add('  - Supported Versions');
                            if ExtDataLen^ = 2 then
                            begin
                              ExtData := CurrentExtPtr;
                              mmoSSL.Lines.Add(Format('    Selected Version: $%4.4x',
                                  [UInt16NetworkToHost(PWord(ExtData)^)]));
                            end;
                          end;
                        CN_TLS_EXTENSIONTYPE_KEY_SHARE:
                          begin
                            mmoSSL.Lines.Add('  - Key Share (TLS 1.3)');
                            // TLS 1.3 的扩展，如果出现说明协商了 TLS 1.3
                          end;
                      else
                        mmoSSL.Lines.Add(Format('  - Unknown Extension Type: %d',
                          [ExtType^]));
                      end;

                      // 跳到下一个 Extension
                      Inc(CurrentExtPtr, ExtDataLen^);
                    end;
                  end;
                end
                else
                  mmoSSL.Lines.Add('No Extensions in ServerHello');

                mmoSSL.Lines.Add('=== End of ServerHello ===');
              end;
            end;
        end;

        SigVerified := False;
        // 还有第二个包
        if TotalHandshakeLen < BytesReceived then
        begin
          H := PCnTLSRecordLayer(@Buffer[TotalHandshakeLen]);
          mmoSSL.Lines.Add(Format('2 TLSRecordLayer.ContentType %d', [H^.ContentType]));
          mmoSSL.Lines.Add(Format('2 TLSRecordLayer.MajorVersion %d', [H^.MajorVersion]));
          mmoSSL.Lines.Add(Format('2 TLSRecordLayer.MinorVersion %d', [H^.MinorVersion]));
          mmoSSL.Lines.Add(Format('2 TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(H)]));

          TotalHandshakeLen := TotalHandshakeLen + 5 + CnGetTLSRecordLayerBodyLength(H);
        end
        else
          Exit;

        // 第二个包如果是握手包
        if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
        begin
          B := PCnTLSHandShakeHeader(@(H^.Body[0]));
          TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H))); // 保存收包

          mmoSSL.Lines.Add(Format('2 HandShakeType: %d', [B^.HandShakeType]));

          // 如果是服务器证书包
          if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE then
          begin
            Cer := PCnTLSHandShakeCertificate(@B^.Content[0]);
            mmoSSL.Lines.Add('=== Server Certificates ===');
            mmoSSL.Lines.Add('2 Certificate Total Cert List Bytes: ' + IntToStr(CnGetTLSHandShakeCertificateListLength(Cer)));

            // 第一个证书
            CerBytes := 0;
            CI := CnGetTLSHandShakeCertificateItem(Cer);
            Inc(CerBytes, 3 + CnGetTLSHandShakeCertificateItemCertificateLength(CI));

            mmoSSL.Lines.Add(Format('2 Certificate #1 Cert Bytes %d', [CnGetTLSHandShakeCertificateItemCertificateLength(CI)]));
            ServerCertBytes := CnGetTLSHandShakeCertificateItemCertificate(CI);
            mmoSSL.Lines.Add(BytesToHex(ServerCertBytes));

            ServerCert := TCnCertificate.Create;
            try
              if CnCALoadCertificateFromBytes(ServerCertBytes, ServerCert) then
                mmoSSL.Lines.Add('2 Certificate #1 Parsing: ' + ServerCert.ToString)
              else
                mmoSSL.Lines.Add('2 Certificate #1 Parsing Fail.');
            finally
              ServerCert.Free;
            end;

            // 第二个证书
            if CerBytes < CnGetTLSHandShakeCertificateListLength(Cer) then
            begin
              CI := CnGetTLSHandShakeCertificateItem(Cer, CI);
              Inc(CerBytes, 3 + CnGetTLSHandShakeCertificateItemCertificateLength(CI));

              mmoSSL.Lines.Add(Format('2 Certificate #2 Cert Bytes %d', [CnGetTLSHandShakeCertificateItemCertificateLength(CI)]));
              mmoSSL.Lines.Add(BytesToHex(CnGetTLSHandShakeCertificateItemCertificate(CI)));

              ServerCert := TCnCertificate.Create;
              try
                if CnCALoadCertificateFromBytes(CnGetTLSHandShakeCertificateItemCertificate(CI), ServerCert) then
                  mmoSSL.Lines.Add('2 Certificate #2 Parsing: ' + ServerCert.ToString)
                else
                  mmoSSL.Lines.Add('2 Certificate #2 Parsing Fail.');
              finally
                ServerCert.Free;
              end;

              // 第三个证书
              if CerBytes < CnGetTLSHandShakeCertificateListLength(Cer) then
              begin
                CI := CnGetTLSHandShakeCertificateItem(Cer, CI);
                Inc(CerBytes, 3 + CnGetTLSHandShakeCertificateItemCertificateLength(CI));

                mmoSSL.Lines.Add(Format('2 Certificate #3 Cert Bytes %d', [CnGetTLSHandShakeCertificateItemCertificateLength(CI)]));
                mmoSSL.Lines.Add(BytesToHex(CnGetTLSHandShakeCertificateItemCertificate(CI)));

                ServerCert := TCnCertificate.Create;
                try
                  if CnCALoadCertificateFromBytes(CnGetTLSHandShakeCertificateItemCertificate(CI), ServerCert) then
                    mmoSSL.Lines.Add('2 Certificate #3 Parsing: ' + ServerCert.ToString)
                  else
                    mmoSSL.Lines.Add('2 Certificate #3 Parsing Fail.');
                finally
                  ServerCert.Free;
                end;

                if CerBytes < CnGetTLSHandShakeCertificateListLength(Cer) then
                  mmoSSL.Lines.Add('2 Certificate Other Certs Ignored.');
              end;
            end;
            mmoSSL.Lines.Add('=== Server Certificates End ===');
          end;
        end;

        // 还有第三个包
        if TotalHandshakeLen < BytesReceived then
        begin
          H := PCnTLSRecordLayer(@Buffer[TotalHandshakeLen]);
          mmoSSL.Lines.Add(Format('3 TLSRecordLayer.ContentType %d', [H^.ContentType]));
          mmoSSL.Lines.Add(Format('3 TLSRecordLayer.MajorVersion %d', [H^.MajorVersion]));
          mmoSSL.Lines.Add(Format('3 TLSRecordLayer.MinorVersion %d', [H^.MinorVersion]));
          mmoSSL.Lines.Add(Format('3 TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(H)]));

          TotalHandshakeLen := TotalHandshakeLen + 5 + CnGetTLSRecordLayerBodyLength(H);
        end
        else
          Exit;

        // 第三个包如果是握手包
        if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
        begin
          B := PCnTLSHandShakeHeader(@(H^.Body[0]));
          TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H))); // 保存收包

          mmoSSL.Lines.Add(Format('3 HandShakeType: %d', [B^.HandShakeType]));

          // 如果是服务器密钥交换包
          if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED then
          begin
            SK := PCnTLSHandShakeServerKeyExchange(@B^.Content[0]);
            mmoSSL.Lines.Add('=== Server Key Exchange ===');
            mmoSSL.Lines.Add('3 ECCurveType: ' + IntToStr(SK^.ECCurveType));
            mmoSSL.Lines.Add('3 Named Curve: ' + IntToStr(CnGetTLSHandShakeServerKeyExchangeNamedCurve(SK)));
            mmoSSL.Lines.Add('3 ECPointLength: ' + IntToStr(SK^.ECPointLength));
            ServerKeyBytes := CnGetTLSHandShakeServerKeyExchangeECPoint(SK);
            mmoSSL.Lines.Add('3 ECCPoint: ' + BytesToHex(ServerKeyBytes));

            SP := CnGetTLSHandShakeSignedParamsFromServerKeyExchange(SK);
            mmoSSL.Lines.Add(Format('3 Sign Alg: %4.4x', [CnGetTLSHandShakeSignedParamsSignatureAlgorithm(SP)])); // 服务器选择的签名算法
            mmoSSL.Lines.Add('3 SignLength: ' + IntToStr(CnGetTLSHandShakeSignedParamsSignatureLength(SP)));
            ServerSigBytes := CnGetTLSHandShakeSignedParamsSignature(SP);
            mmoSSL.Lines.Add('3 Signature: ' + BytesToHex(ServerSigBytes));
            mmoSSL.Lines.Add('=== Server Key Exchange End ===');

            // 验证签名，先拼凑待签名数据：普通椭圆曲线是
            // ClientHello.random + ServerHello.random + ServerKeyExchange.params 的指定类型杂凑结果
            // 25519/448 则是ClientHello.random + ServerHello.random + ServerKeyExchange.params 没杂凑，让 25519/448 内部杂凑

            if ExtractEccCurveDigest(CnGetTLSHandShakeSignedParamsSignatureAlgorithm(SP), CurveType, DigestType) then
            begin
              // 我们先拼凑数据：
              Ecc := nil;
              SignStream := nil;
              SignValueStream := nil;
              ServerCert := nil;

              try
                SignStream := TMemoryStream.Create;
                WriteBytesToStream(RandClient, SignStream);
                WriteBytesToStream(RandServer, SignStream);

                // 3.1 写入 curve_type (1 byte)
                SignStream.Write(SK^.ECCurveType, 1);
                // 3.2 写入 named_curve (2 bytes, 原始也就是网络字节序)
                SignStream.Write(SK^.NamedCurve, 2);
                // 3.3 写入 public_key_length (1 byte)
                SignStream.Write(SK^.ECPointLength, 1);
                // 3.4 写入 public_key (ECPointLength bytes)
                SignStream.Write(SK^.ECPoint[0], SK^.ECPointLength);
                SignStream.Position := 0;

                ServerCert := TCnCertificate.Create;
                if CnCALoadCertificateFromBytes(ServerCertBytes, ServerCert) and not ServerCert.IsRSA then
                begin
                  Ecc := TCnEcc.Create(CurveType); // 使用服务器指定的 Cipher，而不是证书中的类型，虽然两者可能相等
                  SignValueStream := TMemoryStream.Create;
                  WriteBytesToStream(ServerSigBytes, SignValueStream);
                  SignValueStream.Position := 0;

                  // 数据是杂凑过的 SignStream，曲线在 Ecc 里，公钥在 ServerCert 的公钥里，签名在 SignValueStream 里
                  if CnEccVerifyStream(SignStream, SignValueStream, Ecc,
                    ServerCert.BasicCertificate.SubjectEccPublicKey, DigestType) then
                  begin
                    mmoSSL.Lines.Add('*** Signature Verify OK ***'); // 使用服务器指定的 Cipher 中的 DigestType，而不是证书中的类型，虽然两者可能相等
                    SigVerified := True;
                  end
                  else
                    mmoSSL.Lines.Add('*** Signature Verify Fail ***');
                end;
              finally
                SignValueStream.Free;
                ServerCert.Free;
                SignStream.Free;
                Ecc.Free;
              end;
            end;
          end;
        end;

        // 还有第四个包
        if TotalHandshakeLen < BytesReceived then
        begin
          H := PCnTLSRecordLayer(@Buffer[TotalHandshakeLen]);
          mmoSSL.Lines.Add(Format('4 TLSRecordLayer.ContentType %d', [H^.ContentType]));
          mmoSSL.Lines.Add(Format('4 TLSRecordLayer.MajorVersion %d', [H^.MajorVersion]));
          mmoSSL.Lines.Add(Format('4 TLSRecordLayer.MinorVersion %d', [H^.MinorVersion]));
          mmoSSL.Lines.Add(Format('4 TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(H)]));

          TotalHandshakeLen := TotalHandshakeLen + 5 + CnGetTLSRecordLayerBodyLength(H);
        end
        else
          Exit;

        // 第四个包如果是握手包
        if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
        begin
          B := PCnTLSHandShakeHeader(@(H^.Body[0]));
          TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H))); // 保存收包
          mmoSSL.Lines.Add(Format('4 HandShakeType: %d', [B^.HandShakeType]));

          // 如果是 ServerHello 结束，才完成这一轮接收
          if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED then
          begin
            mmoSSL.Lines.Add('=== Server Hello Done ===');
          end;
        end;

        if SigVerified then
        begin
          // 签名验证通过后，开始密钥协商
          Ecc := nil;
          EccPrivKey := nil;
          EccPubKey := nil;
          PreMasterKey := nil;

          try
            Ecc := TCnEcc.Create(CurveType);
            // 生成一对随机公私钥

            EccPrivKey := TCnEccPrivateKey.Create;
            EccPubKey := TCnEccPublicKey.Create;

            // 先根据对方发来的 Point 计算 PreMasterKey
            Ecc.GenerateKey(EccPrivKey);
            PreMasterKey := TCnEccPublicKey.Create;

            // EccPubKey 取对方发来的 Point
            EccPubKey.SetBytes(ServerKeyBytes);
            CnEccDiffieHellmanComputeKey(Ecc, EccPrivKey, EccPubKey, PreMasterKey);

            // 保留 PreMasterKey，握手基本 OK
            mmoSSL.Lines.Add('5 Client Key Exchange. Got PreMaster Key: ' + PreMasterKey.ToString);

            // 再计算 MasterKey！
            MasterKey := PseudoRandomFunc(PreMasterKey.ToBytes(Ecc.BytesCount), 'master secret',
              ConcatBytes(RandClient, RandServer), DigestType, 48);
            mmoSSL.Lines.Add('5 Client Key Exchange. Got Master Key: ' + BytesToHex(MasterKey));

            // 复用 EccPubKey，计算新的 EccPubKey 作为本次中间结果
            CnEccDiffieHellmanGenerateOutKey(Ecc, EccPrivKey, EccPubKey);

            // 把 EccPubKey 发出去供对方计算
            // TLS 层头
            H := PCnTLSRecordLayer(@Buffer[0]);
            H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
            H^.MajorVersion := 3;
            H^.MinorVersion := 3;  // TLS 1.2

            // 握手协议头
            B := PCnTLSHandShakeHeader(@H^.Body[0]);
            B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_CLIENT_KEY_EXCHANGE_RESERVED;

            // Client Key Exchange 包头
            CK := PCnTLSHandShakeClientKeyExchange(@B^.Content[0]);
            T := EccPubKey.ToBytes(Ecc.BytesCount);
            CnSetTLSHandShakeClientKeyExchangeECPoint(CK, T);

            // 计算整个握手消息的内容长度
            TotalHandshakeLen := Length(T);
            // 设置握手头的内容长度
            CnSetTLSHandShakeHeaderContentLength(B, TotalHandshakeLen);
            // 设置 TLS Record Layer 的 Body 长度，1 + 3 表示 TCnTLSHandShakeHeader 的前三个字段 1 + 1 + 2
            CnSetTLSRecordLayerBodyLength(H, 1 + 3 + TotalHandshakeLen);

            // 发送 Client Key Exchange 包，5 表示 TCnTLSRecordLayer 中的前四个字段：3 Byte + 1 Word
            if CnSend(FTlsClientSocket, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) <> SOCKET_ERROR then
            begin
              mmoSSL.Lines.Add('Sent Client Key Exchange Packet, Size: ' +
                IntToStr(5 + CnGetTLSRecordLayerBodyLength(H)));

              // 保存发包
              TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H)));
            end;

            // 发 ChangeCipherSpec
            // TLS 层头
            H := PCnTLSRecordLayer(@Buffer[0]);
            H^.ContentType := CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC;
            H^.MajorVersion := 3;
            H^.MinorVersion := 3;  // TLS 1.2

            // ChangeCipherSpec 协议头
            CC := PCnTLSChangeCipherSpecPacket(@H^.Body[0]);
            CC.Content := CN_TLS_CHANGE_CIPHER_SPEC;

            // 设置 TLS Record Layer 的 Body 长度，就 ChangeCipherSpec 的一个字节
            CnSetTLSRecordLayerBodyLength(H, 1);

            // 发送 ChangeCipherSpec 包，5 表示 TCnTLSRecordLayer 中的前四个字段：3 Byte + 1 Word
            if CnSend(FTlsClientSocket, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) <> SOCKET_ERROR then
            begin
              mmoSSL.Lines.Add('Sent Change Cipher Spec Packet, Size: ' +
                IntToStr(5 + CnGetTLSRecordLayerBodyLength(H)));

              // 注意这个包无需保存
            end;

            // 计算并发送 Finished 包，先计算所有握手包的杂凑值
            TotalHash := EccDigestBytes(TotalHandShake, DigestType);
            VerifyData := PseudoRandomFunc(MasterKey, 'client finished', TotalHash, DigestType, 12);

            // TLS 层头
            H := PCnTLSRecordLayer(@Buffer[0]);
            H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
            H^.MajorVersion := 3;
            H^.MinorVersion := 3;  // TLS 1.2

            // 握手协议头
            B := PCnTLSHandShakeHeader(@H^.Body[0]);
            B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_FINISHED;

            F := PCnTLSHandShakeFinished(@B^.Content[0]);
            CnSetTLSTLSHandShakeFinishedVerifyData(F, VerifyData);

            // 设置握手头的内容长度，就 VerifyData 的 12 字节
            CnSetTLSHandShakeHeaderContentLength(B, 12);

            // 设置 TLS Record Layer 的 Body 长度
            CnSetTLSRecordLayerBodyLength(H, 1 + 3 + CnGetTLSHandShakeHeaderContentLength(B));

            // 发送 Finished 包，5 表示 TCnTLSRecordLayer 中的前四个字段：3 Byte + 1 Word
            if CnSend(FTlsClientSocket, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) <> SOCKET_ERROR then
            begin
              mmoSSL.Lines.Add('Sent Finished Packet, Size: ' +
                IntToStr(5 + CnGetTLSRecordLayerBodyLength(H)));

              mmoSSL.Lines.Add('');
              // 等一会儿接收回包，但是目前收不到！！！
              Sleep(1000);

              FillChar(Buffer, SizeOf(Buffer), 0);
              BytesReceived := CnRecv(FTlsClientSocket, Buffer[0], Length(Buffer), 0);
              if BytesReceived > SizeOf(TCnTLSRecordLayer) then
              begin
                mmoSSL.Lines.Add(Format('SSL/TLS Get Response %d', [BytesReceived]));

                H := PCnTLSRecordLayer(@Buffer[0]);
                mmoSSL.Lines.Add(Format('TLSRecordLayer.ContentType %d', [H^.ContentType]));
                mmoSSL.Lines.Add(Format('TLSRecordLayer.MajorVersion %d', [H^.MajorVersion]));
                mmoSSL.Lines.Add(Format('TLSRecordLayer.MinorVersion %d', [H^.MinorVersion]));
                mmoSSL.Lines.Add(Format('TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(H)]));

                TotalHandshakeLen := 5 + CnGetTLSRecordLayerBodyLength(H);
              end;


            end;
          finally
            PreMasterKey.Free;
            EccPubKey.Free;
            EccPrivKey.Free;
            Ecc.Free;
          end;
        end;
      end
      else
        mmoSSL.Lines.Add('Receive Failed or NO Data');
    end
    else
      mmoSSL.Lines.Add('Send Failed');
  end
  else
    mmoSSL.Lines.Add('Connect Failed');

  CnCloseSocket(FTlsClientSocket);
end;

// ============================== SSL Client ===================================

function TFormNetDecl.SSLHandShake12(const Host: AnsiString; Port: Word): Boolean;
var
  Sock: TSocket;
  SockAddress: CnSocket.TSockAddr;
  Buffer: array[0..8191] of Byte;
  H: PCnTLSRecordLayer;
  B: PCnTLSHandShakeHeader;
  C: PCnTLSHandShakeClientHello;
  SNI: PCnTLSHandShakeServerNameIndication;
  E: PCnTLSHandShakeExtensions;
  EI: PCnTLSHandShakeExtensionItem;
  S: PCnTLSHandShakeServerHello;
  Cer: PCnTLSHandShakeCertificate;
  CI: PCnTLSHandShakeCertificateItem;
  SK: PCnTLSHandShakeServerKeyExchange;
  SP: PCnTLSHandShakeSignedParams;
  CK: PCnTLSHandShakeClientKeyExchange;
  CC: PCnTLSChangeCipherSpecPacket;
  F: PCnTLSHandShakeFinished;
  ServerHelloExt: PCnTLSHandShakeExtensions;
  ExtItem2: PCnTLSHandShakeExtensionItem;
  RandClient, RandServer: TBytes;
  SessionId: TBytes;
  CompressionMethod: TBytes;
  Ciphers: TWords;
  Rs: TCnFDSet;
  TotalHandShake: TBytes;
  BytesReceived: Integer;
  TotalConsumed: Integer;
  CurrentPtr: PByte;
  CurveType: TCnEccCurveType;
  DigestType: TCnEccSignDigestType;
  ServerCertBytes: TBytes;
  ServerKeyBytes: TBytes;
  Ecc: TCnEcc;
  EccPrivKey: TCnEccPrivateKey;
  EccPubKey: TCnEccPublicKey;
  PreMasterKey: TCnEccPublicKey;
  PreMasterX: TCnBigNumber;
  PreMasterBytes: TBytes;
  MasterKey: TBytes;
  SelectedCipher: Word;
  ClientWriteKey, ServerWriteKey: TBytes;
  ClientFixedIV, ServerFixedIV: TBytes;
  KeyBlock: TBytes;
  KeyLen, IvLen: Integer;
  ClientSeq, ServerSeq: TUInt64;
  CipherIsSM4GCM: Boolean;
  CipherIsChaCha20Poly1305: Boolean;
  VerifyData: TBytes;
  PlainFinished: TBytes;
  AAD: TBytes;
  ExplicitNonce: TBytes;
  EnCipher: TBytes;
  Tag: TCnGCM128Tag;
  ServerFinTag: TCnGCM128Tag;
  ServerEn: TBytes;
  ServerPlain: TBytes;
  RecBodyLen: Integer;
  Ok: Boolean;
  IpStr: string;
{$IFNDEF MSWINDOWS}
  HE: THostEntry;
{$ENDIF}
  AADFix: array[0..12] of Byte;
  AADFix2: array[0..12] of Byte;
  SeqBytes: array[0..7] of Byte;
  I: Integer;
  EmsNegotiated: Boolean;
  Lvl, Desc: Integer;
  ClientRecordCount, ServerRecordCount: TUInt64;
  Req: TBytes;
  IVNonce: TBytes;
  TmpStr: AnsiString;
  GotSH, GotCert, GotSKE, GotSHD: Boolean;
begin
  Result := False;
  EmsNegotiated := False;
  Sock := CnNewSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if Sock = INVALID_SOCKET then
  begin
    MySSLLog('Create socket failed');
    Exit;
  end;
  IpStr := '';
  if (Host <> '') and (Host[1] in ['0'..'9']) then
    IpStr := string(Host);
  if IpStr = '' then
  begin
  {$IFNDEF MSWINDOWS}
    if NetDB.GetHostByName(string(Host), HE) then
      IpStr := HostAddrToStr(HE.Addr);
  {$ENDIF}
    if IpStr = '' then
      IpStr := ResolveHostIPv4(string(Host));
  end;
  if IpStr = '' then
  begin
    MySSLLog('DNS resolve failed: ' + string(Host));
    CnCloseSocket(Sock);
    Exit;
  end;
  SockAddress.sin_family := AF_INET;
  SockAddress.sin_port := htons(Port);
{$IFDEF MSWINDOWS}
  SockAddress.sin_addr.s_addr := WinSock.inet_addr(PAnsiChar(AnsiString(IpStr)));
    {$ELSE}
  SockAddress.sin_addr := StrToHostAddr(IpStr);
{$ENDIF}
  if SOCKET_ERROR <> CnConnect(Sock, SockAddress, SizeOf(SockAddress)) then
  begin
    MySSLLog(Format('Connect %s:%d', [IpStr, Port]));
    ClientSeq := 0;
    ServerSeq := 0;
    ClientRecordCount := 0;
    ServerRecordCount := 0;
    FillChar(Buffer, SizeOf(Buffer), 0);
    H := PCnTLSRecordLayer(@Buffer[0]);
    H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
    H^.MajorVersion := 3;
    H^.MinorVersion := 3;
    B := PCnTLSHandShakeHeader(@H^.Body[0]);
    B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_CLIENT_HELLO;
    C := PCnTLSHandShakeClientHello(@B^.Content[0]);
    C^.ProtocolVersion := CN_TLS_SSL_VERSION_TLS_12;
    SetLength(RandClient, SizeOf(C^.Random));
    CnRandomFillBytes2(@RandClient[0], Length(RandClient));
    Move(RandClient[0], C^.Random[0], SizeOf(C^.Random));
    SetLength(SessionId, 32);
    CnRandomFillBytes2(@SessionId[0], 32);
    CnSetTLSHandShakeClientHelloSessionId(C, SessionId);
    SetLength(Ciphers, 8);
    Ciphers[0] := CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305;
    Ciphers[1] := CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305;
    Ciphers[2] := CN_CIPHER_ECDHE_RSA_AES128_GCM_SHA256;
    Ciphers[3] := CN_CIPHER_ECDHE_ECDSA_AES128_GCM_SHA256;
    Ciphers[4] := CN_CIPHER_ECDHE_RSA_AES256_GCM_SHA384;
    Ciphers[5] := CN_CIPHER_ECDHE_ECDSA_AES256_GCM_SHA384;
    Ciphers[6] := CN_CIPHER_AES128_GCM_SHA256;
    Ciphers[7] := CN_CIPHER_AES256_GCM_SHA384;
    CnSetTLSHandShakeClientHelloCipherSuites(C, Ciphers);
    SetLength(CompressionMethod, 1);
    CompressionMethod[0] := 0;
    CnSetTLSHandShakeClientHelloCompressionMethod(C, CompressionMethod);
    E := PCnTLSHandShakeExtensions(PAnsiChar(C) + SizeOf(Word) + 32 + 1 + C^.SessionLength
      + SizeOf(Word) + CnGetTLSHandShakeClientHelloCipherSuitesLength(C) + 1 +
      CnGetTLSHandShakeClientHelloCompressionMethodLength(C));
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E);
    CnSetTLSHandShakeExtensionsExtensionType(EI, CN_TLS_EXTENSIONTYPE_SERVER_NAME);
    SNI := CnGetTLSHandShakeExtensionsExtensionData(EI);
    CnSetTLSHandShakeExtensionsExtensionDataLength(EI,
      CnTLSHandShakeServerNameIndicationAddHost(SNI, Host));
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
    SetLength(Ciphers, 4);
    Ciphers[0] := CN_TLS_NAMED_GROUP_X25519;
    Ciphers[1] := CN_TLS_NAMED_GROUP_SECP256R1;
    Ciphers[2] := CN_TLS_NAMED_GROUP_SECP384R1;
    Ciphers[3] := CN_TLS_NAMED_GROUP_SECP521R1;
    CnSetTLSHandShakeSupportedGroups(EI, Ciphers);
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
    CnSetTLSHandShakeECPointFormats(EI, CN_TLS_EC_POINT_FORMATS_UNCOMPRESSED);
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
    SetLength(Ciphers, 6);
    Ciphers[0] := CN_TLS_SIGN_ALG_ECDSA_SECP256R1_SHA256;
    Ciphers[1] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA256;
    Ciphers[2] := CN_TLS_SIGN_ALG_ECDSA_SECP384R1_SHA384;
    Ciphers[3] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA384;
    Ciphers[4] := CN_TLS_SIGN_ALG_ECDSA_SECP521R1_SHA512;
    Ciphers[5] := CN_TLS_SIGN_ALG_RSA_PKCS1_SHA512;
    CnSetTLSHandShakeSignatureAlgorithms(EI, Ciphers);
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
    CnSetTLSHandShakeExtensionsExtensionType(EI,
      CN_TLS_EXTENSIONTYPE_EXTENDED_MASTER_SECRET);
    CnSetTLSHandShakeExtensionsExtensionDataLength(EI, 0);
    EI := CnGetTLSHandShakeExtensionsExtensionItem(E, EI);
    CnSetTLSHandShakeExtensionsExtensionType(EI,
      CN_TLS_EXTENSIONTYPE_APPLICATION_LAYER_PROTOCOL_NEGOTIATION);
    SetLength(Ciphers, 0);
    // Build ALPN: ProtocolNameList = 2-byte length + entries
    // entries: 'h2' and 'http/1.1'
    // total entries length = (1+2) + (1+8) = 12
    SetLength(CompressionMethod, 2 + (1 + 2) + (1 + 8));
    CompressionMethod[0] := 0;
    CompressionMethod[1] := 12;
    CompressionMethod[2] := 2;
    CompressionMethod[3] := Ord('h');
    CompressionMethod[4] := Ord('2');
    CompressionMethod[5] := 8;
    CompressionMethod[6] := Ord('h');
    CompressionMethod[7] := Ord('t');
    CompressionMethod[8] := Ord('t');
    CompressionMethod[9] := Ord('p');
    CompressionMethod[10] := Ord('/');
    CompressionMethod[11] := Ord('1');
    CompressionMethod[12] := Ord('.');
    CompressionMethod[13] := Ord('1');
    CnSetTLSHandShakeExtensionsExtensionData(EI, CompressionMethod);
    CnSetTLSHandShakeExtensionsExtensionLengthByItemCount(E, 6);
    CnSetTLSHandShakeHeaderContentLength(B,
      SizeOf(Word) + 32 + 1 + C^.SessionLength + SizeOf(Word) + CnGetTLSHandShakeClientHelloCipherSuitesLength(C) + 1 + CnGetTLSHandShakeClientHelloCompressionMethodLength(C) + SizeOf(Word) + CnGetTLSHandShakeExtensionsExtensionLength(E));
    CnSetTLSRecordLayerBodyLength(H, 1 + 3 + CnGetTLSHandShakeHeaderContentLength(B));
    if CnSend(Sock, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
    begin
      MySSLLog('Send ClientHello failed');
      CnCloseSocket(Sock);
      Exit;
    end;
    MySSLLog('Sent ClientHello, Size: ' + IntToStr(5 + CnGetTLSRecordLayerBodyLength(H)));
    Inc(ClientSeq);
    Inc(ClientRecordCount);
    Inc(ClientSeq);
    TotalHandShake := NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H));
    MySSLLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType,
      CnGetTLSRecordLayerBodyLength(H)]));
    FillChar(Buffer, SizeOf(Buffer), 0);
    BytesReceived := CnRecv(Sock, Buffer[0], Length(Buffer), 0);
    if BytesReceived <= SizeOf(TCnTLSRecordLayer) then
    begin
      MySSLLog('Receive response failed or no data');
      CnCloseSocket(Sock);
      Exit;
    end;
    MySSLLog(Format('SSL/TLS Get Response %d', [BytesReceived]));
    TotalConsumed := 0;
    SelectedCipher := 0;
    GotSH := False;
    GotCert := False;
    GotSKE := False;
    GotSHD := False;
    while TotalConsumed < BytesReceived do
    begin
      CurrentPtr := @Buffer[TotalConsumed];
      H := PCnTLSRecordLayer(CurrentPtr);
      MySSLLog(Format('TLSRecordLayer.ContentType %d', [H^.ContentType]));
      MySSLLog(Format('TLSRecordLayer.MajorVersion %d', [H^.MajorVersion]));
      MySSLLog(Format('TLSRecordLayer.MinorVersion %d', [H^.MinorVersion]));
      MySSLLog(Format('TLSRecordLayer.BodyLength %d', [CnGetTLSRecordLayerBodyLength(H)]));
      Inc(TotalConsumed, 5 + CnGetTLSRecordLayerBodyLength(H));
      Inc(ServerSeq);
      if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
      begin
        B := PCnTLSHandShakeHeader(@H^.Body[0]);
        TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B,
          CnGetTLSRecordLayerBodyLength(H)));
        MySSLLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType,
          CnGetTLSRecordLayerBodyLength(H)]));
        if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO then
        begin
          GotSH := True;
          S := PCnTLSHandShakeServerHello(@B^.Content[0]);
          RandServer := NewBytesFromMemory(@S^.Random[0], SizeOf(S^.Random));
          SelectedCipher := CnGetTLSHandShakeServerHelloCipherSuite(S);
          MySSLLog(Format('ServerHello CipherSuite 0x%.4x', [SelectedCipher]));
          ServerHelloExt := CnGetTLSHandShakeServerHelloExtensions(B);
          if ServerHelloExt <> nil then
          begin
            try
              ExtItem2 := CnGetTLSHandShakeExtensionsExtensionItem(ServerHelloExt);
              while ExtItem2 <> nil do
              begin
                if CnGetTLSHandShakeExtensionsExtensionType(ExtItem2) =
                  CN_TLS_EXTENSIONTYPE_EXTENDED_MASTER_SECRET then
                  EmsNegotiated := True;
                ExtItem2 := CnGetTLSHandShakeExtensionsExtensionItem(ServerHelloExt,
                  ExtItem2);
              end;
            except
              EmsNegotiated := False;
            end;
          end;
          MySSLLog('EMS Negotiated: ' + BoolToStr(EmsNegotiated, True));
        end
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE then
        begin
          GotCert := True;
          Cer := PCnTLSHandShakeCertificate(@B^.Content[0]);
          CI := CnGetTLSHandShakeCertificateItem(Cer);
          ServerCertBytes := CnGetTLSHandShakeCertificateItemCertificate(CI);
          MySSLLog(Format('Certificate Length %d', [Length(ServerCertBytes)]));
        end
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED then
        begin
          GotSKE := True;
          SK := PCnTLSHandShakeServerKeyExchange(@B^.Content[0]);
          ServerKeyBytes := CnGetTLSHandShakeServerKeyExchangeECPoint(SK);
        // set curve by named group from ServerKeyExchange
          case CnGetTLSHandShakeServerKeyExchangeNamedCurve(SK) of
            CN_TLS_NAMED_GROUP_SECP256R1:
              CurveType := ctSecp256r1;
            CN_TLS_NAMED_GROUP_SECP384R1:
              CurveType := ctSecp384r1;
            CN_TLS_NAMED_GROUP_SECP521R1:
              CurveType := ctSecp521r1;
          else
            CurveType := ctSecp256r1;
          end;
          MySSLLog(Format('ServerKeyExchange ECPoint Length %d', [Length(ServerKeyBytes)]));
        end
        else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED then
        begin
          GotSHD := True;
          MySSLLog('ServerHelloDone');
        end;
      end
    end;
    // Keep receiving until we have full SH, Certificate, SKE, SHD
    while not (GotSH and GotCert and GotSKE and GotSHD) do
    begin
      CnFDZero(Rs);
      CnFDSet(Sock, Rs);
      if CnSelect(Sock + 1, @Rs, nil, nil, nil) <= 0 then
        Break;
      FillChar(Buffer, SizeOf(Buffer), 0);
      BytesReceived := CnRecv(Sock, Buffer[0], Length(Buffer), 0);
      if BytesReceived <= SizeOf(TCnTLSRecordLayer) then
        Break;
      MySSLLog(Format('SSL/TLS Get Response %d', [BytesReceived]));
      TotalConsumed := 0;
      while TotalConsumed < BytesReceived do
      begin
        CurrentPtr := @Buffer[TotalConsumed];
        H := PCnTLSRecordLayer(CurrentPtr);
        RecBodyLen := CnGetTLSRecordLayerBodyLength(H);
        if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
        begin
          B := PCnTLSHandShakeHeader(@H^.Body[0]);
          TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B, CnGetTLSRecordLayerBodyLength(H)));
          MySSLLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType, CnGetTLSRecordLayerBodyLength(H)]));
          if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO then
            GotSH := True
          else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE_RESERVED then
          begin
            GotSKE := True;
            SK := PCnTLSHandShakeServerKeyExchange(@B^.Content[0]);
            ServerKeyBytes := CnGetTLSHandShakeServerKeyExchangeECPoint(SK);
            case CnGetTLSHandShakeServerKeyExchangeNamedCurve(SK) of
              CN_TLS_NAMED_GROUP_SECP256R1: CurveType := ctSecp256r1;
              CN_TLS_NAMED_GROUP_SECP384R1: CurveType := ctSecp384r1;
              CN_TLS_NAMED_GROUP_SECP521R1: CurveType := ctSecp521r1;
            else
              CurveType := ctSecp256r1;
            end;
            MySSLLog(Format('ServerKeyExchange ECPoint Length %d', [Length(ServerKeyBytes)]));
          end
          else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE_RESERVED then
            GotSHD := True
          else if B^.HandShakeType = CN_TLS_HANDSHAKE_TYPE_CERTIFICATE then
            GotCert := True;
        end;
        Inc(TotalConsumed, 5 + RecBodyLen);
        Inc(ServerSeq);
      end;
    end;
    Ecc := nil;
    EccPrivKey := nil;
    EccPubKey := nil;
    PreMasterKey := nil;
    try
      Ecc := TCnEcc.Create(CurveType);
      EccPrivKey := TCnEccPrivateKey.Create;
      EccPubKey := TCnEccPublicKey.Create;
      Ecc.GenerateKey(EccPrivKey);
      PreMasterKey := TCnEccPublicKey.Create;
      PreMasterX := TCnBigNumber.Create;
      EccPubKey.SetBytes(ServerKeyBytes);
      CnEccDiffieHellmanComputeKey(Ecc, EccPrivKey, EccPubKey, PreMasterKey);
      CipherIsSM4GCM := False;
      CipherIsChaCha20Poly1305 := False;
      if (SelectedCipher = CN_CIPHER_ECDHE_RSA_AES128_GCM_SHA256) or
         (SelectedCipher = CN_CIPHER_ECDHE_ECDSA_AES128_GCM_SHA256) or
         (SelectedCipher = CN_CIPHER_AES128_GCM_SHA256) then
      begin
        KeyLen := 16;
        IvLen := 4;
        DigestType := esdtSHA256;
      end
      else if (SelectedCipher = CN_CIPHER_ECDHE_RSA_AES256_GCM_SHA384) or
              (SelectedCipher = CN_CIPHER_ECDHE_ECDSA_AES256_GCM_SHA384) or
              (SelectedCipher = CN_CIPHER_AES256_GCM_SHA384) then
      begin
        KeyLen := 32;
        IvLen := 4;
        DigestType := esdtSHA384;
      end
      else if (SelectedCipher = CN_CIPHER_TLS_SM4_GCM_SM3) then
      begin
        KeyLen := 16;
        IvLen := 4;
        DigestType := esdtSM3;
        CipherIsSM4GCM := True;
      end
      else if (SelectedCipher = CN_CIPHER_ECDHE_RSA_CHACHA20_POLY1305) or
               (SelectedCipher = CN_CIPHER_ECDHE_ECDSA_CHACHA20_POLY1305) then
      begin
        KeyLen := 32;
        IvLen := 12;
        DigestType := esdtSHA256;
        CipherIsChaCha20Poly1305 := True;
      end
      else
      begin
        MySSLLog(Format('Unsupported CipherSuite 0x%.4x', [SelectedCipher]));
        CnCloseSocket(Sock);
        Exit;
      end;
      CnEccDiffieHellmanGenerateOutKey(Ecc, EccPrivKey, EccPubKey);
      H := PCnTLSRecordLayer(@Buffer[0]);
      H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
      H^.MajorVersion := 3;
      H^.MinorVersion := 3;
      B := PCnTLSHandShakeHeader(@H^.Body[0]);
      B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_CLIENT_KEY_EXCHANGE_RESERVED;
      PByte(@B^.Content[0])^ := Byte(Length(EccPubKey.ToBytes(Ecc.BytesCount)));
      CK := PCnTLSHandShakeClientKeyExchange(PAnsiChar(@B^.Content[0]) + 1);
      CnSetTLSHandShakeClientKeyExchangeECPoint(CK, EccPubKey.ToBytes(Ecc.BytesCount));
      CnSetTLSHandShakeHeaderContentLength(B, 1 + Length(EccPubKey.ToBytes(Ecc.BytesCount)));
      CnSetTLSRecordLayerBodyLength(H, 1 + 3 + CnGetTLSHandShakeHeaderContentLength(B));
      if CnSend(Sock, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
      begin
        MySSLLog('Send ClientKeyExchange failed');
        CnCloseSocket(Sock);
        Exit;
      end;
      MySSLLog('Sent ClientKeyExchange, Size: ' + IntToStr(5 +
        CnGetTLSRecordLayerBodyLength(H)));
      TotalHandShake := ConcatBytes(TotalHandShake, NewBytesFromMemory(B,
        CnGetTLSRecordLayerBodyLength(H)));
      MySSLLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType,
        CnGetTLSRecordLayerBodyLength(H)]));
      Inc(ClientSeq);
      Inc(ClientRecordCount);
      if Ecc.PointToPlain(PreMasterKey, PreMasterX) then
      begin
        PreMasterBytes := BigNumberToBytes(PreMasterX, Ecc.BytesCount);
        MySSLLog('PreMaster (X coord): ' + BytesToHex(PreMasterBytes));
        if EmsNegotiated then
          MasterKey := PseudoRandomFunc(PreMasterBytes, 'extended master secret',
            EccDigestBytes(TotalHandShake, DigestType), DigestType, 48)
        else
          MasterKey := PseudoRandomFunc(PreMasterBytes, 'master secret',
            ConcatBytes(RandClient, RandServer), DigestType, 48);
      end
      else
      begin
        MySSLLog('Failed to extract ECDH X coordinate');
        CnCloseSocket(Sock);
        Exit;
      end;
      MySSLLog('MasterSecret: ' + BytesToHex(MasterKey));
      H := PCnTLSRecordLayer(@Buffer[0]);
      H^.ContentType := CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC;
      H^.MajorVersion := 3;
      H^.MinorVersion := 3;
      CC := PCnTLSChangeCipherSpecPacket(@H^.Body[0]);
      CC^.Content := CN_TLS_CHANGE_CIPHER_SPEC;
      CnSetTLSRecordLayerBodyLength(H, 1);
      if CnSend(Sock, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
      begin
        MySSLLog('Send ChangeCipherSpec failed');
        CnCloseSocket(Sock);
        Exit;
      end;
      MySSLLog('Sent ChangeCipherSpec, Size: ' + IntToStr(5 +
        CnGetTLSRecordLayerBodyLength(H)));
      ClientSeq := 0;
      MySSLLog('HandshakeHash: ' + BytesToHex(EccDigestBytes(TotalHandShake, DigestType)));
      VerifyData := PseudoRandomFunc(MasterKey, 'client finished',
        EccDigestBytes(TotalHandShake, DigestType), DigestType, 12);
      MySSLLog('Client VerifyData: ' + BytesToHex(VerifyData));
      H := PCnTLSRecordLayer(@Buffer[0]);
      H^.ContentType := CN_TLS_CONTENT_TYPE_HANDSHAKE;
      H^.MajorVersion := 3;
      H^.MinorVersion := 3;
      B := PCnTLSHandShakeHeader(@H^.Body[0]);
      B^.HandShakeType := CN_TLS_HANDSHAKE_TYPE_FINISHED;
      F := PCnTLSHandShakeFinished(@B^.Content[0]);
      CnSetTLSTLSHandShakeFinishedVerifyData(F, VerifyData);
      CnSetTLSHandShakeHeaderContentLength(B, 12);
      PlainFinished := NewBytesFromMemory(B, 1 + 3 + 12);
      MySSLLog('PlainFinished: ' + BytesToHex(PlainFinished));
      MySSLLog('PlainFinished: ' + BytesToHex(PlainFinished));
      TotalHandShake := ConcatBytes(TotalHandShake, PlainFinished);
      MySSLLog(Format('Transcript add: hs_type=%d len=%d', [B^.HandShakeType,
        Length(PlainFinished)]));
      FillChar(AADFix[0], SizeOf(AADFix), 0);
      for I := 0 to 7 do
        SeqBytes[7 - I] := Byte((ClientSeq shr (I * 8)) and $FF);
      Move(SeqBytes[0], AADFix[0], 8);
      AADFix[8] := CN_TLS_CONTENT_TYPE_HANDSHAKE;
      AADFix[9] := 3;
      AADFix[10] := 3;
      AADFix[11] := Byte(Length(PlainFinished) shr 8);
      AADFix[12] := Byte(Length(PlainFinished) and $FF);
      AAD := NewBytesFromMemory(@AADFix[0], SizeOf(AADFix));
      SetLength(ExplicitNonce, 8);
      for I := 0 to 7 do
        ExplicitNonce[7 - I] := Byte((ClientSeq shr (I * 8)) and $FF);
      MySSLLog('ClientSeq used for AAD: ' + IntToStr(ClientSeq));
      MySSLLog('Client AAD: ' + BytesToHex(AAD));
      // move printing of full nonce after IV derived
      KeyBlock := PseudoRandomFunc(MasterKey, 'key expansion', ConcatBytes(RandServer,
        RandClient), DigestType, 2 * KeyLen + 2 * IvLen);
      ClientWriteKey := Copy(KeyBlock, 0, KeyLen);
      ServerWriteKey := Copy(KeyBlock, KeyLen, KeyLen);
      ClientFixedIV := Copy(KeyBlock, 2 * KeyLen, IvLen);
      ServerFixedIV := Copy(KeyBlock, 2 * KeyLen + IvLen, IvLen);
      MySSLLog('KeyBlock: ' + BytesToHex(KeyBlock));
      MySSLLog('ClientWriteKey: ' + BytesToHex(ClientWriteKey));
      MySSLLog('ServerWriteKey: ' + BytesToHex(ServerWriteKey));
      MySSLLog('Client FixedIV: ' + BytesToHex(ClientFixedIV));
      if CipherIsChaCha20Poly1305 then
        MySSLLog('Client Nonce (full 12B): ' + BytesToHex(TLSChaChaNonce(ClientFixedIV, ClientSeq)))
      else
        MySSLLog('Client Nonce (full 12B): ' + BytesToHex(ConcatBytes(ClientFixedIV, ExplicitNonce)));
      MySSLLog('Server FixedIV: ' + BytesToHex(ServerFixedIV));
      EnCipher := nil;
      if CipherIsChaCha20Poly1305 then
        EnCipher := ChaCha20Poly1305EncryptTLS(ClientWriteKey, ClientFixedIV, PlainFinished, AAD, ClientSeq, Tag)
      else if CipherIsSM4GCM then
        EnCipher := SM4GCMEncryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV, ExplicitNonce), PlainFinished, AAD, Tag)
      else if KeyLen = 16 then
        EnCipher := AES128GCMEncryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV, ExplicitNonce), PlainFinished, AAD, Tag)
      else
        EnCipher := AES256GCMEncryptBytes(ClientWriteKey, ConcatBytes(ClientFixedIV, ExplicitNonce), PlainFinished, AAD, Tag);
      MySSLLog('Client EnCipher Len: ' + IntToStr(Length(EnCipher)));
      MySSLLog('Client Tag: ' + BytesToHex(NewBytesFromMemory(@Tag, SizeOf(Tag))));
      if CipherIsChaCha20Poly1305 then
      begin
        MySSLLog('Client Finished Record Body: ' + BytesToHex(ConcatBytes(EnCipher, NewBytesFromMemory(@Tag, SizeOf(Tag)))));
        MySSLLog('Client Finished Record Body: ' + BytesToHex(ConcatBytes(EnCipher, NewBytesFromMemory(@Tag, SizeOf(Tag)))));
        CnSetTLSRecordLayerBodyLength(H, Length(EnCipher) + SizeOf(Tag));
        Move(EnCipher[0], H^.Body[0], Length(EnCipher));
        Move(Tag, (PAnsiChar(@H^.Body[0]) + Length(EnCipher))^, SizeOf(Tag));
      end
      else
      begin
        MySSLLog('Client Finished Record Body: ' + BytesToHex(ConcatBytes(ExplicitNonce, EnCipher, NewBytesFromMemory(@Tag, SizeOf(Tag)))));
        MySSLLog('Client Finished Record Body: ' + BytesToHex(ConcatBytes(ExplicitNonce, EnCipher, NewBytesFromMemory(@Tag, SizeOf(Tag)))));
        CnSetTLSRecordLayerBodyLength(H, 8 + Length(EnCipher) + SizeOf(Tag));
        Move(ExplicitNonce[0], H^.Body[0], 8);
        Move(EnCipher[0], (PAnsiChar(@H^.Body[0]) + 8)^, Length(EnCipher));
        Move(Tag, (PAnsiChar(@H^.Body[0]) + 8 + Length(EnCipher))^, SizeOf(Tag));
      end;
      if CnSend(Sock, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
      begin
        MySSLLog('Send Finished failed');
        CnCloseSocket(Sock);
        Exit;
      end;
      MySSLLog('Sent Finished Packet, Size: ' + IntToStr(5 +
        CnGetTLSRecordLayerBodyLength(H)));
      Inc(ClientSeq);
      FillChar(Buffer, SizeOf(Buffer), 0);
      CnFDZero(Rs);
      CnFDSet(Sock, Rs);
      MySSLLog('Waiting server Finished...');
      if CnSelect(Sock + 1, @Rs, nil, nil, nil) > 0 then
      begin
        MySSLLog('Readable after Finished');
        BytesReceived := CnRecv(Sock, Buffer[0], Length(Buffer), 0);
        MySSLLog('Recv len after Finished: ' + IntToStr(BytesReceived));
      end
      else
      begin
        MySSLLog('Select timeout after Finished');
        BytesReceived := 0;
      end;
      if BytesReceived <= SizeOf(TCnTLSRecordLayer) then
      begin
        MySSLLog('Receive after Finished failed or no data, len=' + IntToStr(BytesReceived)
          + ', errno=' + IntToStr(CnGetNetErrorNo));
        CnCloseSocket(Sock);
        Exit;
      end;
      MySSLLog(Format('SSL/TLS Get Response %d', [BytesReceived]));
      TotalConsumed := 0;
      ServerSeq := 0;
      while TotalConsumed < BytesReceived do
      begin
        H := PCnTLSRecordLayer(@Buffer[TotalConsumed]);
        RecBodyLen := CnGetTLSRecordLayerBodyLength(H);
        if H^.ContentType = CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC then
        begin
          MySSLLog('Recv ChangeCipherSpec');
        end
        else if H^.ContentType = CN_TLS_CONTENT_TYPE_HANDSHAKE then
        begin
          MySSLLog('Recv Encrypted Handshake');
          MySSLLog('ServerSeq used for AAD: ' + IntToStr(ServerSeq));
          FillChar(AADFix2[0], SizeOf(AADFix2), 0);
          for I := 0 to 7 do
            SeqBytes[7 - I] := Byte((ServerSeq shr (I * 8)) and $FF);
          Move(SeqBytes[0], AADFix2[0], 8);
          AADFix2[8] := CN_TLS_CONTENT_TYPE_HANDSHAKE;
          AADFix2[9] := 3;
          AADFix2[10] := 3;
          if CipherIsChaCha20Poly1305 then
          begin
            SetLength(ServerEn, RecBodyLen - SizeOf(ServerFinTag));
            Move((PAnsiChar(@H^.Body[0]))^, ServerEn[0], Length(ServerEn));
            Move((PAnsiChar(@H^.Body[0]) + Length(ServerEn))^, ServerFinTag, SizeOf(ServerFinTag));
            AADFix2[11] := Byte((Length(ServerEn)) shr 8);
            AADFix2[12] := Byte((Length(ServerEn)) and $FF);
            AAD := NewBytesFromMemory(@AADFix2[0], SizeOf(AADFix2));
            MySSLLog('Server FixedIV: ' + BytesToHex(ServerFixedIV));
            MySSLLog('Server AAD: ' + BytesToHex(AAD));
            MySSLLog('Server EnCipher Len: ' + IntToStr(Length(ServerEn)));
            MySSLLog('Server Tag: ' + BytesToHex(NewBytesFromMemory(@ServerFinTag, SizeOf(ServerFinTag))));
            ServerPlain := ChaCha20Poly1305DecryptTLS(ServerWriteKey, ServerFixedIV, ServerEn, AAD, ServerSeq, ServerFinTag);
          end
          else
          begin
            ExplicitNonce := NewBytesFromMemory(@H^.Body[0], 8);
            SetLength(ServerEn, RecBodyLen - 8 - SizeOf(ServerFinTag));
            Move((PAnsiChar(@H^.Body[0]) + 8)^, ServerEn[0], Length(ServerEn));
            Move((PAnsiChar(@H^.Body[0]) + 8 + Length(ServerEn))^, ServerFinTag, SizeOf(ServerFinTag));
            AADFix2[11] := Byte(((RecBodyLen - 8 - SizeOf(ServerFinTag))) shr 8);
            AADFix2[12] := Byte(((RecBodyLen - 8 - SizeOf(ServerFinTag))) and $FF);
            AAD := NewBytesFromMemory(@AADFix2[0], SizeOf(AADFix2));
            MySSLLog('Server ExplicitNonce: ' + BytesToHex(ExplicitNonce));
            MySSLLog('Server FixedIV: ' + BytesToHex(ServerFixedIV));
            MySSLLog('Server AAD: ' + BytesToHex(AAD));
            MySSLLog('Server EnCipher Len: ' + IntToStr(Length(ServerEn)));
            MySSLLog('Server Tag: ' + BytesToHex(NewBytesFromMemory(@ServerFinTag, SizeOf(ServerFinTag))));
            if CipherIsSM4GCM then
              ServerPlain := SM4GCMDecryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, ExplicitNonce), ServerEn, AAD, ServerFinTag)
            else if KeyLen = 16 then
              ServerPlain := AES128GCMDecryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, ExplicitNonce), ServerEn, AAD, ServerFinTag)
            else
              ServerPlain := AES256GCMDecryptBytes(ServerWriteKey, ConcatBytes(ServerFixedIV, ExplicitNonce), ServerEn, AAD, ServerFinTag);
          end;
          if (Length(ServerPlain) = 0) then
          begin
            MySSLLog('Decrypt Server Finished Failed');
            CnCloseSocket(Sock);
            Exit;
          end;
          MySSLLog('Server Finished Decrypted Length: ' + IntToStr(Length(ServerPlain)));
          B := PCnTLSHandShakeHeader(@ServerPlain[0]);
          F := PCnTLSHandShakeFinished(@B^.Content[0]);
          VerifyData := PseudoRandomFunc(MasterKey, 'server finished',
            EccDigestBytes(TotalHandShake, DigestType), DigestType, 12);
          Ok := CompareMem(@VerifyData[0], @F^.VerifyData[0], 12);
          MySSLLog('Server VerifyData: ' + BytesToHex(VerifyData));
          MySSLLog('Verify Match: ' + BoolToStr(Ok, True));
          if not Ok then
          begin
            MySSLLog('Server Finished verify mismatch');
            CnCloseSocket(Sock);
            Exit;
          end;
          Result := True;
          MySSLLog('Handshake Completed');
          Inc(ServerSeq);
          H := PCnTLSRecordLayer(@Buffer[0]);
          H^.ContentType := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
          H^.MajorVersion := 3;
          H^.MinorVersion := 3;
          SetLength(Req, 0);
          Req := ConcatBytes(AnsiToBytes('GET / HTTP/1.1'#13#10),
                             AnsiToBytes('Host: ' + string(Host) + #13#10),
                             AnsiToBytes('Connection: close'#13#10),
                             AnsiToBytes('Accept: */*'#13#10#13#10));
          FillChar(AADFix[0], SizeOf(AADFix), 0);
          for I := 0 to 7 do
            SeqBytes[7 - I] := Byte((ClientSeq shr (I * 8)) and $FF);
          Move(SeqBytes[0], AADFix[0], 8);
          AADFix[8] := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
          AADFix[9] := 3;
          AADFix[10] := 3;
          AADFix[11] := Byte(Length(Req) shr 8);
          AADFix[12] := Byte(Length(Req) and $FF);
          AAD := NewBytesFromMemory(@AADFix[0], SizeOf(AADFix));
          if CipherIsChaCha20Poly1305 then
          begin
            EnCipher := ChaCha20Poly1305EncryptTLS(ClientWriteKey, ClientFixedIV, Req, AAD, ClientSeq, Tag);
            MySSLLog('Client AppData AAD: ' + BytesToHex(AAD));
            MySSLLog('Client AppData EnCipher Len: ' + IntToStr(Length(EnCipher)));
            MySSLLog('Client AppData Tag: ' + BytesToHex(NewBytesFromMemory(@Tag, SizeOf(Tag))));
            CnSetTLSRecordLayerBodyLength(H, Length(EnCipher) + SizeOf(Tag));
            Move(EnCipher[0], H^.Body[0], Length(EnCipher));
            Move(Tag, (PAnsiChar(@H^.Body[0]) + Length(EnCipher))^, SizeOf(Tag));
          end
          else
          begin
            SetLength(ExplicitNonce, 8);
            for I := 0 to 7 do
              ExplicitNonce[7 - I] := Byte((ClientSeq shr (I * 8)) and $FF);
            IVNonce := ConcatBytes(ClientFixedIV, ExplicitNonce);
            if CipherIsSM4GCM then
              EnCipher := SM4GCMEncryptBytes(ClientWriteKey, IVNonce, Req, AAD, Tag)
            else if KeyLen = 16 then
              EnCipher := AES128GCMEncryptBytes(ClientWriteKey, IVNonce, Req, AAD, Tag)
            else
              EnCipher := AES256GCMEncryptBytes(ClientWriteKey, IVNonce, Req, AAD, Tag);
            MySSLLog('Client AppData AAD: ' + BytesToHex(AAD));
            MySSLLog('Client AppData ExplicitNonce: ' + BytesToHex(ExplicitNonce));
            MySSLLog('Client AppData EnCipher Len: ' + IntToStr(Length(EnCipher)));
            MySSLLog('Client AppData Tag: ' + BytesToHex(NewBytesFromMemory(@Tag, SizeOf(Tag))));
            CnSetTLSRecordLayerBodyLength(H, 8 + Length(EnCipher) + SizeOf(Tag));
            Move(ExplicitNonce[0], H^.Body[0], 8);
            Move(EnCipher[0], (PAnsiChar(@H^.Body[0]) + 8)^, Length(EnCipher));
            Move(Tag, (PAnsiChar(@H^.Body[0]) + 8 + Length(EnCipher))^, SizeOf(Tag));
          end;
          if CnSend(Sock, H^, 5 + CnGetTLSRecordLayerBodyLength(H), 0) = SOCKET_ERROR then
          begin
            MySSLLog('Send ApplicationData failed');
            CnCloseSocket(Sock);
            Exit;
          end;
          MySSLLog('Sent ApplicationData, Size: ' + IntToStr(5 + CnGetTLSRecordLayerBodyLength(H)));
          Inc(ClientSeq);
          FillChar(Buffer, SizeOf(Buffer), 0);
          CnFDZero(Rs);
          CnFDSet(Sock, Rs);
          if CnSelect(Sock + 1, @Rs, nil, nil, nil) > 0 then
          begin
            BytesReceived := CnRecv(Sock, Buffer[0], Length(Buffer), 0);
            MySSLLog(Format('SSL/TLS Get Response %d', [BytesReceived]));
            TotalConsumed := 0;
            while TotalConsumed < BytesReceived do
            begin
              H := PCnTLSRecordLayer(@Buffer[TotalConsumed]);
              RecBodyLen := CnGetTLSRecordLayerBodyLength(H);
              if H^.ContentType = CN_TLS_CONTENT_TYPE_APPLICATION_DATA then
              begin
                ExplicitNonce := NewBytesFromMemory(@H^.Body[0], 8);
                SetLength(ServerEn, RecBodyLen - 8 - SizeOf(ServerFinTag));
                Move((PAnsiChar(@H^.Body[0]) + 8)^, ServerEn[0], Length(ServerEn));
                Move((PAnsiChar(@H^.Body[0]) + 8 + Length(ServerEn))^, ServerFinTag, SizeOf(ServerFinTag));
                FillChar(AADFix2[0], SizeOf(AADFix2), 0);
                for I := 0 to 7 do
                  SeqBytes[7 - I] := Byte((ServerSeq shr (I * 8)) and $FF);
                Move(SeqBytes[0], AADFix2[0], 8);
                AADFix2[8] := CN_TLS_CONTENT_TYPE_APPLICATION_DATA;
                AADFix2[9] := 3;
                AADFix2[10] := 3;
                AADFix2[11] := Byte(((RecBodyLen - 8 - SizeOf(ServerFinTag))) shr 8);
                AADFix2[12] := Byte(((RecBodyLen - 8 - SizeOf(ServerFinTag))) and $FF);
                AAD := NewBytesFromMemory(@AADFix2[0], SizeOf(AADFix2));
                IVNonce := ConcatBytes(ServerFixedIV, ExplicitNonce);
                if CipherIsSM4GCM then
                  ServerPlain := SM4GCMDecryptBytes(ServerWriteKey, IVNonce, ServerEn, AAD, ServerFinTag)
                else if KeyLen = 16 then
                  ServerPlain := AES128GCMDecryptBytes(ServerWriteKey, IVNonce, ServerEn, AAD, ServerFinTag)
                else
                  ServerPlain := AES256GCMDecryptBytes(ServerWriteKey, IVNonce, ServerEn, AAD, ServerFinTag);
                MySSLLog('ServerSeq used for AAD: ' + IntToStr(ServerSeq));
                MySSLLog('Server AppData ExplicitNonce: ' + BytesToHex(ExplicitNonce));
                MySSLLog('Server AppData AAD: ' + BytesToHex(AAD));
                MySSLLog('Server AppData EnCipher Len: ' + IntToStr(Length(ServerEn)));
                MySSLLog('Server AppData Tag: ' + BytesToHex(NewBytesFromMemory(@ServerFinTag, SizeOf(ServerFinTag))));
                if Length(ServerPlain) > 0 then
                begin
                  SetLength(TmpStr, Length(ServerPlain));
                  Move(ServerPlain[0], PAnsiChar(TmpStr)^, Length(ServerPlain));
                  MySSLLog('HTTP Response Chunk: ' + string(TmpStr));
                end
                else
                begin
                  MySSLLog('Decrypt Server AppData Failed');
                  CnCloseSocket(Sock);
                  Exit;
                end;
                Inc(ServerSeq);
              end
              else if H^.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
              begin
                MySSLLog('Recv Alert');
              end;
              Inc(TotalConsumed, 5 + RecBodyLen);
            end;
          end;
          Inc(ServerSeq);
          Break;
        end
        else if H^.ContentType = CN_TLS_CONTENT_TYPE_ALERT then
        begin
          MySSLLog('Recv Alert');
          if RecBodyLen >= 2 then
          begin
            Lvl := Ord(PAnsiChar(@H^.Body[0])^);
            Desc := Ord((PAnsiChar(@H^.Body[0]) + 1)^);
            MySSLLog(Format('Alert level=%d, desc=%d', [Lvl, Desc]));
          end;
        end;
        Inc(TotalConsumed, 5 + RecBodyLen);
        if H^.ContentType <> CN_TLS_CONTENT_TYPE_CHANGE_CIPHER_SPEC then
          Inc(ServerSeq);
      end;
    finally
      PreMasterKey.Free;
      PreMasterX.Free;
      EccPubKey.Free;
      EccPrivKey.Free;
      Ecc.Free;
    end;
  end
  else
  begin
    MySSLLog('Connect failed to ' + IpStr + ':' + IntToStr(Port));
    MySSLLog('errno: ' + IntToStr(CnGetNetErrorNo));
  end;
  CnCloseSocket(Sock);
end;

procedure TFormNetDecl.btnSSLClientClick(Sender: TObject);
begin
  mmoSSL.Lines.Clear;
  if SSLHandShake12(edtTLSHost.Text, StrToInt(edtTLSPort.Text)) then
    MySSLLog('SSL 1.2 Handshake OK');
end;

procedure TFormNetDecl.MySSLLog(const Str: string);
begin
  mmoSSL.Lines.Add(Str);
end;

end.

