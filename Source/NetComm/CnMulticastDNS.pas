{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnMulticastDNS;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：Multicast DNS 广播服务与发现单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：用于 mDNS 协议的广播、服务注册与发现
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP/10+ Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.01.14 V1.0
*                创建单元，在 AI 的帮助下实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Contnrs,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  CnClasses, CnConsts, CnNetConsts, CnNative, CnSocket, CnUDP, CnDNS, CnNetwork, CnIP;

type
  TCnMDNSService = record
  {* 供外部通知及调用使用的服务代表结构}
    Instance: string;
    TypeName: string;
    Domain: string;
    Host: string;
    Port: Word;
    TxtRaw: TBytes;
    Local: Boolean;
  end;

  TCnMDNSServiceEvent = procedure(Sender: TObject; const Service: TCnMDNSService) of object;
  {* mDNS 服务变化通知事件}

  TCnMDNSServiceItem = class
  {* 内部服务条目代表类}
  private
    FIsLocal: Boolean;
    FExpireTick: Cardinal;
    FHost: string;
    FInstance: string;
    FDomain: string;
    FTypeName: string;
    FPort: Word;
  public
    TxtRaw: TBytes;

    property Instance: string read FInstance write FInstance;
    property TypeName: string read FTypeName write FTypeName;
    property Domain: string read FDomain write FDomain;
    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort;
    property ExpireTick: Cardinal read FExpireTick write FExpireTick;
    property IsLocal: Boolean read FIsLocal write FIsLocal;
  end;

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnMulticastDNS = class(TCnComponent)
  private
    FUDP: TCnUDP;
    FActive: Boolean;
    FServices: TObjectList;
    FLocalIPs: TStringList;
    FLocalInstances: TStringList;
    FLastFromIP: string;
    FOnServiceAdded: TCnMDNSServiceEvent;
    FOnServiceRemoved: TCnMDNSServiceEvent;
    FOnServiceUpdated: TCnMDNSServiceEvent;
    procedure SetActive(const Value: Boolean);
    procedure UDPDataReceived(Sender: TComponent; Buffer: Pointer; Len: Integer;
      const FromIP: string; Port: Integer);
    function FindServiceItem(const Instance: string): TCnMDNSServiceItem;
    procedure DoServiceAdded(Item: TCnMDNSServiceItem);
    procedure DoServiceUpdated(Item: TCnMDNSServiceItem);
    procedure SweepExpiredCache;
    procedure DoBrowsePtrAnswer(Packet: TCnDNSPacketObject);
    procedure DoResolveAnswers(Packet: TCnDNSPacketObject);
    procedure ProbeName(const Name: string);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Browse(const TypeName: string);
    procedure Resolve(const Instance: string);

    procedure RegisterService(const Service: TCnMDNSService);
    procedure DeregisterService(const Instance: string);
    procedure ClearCache;
  published
    property Active: Boolean read FActive write SetActive;

    property OnServiceAdded: TCnMDNSServiceEvent read FOnServiceAdded write
      FOnServiceAdded;
    property OnServiceRemoved: TCnMDNSServiceEvent read FOnServiceRemoved write
      FOnServiceRemoved;
    property OnServiceUpdated: TCnMDNSServiceEvent read FOnServiceUpdated write
      FOnServiceUpdated;
  end;

implementation

const
  SCN_MDNS_GROUP_V4 = '224.0.0.251';
  SCN_MDNS_GROUP_V6 = 'FF02::FB';

procedure LogLine(const S: string);
begin
  // WriteLn(S);
end;

procedure DumpDNSStream(St: TMemoryStream);
var
  P: PAnsiChar;
  Pos: Integer;
  I: Integer;
  L: Integer;
  B: TBytes;
  S: string;

  function ReadByte: Byte;
  begin
    Result := Byte(P[Pos]);
    Inc(Pos);
  end;

  function ReadWordBE: Word;
  var
    H, Lb: Byte;
  begin
    H := ReadByte;
    Lb := ReadByte;
    Result := (H shl 8) or Lb;
  end;

  function ReadCardinalBE: Cardinal;
  var
    B0, B1, B2, B3: Byte;
  begin
    B0 := ReadByte;
    B1 := ReadByte;
    B2 := ReadByte;
    B3 := ReadByte;
    Result := (Cardinal(B0) shl 24) or (Cardinal(B1) shl 16) or (Cardinal(B2)
      shl 8) or Cardinal(B3);
  end;

  function ReadName: string;
  var
    Len: Integer;
    T: string;
    J: Integer;
  begin
    Result := '';
    while True do
    begin
      Len := ReadByte;
      if Len = 0 then
        Break;
      SetLength(T, Len);
      for J := 1 to Len do
        T[J] := Char(ReadByte);
      if Result = '' then
        Result := T
      else
        Result := Result + '.' + T;
    end;
  end;

begin
  SetLength(B, St.Size);
  if Length(B) > 0 then
  begin
    St.Position := 0;
    St.Read(B[0], Length(B));
    LogLine('Total ' + IntToStr(Length(B)));
    LogLine(BytesToHex(B));
  end;
  if St.Size < 12 then
    Exit;

  P := PAnsiChar(St.Memory);
  Pos := 12;

  for I := 1 to 4 do
  begin
    S := ReadName;
    L := ReadWordBE;
    ReadWordBE;
    ReadCardinalBE;
    L := ReadWordBE;

    LogLine('RR ' + IntToStr(I) + ' Name=' + S + ' RDLen=' + IntToStr(L));
    while L > 0 do
    begin
      ReadByte;
      Dec(L);
    end;
  end;
end;

procedure WriteName(Stream: TStream; const Name: string); forward;

function SetCacheFlush(RClass: Word): Word; forward;

function HasCacheFlush(RClass: Word): Boolean; forward;

procedure WriteWordMSBFirst(Stream: TStream; W: Word);
var
  V: Word;
begin
  V := UInt16HostToNetwork(W);
  Stream.WriteBuffer(V, SizeOf(V));
end;

procedure TCnMulticastDNS.ProbeName(const Name: string);
var
  S: TMemoryStream;
  Head: TCnDNSHeader;
  I: Integer;
begin
  S := TMemoryStream.Create;
  try
    FillChar(Head, SizeOf(Head), 0);
    CnSetDNSHeaderId(@Head, 0);
    CnSetDNSHeaderQR(@Head, False);
    CnSetDNSHeaderQDCount(@Head, 1);
    CnSetDNSHeaderANCount(@Head, 0);
    CnSetDNSHeaderNSCount(@Head, 0);
    CnSetDNSHeaderARCount(@Head, 0);
    S.WriteBuffer(Head, SizeOf(Head));
    WriteName(S, Name);
    WriteWordMSBFirst(S, CN_DNS_TYPE_ANY);
    WriteWordMSBFirst(S, SetCacheFlush(CN_DNS_CLASS_IN));
    for I := 1 to 3 do
    begin
      FUDP.SendStream(S, False);
    end;
  finally
    S.Free;
  end;
end;

procedure WriteCardinalMSBFirst(Stream: TStream; C: Cardinal);
var
  V: Cardinal;
begin
  V := UInt32HostToNetwork(C);
  Stream.WriteBuffer(V, SizeOf(V));
end;

procedure WriteName(Stream: TStream; const Name: string);
var
  P: PAnsiChar;
  Buf: array[0..255] of AnsiChar;
  Q: PAnsiChar;
begin
  P := PAnsiChar(@Buf[0]);
  Q := TCnDNS.BuildNameBuffer(Name, P);
  Stream.WriteBuffer(Buf[0], Q - P);
end;

function NameEncodedLength(const Name: string): Word;
var
  P, Q: PAnsiChar;
  Buf: array[0..255] of AnsiChar;
begin
  P := PAnsiChar(@Buf[0]);
  Q := TCnDNS.BuildNameBuffer(Name, P);
  Result := Word(Q - P);
end;

function EncodeName(const Name: string): TBytes;
var
  P, Q: PAnsiChar;
  Buf: array[0..255] of AnsiChar;
  L: Integer;
begin
  P := PAnsiChar(@Buf[0]);
  Q := TCnDNS.BuildNameBuffer(Name, P);
  L := Q - P;
  SetLength(Result, L);
  if L > 0 then
    Move(Buf[0], Result[0], L);
end;

procedure WriteRR(S: TStream; const Name: string; RType: Word; RClass: Word; TTL:
  Cardinal; const RData: TBytes);
var
  Enc: TBytes;
  L: Word;
begin
  Enc := EncodeName(Name);
  if Length(Enc) > 0 then
    S.WriteBuffer(Enc[0], Length(Enc));
  WriteWordMSBFirst(S, RType);
  WriteWordMSBFirst(S, RClass);
  WriteCardinalMSBFirst(S, TTL);
  L := Length(RData);
  WriteWordMSBFirst(S, L);
  if L > 0 then
    S.WriteBuffer(RData[0], L);
end;

function StreamToBytes(St: TMemoryStream): TBytes;
var
  L: Integer;
begin
  L := St.Size;
  SetLength(Result, L);
  if L > 0 then
  begin
    St.Position := 0;
    St.Read(Result[0], L);
  end;
end;

function BuildSrvRData(Port: Word; const Host: string): TBytes;
var
  M: TMemoryStream;
  Enc: TBytes;
begin
  M := TMemoryStream.Create;
  try
    WriteWordMSBFirst(M, 0);
    WriteWordMSBFirst(M, 0);
    WriteWordMSBFirst(M, Port);
    Enc := EncodeName(Host);
    if Length(Enc) > 0 then
      M.WriteBuffer(Enc[0], Length(Enc));
    Result := StreamToBytes(M);
  finally
    M.Free;
  end;
end;

function CardinalToBytesBE(C: Cardinal): TBytes;
var
  V: Cardinal;
begin
  V := UInt32HostToNetwork(C);
  SetLength(Result, 4);
  Move(V, Result[0], 4);
end;

function SetCacheFlush(RClass: Word): Word;
begin
  Result := RClass or $8000;
end;

function HasCacheFlush(RClass: Word): Boolean;
begin
  Result := (RClass and $8000) <> 0;
end;

function GetTick: Cardinal;
begin
{$IFDEF MSWINDOWS}
  Result := GetTickCount;
{$ELSE}
  Result := Cardinal(0);
{$ENDIF}
end;

function BuildQuerySimple(const Name: string; QType, QClass: Word): TBytes;
var
  Head: PCnDNSHeader;
  P: PAnsiChar;
  PW: PWORD;
begin
  SetLength(Result, SizeOf(TCnDNSHeader) + Length(Name) + SizeOf(Byte) + 2 *
    SizeOf(Word));
  Head := PCnDNSHeader(@Result[0]);
  CnSetDNSHeaderId(Head, 0);
  CnSetDNSHeaderQR(Head, False);
  CnSetDNSHeaderRD(Head, False);
  CnSetDNSHeaderQDCount(Head, 1);
  P := PAnsiChar(@Head^.SectionData[0]);
  PW := PWORD(TCnDNS.BuildNameBuffer(Name, P));
  PW^ := UInt16HostToNetwork(QType);
  Inc(PW);
  PW^ := UInt16HostToNetwork(QClass);
end;

constructor TCnMulticastDNS.Create(AOwner: TComponent);
begin
  inherited;
  FServices := TObjectList.Create;
  FLocalIPs := TStringList.Create;
  FLocalInstances := TStringList.Create;
  FUDP := TCnUDP.Create(Self);
  FUDP.ReuseAddr := True;
  FUDP.ReusePort := True;
  FUDP.MulticastTTL := 255;
  FUDP.MulticastLoop := 1;
  FUDP.RemoteHost := SCN_MDNS_GROUP_V4;
  FUDP.RemotePort := CN_PORT_MDNS;
  FUDP.LocalPort := CN_PORT_MDNS;
  FUDP.BindAddr := '0.0.0.0';
  FUDP.OnDataReceived := UDPDataReceived;
end;

destructor TCnMulticastDNS.Destroy;
begin
  ClearCache;
  FServices.Free;
  FLocalIPs.Free;
  FLocalInstances.Free;
  FUDP.Free;
  inherited;
end;

procedure TCnMulticastDNS.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnMulticastDNSName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnMulticastDNSComment;
end;

function IsLocalAddress(List: TStrings; const IP: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if List = nil then
    Exit;

  for I := 0 to List.Count - 1 do
  begin
    if List[I] = IP then
    begin
      Result := True;
      Exit;
    end;
  end;
  if IP = '127.0.0.1' then
    Result := True;
end;

procedure TCnMulticastDNS.Loaded;
begin

  inherited;
end;

procedure TCnMulticastDNS.SetActive(const Value: Boolean);
var
  LocalIP: string;
  I: Integer;
  S: string;
  IpComp: TCnIp;

  function IsIPv4Address(const S: string): Boolean;
  var
    Part: string;
    V: Integer;
    DotCount: Integer;
    P: Integer;
    Q: Integer;
  begin
    Result := False;
    if S = '' then
      Exit;
    DotCount := 0;
    P := 1;
    for Q := 1 to Length(S) do
    begin
      if S[Q] = '.' then
      begin
        Part := Copy(S, P, Q - P);
        if (Part = '') or (not TryStrToInt(Part, V)) or (V < 0) or (V > 255) then
          Exit;
        Inc(DotCount);
        P := Q + 1;
      end
      else if (S[Q] < '0') or (S[Q] > '9') then
        Exit;
    end;
    Part := Copy(S, P, Length(S) - P + 1);
    if (Part = '') or (not TryStrToInt(Part, V)) or (V < 0) or (V > 255) then
      Exit;
    Result := DotCount = 3;
  end;
begin
  if FActive = Value then
    Exit;

  FActive := Value;
  if FActive then
  begin
    FUDP.UpdateBinding;
    FLocalIPs.Clear;
    IpComp := TCnIp.Create(nil);
    try
      if IpComp.LocalIPCount > 0 then
        LocalIP := TCnIp.IntToIP(IpComp.LocalIPs[0].IPAddress)
      else
        LocalIP := '0.0.0.0';
      for I := 0 to IpComp.LocalIPCount - 1 do
      begin
        S := TCnIp.IntToIP(IpComp.LocalIPs[I].IPAddress);
        if IsIPv4Address(S) and (FLocalIPs.IndexOf(S) < 0) then
          FLocalIPs.Add(S);
      end;
    finally
      IpComp.Free;
    end;
    if not IsIPv4Address(LocalIP) then
      LocalIP := '0.0.0.0';
    if FLocalIPs.Count = 0 then
      FUDP.JoinMulticastGroup(SCN_MDNS_GROUP_V4, LocalIP)
    else
      for I := 0 to FLocalIPs.Count - 1 do
        FUDP.JoinMulticastGroup(SCN_MDNS_GROUP_V4, FLocalIPs[I]);
  end
  else
  begin
    for I := 0 to FLocalIPs.Count - 1 do
      FUDP.LeaveMulticastGroup(SCN_MDNS_GROUP_V4, FLocalIPs[I]);
    FUDP.LeaveMulticastGroup(SCN_MDNS_GROUP_V4, '0.0.0.0');
    FLocalIPs.Clear;
  end;
end;

procedure TCnMulticastDNS.ClearCache;
var
  I: Integer;
begin
  for I := FServices.Count - 1 downto 0 do
    FServices.Delete(I);
end;

function TCnMulticastDNS.FindServiceItem(const Instance: string): TCnMDNSServiceItem;
var
  I: Integer;
  S: TCnMDNSServiceItem;
begin
  Result := nil;
  for I := 0 to FServices.Count - 1 do
  begin
    S := TCnMDNSServiceItem(FServices[I]);
    if S.Instance = Instance then
    begin
      Result := S;
      Exit;
    end;
  end;
end;

procedure TCnMulticastDNS.DoServiceAdded(Item: TCnMDNSServiceItem);
var
  Svc: TCnMDNSService;
begin
  Svc.Instance := Item.Instance;
  Svc.TypeName := Item.TypeName;
  Svc.Domain := Item.Domain;
  Svc.Host := Item.Host;
  Svc.Port := Item.Port;
  Svc.TxtRaw := Item.TxtRaw;
  Svc.Local := Item.IsLocal;
  if Assigned(FOnServiceAdded) then
    FOnServiceAdded(Self, Svc);
end;

procedure TCnMulticastDNS.DoServiceUpdated(Item: TCnMDNSServiceItem);
var
  Svc: TCnMDNSService;
begin
  Svc.Instance := Item.Instance;
  Svc.TypeName := Item.TypeName;
  Svc.Domain := Item.Domain;
  Svc.Host := Item.Host;
  Svc.Port := Item.Port;
  Svc.TxtRaw := Item.TxtRaw;
  Svc.Local := Item.IsLocal;
  if Assigned(FOnServiceUpdated) then
    FOnServiceUpdated(Self, Svc);
end;

procedure TCnMulticastDNS.SweepExpiredCache;
var
  I: Integer;
  Item: TCnMDNSServiceItem;
  Svc: TCnMDNSService;
begin
  for I := FServices.Count - 1 downto 0 do
  begin
    Item := TCnMDNSServiceItem(FServices[I]);
    if (Item.ExpireTick > 0) and (GetTick >= Item.ExpireTick) then
    begin
      Svc.Instance := Item.Instance;
      Svc.TypeName := Item.TypeName;
      Svc.Domain := Item.Domain;
      Svc.Host := Item.Host;
      Svc.Port := Item.Port;
      Svc.TxtRaw := Item.TxtRaw;
      Svc.Local := Item.IsLocal;
      FServices.Delete(I);
      if Assigned(FOnServiceRemoved) then
        FOnServiceRemoved(Self, Svc);
    end;
  end;
end;

procedure TCnMulticastDNS.UDPDataReceived(Sender: TComponent; Buffer: Pointer;
  Len: Integer; const FromIP: string; Port: Integer);
var
  Packet: TCnDNSPacketObject;
begin
  FLastFromIP := FromIP;
  Packet := TCnDNSPacketObject.Create;
  try
    TCnDNS.ParseDNSResponsePacket(PAnsiChar(Buffer), Len, Packet);
    DoBrowsePtrAnswer(Packet);
    DoResolveAnswers(Packet);
    SweepExpiredCache;
  finally
    Packet.Free;
  end;
end;

procedure TCnMulticastDNS.DoBrowsePtrAnswer(Packet: TCnDNSPacketObject);
var
  I: Integer;
  R: TCnDNSResourceRecord;
  Item: TCnMDNSServiceItem;
  Ttl: Cardinal;

  function Norm(const S: string): string;
  begin
    Result := S;
    while (Result <> '') and (Result[1] = '.') do
      Delete(Result, 1, 1);
  end;
begin
  for I := 0 to Packet.ANCount - 1 do
  begin
    R := Packet.AN[I];
    if R.RType = CN_DNS_TYPE_PTR then
    begin
      Item := FindServiceItem(Norm(R.RDString));
      if Item = nil then
      begin
        Item := TCnMDNSServiceItem.Create;
        Item.Instance := Norm(R.RDString);
        Item.TypeName := Norm(R.RName);
        Item.Domain := 'local';
        Item.Host := '';
        Item.Port := 0;
        SetLength(Item.TxtRaw, 0);
        Item.ExpireTick := 0;
        Item.IsLocal := (FLocalInstances.IndexOf(Item.Instance) >= 0) or
          IsLocalAddress(FLocalIPs, FLastFromIP);
        FServices.Add(Item);
        DoServiceAdded(Item);
        Resolve(Item.Instance);
      end;
      Ttl := R.TTL;
      if Ttl > 0 then
        Item.ExpireTick := GetTick + Ttl * 1000;
    end;
  end;
end;

procedure TCnMulticastDNS.DoResolveAnswers(Packet: TCnDNSPacketObject);
var
  I: Integer;
  Item: TCnMDNSServiceItem;
  Ttl: Cardinal;
  OldHost: string;
  OldPort: Word;
  OldTxtLen: Integer;

  function Norm(const S: string): string;
  begin
    Result := S;
    while (Result <> '') and (Result[1] = '.') do
      Delete(Result, 1, 1);
  end;

  procedure HandleRR(R: TCnDNSResourceRecord);
  var
    J: Integer;
    Name: string;
  begin
    Name := Norm(R.RName);
    if R.RType = CN_DNS_TYPE_SRV then
    begin
      Item := FindServiceItem(Name);
      if Item = nil then
      begin
        Item := TCnMDNSServiceItem.Create;
        Item.Instance := Name;
        Item.TypeName := '';
        Item.Domain := 'local';
        FServices.Add(Item);
      end;
      OldHost := Item.Host;
      OldPort := Item.Port;
      Item.Port := Word(R.IP);
      Item.Host := Norm(R.RDString);
      Item.IsLocal := (FLocalInstances.IndexOf(Item.Instance) >= 0) or
        IsLocalAddress(FLocalIPs, FLastFromIP);
      Ttl := R.TTL;
      if Ttl > 0 then
        Item.ExpireTick := GetTick + Ttl * 1000;
      if (OldHost <> Item.Host) or (OldPort <> Item.Port) then
        DoServiceUpdated(Item);
    end
    else if R.RType = CN_DNS_TYPE_TXT then
    begin
      Item := FindServiceItem(Name);
      if Item <> nil then
      begin
        OldTxtLen := Length(Item.TxtRaw);
        SetLength(Item.TxtRaw, Length(R.RDString));
        if Length(Item.TxtRaw) > 0 then
          Move(R.RDString[1], Item.TxtRaw[0], Length(R.RDString));
        if OldTxtLen <> Length(Item.TxtRaw) then
          DoServiceUpdated(Item);
      end;
    end
    else if R.RType = CN_DNS_TYPE_A then
    begin
      for J := 0 to FServices.Count - 1 do
      begin
        Item := TCnMDNSServiceItem(FServices[J]);
        if Item.Host <> '' then
        begin
          Ttl := R.TTL;
          if Ttl > 0 then
            Item.ExpireTick := GetTick + Ttl * 1000;
        end;
      end;
    end;
  end;
begin
  for I := 0 to Packet.ANCount - 1 do
    HandleRR(Packet.AN[I]);
  for I := 0 to Packet.NSCount - 1 do
    HandleRR(Packet.NS[I]);
  for I := 0 to Packet.ARCount - 1 do
    HandleRR(Packet.AR[I]);
end;

procedure TCnMulticastDNS.Browse(const TypeName: string);
var
  Buf: TBytes;
begin
  Buf := BuildQuerySimple(TypeName, CN_DNS_TYPE_PTR, CN_DNS_CLASS_IN);
  FUDP.SendBuffer(@Buf[0], Length(Buf), False);
  SetLength(Buf, 0);
end;

procedure TCnMulticastDNS.Resolve(const Instance: string);
var
  Buf: TBytes;
begin
  Buf := BuildQuerySimple(Instance, CN_DNS_TYPE_SRV, SetCacheFlush(CN_DNS_CLASS_IN));
  FUDP.SendBuffer(@Buf[0], Length(Buf), False);
  SetLength(Buf, 0);
  Buf := BuildQuerySimple(Instance, CN_DNS_TYPE_TXT, SetCacheFlush(CN_DNS_CLASS_IN));
  FUDP.SendBuffer(@Buf[0], Length(Buf), False);
  SetLength(Buf, 0);
end;

procedure TCnMulticastDNS.RegisterService(const Service: TCnMDNSService);
var
  HostName: string;
  S: TMemoryStream;
  TTL: Cardinal;
  Enc: TBytes;
begin
  HostName := Service.Host;
  if HostName = '' then
    HostName := 'local-host.local';

  TTL := 120;
  S := TMemoryStream.Create;
  try
    ProbeName(Service.Instance);
    ProbeName(HostName);
    WriteWordMSBFirst(S, 0);           // ID
    WriteWordMSBFirst(S, $8400);       // Flags: QR=1, AA=1
    WriteWordMSBFirst(S, 0);           // QDCount
    WriteWordMSBFirst(S, 4);           // ANCount
    WriteWordMSBFirst(S, 0);           // NSCount
    WriteWordMSBFirst(S, 0);           // ARCount
    WriteRR(S, Service.TypeName, CN_DNS_TYPE_PTR, SetCacheFlush(CN_DNS_CLASS_IN),
      TTL, EncodeName(Service.Instance));
    WriteRR(S, Service.Instance, CN_DNS_TYPE_SRV, SetCacheFlush(CN_DNS_CLASS_IN),
      TTL, BuildSrvRData(Service.Port, Service.Host));

    if Length(Service.TxtRaw) > 0 then
      WriteRR(S, Service.Instance, CN_DNS_TYPE_TXT, SetCacheFlush(CN_DNS_CLASS_IN),
        TTL, Service.TxtRaw)
    else
    begin
      SetLength(Enc, 1);
      Enc[0] := 0;
      WriteRR(S, Service.Instance, CN_DNS_TYPE_TXT, SetCacheFlush(CN_DNS_CLASS_IN),
        TTL, Enc);
    end;

    WriteRR(S, Service.Host, CN_DNS_TYPE_A, SetCacheFlush(CN_DNS_CLASS_IN), TTL,
      CardinalToBytesBE(CnIPv4StringToCardinal(FUDP.LocalHost)));
    DumpDNSStream(S);
    FUDP.SendStream(S, False);

    if FLocalInstances.IndexOf(Service.Instance) < 0 then
      FLocalInstances.Add(Service.Instance);
  finally
    S.Free;
  end;
end;

procedure TCnMulticastDNS.DeregisterService(const Instance: string);
var
  S: TMemoryStream;
  TTL: Cardinal;
  Zero: Byte;
begin
  TTL := 0;
  S := TMemoryStream.Create;
  try
    WriteWordMSBFirst(S, 0);           // ID
    WriteWordMSBFirst(S, $8400);       // Flags: QR=1, AA=1
    WriteWordMSBFirst(S, 0);           // QDCount
    WriteWordMSBFirst(S, 1);           // ANCount
    WriteWordMSBFirst(S, 0);           // NSCount
    WriteWordMSBFirst(S, 0);           // ARCount
    WriteName(S, Instance);
    WriteWordMSBFirst(S, CN_DNS_TYPE_TXT);
    WriteWordMSBFirst(S, SetCacheFlush(CN_DNS_CLASS_IN));
    WriteCardinalMSBFirst(S, TTL);
    WriteWordMSBFirst(S, 1);
    Zero := 0;
    S.WriteBuffer(Zero, 1);
    FUDP.SendStream(S, False);
  finally
    if FLocalInstances.IndexOf(Instance) >= 0 then
      FLocalInstances.Delete(FLocalInstances.IndexOf(Instance));
    S.Free;
  end;
end;

end.

