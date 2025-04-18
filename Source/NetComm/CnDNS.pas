{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnDNS;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：DNS 解析处理单元
* 单元作者：CnPack 开发组
* 备    注：基于 RFC 1035，支持 Windows 与 POSIX
* 开发平台：PWin7 + Delphi 5
* 兼容测试：PWinXP/7 + Delphi 2009 ~
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.12.17 V1.2
*                增加对 MacOS 的支持
*           2022.11.24 V1.1
*                修正解析数据时如果末块下一个指向自己时导致无限递归堆栈溢出的问题
*           2019.03.04 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF} Contnrs,
  CnClasses, CnConsts, CnNetConsts, CnNative, CnIP, CnUDP, CnNetwork;

type
  ECnDNSException = class(Exception);
  {* DNS 相关异常}

  TCnDNSQuestion = class
  {* 代表一个 DNS 包中的 Question 类型的记录}
  private
    FQName: string;
    FQClass: Word;
    FQType: Word;
  public
    procedure DumpToStrings(List: TStrings);

    property QName: string read FQName write FQName;
    {* 域名，查询用}
    property QType: Word read FQType write FQType;
    {* 资源类型}
    property QClass: Word read FQClass write FQClass;
    {* 网络类别}
  end;

  TCnDNSResourceRecord = class
  {* 代表一个 DNS 包中的 Resource Record 类型的记录}
  private
    FRDLength: Word;
    FTTL: Cardinal;
    FRName: string;
    FRType: Word;
    FRClass: Word;
    FIP: Cardinal;
    FRDString: string;
  public
    procedure DumpToStrings(List: TStrings);

    property RName: string read FRName write FRName;
    {* 域名，应答用}
    property RType: Word read FRType write FRType;
    {* 资源类型，对应 QTYPE}
    property RClass: Word read FRClass write FRClass;
    {* 网络类别，对应 QCLASS}
    property TTL: Cardinal read FTTL write FTTL;
    {* Time to Live}
    property RDLength: Word read FRDLength write FRDLength;
    {* 资源数据长度}

    property IP: Cardinal read FIP write FIP;
    {* IP 地址}
    property RDString: string read FRDString write FRDString;
    {* 资源数据，可以是域名等}
  end;

  TCnDNSPacketObject = class
  {* 表示一个 DNS 包的内容，可以持有多个 TCnDNSQuestion 与 TCnDNSResourceRecord 实例}
  private
    FQDCount: Integer;
    FANCount: Integer;
    FNSCount: Integer;
    FARCount: Integer;
    FIsResponse: Boolean;
    FIsQuery: Boolean;
    FQDList: TObjectList;
    FANList: TObjectList;
    FNSList: TObjectList;
    FARList: TObjectList;
    FQR: Integer;
    FId: Word;
    FAA: Boolean;
    FTC: Boolean;
    FRA: Boolean;
    FRD: Boolean;
    FRCode: Integer;
    FOpCode: Integer;
    function GetAN(Index: Integer): TCnDNSResourceRecord;
    function GetAR(Index: Integer): TCnDNSResourceRecord;
    function GetNS(Index: Integer): TCnDNSResourceRecord;
    function GetQD(Index: Integer): TCnDNSQuestion;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddQuestion: TCnDNSQuestion;
    function AddAnswer: TCnDNSResourceRecord;
    function AddNameServer: TCnDNSResourceRecord;
    function AddAdditionalRecord: TCnDNSResourceRecord;

    procedure DumpToStrings(List: TStrings);

    property Id: Word read FId write FId;
    {* 查询应答的 16 位 ID}
    property QR: Integer read FQR write FQR;
    {* 报文类型，0 查询、1 应答}
    property OpCode: Integer read FOpCode write FOpCode;
    {* 查询类型，0 标准、1 反向、2 服务器状态}
    property AA: Boolean read FAA write FAA;
    {* 授权应答标志，应答用，1 表示服务器有授权}
    property TC: Boolean read FTC write FTC;
    {* 截断标志，1 表示本包被截断}
    property RD: Boolean read FRD write FRD;
    {* 期望递归，查询用，1 表示客户端需要递归}
    property RA: Boolean read FRA write FRA;
    {* 递归可用，应答用，1 表示可得到递归应答}
    property RCode: Integer read FRCode write FRCode;
    {* 返回码，0 ~ 5，0 表示没错}

    property QD[Index: Integer]: TCnDNSQuestion read GetQD;
    {* 查询对象列表}
    property AN[Index: Integer]: TCnDNSResourceRecord read GetAN;
    {* 应答对象列表}
    property NS[Index: Integer]: TCnDNSResourceRecord read GetNS;
    {* 域名服务器列表}
    property AR[Index: Integer]: TCnDNSResourceRecord read GetAR;
    {* 附加信息对象列表}

    property IsQuery: Boolean read FIsQuery write FIsQuery;
    {* 是否是查询包}
    property IsResponse: Boolean read FIsResponse write FIsResponse;
    {* 是否是应答包}

    property QDCount: Integer read FQDCount write FQDCount;
    {* 查询的数量}
    property ANCount: Integer read FANCount write FANCount;
    {* 应答的数量}
    property NSCount: Integer read FNSCount write FNSCount;
    {* 域名服务器的数量}
    property ARCount: Integer read FARCount write FARCount;
    {* 附加信息的数量}
  end;

  TCnDNSResponseEvent = procedure(Sender: TObject; Response: TCnDNSPacketObject) of object;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnDNS = class(TCnComponent)
  {* DNS 收发包组件}
  private
    FUDP: TCnUDP;
    FNameServerPort: Integer;
    FNameServerIP: string;
    FOnResponse: TCnDNSResponseEvent;
    procedure SetNameServerIP(const Value: string);
    procedure SetNameServerPort(const Value: Integer);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
    procedure Loaded; override;
    procedure UDPDataReceived(Sender: TComponent; Buffer: Pointer;
      Len: Integer; const FromIP: string; Port: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function BuildDNSQueryPacket(const Name: string; RandId: Word;
      QueryType: Word = CN_DNS_TYPE_A; QueryClass: Word = CN_DNS_CLASS_IN): TBytes;
    {* 构造一简单的单一域名查询报文}

    class function ParseIndexedString(var StrResult: string; Base, StrData: PAnsiChar;
      MaxLen: Integer = 0): Integer;
    {* 解析通过索引或字符串长度串联起来的数据，表示从 StrData 开始扫描，最大不超过 StrData + MaxLen
      Base 为本 DNS 数据包起始地址。如果 MaxLen 为 0，则表示以 #0 结尾。
      返回值为本次前进的长度；本次解析结果拼在 StrResult 后面}

    class function BuildNameBuffer(const Name: string; Buf: PAnsiChar): PAnsiChar;
    {* 将域名构造成分点号并加上长度字节的格式，Buf 所指的缓冲区应该至少有 Length(Name) + 2 的字节长度，
      返回 Buf 放满 Name 内容与 #0 结束符之后的地址，供调用者放其他参数}

    class function ParseDNSResponsePacket(const Response: PAnsiChar; ResponseByteLen: Integer;
      Packet: TCnDNSPacketObject): Boolean;
    {* 解析回应包，将内容放入 Packet 对象}

    procedure SendHostQuery(const Name: string; QueryType: Word = CN_DNS_TYPE_A;
      QueryClass: Word = CN_DNS_CLASS_IN; ID: Word = 0);
    {* 发送一简单的主机查询请求，ID 如果为 0 则内部生成}

  published
    property NameServerIP: string read FNameServerIP write SetNameServerIP;
    {* DNS 服务器 IP}
    property NameServerPort: Integer read FNameServerPort write SetNameServerPort;
    {* DNS 服务器端口，默认 53}
    property OnResponse: TCnDNSResponseEvent read FOnResponse write FOnResponse;
    {* 收到 DNS 回应包时触发的事件}
  end;

implementation

{ TCnDNSPacketObject }

function TCnDNSPacketObject.AddAdditionalRecord: TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord.Create;
  FARList.Add(Result);
end;

function TCnDNSPacketObject.AddAnswer: TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord.Create;
  FANList.Add(Result);
end;

function TCnDNSPacketObject.AddNameServer: TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord.Create;
  FNSList.Add(Result);
end;

function TCnDNSPacketObject.AddQuestion: TCnDNSQuestion;
begin
  Result := TCnDNSQuestion.Create;
  FQDList.Add(Result);
end;

constructor TCnDNSPacketObject.Create;
begin
  inherited;
  FQDList := TObjectList.Create;
  FANList := TObjectList.Create;
  FNSList := TObjectList.Create;
  FARList := TObjectList.Create;
end;

destructor TCnDNSPacketObject.Destroy;
begin
  FARList.Free;
  FNSList.Free;
  FANList.Free;
  FQDList.FRee;
  inherited;
end;

procedure TCnDNSPacketObject.DumpToStrings(List: TStrings);
var
  Q: string;
  I: Integer;
begin
  if IsQuery then
    Q := 'Query'
  else if IsResponse then
    Q := 'Response'
  else
    Q := '';

  List.Add(Format('DNS Packet %s. Id %d, OpCode %d, RCode %d.', [Q, FId, FOpCode, FRCode]));
  List.Add(Format('AA %d, TC %d, RD %d, RA %d, QNCount %d, ANCount %d, NSCount %d, ARCount %d.',
    [Integer(FAA), Integer(FTC), Integer(FRD), Integer(FRA),
    FQDCount, FANCount, FNSCount, FARCount]));

  for I := 0 to FQDCount - 1 do
  begin
    List.Add(Format('Query #%d:', [I + 1]));
    QD[I].DumpToStrings(List);
  end;

  for I := 0 to FANCount - 1 do
  begin
    List.Add(Format('Answer #%d:', [I + 1]));
    AN[I].DumpToStrings(List);
  end;

  for I := 0 to FNSCount - 1 do
  begin
    List.Add(Format('Nameserver #%d:', [I + 1]));
    NS[I].DumpToStrings(List);
  end;

  for I := 0 to FARCount - 1 do
  begin
    List.Add(Format('Additional Resource #%d:', [I + 1]));
    AR[I].DumpToStrings(List);
  end;
end;

function TCnDNSPacketObject.GetAN(Index: Integer): TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord(FANList[Index]);
end;

function TCnDNSPacketObject.GetAR(Index: Integer): TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord(FARList[Index]);
end;

function TCnDNSPacketObject.GetNS(Index: Integer): TCnDNSResourceRecord;
begin
  Result := TCnDNSResourceRecord(FNSList[Index]);
end;

function TCnDNSPacketObject.GetQD(Index: Integer): TCnDNSQuestion;
begin
  Result := TCnDNSQuestion(FQDList[Index]);
end;

{ TCnDNS }

constructor TCnDNS.Create(AOwner: TComponent);
begin
  inherited;
  FNameServerPort := 53;
  FUDP := TCnUDP.Create(Self);
  FUDP.RemotePort := FNameServerPort;
  FUDP.OnDataReceived := UDPDataReceived;

  if not (csDesigning in ComponentState) then
    FUDP.UpdateBinding;
end;

destructor TCnDNS.Destroy;
begin
  FUDP.Free;
  inherited;
end;

procedure TCnDNS.SetNameServerIP(const Value: string);
begin
  FNameServerIP := Value;
  FUDP.RemoteHost := Value;
end;

procedure TCnDNS.SetNameServerPort(const Value: Integer);
begin
  FNameServerPort := Value;
  FUDP.RemotePort := Value;
end;

class function TCnDNS.BuildNameBuffer(const Name: string; Buf: PAnsiChar): PAnsiChar;
var
  P: PChar;
  Q: PAnsiChar;
  Len: Byte;
begin
  Result := Buf;
  if (Length(Name) <= 1) or (Length(Name) > 63) then
    Exit;

  P := @Name[1];
  Q := Buf;
  Inc(Q);

  while P ^ <> #0 do
  begin
    Len := 0;
    while (P^ <> '.') and (P^ <> #0) do
    begin
      Q^ := AnsiChar(P^);
      Inc(Len);
      Inc(P);
      Inc(Q);
    end;
    if P^ = #0 then
    begin
      Q^ := #0;
      Buf^ := AnsiChar(Chr(Len));
      Inc(Buf, Len + 2); // Buf 指向 #0 的后一个位置
      Result := Buf;
      Exit;
    end
    else // 碰到点了
    begin
      Buf^ := AnsiChar(Chr(Len));  // 写前面的长度
      Inc(Buf, Len + 1); // Buf 指向下个空
    end;
    Inc(P);
    Inc(Q);
  end;
end;

class function TCnDNS.BuildDNSQueryPacket(const Name: string; RandId: Word; QueryType: Word;
  QueryClass: Word): TBytes;
var
  Head: PCnDNSHeader;
  P: PAnsiChar;
  PW: PWORD;
begin
  Result := nil;
  if (Length(Name) <= 1) or (Length(Name) > 63) then
    Exit;

  SetLength(Result, SizeOf(TCnDNSHeader) + Length(Name) + SizeOf(Byte) + 2 * SizeOf(Word));
  Head := PCnDNSHeader(@Result[0]);
  CnSetDNSHeaderId(Head, RandId);  // 查询 ID 号
  CnSetDNSHeaderQR(Head, True);    // 是查询类型
  CnSetDNSHeaderRD(Head, True);    // 是期望递归类型
  CnSetDNSHeaderQDCount(Head, 1);  // 就一个查询

  P := PAnsiChar(@Head^.SectionData[0]);
  PW := PWORD(BuildNameBuffer(Name, P));
  if PW = nil then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  PW^ := UInt16HostToNetwork(QueryType);
  Inc(PW);
  PW^ := UInt16HostToNetwork(QueryClass);
end;

class function TCnDNS.ParseIndexedString(var StrResult: string; Base, StrData: PAnsiChar; MaxLen: Integer): Integer;
var
  PB: PByte;
  B: Byte;
  Idx, Len: Integer;
  Str: AnsiString;
  First: Boolean;
begin
  if MaxLen > 0 then // 有长度限制，一般用于 RDLength/RData 场合
  begin
    PB := PByte(StrData);
    Result := 0;
    while True do
    begin
      if PB^ = 0 then // 到尾巴了，退出
      begin
        Inc(Result);  // #0 是属于字符串里面的，所以要跳出去
        Exit;
      end
      else if (PB^ and $C0) = $C0 then // 高两位都为 1，表示是索引不是字符串
      begin
        B := PB^;
        Inc(PB);
        Inc(Result);
        if Result >= MaxLen then
          Exit;

        Idx := (Word(B and $3F) shl 8) or Word(PB^);
        if Base + Idx = StrData then  // 避免下一个指向本块，导致无限递归
          Exit;

        ParseIndexedString(StrResult, Base, Base + Idx);
        Inc(PB);
        Inc(Result);         // 指向下一个 2 字节
        if Result >= MaxLen then
          Exit;
      end
      else if (PB^ and $C0) = 0 then // 高两位都为 0，是字符串长度
      begin
        Len := PB^;              // 得到本轮长度
        SetLength(Str, Len + 1); // 前面的点也算上
        Str[1] := '.';           // 写点

        Inc(PB);                 // PB 指向本轮字符串，Len 为长度
        Inc(Result);
        if Result >= MaxLen then
          raise ECnDNSException.Create(SCnDNSTooLong);

        Move(PB^, Str[2], Len);       // Str 内容塞为 .xxxxx 这种

        Inc(PB, Len);            // PB 指向下一个长度或索引位置
        Inc(Result, Len);

        StrResult := StrResult + string(Str);
        SetLength(Str, 0);

        if Result >= MaxLen then
          Exit;
      end
      else
        raise ECnDNSException.CreateFmt(SCnDNSInvalidHeadByteFmt, [PB^, PB - Base]);
    end;
  end
  else // 无长度限制，可能就一个索引（无结束符），或字符串开头，可能有索引，碰到 #0 结束
  begin
    PB := PByte(StrData);
    Result := 0;
    First := True;

    while True do
    begin
      if PB^ = 0 then // 到尾巴了，退出
      begin
        Inc(Result);  // #0 是属于字符串里面的，所以要跳出去
        Exit;
      end
      else if (PB^ and $C0) = $C0 then // 高两位都为 1，表示是索引不是字符串
      begin
        B := PB^;
        Inc(PB);
        Inc(Result);

        Idx := (Word(B and $3F) shl 8) or Word(PB^);
        if Base + Idx = StrData then  // 避免下一个指向本块，导致无限递归
          Exit;

        ParseIndexedString(StrResult, Base, Base + Idx);
        Inc(PB);
        Inc(Result);         // 指向下一个 2 字节

        if First then // 如果第一个进来的就是索引，说明就一个
          Exit;
      end
      else if (PB^ and $C0) = 0 then // 高两位都为 0，是字符串长度
      begin
        First := False;

        Len := PB^;              // 得到本轮长度
        SetLength(Str, Len + 1); // 前面的点也算上
        Str[1] := '.';           // 写点

        Inc(PB);                 // PB 指向本轮字符串，Len 为长度
        Inc(Result);
        Move(PB^, Str[2], Len);  // Str 内容塞为 .xxxxx 这种

        Inc(PB, Len);            // PB 指向下一个长度或索引位置
        Inc(Result, Len);

        StrResult := StrResult + string(Str);
        SetLength(Str, 0);
      end
      else
        raise ECnDNSException.CreateFmt(SCnDNSInvalidHeadByteFmt, [PB^, PB - Base]);
    end;
  end;
end;

class function TCnDNS.ParseDNSResponsePacket(const Response: PAnsiChar;
  ResponseByteLen: Integer; Packet: TCnDNSPacketObject): Boolean;
var
  I: Integer;
  Head: PCnDNSHeader;
  Data: PAnsiChar;
  Q: TCnDNSQuestion;
  R: TCnDNSResourceRecord;

  // 解析一个 Question
  function ParseQuestion(QuestionData: PAnsiChar; Question: TCnDNSQuestion): PAnsiChar;
  var
    H: PCnDNSQuestionSectionAfterName;
    S: string;
    Len: Integer;
  begin
    Result := QuestionData;
    if (QuestionData <> nil) and (Question <> nil) then
    begin
      Len := ParseIndexedString(S, Response, QuestionData);
      Question.QName := S;
      Inc(QuestionData, Len);

      H := PCnDNSQuestionSectionAfterName(QuestionData);
      Question.QType := UInt16NetworkToHost(H^.QType);
      Question.QClass := UInt16NetworkToHost(H^.QClass);
      Result := QuestionData + SizeOf(TCnDNSQuestionSectionAfterName);
    end;
  end;

  // 解析一个 Resource Record
  function ParseResourceRecord(ResourceRecordData: PAnsiChar; Resource: TCnDNSResourceRecord): PAnsiChar;
  var
    H: PCnDNSResourceRecordAfterName;
    S: string;
    Len: Integer;
  begin
    Result := ResourceRecordData;
    if (ResourceRecordData <> nil) and (Resource <> nil) then
    begin
      S := '';
      Len := ParseIndexedString(S, Response, ResourceRecordData);
      Resource.RName := S;
      Inc(ResourceRecordData, Len);

      H := PCnDNSResourceRecordAfterName(ResourceRecordData);
      Resource.RType := UInt16NetworkToHost(H^.RType);
      Resource.RClass := UInt16NetworkToHost(H^.RClass);
      Resource.TTL := UInt32NetworkToHost(H^.TTL);
      Resource.RDLength := UInt16NetworkToHost(H^.RDLength);
      if Resource.RDLength = SizeOf(Cardinal) then
      begin
        // 4 字节的应答数据是 IP 地址
{$IFDEF MSWINDOWS}
        Resource.IP := UInt32NetworkToHost((PDWORD(@H^.RData[0]))^);
{$ELSE}
        Resource.IP := UInt32NetworkToHost((PCardinal(@H^.RData[0]))^);
{$ENDIF}
      end
      else
      begin
        // 其他长度当作字符串解析
        S := '';
        ParseIndexedString(S, Response, @H^.RData[0], Resource.RDLength);
        Resource.RDString := S;
      end;

      Result := ResourceRecordData + SizeOf(TCnDNSResourceRecordAfterName) + Resource.RDLength - 1;
      // 减一是因为 TCnDNSResourceRecordAfterName 最后已经有个 1 字节的 RData 声明
    end;
  end;

begin
  Result := False;
  if (Response = nil) or (ResponseByteLen < SizeOf(TCnDNSHeader)) or (Packet = nil) then
    Exit;

  Head := PCnDNSHeader(Response);
  Packet.Id := CnGetDNSHeaderId(Head);
  Packet.IsQuery := CnGetDNSHeaderQR(Head) = CN_DNS_HEADER_TYPE_QUERY;
  Packet.IsResponse := CnGetDNSHeaderQR(Head) = CN_DNS_HEADER_TYPE_RESPONSE;
  Packet.OpCode := CnGetDNSHeaderOpCode(Head);
  Packet.AA := CnGetDNSHeaderAA(Head);
  Packet.TC := CnGetDNSHeaderTC(Head);
  Packet.RA := CnGetDNSHeaderRA(Head);
  Packet.RD := CnGetDNSHeaderRD(Head);
  Packet.RCode := CnGetDNSHeaderRCode(Head);

  Packet.QDCount := CnGetDNSHeaderQDCount(Head);
  Packet.ANCount := CnGetDNSHeaderANCount(Head);
  Packet.NSCount := CnGetDNSHeaderNSCount(Head);
  Packet.ARCount := CnGetDNSHeaderARCount(Head);

  // 解析包头后部的可变部分，先按 Question 解析 QD，再按 Resource Record 解析其余仨
  Data := PAnsiChar(@Head^.SectionData[0]);
  I := 1;
  while I <= Packet.QDCount do
  begin
    // 解析 QD 里的 Question 们
    Q := Packet.AddQuestion;
    Data := ParseQuestion(Data, Q);
    Inc(I);
  end;

  I := 1;
  while I <= Packet.ANCount do
  begin
    // 解析 AN 里的 Resource Record 们
    R := Packet.AddAnswer;
    Data := ParseResourceRecord(Data, R);
    Inc(I);
  end;

  I := 1;
  while I <= Packet.NSCount do
  begin
    // 解析 NS 里的 Resource Record 们
    R := Packet.AddNameServer;
    Data := ParseResourceRecord(Data, R);
    Inc(I);
  end;

  I := 1;
  while I <= Packet.ARCount do
  begin
    // 解析 AR 里的 Resource Record 们
    R := Packet.AddAdditionalRecord;
    Data := ParseResourceRecord(Data, R);
    Inc(I);
  end;
end;

procedure TCnDNS.Loaded;
begin
  inherited;
  FUDP.UpdateBinding;
end;

procedure TCnDNS.SendHostQuery(const Name: string; QueryType, QueryClass,
  ID: Word);
var
  Buf: TBytes;
begin
  if ID = 0 then
  begin
    Randomize;
    ID := Trunc(Random * 65535);
  end;

  Buf := TCnDNS.BuildDNSQueryPacket(Name, ID, QueryType, QueryClass);
  FUDP.SendBuffer(@Buf[0], Length(Buf));
  SetLength(Buf, 0);
end;

procedure TCnDNS.UDPDataReceived(Sender: TComponent; Buffer: Pointer;
  Len: Integer; const FromIP: string; Port: Integer);
var
  Packet: TCnDNSPacketObject;
begin
  if Assigned(FOnResponse) then
  begin
    Packet := TCnDNSPacketObject.Create;
    try
      ParseDNSResponsePacket(PAnsiChar(Buffer), Len, Packet);
      FOnResponse(Self, Packet);
    finally
      Packet.Free;
    end;
  end;
end;

procedure TCnDNS.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnDNSName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnDNSComment;
end;

{ TCnDNSResourceRecord }

procedure TCnDNSResourceRecord.DumpToStrings(List: TStrings);
begin
  if FRDString = '' then
    List.Add(Format('RType %d, RClass %d, RName: %s, RDLength %d, TTL %d, IP: %s',
      [FRType, FRClass, FRName, FRDLength, FTTL, TCnIp.IntToIP(FIP)]))
  else
    List.Add(Format('RType %d, RClass %d, RName: %s, RDLength %d, TTL %d, RDString: %s',
      [FRType, FRClass, FRName, FRDLength, FTTL, FRDString]));
end;

{ TCnDNSQuestion }

procedure TCnDNSQuestion.DumpToStrings(List: TStrings);
begin
  List.Add(Format('QType %d, QClass %d, QName: %s', [FQType, FQClass, FQName]));
end;

end.
