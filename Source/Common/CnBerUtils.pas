{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnBerUtils;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：处理 ASN.1 的 BER 编码单元
* 单元作者：刘啸
* 备    注：
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2018.05.27 V1.1
*               将 Parser 改为 Reader 并实现 Writer
*           2018.05.24 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, TypInfo, CnBigNumber, CnTree
  {$IFDEF DEBUG}, ComCtrls {$ENDIF};

const
  CN_BER_TAG_TYPE_MASK                      = $C0;
  // 最高两位：00 为 Universal，01 为 Application，10 为 Context-Specific，11 为 Private

  CN_BER_TAG_STRUCT_MASK                    = $20;
  CN_BER_TAG_VALUE_MASK                     = $1F;
  CN_BER_LENLEN_MASK                        = $80;
  CN_BER_LENGTH_MASK                        = $7F;

  CN_BER_TAG_RESERVED                       = $00;
  CN_BER_TAG_BOOLEAN                        = $01;
  CN_BER_TAG_INTEGER                        = $02;
  CN_BER_TAG_BIT_STRING                     = $03;
  CN_BER_TAG_OCTET_STRING                   = $04;
  CN_BER_TAG_NULL                           = $05;
  CN_BER_TAG_OBJECT_IDENTIFIER              = $06;
  CN_BER_TAG_OBJECT_DESCRIPION              = $07;
  CN_BER_TAG_EXTERNAL                       = $08;
  CN_BER_TAG_REAL                           = $09;
  CN_BER_TAG_ENUMERATED                     = $0A;
  CN_BER_TAG_EMBEDDED_PDV                   = $0B;
  CN_BER_TAG_UFT8STRING                     = $0C;
  CN_BER_TAG_RELATIVE_OID                   = $0D;

  CN_BER_TAG_SEQUENCE                       = $10;
  CN_BER_TAG_SET                            = $11;
  CN_BER_TAG_NUMERICSTRING                  = $12;
  CN_BER_TAG_PRINTABLESTRING                = $13;
  CN_BER_TAG_TELETEXSTRING                  = $14;  // T61String
  CN_BER_TAG_VIDEOTEXSTRING                 = $15;
  CN_BER_TAG_IA5STRING                      = $16;
  CN_BER_TAG_UTCTIME                        = $17;
  CN_BER_TAG_GENERALIZEDTIME                = $18;
  CN_BER_TAG_GRAPHICSTRING                  = $19;
  CN_BER_TAG_VISIBLESTRING                  = $1A;
  CN_BER_TAG_GENERALSTRING                  = $1B;
  CN_BER_TAG_UNIVERSALSTRING                = $1C;
  CN_BER_TAG_CHARACTER_STRING               = $1D;
  CN_BER_TAG_BMPSTRING                      = $1E;

type
  TCnBerTagRange = CN_BER_TAG_BOOLEAN..CN_BER_TAG_BMPSTRING;
  TCnBerTagSet = set of TCnBerTagRange;

  TCnBerTag = (cbtReserved_0, cbtBoolean, cbtInteger, cbtBit_String,
    cbtOctet_String, cbtNull, cbtObject_Identifier, cbtObject_Descripion,
    cbtExternal, cbtReal, cbtEnumerated, cbtEmbedded_Pdv, cbtUft8String,
    cbtRelative_Oid, cbtReserved_0E, cbtReserved_0F, cbtSequence, cbtSet,
    cbtNumericString, cbtPrintableString, cbtTeletexString, cbtVideotexString,
    cbtIa5String, cbtUtcTime, cbtGeneralizedTime, cbtGraphicString,
    cbtVisibleString, cbtGeneralString, cbtUniversalString, cbtCharacter_String,
    cbtBmpstring);

  TCnBerTags = set of TCnBerTag;

  TCnBerReadNode = class(TCnLeaf)
  {* 描述一解析出来的 ASN.1 节点}
  private
    FOriginData: PByte;
    FBerLength: Integer;
    FBerOffset: Integer;
    FBerTag: Integer;
    FBerDataLength: Integer;
    FBerDataOffset: Integer;
    function GetItems(Index: Integer): TCnBerReadNode;
    procedure SetItems(Index: Integer; const Value: TCnBerReadNode);

    function InternalAsInteger(ByteSize: Integer): Integer;
    function InternalAsString(TagSet: TCnBerTagSet): AnsiString;
    function GetBerDataAddress: Pointer;
    function GetBerAddress: Pointer;
  public
    procedure CopyDataTo(DestBuf: Pointer);
    {* 将数据复制至缓冲区，缓冲区尺寸至少需要 BerDataLength 大}
    procedure CopyHeadTo(DestBuf: Pointer);
    {* 将节点头部也就是 TL 内容复制至缓冲区，缓冲区尺寸至少需要 BerLength - BerDataLength 大}
    procedure CopyTLVTo(DestBuf: Pointer);
    {* 将节点全部内容复制至缓冲区，，缓冲区尺寸至少需要 BerLength 大}

    function AsBoolean: Boolean;
    function AsShortInt: ShortInt;
    function AsByte: Byte;
    function AsSmallInt: SmallInt;
    function AsWord: Word;
    function AsInteger: Integer;
    function AsCardinal: Cardinal;
    function AsInt64: Int64;
    procedure AsBigNumber(OutNum: TCnBigNumber);
    {* 按尺寸返回整型值或大数}

    function AsString: string;
    {* 返回字符串，可能是以下几种类型}
    function AsPrintableString: string;
    {* 返回可打印字符串}
    function AsIA5String: string;
    {* 返回纯 ASCII 码的字符串}

    function AsDateTime: TDateTime;
    {* 返回 UTCTime 或 GeneralizedTime}

    function IsNull: Boolean;
    function IsString: Boolean;
    function IsInteger: Boolean;
    function IsDateTime: Boolean;

    function GetNextSibling: TCnBerReadNode;
    function GetPrevSibling: TCnBerReadNode;

    property Items[Index: Integer]: TCnBerReadNode read GetItems write SetItems; default;

    property BerOffset: Integer read FBerOffset write FBerOffset;
    {* 该节点对应的 ASN.1 内容编码在整体中的偏移}
    property BerAddress: Pointer read GetBerAddress;
    {* 整个节点的内容起始地址，也就是 FOriginData + FBerOffset}
    property BerLength: Integer read FBerLength write FBerLength;
    {* 整个节点的内容长度}

    property BerTag: Integer read FBerTag write FBerTag;
    {* 节点类型，也就是 Tag}
    property BerDataLength: Integer read FBerDataLength write FBerDataLength;
    {* 节点数据长度}
    property BerDataOffset: Integer read FBerDataOffset write FBerDataOffset;
    {* 该节点对应的数据内容在整体中的偏移}
    property BerDataAddress: Pointer read GetBerDataAddress;
    {* 该节点对应的数据的起始地址，等于 FOriginData + FBerDataOffset}

  end;

  TCnBerReader = class(TObject)
  {* 读取并解析 BER 编码数据块的解析器类}
  private
    FBerTree: TCnTree;
    FData: PByte;
    FDataLen: Cardinal;
    FParseInnerString: Boolean;
{$IFDEF DEBUG}
    function GetOnSaveNode: TCnTreeNodeEvent;
    procedure SetOnSaveNode(const Value: TCnTreeNodeEvent);
{$ENDIF}
    function GetTotalCount: Integer;
    function GetItems(Index: Integer): TCnBerReadNode;
    procedure ParseArea(Parent: TCnLeaf; AData: PByteArray;
      ADataLen: Cardinal; AStartOffset: Cardinal);
    {* 解析一段数据，该数据里的所有 ASN.1 节点均序次挂在 Parent 节点下}
  protected

  public
    constructor Create(Data: PByte; DataLen: Cardinal; AParseInnerString: Boolean = False);
    destructor Destroy; override;

    procedure ParseToTree;
    {* 创建后需要调用此方法实施解析}
    procedure ManualParseNodeData(RootNode: TCnBerReadNode);
    {* 某些节点的 Tag 并非 SEQUENCE/SET 等但内容却有子内容，需要外部手工调用此方法来实施二次解析}

{$IFDEF DEBUG}
    procedure DumpToTreeView(ATreeView: TTreeView);
    property OnSaveNode: TCnTreeNodeEvent read GetOnSaveNode write SetOnSaveNode;
{$ENDIF}

    property ParseInnerString: Boolean read FParseInnerString;
    {* 是否将 BitString/OctetString 类型也当作复合类型来解析，PKCS#8 的 Pem 文件中常见}
    property TotalCount: Integer read GetTotalCount;
    {* 解析出来的 ASN.1 节点总数}
    property Items[Index: Integer]: TCnBerReadNode read GetItems;
    {* 顺序访问所有解析出来的 ASN.1 节点，下标从 0 开始，不包括 Tree 自身的 Root}
  end;

  TCnBerWriteNode = class(TCnLeaf)
  {* 描述一用于编码并写入的 ASN.1 节点}
  private
    FMem: TMemoryStream; // 容纳基本类型节点的所有内容，不只是数据区
    FHead: array[0..5] of Byte; // 容纳数据区之前的头内容，包括 Tag、Len 等
    FHeadLen: Integer;
    FIsContainer: Boolean;
    FDataLength: Integer;
    FData: Pointer;
    FBerTag: Integer;
    function GetIsContainer: Boolean;
    procedure SetIsContainer(const Value: Boolean);
    function GetItems(Index: Integer): TCnBerWriteNode;
    procedure SetItems(Index: Integer; const Value: TCnBerWriteNode);

    procedure FillHeadCalcLen(ATag, ADataLen: Integer); // 计算并填充 FHead 与 FHeadLen
  public
    constructor Create(ATree: TCnTree); override;
    destructor Destroy; override;

    function SaveToStream(Stream: TStream): Integer;
    {* 如果是基本类型就将自己写入流并返回写入长度，
       如果是容器则挨个让子节点写出来，然后加自己头来拼成流并拼各子节点的返回长度。
       返回值为本节点包括子节点的所有内容长度}

    function SaveValueToStream(Stream: TStream): Integer;
    {* 如果是基本类型就将自己除了 Tag 与长度之外后面的数据内容写入流并返回写入长度。
       如果是容器则挨个让子节点写出来后返回。
       返回值为子节点所有内容长度但不包括自己的 Tag 与长度}

    function GetNodeLength: Integer;
    {* 如果是基本类型就返回自身长度，如果是容器则自己头加各子节点长度}

    procedure FillBasicNode(ATag: Integer; Data: PByte; DataLen: Integer);
    {* 外界创建此基本节点后用此方法填充基本数据，Container 节点不用，
       注意原始 BitString 不支持头字节，暂不需要自己填充}

    property Items[Index: Integer]: TCnBerWriteNode read GetItems write SetItems;

    property Data: Pointer read FData write FData;
    property DataLength: Integer read FDataLength write FDataLength;
    property IsContainer: Boolean read GetIsContainer write SetIsContainer;

    property BerTag: Integer read FBerTag write FBerTag;
    {* 节点类型，也就是 Tag}
  end;

  TCnBerWriter = class(TObject)
  {* 写 BER 编码的数据的工具类}
  private
    FBerTree: TCnTree;
    function GetTotalSize: Integer;
{$IFDEF DEBUG}
    function GetOnSaveNode: TCnTreeNodeEvent;
    procedure SetOnSaveNode(const Value: TCnTreeNodeEvent);
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveTo(DestBuf: Pointer);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);

{$IFDEF DEBUG}
    procedure DumpToTreeView(ATreeView: TTreeView);
    property OnSaveNode: TCnTreeNodeEvent read GetOnSaveNode write SetOnSaveNode;
{$ENDIF}

    function AddNullNode(Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* 添加一个 Null 节点}
    function AddBasicNode(ATag: Integer; AData: PByte; DataLen: Integer;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode; overload;
    {* 添加一个基本类型的节点，内容从 AData 复制长度为 DataLen 的而来}
    function AddBasicNode(ATag: Integer; AStream: TStream;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode; overload;
    {* 添加一个基本类型的节点，内容从指定流复制而来}
    function AddAnsiStringNode(ATag: Integer; const AStr: AnsiString;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* 添加一个字符串型的 Node，内容从指定 AnsiString 复制而来}
    function AddContainerNode(ATag: Integer; Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* 添加一个容器类型的节点，此节点可以作为上面 BasicNode 的 Parent}
    function AddRawNode(RawTag: Integer; RawLV: PByte; LVLen: Integer;
      Parent: TCnBerWriteNode = nil): TCnBerWriteNode;
    {* 添加一个原始节点，此节点的 Tag 值直接由 RawTag 指定，
       后面的长度内容等不计算了，直接由 RawLV 与 LVLen 的区域指定}
    property TotalSize: Integer read GetTotalSize;
  end;

function CompareObjectIdentifier(Node: TCnBerReadNode; OIDAddr: Pointer;
  OIDSize: Integer): Boolean;
{* 比较一个 Node 中的数据是否等于一个指定的 OID}

implementation

const
  CN_TAG_SET_STRING: TCnBerTagSet = [CN_BER_TAG_NUMERICSTRING, CN_BER_TAG_PRINTABLESTRING,
    CN_BER_TAG_IA5STRING, CN_BER_TAG_TELETEXSTRING];

  CN_TAG_SET_TIME: TCnBerTagSet = [CN_BER_TAG_UTCTIME, CN_BER_TAG_GENERALIZEDTIME];

function GetTagName(Tag: Integer): string;
begin
  Result := 'Invalid';
  if Tag in [Ord(Low(TCnBerTag))..Ord(High(TCnBerTag))] then
  begin
    Result := GetEnumName(TypeInfo(TCnBerTag), Tag);
    if (Length(Result) > 3) and (Copy(Result, 1, 3) = 'cbt') then
      Delete(Result, 1, 3);
  end;
end;

function SwapWord(Value: Word): Word;
begin
  Result := ((Value and $FF00) shr 8) or ((Value and $00FF) shl 8);
end;

function SwapLongWord(Value: LongWord): LongWord;
begin
  Result := ((Value and $000000FF) shl 24) or ((Value and $0000FF00) shl 8)
    or ((Value and $00FF0000) shr 8) or ((Value and $FF000000) shr 24);
end;

function SwapInt64(Value: Int64): Int64;
var
  Lo, Hi: LongWord;
  Rec: Int64Rec;
begin
  Lo := Int64Rec(Value).Lo;
  Hi := Int64Rec(Value).Hi;
  Lo := SwapLongWord(Lo);
  Hi := SwapLongWord(Hi);
  Rec.Lo := Hi;
  Rec.Hi := Lo;
  Result := Int64(Rec);
end;

function CompareObjectIdentifier(Node: TCnBerReadNode; OIDAddr: Pointer;
  OIDSize: Integer): Boolean;
var
  P: Pointer;
begin
  Result := False;
  if (Node <> nil) then
  begin
    P := Node.BerDataAddress;
    if (P <> nil) and (OIDAddr <> nil) and (OIDSize > 0) then
    begin
      if OIDSize = Node.BerDataLength then
        Result := CompareMem(OIDAddr, P, OIDSize);
    end;
  end;
end;

{ TCnBerReader }

constructor TCnBerReader.Create(Data: PByte; DataLen: Cardinal;
  AParseInnerString: Boolean);
begin
  FData := Data;
  FDataLen := DataLen;
  FParseInnerString := AParseInnerString;
  FBerTree := TCnTree.Create(TCnBerReadNode);
end;

destructor TCnBerReader.Destroy;
begin
  FBerTree.Free;
  inherited;
end;

{$IFDEF DEBUG}

procedure TCnBerReader.DumpToTreeView;
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerReader.GetOnSaveNode: TCnTreeNodeEvent;
begin
  Result := FBerTree.OnSaveANode;
end;

procedure TCnBerReader.SetOnSaveNode(const Value: TCnTreeNodeEvent);
begin
  FBerTree.OnSaveANode := Value;
end;

{$ENDIF}

function TCnBerReader.GetItems(Index: Integer): TCnBerReadNode;
begin
  Result := TCnBerReadNode(FBerTree.Items[Index + 1]);
end;

function TCnBerReader.GetTotalCount: Integer;
begin
  Result := FBerTree.Root.AllCount;
end;

procedure TCnBerReader.ParseArea(Parent: TCnLeaf; AData: PByteArray;
  ADataLen: Cardinal; AStartOffset: Cardinal);
var
  Run, Start: Cardinal;
  Tag, DataLen, DataOffset, LenLen, Delta: Integer;
  B: Byte;
  IsStruct: Boolean;
  ALeaf: TCnBerReadNode;
begin
  Run := 0;  // Run 是基于 AData 起始处的偏移量

  while Run < ADataLen do
  begin
    B := AData[Run];

    if B = $FF then
      Exit;

    Start := Run;

    // 处理 Tag 类型
    IsStruct := (B and CN_BER_TAG_STRUCT_MASK) <> 0;
    Tag := B and CN_BER_TAG_VALUE_MASK;

    Inc(Run);
    if Run >= ADataLen then
      raise Exception.CreateFmt('Data Corruption when Processing Tag (Base %d), %d > %d.',
        [AStartOffset, Run, ADataLen]);

    // Run 指向长度，处理长度
    Delta := 1;  // 1 表示 Tag 所占字节
    B := AData[Run];
    if (B and CN_BER_LENLEN_MASK) = 0 then
    begin
      // 本字节就是长度
      DataLen := B;
      DataOffset := AStartOffset + Run + 1;
      Inc(Delta); // 加上长度的这一字节
      Inc(Run);   // Run 指向数据
    end
    else
    begin
      // 本字节高位为 1，表示长度的长度
      LenLen := B and CN_BER_LENGTH_MASK;
      Inc(Delta); // 加上长度的长度这一字节
      Inc(Run);   // Run 指向长度

      // AData[Run] 到 AData[Run + LenLen - 1] 是长度
      if Run + Cardinal(LenLen) - 1 >= ADataLen then
        raise Exception.CreateFmt('Data Corruption when Processing Tag (Base %d) at %d Got Len %d.',
          [AStartOffset, Run, LenLen]);

      if LenLen = SizeOf(Byte) then
        DataLen := AData[Run]
      else if LenLen = SizeOf(Word) then
        DataLen := (Cardinal(AData[Run]) shl 8) or Cardinal(AData[Run + 1])
      else // if LenLen > SizeOf(Word) then
        raise Exception.CreateFmt('Length Too Long (Base %d) %d.', [AStartOffset, LenLen]);

      DataOffset := AStartOffset + Run + Cardinal(LenLen);
      Inc(Delta, LenLen);
      Inc(Run, LenLen);   // Run 指向数据
    end;

    // Tag, Len, DataOffset 都齐全了，Delta 是数据起始区与当前节点起始区的偏移
    if Parent = nil then
      Parent := FBerTree.Root;

    ALeaf := FBerTree.AddChild(Parent) as TCnBerReadNode;
    ALeaf.FOriginData := FData;

    ALeaf.BerOffset := AStartOffset + Start;
    ALeaf.BerLength := DataLen + Delta;
    ALeaf.BerTag := Tag;
    ALeaf.BerDataLength := DataLen;
    ALeaf.BerDataOffset := DataOffset;

{$IFDEF DEBUG}
    ALeaf.Text := Format('Offset %d. Len %d. Tag %d (%s). DataLen %d', [ALeaf.BerOffset,
      ALeaf.BerLength, ALeaf.BerTag, GetTagName(ALeaf.BerTag), ALeaf.BerDataLength]);
{$ENDIF}

    if IsStruct or (FParseInnerString and (ALeaf.BerTag in [CN_BER_TAG_BIT_STRING,
      CN_BER_TAG_OCTET_STRING])) then
    begin
      // 说明 BerDataOffset 到 BerDataLength 内可能有子节点
      try
        if ALeaf.BerTag = CN_BER_TAG_BIT_STRING then
        begin
          // BIT_STRING 数据区第一个内容字节是该 BIT_STRING 凑成 8 的倍数所缺少的 Bit 数，这里跳过
          ParseArea(ALeaf, PByteArray(Cardinal(AData) + Run + 1),
            ALeaf.BerDataLength - 1, ALeaf.BerDataOffset + 1);
        end
        else
          ParseArea(ALeaf, PByteArray(Cardinal(AData) + Run),
            ALeaf.BerDataLength, ALeaf.BerDataOffset);
      except
        ; // 如果内嵌解析失败，不终止，当做普通节点
      end;
    end;

    Inc(Run, DataLen);
  end;
end;

procedure TCnBerReader.ParseToTree;
begin
  ParseArea(FBerTree.Root, PByteArray(FData), FDataLen, 0);
end;

procedure TCnBerReader.ManualParseNodeData(RootNode: TCnBerReadNode);
begin
  RootNode.Clear;
  ParseArea(RootNode, PByteArray(RootNode.BerDataAddress), RootNode.BerDataLength, RootNode.BerDataOffset);
end;

{ TCnBerReadNode }

function TCnBerReadNode.AsPrintableString: string;
begin
  Result := InternalAsString([CN_BER_TAG_PRINTABLESTRING]);
end;

function TCnBerReadNode.InternalAsInteger(ByteSize: Integer): Integer;
var
  IntValue: Integer;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise Exception.Create('Ber Tag Type Mismatch for ByteSize: ' + IntToStr(ByteSize));

  if not (ByteSize in [SizeOf(Byte)..SizeOf(DWORD)]) then
    raise Exception.Create('Invalid ByteSize: ' + IntToStr(ByteSize));

  if FBerDataLength > ByteSize then
    raise Exception.CreateFmt('Data Length %d Overflow for Required %d.',
      [FBerDataLength, ByteSize]);

  IntValue := 0;
  CopyDataTo(@IntValue);

  // Byte 不需交换，SmallInt 交换两位，Integer 交换四位
  if ByteSize = SizeOf(Word) then
    IntValue := Integer(SwapWord(Word(IntValue)))
  else if ByteSize = SizeOf(DWORD) then
    IntValue := SwapLongWord(IntValue);
  Result := IntValue;
end;

function TCnBerReadNode.AsInt64: Int64;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise Exception.Create('Ber Tag Type Mismatch for Int64: ' + IntToStr(FBerTag));

  if FBerDataLength > SizeOf(Int64) then
    raise Exception.CreateFmt('Data Length %d Overflow for Required %d.',
      [FBerDataLength, SizeOf(Int64)]);

  Result := 0;
  CopyDataTo(@Result);
  Result := SwapInt64(Result);
end;

function TCnBerReadNode.AsByte: Byte;
begin
  Result := Byte(InternalAsInteger(SizeOf(Byte)));
end;

function TCnBerReadNode.AsCardinal: Cardinal;
begin
  Result := Cardinal(InternalAsInteger(SizeOf(Cardinal)));
end;

function TCnBerReadNode.AsInteger: Integer;
begin
  Result := Integer(InternalAsInteger(SizeOf(Integer)));
end;

function TCnBerReadNode.AsShortInt: ShortInt;
begin
  Result := ShortInt(InternalAsInteger(SizeOf(ShortInt)));
end;

function TCnBerReadNode.AsSmallInt: SmallInt;
begin
  Result := SmallInt(InternalAsInteger(SizeOf(SmallInt)));
end;

function TCnBerReadNode.AsWord: Word;
begin
  Result := Word(InternalAsInteger(SizeOf(Word)));
end;

procedure TCnBerReadNode.CopyDataTo(DestBuf: Pointer);
begin
  if (FOriginData <> nil) and (FBerDataLength > 0) then
    CopyMemory(DestBuf, Pointer(Integer(FOriginData) + FBerDataOffset), FBerDataLength);
end;

function TCnBerReadNode.GetItems(Index: Integer): TCnBerReadNode;
begin
  Result := inherited GetItems(Index) as TCnBerReadNode;
end;

procedure TCnBerReadNode.SetItems(Index: Integer; const Value: TCnBerReadNode);
begin
  inherited SetItems(Index, Value);
end;

function TCnBerReadNode.GetBerDataAddress: Pointer;
begin
  if FOriginData = nil then
    Result := nil
  else
    Result := Pointer(Integer(FOriginData) + FBerDataOffset);
end;

function TCnBerReadNode.GetNextSibling: TCnBerReadNode;
begin
  Result := TCnBerReadNode(inherited GetNextSibling);
end;

function TCnBerReadNode.GetPrevSibling: TCnBerReadNode;
begin
  Result := TCnBerReadNode(inherited GetPrevSibling);
end;

procedure TCnBerReadNode.CopyHeadTo(DestBuf: Pointer);
begin
  if FOriginData <> nil then
    CopyMemory(DestBuf, Pointer(Integer(FOriginData) + FBerOffset), FBerLength - FBerDataLength);
end;

procedure TCnBerReadNode.CopyTLVTo(DestBuf: Pointer);
begin
  if (FOriginData <> nil) and (FBerLength > 0) then
    CopyMemory(DestBuf, Pointer(Integer(FOriginData) + FBerOffset), FBerLength);
end;

function TCnBerReadNode.AsIA5String: string;
begin
  Result := InternalAsString([CN_BER_TAG_IA5STRING]);
end;

function TCnBerReadNode.AsString: string;
begin
  Result := InternalAsString(CN_TAG_SET_STRING + CN_TAG_SET_TIME);
end;

function TCnBerReadNode.AsDateTime: TDateTime;
var
  S: string;
begin
  S := InternalAsString(CN_TAG_SET_TIME);
  // TODO: YYMMDDhhmm 后面加 Z 或 ss 或 +- 时区

  Result := StrToDateTime(S);
  // TODO: 也可能是 Integer 的 Binary Time 格式，
  // 1970 年 1 月 1 日零时起的秒数，参考 rfc4049
end;

function TCnBerReadNode.InternalAsString(TagSet: TCnBerTagSet): AnsiString;
var
  P: Pointer;
begin
  if not FBerTag in TagSet then
    raise Exception.Create('Ber Tag Type Mismatch for String: ' + IntToStr(FBerTag));

  Result := '';
  P := GetBerDataAddress;
  if (P <> nil) and (BerDataLength > 0) then
  begin
    SetLength(Result, BerDataLength);
    CopyMemory(@Result[1], P, BerDataLength);
  end;
end;

function TCnBerReadNode.IsNull: Boolean;
begin
  Result := FBerTag = CN_BER_TAG_NULL;
end;

function TCnBerReadNode.IsDateTime: Boolean;
begin
  Result := FBerTag in CN_TAG_SET_TIME;
end;

function TCnBerReadNode.IsString: Boolean;
begin
  Result := FBerTag in (CN_TAG_SET_STRING + CN_TAG_SET_TIME);
end;

function TCnBerReadNode.IsInteger: Boolean;
begin
  Result := FBerTag = CN_BER_TAG_INTEGER;
end;

procedure TCnBerReadNode.AsBigNumber(OutNum: TCnBigNumber);
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise Exception.Create('Ber Tag Type Mismatch for BigNumber.');

  OutNum.SetBinary(GetBerDataAddress, FBerDataLength);
end;

function TCnBerReadNode.GetBerAddress: Pointer;
begin
  if FOriginData = nil then
    Result := nil
  else
    Result := Pointer(Integer(FOriginData) + FBerOffset);
end;

function TCnBerReadNode.AsBoolean: Boolean;
var
  B: Byte;
begin
  if (FBerTag <> CN_BER_TAG_BOOLEAN) and (FBerDataLength <> 1) then
    raise Exception.Create('Ber Tag Type Mismatch for Boolean: ' + IntToStr(FBerTag));

  CopyDataTo(@B);
  Result := B <> 0;
end;

{ TCnBerWriter }

function TCnBerWriter.AddBasicNode(ATag: Integer; AData: PByte;
  DataLen: Integer; Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.FIsContainer := False;
  Result.FillBasicNode(ATag, AData, DataLen);
end;

function TCnBerWriter.AddContainerNode(ATag: Integer;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.BerTag := ATag;
  Result.IsContainer := True;
end;

function TCnBerWriter.AddNullNode(Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.IsContainer := False;
  Result.FillBasicNode(CN_BER_TAG_NULL, nil, 0); // TODO: Null 的数据
end;

constructor TCnBerWriter.Create;
begin
  inherited;
  FBerTree := TCnTree.Create(TCnBerWriteNode);
end;

destructor TCnBerWriter.Destroy;
begin
  FBerTree.Free;
  inherited;
end;

{$IFDEF DEBUG}

procedure TCnBerWriter.DumpToTreeView(ATreeView: TTreeView);
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerWriter.GetOnSaveNode: TCnTreeNodeEvent;
begin
  Result := FBerTree.OnSaveANode;
end;

procedure TCnBerWriter.SetOnSaveNode(const Value: TCnTreeNodeEvent);
begin
  FBerTree.OnSaveANode := Value;
end;

{$ENDIF}

function TCnBerWriter.GetTotalSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FBerTree.Root.Count - 1 do
    Result := Result + TCnBerWriteNode(FBerTree.Root).Items[I].GetNodeLength;
end;

procedure TCnBerWriter.SaveTo(DestBuf: Pointer);
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    SaveToStream(Mem);
    Mem.Write(DestBuf^, Mem.Size);
  finally
    Mem.Free;
  end;
end;

procedure TCnBerWriter.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCnBerWriter.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  for I := 0 to FBerTree.Root.Count - 1 do
    TCnBerWriteNode(FBerTree.Root).Items[I].SaveToStream(Stream);
end;

function TCnBerWriter.AddRawNode(RawTag: Integer; RawLV: PByte;
  LVLen: Integer; Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  B: Byte;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;

  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.BerTag := RawTag;

  B := RawTag;
  Result.FMem.Write(B, 1);
  if (RawLV <> nil) and (LVLen > 0) then
    Result.FMem.Write(RawLV^, LVLen);
end;

function TCnBerWriter.AddBasicNode(ATag: Integer; AStream: TStream;
  Parent: TCnBerWriteNode): TCnBerWriteNode;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.LoadFromStream(AStream);
    Result := AddBasicNode(ATag, Mem.Memory, Mem.Size, Parent);
  finally
    Mem.Free;
  end;
end;

function TCnBerWriter.AddAnsiStringNode(ATag: Integer;
  const AStr: AnsiString; Parent: TCnBerWriteNode): TCnBerWriteNode;
begin
  if Parent = nil then
    Parent := FBerTree.Root as TCnBerWriteNode;
  
  Result := FBerTree.AddChild(Parent) as TCnBerWriteNode;
  Result.FIsContainer := False;
  Result.FillBasicNode(ATag, PByte(AStr), Length(AStr));
end;

{ TCnBerWriteNode }

procedure TCnBerWriteNode.FillHeadCalcLen(ATag, ADataLen: Integer);
var
  LenLen: Cardinal;
  B: Byte;
  W: Word;
  D: Cardinal;
begin
  FHeadLen := 0;
  if FIsContainer and (FBerTag in [CN_BER_TAG_SEQUENCE, CN_BER_TAG_SET]) then
    FHead[0] := ATag or CN_BER_TAG_STRUCT_MASK // 有子节点且是指定类型，高位置 1
  else
    FHead[0] := ATag;

  Inc(FHeadLen);
  if FBerTag = CN_BER_TAG_BIT_STRING then // BitString 塞进去要加个头字节
    Inc(ADataLen);

  if ADataLen <= 127 then // 单字节长度
  begin
    FHead[1] := ADataLen;
    Inc(FHeadLen);
  end
  else
  begin
    // 大于或等于 128，先求 LeafLen 的字节数
    if ADataLen < $100 then
    begin
      LenLen := 1;
      B := ADataLen;
      CopyMemory(@FHead[2], @B, LenLen);
    end
    else if ADataLen < $10000 then
    begin
      LenLen := 2;
      W := ADataLen;
      W := SwapWord(W);
      CopyMemory(@FHead[2], @W, LenLen);
    end
    else if ADataLen < $1000000 then
    begin
      LenLen := 3;
      D := ADataLen;
      D := SwapLongWord(D);
      D := D shr 8;
      CopyMemory(@FHead[2], @D, LenLen);
    end
    else
    begin
      LenLen := 4;
      D := ADataLen;
      D := SwapLongWord(D);
      CopyMemory(@FHead[2], @D, LenLen);
    end;

    FHead[1] := CN_BER_LENLEN_MASK or LenLen;
    Inc(FHeadLen, 1 + LenLen);
  end;
end;

constructor TCnBerWriteNode.Create(ATree: TCnTree);
begin
  inherited;
  FMem := TMemoryStream.Create;
end;

destructor TCnBerWriteNode.Destroy;
begin
  FMem.Free;
  inherited;
end;

function TCnBerWriteNode.GetIsContainer: Boolean;
begin
  Result := FIsContainer;
end;

function TCnBerWriteNode.GetItems(Index: Integer): TCnBerWriteNode;
begin
  Result := TCnBerWriteNode(inherited GetItems(Index));
end;

function TCnBerWriteNode.SaveToStream(Stream: TStream): Integer;
var
  B: Byte;
  I: Integer;
  LeafLen: Cardinal;
  AMem: TMemoryStream;
begin
  if FIsContainer then
  begin
    LeafLen := 0;
    Result := 0;
    AMem := TMemoryStream.Create;
    try
      for I := 0 to Count - 1 do
        LeafLen := LeafLen + Cardinal(Items[I].SaveToStream(AMem));

      FillHeadCalcLen(FBerTag, LeafLen);
      // 把 Tag、LeafLen 以及 AMem 的数据组合后写入

      Result := Result + Stream.Write(FHead[0], FHeadLen); // 写头与长度
      if FBerTag = CN_BER_TAG_BIT_STRING then              // BitString 留一个 bit 数，暂时作为全 0 处理
      begin
        B := 0;
        Result := Result + Stream.Write(B, 1);
      end;

      // 写具体内容
      Result := Result + Stream.Write(AMem.Memory^, AMem.Size);
    finally
      AMem.Free;
    end;
  end
  else
  begin
    Result := Stream.Write(FMem.Memory^, FMem.Size);
  end;
end;

procedure TCnBerWriteNode.SetIsContainer(const Value: Boolean);
begin
  FIsContainer := Value;
end;

procedure TCnBerWriteNode.SetItems(Index: Integer;
  const Value: TCnBerWriteNode);
begin
  inherited SetItems(Index, Value);
end;

function TCnBerWriteNode.GetNodeLength: Integer;
var
  I, LeafLen: Integer;
begin
  if FIsContainer then
  begin
    LeafLen := 0;
    for I := 0 to Count - 1 do
      LeafLen := LeafLen + Items[I].GetNodeLength;

    FillHeadCalcLen(FBerTag, LeafLen);
    Result := FHeadLen + LeafLen;

    // BitString 需要一个前导字节表示补足的 bit
    if FBerTag = CN_BER_TAG_BIT_STRING then
      Inc(Result);
  end
  else
  begin
    Result := FMem.Size;
  end;
end;

procedure TCnBerWriteNode.FillBasicNode(ATag: Integer; Data: PByte;
  DataLen: Integer);
var
  B: Byte;
begin
  FBerTag := ATag;
  if FIsContainer then
    Exit;

  FData := Data;
  FDataLength := DataLen;
  FillHeadCalcLen(ATag, DataLen);

  FMem.Clear;
  FMem.Write(FHead[0], FHeadLen);
  if DataLen > 0 then
  begin
    // 纯 BitString 需要补一个前导字节，头长度已经在 FillHeadCalcLen 内补上了
    if ATag = CN_BER_TAG_BIT_STRING then
    begin
      B := 0;
      FMem.Write(B, 1);
    end;
    FMem.Write(Data^, DataLen);
  end;
end;

function TCnBerWriteNode.SaveValueToStream(Stream: TStream): Integer;
var
  B: Byte;
  I: Integer;
  AMem: TMemoryStream;
begin
  Result := 0;
  if FIsContainer then
  begin
    AMem := TMemoryStream.Create;
    try
      for I := 0 to Count - 1 do
        Items[I].SaveToStream(AMem);

      if FBerTag = CN_BER_TAG_BIT_STRING then // BitString 留一个 bit 数，暂时作为全 0 处理
      begin
        B := 0;
        Result := Result + Stream.Write(B, 1);
      end;

      // 写具体内容
      Result := Result + Stream.Write(AMem.Memory^, AMem.Size);
    finally
      AMem.Free;
    end;
  end
  else
  begin
    if (FHeadLen > 0) and (FMem.Size > FHeadLen) then
      Result := Stream.Write(Pointer(Integer(FMem.Memory) + FHeadLen)^, FMem.Size - FHeadLen);
  end;
end;

end.
