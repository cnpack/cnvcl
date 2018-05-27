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

unit CnBerParser;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：解析 ASN.1 的 BER 编码单元
* 单元作者：刘啸
* 备    注：
* 开发平台：WinXP + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2018.05.24 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, TypInfo, CnTree {$IFDEF DEBUG}, ComCtrls {$ENDIF};

const
  CN_BER_TAG_TYPE_MASK                      = $C0;
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
  CN_BER_TAG_TELETEXSTRING                  = $14;
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
  TCnBerTag = (cbtReserved_0, cbtBoolean, cbtInteger, cbtBit_String,
    cbtOctet_String, cbtNull, cbtObject_Identifier, cbtObject_Descripion,
    cbtExternal, cbtReal, cbtEnumerated, cbtEmbedded_Pdv, cbtUft8String,
    cbtRelative_Oid, cbtReserved_0E, cbtReserved_0F, cbtSequence, cbtSet,
    cbtNumericString, cbtPrintableString, cbtTeletexString, cbtVideotexString,
    cbtIa5String, cbtUtcTime, cbtGeneralizedTime, cbtGraphicString,
    cbtVisibleString, cbtGeneralString, cbtUniversalString, cbtCharacter_String,
    cbtBmpstring);

  TCnBerTags = set of TCnBerTag;

  TCnBerNode = class(TCnLeaf)
  {* 描述一解析出来的 ASN.1 节点}
  private
    FOriginData: PByte;
    FBerLength: Integer;
    FBerOffset: Integer;
    FBerTag: Integer;
    FBerDataLength: Integer;
    FBerDataOffset: Integer;
    function GetItems(Index: Integer): TCnBerNode;
    procedure SetItems(Index: Integer; const Value: TCnBerNode);

    function InternalAsInt(ByteSize: Integer): Integer;
  public
    procedure CopyDataTo(DestBuf: Pointer);
    {* 将数据复制至缓冲区，缓冲区尺寸至少需要 BerDataLength 大}

    function AsShortInt: ShortInt;
    function AsByte: Byte;
    function AsSmallInt: SmallInt;
    function AsWord: Word;
    function AsInteger: Integer;
    function AsCardinal: Cardinal;
    function AsInt64: Int64;

    property Items[Index: Integer]: TCnBerNode read GetItems write SetItems;

    property BerOffset: Integer read FBerOffset write FBerOffset;
    {* 该节点对应的 ASN.1 内容编码在整体中的偏移}
    property BerLength: Integer read FBerLength write FBerLength;
    {* 整个节点的内容长度}

    property BerTag: Integer read FBerTag write FBerTag;
    {* 节点类型，也就是 Tag}
    property BerDataLength: Integer read FBerDataLength write FBerDataLength;
    {* 节点数据长度}
    property BerDataOffset: Integer read FBerDataOffset write FBerDataOffset;
    {* 该节点对应的数据内容在整体中的偏移}
  end;

  TCnBerParser = class
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
    function GetItems(Index: Integer): TCnBerNode;
    procedure ParseArea(Parent: TCnLeaf; AData: PByteArray;
      ADataLen: Cardinal; AStartOffset: Cardinal);
    {* 解析一段数据，该数据里的所有 ASN.1 节点均序次挂在 Parent 节点下}
  protected
    procedure ParseToTree;
  public
    constructor Create(Data: PByte; DataLen: Cardinal; AParseInnerString: Boolean = False);
    destructor Destroy; override;
{$IFDEF DEBUG}
    procedure DumpToTreeView(ATreeView: TTreeView);
    property OnSaveNode: TCnTreeNodeEvent read GetOnSaveNode write SetOnSaveNode;
{$ENDIF}
    property ParseInnerString: Boolean read FParseInnerString;
    {* 是否将 BitString/OctetString 类型也当作复合类型来解析，PKCS#8 的 Pem 文件中常见}
    property TotalCount: Integer read GetTotalCount;
    {* 解析出来的 ASN.1 节点总数}
    property Items[Index: Integer]: TCnBerNode read GetItems;
    {* 顺序访问所有解析出来的 ASN.1 节点，下标从 0 开始，不包括 Tree 自身的 Root}
  end;

implementation

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

{ TCnBerParser }

constructor TCnBerParser.Create(Data: PByte; DataLen: Cardinal;
  AParseInnerString: Boolean);
begin
  FData := Data;
  FDataLen := DataLen;
  FParseInnerString := AParseInnerString;
  FBerTree := TCnTree.Create(TCnBerNode);

  ParseToTree;
end;

destructor TCnBerParser.Destroy;
begin
  inherited;

end;

{$IFDEF DEBUG}

procedure TCnBerParser.DumpToTreeView;
begin
  FBerTree.SaveToTreeView(ATreeView);
end;

function TCnBerParser.GetOnSaveNode: TCnTreeNodeEvent;
begin
  Result := FBerTree.OnSaveANode;
end;

procedure TCnBerParser.SetOnSaveNode(const Value: TCnTreeNodeEvent);
begin
  FBerTree.OnSaveANode := Value;
end;

{$ENDIF}

function TCnBerParser.GetItems(Index: Integer): TCnBerNode;
begin
  Result := TCnBerNode(FBerTree.Items[Index + 1]);
end;

function TCnBerParser.GetTotalCount: Integer;
begin
  Result := FBerTree.Root.AllCount;
end;

procedure TCnBerParser.ParseArea(Parent: TCnLeaf; AData: PByteArray;
  ADataLen: Cardinal; AStartOffset: Cardinal);
var
  Run, Start: Cardinal;
  Tag, DataLen, DataOffset, LenLen, Delta: Integer;
  B: Byte;
  IsStruct: Boolean;
  ALeaf: TCnBerNode;
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
      raise Exception.Create('Data Corruption when Processing Tag.');

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
        raise Exception.Create('Data Corruption when Processing Tag.');

      if LenLen = SizeOf(Byte) then
        DataLen := AData[Run]
      else if LenLen = SizeOf(Word) then
        DataLen := (Cardinal(AData[Run]) shl 8) or Cardinal(AData[Run + 1])
      else // if LenLen > SizeOf(Word) then
        raise Exception.Create('Length Too Long: ' + IntToStr(LenLen));

      DataOffset := AStartOffset + Run + Cardinal(LenLen);
      Inc(Delta, LenLen);
      Inc(Run, LenLen);   // Run 指向数据
    end;

    // Tag, Len, DataOffset 都齐全了，Delta 是数据起始区与当前节点起始区的偏移
    if Parent = nil then
      Parent := FBerTree.Root;

    ALeaf := FBerTree.AddChild(Parent) as TCnBerNode;
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
      // 说明 BerDataOffset 到 BerDataLength 内有子节点

      if (ALeaf.BerTag = CN_BER_TAG_BIT_STRING) and (AData[Run] = 0) then
      begin
        // BIT_STRING 数据区可能有个前导 00
        ParseArea(ALeaf, PByteArray(Cardinal(AData) + Run + 1),
          ALeaf.BerDataLength - 1, ALeaf.BerDataOffset + 1);
      end
      else
        ParseArea(ALeaf, PByteArray(Cardinal(AData) + Run),
          ALeaf.BerDataLength, ALeaf.BerDataOffset);
    end;

    Inc(Run, DataLen);
  end;
end;

procedure TCnBerParser.ParseToTree;
begin
  ParseArea(FBerTree.Root, PByteArray(FData), FDataLen, 0);
end;

{ TCnBerNode }

function TCnBerNode.InternalAsInt(ByteSize: Integer): Integer;
var
  IntValue: Integer;
begin
  if FBerTag <> CN_BER_TAG_INTEGER then
    raise Exception.Create('Ber Tag Type Mismatch for ByteSize: ' + IntToStr(ByteSize));

  if not (ByteSize in [SizeOf(Byte)..SizeOf(Integer)]) then
    raise Exception.Create('Invalid ByteSize: ' + IntToStr(ByteSize));

  if FBerDataLength > ByteSize then
    raise Exception.CreateFmt('Data Length %d Overflow for Required %d.',
      [FBerDataLength, ByteSize]);

  IntValue := 0;
  CopyDataTo(@IntValue);
  IntValue := SwapLongWord(IntValue);
  Result := IntValue;
end;

function TCnBerNode.AsInt64: Int64;
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

function TCnBerNode.AsByte: Byte;
begin
  Result := Byte(InternalAsInt(SizeOf(Byte)));
end;

function TCnBerNode.AsCardinal: Cardinal;
begin
  Result := Cardinal(InternalAsInt(SizeOf(Cardinal)));
end;

function TCnBerNode.AsInteger: Integer;
begin
  Result := Integer(InternalAsInt(SizeOf(Integer)));
end;

function TCnBerNode.AsShortInt: ShortInt;
begin
  Result := ShortInt(InternalAsInt(SizeOf(ShortInt)));
end;

function TCnBerNode.AsSmallInt: SmallInt;
begin
  Result := SmallInt(InternalAsInt(SizeOf(SmallInt)));
end;

function TCnBerNode.AsWord: Word;
begin
  Result := Word(InternalAsInt(SizeOf(Word)));
end;

procedure TCnBerNode.CopyDataTo(DestBuf: Pointer);
begin
  if (FOriginData <> nil) and (FBerDataLength > 0) then
    CopyMemory(DestBuf, Pointer(Integer(FOriginData) + FBerDataOffset), FBerDataLength);
end;

function TCnBerNode.GetItems(Index: Integer): TCnBerNode;
begin
  Result := inherited GetItems(Index) as TCnBerNode;
end;

procedure TCnBerNode.SetItems(Index: Integer; const Value: TCnBerNode);
begin
  inherited SetItems(Index, Value);
end;

end.
