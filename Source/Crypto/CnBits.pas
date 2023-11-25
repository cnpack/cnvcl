{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnBits;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：位处理单元
* 单元作者：刘啸
* 备    注：
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2023.09.09 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative;

type
// =============================================================================
//  位组装类，针对 Bit 提供索引方法，索引顺序如下
//
//  第0字节  第1字节
//  +--------+-------+
//  |76543210|FEDCBA9| ...
//  +--------+-------+
//
// =============================================================================

  TCnBitBuilder = class
  {* 位组装类，只支持增加内容，不支持插入和删除}
  private
    FData: TBytes;
    FMaxByteCapacity: Integer;
    FBitLength: Integer;
    function GetByteCapacity: Integer;
    procedure SetByteCapacity(const Value: Integer);
    procedure SetByteLength(const Value: Integer);
    function GetByteLength: Integer;
    function GetBit(Index: Integer): Boolean;
    procedure SetBit(Index: Integer; const Value: Boolean);
    procedure SetBitLength(const Value: Integer);
  protected
    procedure ExpandCapacity;
    {* 扩展内容区，首先保证大于 ByteLength 否则扩展至两倍，其次容量增长百分之五十}
    procedure EnsureCapacity(const ABitSize: Integer);
    {* 确保至少 ABitSize 的容量，常用的调用办法是 FBitLength + Delta}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    {* 清空内容}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* 按位转换成显示成 0 和 1 的字符串}

    procedure AppendBit(Value: Boolean);
    {* 增加一位}

    procedure AppendByteRange(Value: Byte; MaxRange: Integer);
    {* 增加一个字节中的 0 到 MaxRange 位}
    procedure AppendWordRange(Value: Word; MaxRange: Integer);
    {* 增加一个双字节中的 0 到 MaxRange 位}
    procedure AppendDWordRange(Value: Cardinal; MaxRange: Integer);
    {* 增加一个四字节中的 0 到 MaxRange 位}

    procedure AppendByte(Value: Byte; Full: Boolean = True);
    {* 增加一个字节，Full 表示是八位都加上去还是忽略高位的 0}
    procedure AppendWord(Value: Word; Full: Boolean = True);
    {* 增加一个双字节，Full 表示是十六位都加上去还是忽略高位的 0}
    procedure AppendDWord(Value: Cardinal; Full: Boolean = True);
    {* 增加一个四字节，Full 表示是三十二位都加上去还是忽略高位的 0}

    procedure AppendBytes(Value: TBytes);
    {* 增加一个字节数组}
    procedure AppendData(Data: Pointer; DataByteLen: Integer);
    {* 增加一个数据块}

    function ToBytes: TBytes;
    {* 将全部内容拼凑成字节数组并返回，位数往字节数上凑整}
    procedure SetBytes(Data: TBytes);
    {* 将字节数组内容设置为整个位内容}
    function ReadFrom(AMem: Pointer; AByteLength: Integer): Integer;
    {* 清空自身后从内存区域读入全部内容，返回读入的字节长度}
    function WriteTo(AMem: Pointer): Integer;
    {* 将全部内容写入指定内存区域，返回写入的字节长度，如 AMem 传 nil 则返回所需的长度}
    function Copy(Index, Count: Integer): Cardinal;
    {* 从指定 Index 处复制 Count 个位放入结果中，Count 超长无法容纳则抛异常}

    property Bit[Index: Integer]: Boolean read GetBit write SetBit;
    {* 按索引访问位内容，1 为 True，0 为 False。索引范围为 0 到 BitLength - 1}
    property ByteCapacity: Integer read GetByteCapacity write SetByteCapacity;
    {* 以字节为单位的内部缓冲区的容量，设置时不能比 ByteLength 小}
    property ByteLength: Integer read GetByteLength write SetByteLength;
    {* 以字节为单位的内部已经拼凑的实际内容长度，由 BitLength 计算而来}
    property MaxByteCapacity: Integer read FMaxByteCapacity;
    {* 以字节为单位的可设置的最大容量长度}
    property BitLength: Integer read FBitLength write SetBitLength;
    {* 以位为单位的实际内容长度}
  end;

implementation

resourcestring
  SCnErrorByteCapacityFmt = 'Error New Capacity or Length Value %d';
  SCnErrorBitIndexFmt = 'Invalid Bit Index %d';
  SCnErrorBitTooLargeFmt = 'Bit Count Too Large %d';

const
  BIT_BUILDER_DEFAULT_CAPACITY = 16;

{ TCnBitBuilder }

procedure TCnBitBuilder.AppendBit(Value: Boolean);
begin
  Inc(FBitLength);
  EnsureCapacity(FBitLength);
  if Value then
    FData[GetByteLength - 1] := FData[GetByteLength - 1] or (1 shl ((FBitLength - 1) mod 8))
  else
    FData[GetByteLength - 1] := FData[GetByteLength - 1] and not (1 shl ((FBitLength - 1) mod 8));
end;

procedure TCnBitBuilder.AppendByte(Value: Byte; Full: Boolean);
var
  K, I: Integer;
begin
  K := 7;
  if not Full then
    K := GetUInt8HighBits(Value);

  if K < 0 then
    Exit;

  for I := 0 to K do
    AppendBit((Value and (1 shl I)) <> 0);
end;

procedure TCnBitBuilder.AppendByteRange(Value: Byte; MaxRange: Integer);
var
  I: Integer;
begin
  if MaxRange < 0 then
    Exit;

  if MaxRange > 7 then
    MaxRange := 7;

  for I := 0 to MaxRange do
    AppendBit((Value and (1 shl I)) <> 0);
end;

procedure TCnBitBuilder.AppendBytes(Value: TBytes);
var
  I: Integer;
begin
  if Length(Value) <= 0 then
    Exit;

  for I := 0 to Length(Value) - 1 do
    AppendByte(Value[I]);
end;

procedure TCnBitBuilder.AppendData(Data: Pointer; DataByteLen: Integer);
var
  I: Integer;
  P: PByte;
begin
  if (Data <> nil) and (DataByteLen > 0) then
  begin
    P := PByte(Data);
    for I := 0 to DataByteLen - 1 do
    begin
      AppendByte(P^);
      Inc(P);
    end;
  end;
end;

procedure TCnBitBuilder.AppendDWord(Value: Cardinal; Full: Boolean);
var
  H3, H2, H1, H0: Byte;
begin
  H3 := (Value and $FF000000) shr 24;
  H2 := (Value and $00FF0000) shr 16;
  H1 := (Value and $0000FF00) shr 8;
  H0 := Value and $000000FF;

  AppendByte(H0, Full or (H3 * H2 * H1 <> 0)); // 有高位存在的话，低位必须 Full
  AppendByte(H1, Full or (H3 * H2 <> 0));
  AppendByte(H2, Full or (H3 <> 0));
  AppendByte(H3, Full);
end;

procedure TCnBitBuilder.AppendDWordRange(Value: Cardinal; MaxRange: Integer);
var
  I: Integer;
begin
  if MaxRange < 0 then
    Exit;

  if MaxRange > 31 then
    MaxRange := 31;

  for I := 0 to MaxRange do
    AppendBit((Value and (1 shl I)) <> 0);
end;

procedure TCnBitBuilder.AppendWord(Value: Word; Full: Boolean);
var
  H, L: Byte;
begin
  H := (Value and $FF00) shr 8;
  L := Value and $FF;

  AppendByte(L, Full or (H <> 0)); // 有高位存在的话，低 8 位必须 Full
  AppendByte(H, Full);
end;

procedure TCnBitBuilder.AppendWordRange(Value: Word; MaxRange: Integer);
var
  I: Integer;
begin
  if MaxRange < 0 then
    Exit;

  if MaxRange > 15 then
    MaxRange := 15;

  for I := 0 to MaxRange do
    AppendBit((Value and (1 shl I)) <> 0);
end;

procedure TCnBitBuilder.Clear;
begin
  FBitLength := 0;
  ByteCapacity := BIT_BUILDER_DEFAULT_CAPACITY;
end;

function TCnBitBuilder.Copy(Index, Count: Integer): Cardinal;
var
  I: Integer;
begin
  if Count > SizeOf(Cardinal) * 8 then
    raise ERangeError.CreateFmt(SCnErrorBitTooLargeFmt, [Count]);

  Result := 0;
  for I := Index to Index + Count - 1 do
  begin
    if Bit[I] then
      Result := Result or (1 shl (I - Index))
    else
      Result := Result and not (1 shl (I - Index));
  end;
end;

constructor TCnBitBuilder.Create;
begin
  inherited;
  FMaxByteCapacity := MaxInt div 2;
  ByteCapacity := BIT_BUILDER_DEFAULT_CAPACITY;
  FBitLength := 0;
end;

destructor TCnBitBuilder.Destroy;
begin
  SetLength(FData, 0);
  inherited;
end;

procedure TCnBitBuilder.EnsureCapacity(const ABitSize: Integer);
begin
  while ByteCapacity * 8 < ABitSize do // 如果要设置的实际位尺寸超过了动态数组的字节长度则扩容
    ExpandCapacity;
end;

procedure TCnBitBuilder.ExpandCapacity;
var
  NC: Integer;
begin
  NC := (ByteCapacity * 3) div 2;
  if ByteLength > NC then
    NC := ByteLength * 2;
  if NC > FMaxByteCapacity then
    NC := FMaxByteCapacity;
  if NC < 0 then
    NC := ByteLength;

  ByteCapacity := NC; // 增加实际的容量，同步扩大 ByteCapacity
end;

function TCnBitBuilder.GetBit(Index: Integer): Boolean;
begin
  if (Index >= 0) and (Index < FBitLength) then
    Result := (FData[Index div 8] and (1 shl (Index mod 8))) <> 0
  else
    raise ERangeError.CreateFmt(SCnErrorBitIndexFmt, [Index]);
end;

function TCnBitBuilder.GetByteCapacity: Integer;
begin
  Result := Length(FData);
end;

function TCnBitBuilder.GetByteLength: Integer;
begin
  Result := (FBitLength + 7) div 8;
end;

function TCnBitBuilder.ReadFrom(AMem: Pointer; AByteLength: Integer): Integer;
begin
  Result := 0;
  Clear;

  if (AMem = nil) or (AByteLength <= 0) then
    Exit;

  ByteLength := AByteLength;
  Move(AMem^, FData[0], AByteLength);
  Result := AByteLength;
end;

procedure TCnBitBuilder.SetBit(Index: Integer; const Value: Boolean);
begin
  if (Index >= 0) and (Index < FBitLength) then
  begin
    if Value then
      FData[Index div 8] := FData[Index div 8] or (1 shl (Index mod 8))
    else
      FData[Index div 8] := FData[Index div 8] and not (1 shl (Index mod 8));
  end
  else
    raise ERangeError.CreateFmt(SCnErrorBitIndexFmt, [Index]);
end;

procedure TCnBitBuilder.SetBitLength(const Value: Integer);
begin
  FBitLength := Value;
  EnsureCapacity(FBitLength);
end;

procedure TCnBitBuilder.SetByteCapacity(const Value: Integer);
begin
  if (Value < GetByteLength) or (Value > FMaxByteCapacity) then
    raise ERangeError.CreateResFmt(@SCnErrorByteCapacityFmt, [Value]);

  SetLength(FData, Value);
end;

procedure TCnBitBuilder.SetByteLength(const Value: Integer);
begin
  FBitLength := Value * 8;
  EnsureCapacity(FBitLength);
end;

procedure TCnBitBuilder.SetBytes(Data: TBytes);
begin
  ByteLength := Length(Data);
  if FBitLength > 0 then
    Move(Data[0], FData[0], Length(Data));
end;

function TCnBitBuilder.ToBytes: TBytes;
begin
  SetLength(Result, GetByteLength);
  if Length(Result) > 0 then
    Move(FData[0], Result[0], Length(Result));
end;

function TCnBitBuilder.ToString: string;
var
  I: Integer;
begin
  SetLength(Result, FBitLength);
  if FBitLength > 0 then
  begin
    for I := 0 to FBitLength - 1 do
    begin
      if Bit[I] then
        Result[I + 1] := '1'
      else
        Result[I + 1] := '0';
    end;
  end;
end;

function TCnBitBuilder.WriteTo(AMem: Pointer): Integer;
begin
  Result := GetByteLength;
  if (AMem <> nil) and (Result > 0) then
    Move(FData[0], AMem^, Result);
end;

end.
