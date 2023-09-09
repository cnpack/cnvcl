{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2023 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ���������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnBits;
{* |<PRE>
================================================================================
* �������ƣ�������������
* ��Ԫ���ƣ�λ������Ԫ
* ��Ԫ���ߣ���Х
* ��    ע����
* ����ƽ̨��Win7 + Delphi 5.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2023.09.09 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative;

type
// =============================================================================
//  λ��װ�࣬��� Bit �ṩ��������������˳������
//
//  ��0�ֽ�  ��1�ֽ�
//  +--------+-------+
//  |76543210|FEDCBA9| ...
//  +--------+-------+
//
// =============================================================================

  TCnBitBuilder = class
  {* λ��װ�ֻ֧࣬���������ݣ���֧�ֲ����ɾ��}
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
  protected
    procedure ExpandCapacity;
    {* ��չ�����������ȱ�֤���� ByteLength ������չ��������������������ٷ�֮��ʮ}
    procedure EnsureCapacity(const ABitSize: Integer);
    {* ȷ������ ABitSize �����������õĵ��ð취�� FBitLength + Delta}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    {* �������}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ��λת������ʾ�� 0 �� 1 ���ַ���}

    procedure AppendBit(Value: Boolean);
    {* ����һλ}
    procedure AppendByteRange(Value: Byte; MaxRange: Integer);
    {* ����һ���ֽ��е� 0 �� MaxRange λ}
    procedure AppendByte(Value: Byte; Full: Boolean = True);
    {* ����һ���ֽڣ�Full ��ʾ�ǰ�λ������ȥ���Ǻ��Ը�λ�� 0}
    procedure AppendWord(Value: Word; Full: Boolean = True);
    {* ����һ��˫�ֽڣ�Full ��ʾ��ʮ��λ������ȥ���Ǻ��Ը�λ�� 0}
    procedure AppendDWord(Value: Cardinal; Full: Boolean = True);
    {* ����һ�����ֽڣ�Full ��ʾ����ʮ��λ������ȥ���Ǻ��Ը�λ�� 0}
    procedure AppendBytes(Value: TBytes);
    {* ����һ���ֽ�����}

    function ToBytes: TBytes;
    {* ������ƴ�ճ��ֽ����鲢���أ�λ�����ֽ����ϴ���}

    property Bit[Index: Integer]: Boolean read GetBit write SetBit;
    {* ����������λ����}
    property ByteCapacity: Integer read GetByteCapacity write SetByteCapacity;
    {* ���ֽ�Ϊ��λ���ڲ�������������������ʱ���ܱ� ByteLength С}
    property ByteLength: Integer read GetByteLength write SetByteLength;
    {* ���ֽ�Ϊ��λ���ڲ��Ѿ�ƴ�յ�ʵ�����ݳ��ȣ��� BitLength �������}
    property MaxByteCapacity: Integer read FMaxByteCapacity;
    {* ���ֽ�Ϊ��λ�Ŀ����õ������������}
    property BitLength: Integer read FBitLength;
    {* ��λΪ��λ��ʵ�����ݳ���}
  end;

implementation

resourcestring
  SCnErrorByteCapacityFmt = 'Error New Capacity or Length Value %d';
  SCnErrorBitIndexFmt = 'Invalid Bit Index %d';

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

procedure TCnBitBuilder.AppendDWord(Value: Cardinal; Full: Boolean);
var
  H3, H2, H1, H0: Byte;
begin
  H3 := (Value and $FF000000) shr 24;
  H2 := (Value and $00FF0000) shr 16;
  H1 := (Value and $0000FF00) shr 8;
  H0 := Value and $000000FF;

  AppendByte(H0, Full or (H3 * H2 * H1 <> 0)); // �и�λ���ڵĻ�����λ���� Full
  AppendByte(H1, Full or (H3 * H2 <> 0));
  AppendByte(H2, Full or (H3 <> 0));
  AppendByte(H3, Full);
end;

procedure TCnBitBuilder.AppendWord(Value: Word; Full: Boolean);
var
  H, L: Byte;
begin
  H := (Value and $FF00) shr 8;
  L := Value and $FF;

  AppendByte(L, Full or (H <> 0)); // �и�λ���ڵĻ����� 8 λ���� Full
  AppendByte(H, Full);
end;

procedure TCnBitBuilder.Clear;
begin
  FBitLength := 0;
  ByteCapacity := BIT_BUILDER_DEFAULT_CAPACITY;
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
  while ByteCapacity * 8 < ABitSize do // ���Ҫ���õ�ʵ��λ�ߴ糬���˶�̬������ֽڳ���������
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

  ByteCapacity := NC; // ����ʵ�ʵ�������ͬ������ ByteCapacity
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

end.