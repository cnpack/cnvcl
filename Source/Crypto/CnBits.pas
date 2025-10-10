{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnBits;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�λ����Ԫ
* ��Ԫ���ߣ�CnPack ������
* ��    ע������Ԫʵ�������λ��Bit������װ�Ȳ�����ʵ���� TCnBits��֧�ֻ���λ��������
*           ���������ݣ���֧�ֲ�����ɾ����
*           ������˳�����£�
*
*           �� 0 �ֽ� �� 1 �ֽ�
*
*           +--------+-------+
*
*           |76543210|FEDCBA9| ...
*
*           +--------+-------+
*
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
//  �� 0 �ֽ� �� 1 �ֽ�
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
    procedure SetBitLength(const Value: Integer);
  protected
    procedure ExpandCapacity;
    {* ��չ�����������ȱ�֤���� ByteLength ������չ��������������������ٷ�֮��ʮ}

    procedure EnsureCapacity(ABitSize: Integer);
    {* ȷ������ ABitSize �����������õĵ��ð취�� FBitLength + Delta��

       ������
         const ABitSize: Integer          - ȷ������ ABitSize ������

       ����ֵ�����ޣ�
    }
  public
    constructor Create; virtual;
    {* ���캯��}
    destructor Destroy; override;
    {��������}

    procedure Clear;
    {* �������}

    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}
    {* ��λת���ɰ��� 0 �� 1 ���ַ�����

       ������
         ���ޣ�

       ����ֵ��string                     - ���ذ�λת���ɰ��� 0 �� 1 ���ַ���
    }

    procedure AppendBit(Value: Boolean);
    {* ����һλ��������

       ������
         Value: Boolean                   - ��λ�Ƿ��� 1

       ����ֵ�����ޣ�
    }

    procedure AppendByteRange(Value: Byte; MaxRange: Integer);
    {* ����һ���ֽ��е� 0 �� MaxRange λ��������

       ������
         Value: Byte                      - �����ӵ��ֽ�ֵ
         MaxRange: Integer                - �����ӵ�λ��Χ��0 �� 7

       ����ֵ�����ޣ�
    }

    procedure AppendWordRange(Value: Word; MaxRange: Integer);
    {* ����һ��˫�ֽ��е� 0 �� MaxRange λ��������

       ������
         Value: Word                      - �����ӵ�˫�ֽ�ֵ
         MaxRange: Integer                - �����ӵ�λ��Χ��0 �� 15

       ����ֵ�����ޣ�
    }

    procedure AppendDWordRange(Value: Cardinal; MaxRange: Integer);
    {* ����һ�����ֽ��е� 0 �� MaxRange λ��������

       ������
         Value: Cardinal                  - �����ӵ����ֽ�ֵ
         MaxRange: Integer                - �����ӵ�λ��Χ��0 �� 31

       ����ֵ�����ޣ�
    }

    procedure AppendByte(Value: Byte; Full: Boolean = True);
    {* ����һ���ֽ���������Full ��ʾ�� 8 λ������ȥ���Ǻ��Ը�λ������ 0��

       ������
         Value: Byte                      - �����ӵ��ֽ�ֵ
         Full: Boolean                    - ���� 8 λ���Ǻ��Ը�λ������ 0

       ����ֵ�����ޣ�
    }

    procedure AppendWord(Value: Word; Full: Boolean = True);
    {* ����һ��˫�ֽ���������Full ��ʾ�� 16 λ������ȥ���Ǻ��Ը�λ������ 0��

       ������
         Value: Word                      - �����ӵ�˫�ֽ�ֵ
         Full: Boolean                    - ���� 16 λ���Ǻ��Ը�λ������ 0

       ����ֵ�����ޣ�
    }

    procedure AppendDWord(Value: Cardinal; Full: Boolean = True);
    {* ����һ�����ֽ���������Full ��ʾ�� 32 λ������ȥ���Ǻ��Ը�λ������ 0��

       ������
         Value: Cardinal                  - �����ӵ����ֽ�ֵ
         Full: Boolean                    - ���� 32 λ���Ǻ��Ը�λ������ 0

       ����ֵ�����ޣ�
    }

    procedure AppendBytes(Value: TBytes);
    {* ����һ���ֽ�������������

       ������
         Value: TBytes                    - �����ӵ��ֽ�����

       ����ֵ�����ޣ�
    }

    procedure AppendData(Data: Pointer; DataByteLen: Integer);
    {* ����һ�����ݿ���������

       ������
         Data: Pointer                    - �����ӵ����ݿ��ַ
         DataByteLen: Integer             - �����ӵ����ݿ���ֽڳ���

       ����ֵ�����ޣ�
    }

    function ToBytes: TBytes;
    {* ��ȫ������ƴ�ճ��ֽ����鲢���أ�λ�����ֽ����ϴ�����

       ������
         ���ޣ�

       ����ֵ��TBytes                     - ����ȫ������ת�����ֽ�����
    }

    procedure SetBytes(Data: TBytes);
    {* ���ֽ�������������Ϊ����λ���ݡ�

       ������
         Data: TBytes                     - �����õ��ֽ�����

       ����ֵ�����ޣ�
    }

    function ReadFrom(AMem: Pointer; AByteLength: Integer): Integer;
    {* ����������ڴ��������ȫ�����ݣ����ض�����ֽڳ��ȡ�

       ������
         AMem: Pointer                    - ��������ڴ��ַ
         AByteLength: Integer             - ��������ֽڳ���

       ����ֵ��Integer                    - ���سɹ�������ֽڳ���
    }

    function WriteTo(AMem: Pointer): Integer;
    {* ��ȫ������д��ָ���ڴ����򣬷���д����ֽڳ��ȣ��� AMem �� nil �򷵻�����ĳ��ȡ�

       ������
         AMem: Pointer                    - ��д����ڴ��ַ

       ����ֵ��Integer                    - ����д��������ֽڳ���
    }

    function Copy(Index: Integer; Count: Integer): Cardinal;
    {* ��ָ�� Index ������ Count ��λ�������У�Count �����޷����������쳣��

       ������
         Index: Integer                   - �����Ƶ���ʼλƫ����
         Count: Integer                   - �����Ƶ�λ�������ܴ��� 32

       ����ֵ��Cardinal                   - ���Ƶ�����
    }

    property Bit[Index: Integer]: Boolean read GetBit write SetBit;
    {* ����������λ���ݣ�1 Ϊ True��0 Ϊ False��������ΧΪ 0 �� BitLength - 1}

    property ByteCapacity: Integer read GetByteCapacity write SetByteCapacity;
    {* ���ֽ�Ϊ��λ���ڲ�������������������ʱ���ܱ� ByteLength С}

    property ByteLength: Integer read GetByteLength write SetByteLength;
    {* ���ֽ�Ϊ��λ���ڲ��Ѿ�ƴ�յ�ʵ�����ݳ��ȣ��� BitLength �������}

    property MaxByteCapacity: Integer read FMaxByteCapacity;
    {* ���ֽ�Ϊ��λ�Ŀ����õ������������}

    property BitLength: Integer read FBitLength write SetBitLength;
    {* ��λΪ��λ��ʵ�����ݳ���}
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

  AppendByte(H0, Full or (H3 * H2 * H1 <> 0)); // �и�λ���ڵĻ�����λ���� Full
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

  AppendByte(L, Full or (H <> 0)); // �и�λ���ڵĻ����� 8 λ���� Full
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

function TCnBitBuilder.Copy(Index: Integer; Count: Integer): Cardinal;
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

procedure TCnBitBuilder.EnsureCapacity(ABitSize: Integer);
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
