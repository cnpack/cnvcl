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

{ -----------------------------------------------------------------------------}
{ uTBase64 v1.0 - Simple Base64 encoding/decoding class                        }
{ Base64 described in RFC2045, Page 24, (w) 1996 Freed & Borenstein            }
{ Delphi implementation (w) 1999 Dennis D. Spreen (dennis@spreendigital.de)    }
{ This unit is freeware. Just drop me a line if this unit is useful for you.   }
{ -----------------------------------------------------------------------------}

unit CnBase64;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�Base64 ������㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�ղ����Solin�� solin@21cn.com; http://www.ilovezhuzhu.net
*           wr960204
*           CnPack ������ (master@cnpack.org)
*           �������ݻ��� Dennis D. Spreen �� UTBASE64.pas ��д������ԭ�а�Ȩ��Ϣ��
* ��    ע������Ԫʵ���˱�׼ Base64 �� Base64URL �ı�������빦�ܡ�
*           Base64URL ������ڱ�׼ Base64�����ѷ��� + / �滻���� - _ ������ URL �����
*           �Ѻã���ɾ����β���� =
*
* ����ƽ̨��PWin2003Std + Delphi 6.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2023.10.04 V1.6
*               ɾ������ʵ�֡�Base64Encode �� Base64Decode ֧�� Base64URL �ı��������
*           2019.12.12 V1.5
*               ֧�� TBytes
*           2019.04.15 V1.4
*               ֧�� Win32/Win64/MacOS
*           2018.06.22 V1.3
*               ���������ԭʼ���ݿ��ܰ������� #0 ��ԭʼβ�� #0 �������Ƴ�������
*           2016.05.03 V1.2
*               �����ַ����а��� #0 ʱ���ܻᱻ�ضϵ�����
*           2006.10.25 V1.1
*               ���� wr960204 ���Ż��汾
*           2003.10.14 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnConsts;

const
  // ������
  ECN_BASE64_OK                        = ECN_OK;
  {* Base64 ϵ�д����룺�޴���ֵΪ 0}

  ECN_BASE64_ERROR_BASE                = ECN_CUSTOM_ERROR_BASE + $500;
  {* Base64 ϵ�д�����Ļ�׼��ʼֵ��Ϊ ECN_CUSTOM_ERROR_BASE ���� $500}

  ECN_BASE64_LENGTH                    = ECN_BASE64_ERROR_BASE + 1;
  {* Base64 ������֮���ݳ��ȷǷ�}

function Base64Encode(InputData: TStream; var OutputData: string;
  URL: Boolean = False): Integer; overload;
{* �������� Base64 ����� Base64URL ���룬�����ɹ����� ECN_BASE64_OK��

   ������
     InputData: TStream                   - �������������
     var OutputData: string               - ����������ַ���
     URL: Boolean                         - URL ��ǡ�True ��ʹ�� Base64URL ���룬False ��ʹ�ñ�׼ Base64 ����

   ����ֵ��Integer                        - ���ر����Ƿ�ɹ����ɹ��򷵻� ECN_BASE64_OK
}

function Base64Encode(const InputData: AnsiString; var OutputData: string;
  URL: Boolean = False): Integer; overload;
{* ���ַ������� Base64 ����� Base64URL ���룬�����ɹ����� ECN_BASE64_OK��

   ������
     const InputData: AnsiString          - ��������ַ���
     var OutputData: string               - ����������ַ���
     URL: Boolean                         - URL ��ǡ�True ��ʹ�� Base64URL ���룬False ��ʹ�ñ�׼ Base64 ����

   ����ֵ��Integer                        - ���ر����Ƿ�ɹ����ɹ��򷵻� ECN_BASE64_OK
}

function Base64Encode(InputData: Pointer; DataByteLen: Integer; var OutputData: string;
  URL: Boolean = False): Integer; overload;
{* �����ݿ���� Base64 ����� Base64URL ���룬�����ɹ����� ECN_BASE64_OK��

   ������
     InputData: Pointer                   - ����������ݿ��ַ
     DataByteLen: Integer                 - ����������ݿ��ֽڳ���
     var OutputData: string               - ����������ַ���
     URL: Boolean                         - URL ��ǡ�True ��ʹ�� Base64URL ���룬False ��ʹ�ñ�׼ Base64 ����

   ����ֵ��Integer                        - ���ر����Ƿ�ɹ����ɹ��򷵻� ECN_BASE64_OK
}

function Base64Encode(InputData: TBytes; var OutputData: string;
  URL: Boolean = False): Integer; overload;
{* ���ֽ�������� Base64 ����� Base64URL ���룬�����ɹ����� ECN_BASE64_OK��

   ������
     InputData: TBytes                    - ��������ֽ�����
     var OutputData: string               - ����������ַ���
     URL: Boolean                         - URL ��ǡ�True ��ʹ�� Base64URL ���룬False ��ʹ�ñ�׼ Base64 ����

   ����ֵ��Integer                        - ���ر����Ƿ�ɹ����ɹ��򷵻� ECN_BASE64_OK
}

function Base64Decode(const InputData: string; OutputData: TStream;
  FixZero: Boolean = True): Integer; overload;
{* ���ַ������� Base64 ���루���� Base64URL ���룩�����д�����������ɹ����� ECN_BASE64_OK��

   ������
     const InputData: string              - ��������ַ���
     OutputData: TStream                  - �����������
     FixZero: Boolean                     - �Ƿ�ȥ��������β���� #0

   ����ֵ��Integer                        - ���ؽ����Ƿ�ɹ����ɹ��򷵻� ECN_BASE64_OK
}

function Base64Decode(const InputData: string; var OutputData: AnsiString;
  FixZero: Boolean = True): Integer; overload;
{* ���ַ������� Base64 ���루���� Base64URL ���룩�����д���ַ����������ɹ����� ECN_BASE64_OK��

   ������
     const InputData: string              - ��������ַ���
     var OutputData: AnsiString           - ����������ַ���
     FixZero: Boolean                     - �Ƿ�ȥ��������β���� #0

   ����ֵ��Integer                        - ���ؽ����Ƿ�ɹ����ɹ��򷵻� ECN_BASE64_OK
}

function Base64Decode(const InputData: string; OutputData: Pointer;
  DataByteLen: Integer; FixZero: Boolean = True): Integer; overload;
{* ���ַ������� Base64 ���루���� Base64URL ���룩�����д���ڴ����������ɹ����� ECN_BASE64_OK��

   ������
     const InputData: string              - ��������ַ���
     OutputData: Pointer                  - ����������ڴ�����ַ
     DataByteLen: Integer                 - ����ڴ������ֽڳ��ȣ�Ӧ����Ϊ 1 + (Length(InputData) * 3 / 4)
     FixZero: Boolean                     - �Ƿ�ȥ��������β���� #0

   ����ֵ��Integer                        - �� OutputData �� nil����������Ľ��������ֽڳ��ȡ�����������ؽ����Ƿ�ɹ����ɹ��򷵻� ECN_BASE64_OK
}

function Base64Decode(const InputData: string; out OutputData: TBytes;
  FixZero: Boolean = True): Integer; overload;
{* ���ַ������� Base64 ���루���� Base64URL ���룩�����д���ֽ����顣�����ɹ����� ECN_BASE64_OK��

   ������
     const InputData: string              - ��������ַ���
     out OutputData: TBytes               - �������ֽ�����
     FixZero: Boolean                     - �Ƿ�ȥ��������β���� #0

   ����ֵ��Integer                        - ���ؽ����Ƿ�ɹ����ɹ��򷵻� ECN_BASE64_OK
}

implementation

var
  FilterDecodeInput: Boolean = True;

//------------------------------------------------------------------------------
// ����Ĳο���
//------------------------------------------------------------------------------

  EnCodeTab: array[0..64] of AnsiChar =
  (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/',
    '=');

  EnCodeTabURL: array[0..64] of AnsiChar =
  (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '-', '_',
    '=');

//------------------------------------------------------------------------------
// ����Ĳο���
//------------------------------------------------------------------------------

  { �������� Base64 ������ַ�ֱ�Ӹ��㣬����Ҳȡ����}
  DecodeTable: array[#0..#127] of Byte =
  (
    Byte('='), 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 62, 00, 62, 00, 63,  // ����ĵ�һ�� 62���� 63 �� + �� /����� - ���� 62
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 00, 00, 00, 00, 00, 00,
    00, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 00, 00, 00, 00, 63,  // _ ���� 63
    00, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 00, 00, 00, 00, 00
  );

function Base64Encode(InputData: TStream; var OutputData: string; URL: Boolean): Integer;
var
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Mem.CopyFrom(InputData, InputData.Size);
    Result := Base64Encode(Mem.Memory, Mem.Size, OutputData, URL);
  finally
    Mem.Free;
  end;
end;

// ����Ϊ wr960204 �Ľ��Ŀ��� Base64 ������㷨
function Base64Encode(InputData: Pointer; DataByteLen: Integer; var OutputData: string;
  URL: Boolean): Integer;
var
  Times, I: Integer;
  X1, X2, X3, X4: AnsiChar;
  XT: Byte;
begin
  if (InputData = nil) or (DataByteLen <= 0) then
  begin
    Result := ECN_BASE64_LENGTH;
    Exit;
  end;

  if DataByteLen mod 3 = 0 then
    Times := DataByteLen div 3
  else
    Times := DataByteLen div 3 + 1;
  SetLength(OutputData, Times * 4);   // һ�η��������ڴ�,����һ�δ��ַ������,һ�δ��ͷŷ����ڴ�
  FillChar(OutputData[1], Length(OutputData) * SizeOf(Char), 0);

  if URL then
  begin
    for I := 0 to Times - 1 do
    begin
      if DataByteLen >= (3 + I * 3) then
      begin
        X1 := EnCodeTabURL[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        XT := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        XT := XT or (Ord(PAnsiChar(InputData)[1 + I * 3]) shr 4);
        X2 := EnCodeTabURL[XT];
        XT := (Ord(PAnsiChar(InputData)[1 + I * 3]) shl 2) and 60;
        XT := XT or (Ord(PAnsiChar(InputData)[2 + I * 3]) shr 6);
        X3 := EnCodeTabURL[XT];
        XT := (Ord(PAnsiChar(InputData)[2 + I * 3]) and 63);
        X4 := EnCodeTabURL[XT];
      end
      else if DataByteLen >= (2 + I * 3) then
      begin
        X1 := EnCodeTabURL[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        XT := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        XT := XT or (Ord(PAnsiChar(InputData)[1 + I * 3]) shr 4);
        X2 := EnCodeTabURL[XT];
        XT := (Ord(PAnsiChar(InputData)[1 + I * 3]) shl 2) and 60;
        X3 := EnCodeTabURL[XT ];
        X4 := '=';
      end
      else
      begin
        X1 := EnCodeTabURL[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        XT := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        X2 := EnCodeTabURL[XT];
        X3 := '=';
        X4 := '=';
      end;
      OutputData[I shl 2 + 1] := Char(X1);
      OutputData[I shl 2 + 2] := Char(X2);
      OutputData[I shl 2 + 3] := Char(X3);
      OutputData[I shl 2 + 4] := Char(X4);
    end;
  end
  else
  begin
    for I := 0 to Times - 1 do
    begin
      if DataByteLen >= (3 + I * 3) then
      begin
        X1 := EnCodeTab[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        XT := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        XT := XT or (Ord(PAnsiChar(InputData)[1 + I * 3]) shr 4);
        X2 := EnCodeTab[XT];
        XT := (Ord(PAnsiChar(InputData)[1 + I * 3]) shl 2) and 60;
        XT := XT or (Ord(PAnsiChar(InputData)[2 + I * 3]) shr 6);
        X3 := EnCodeTab[XT];
        XT := (Ord(PAnsiChar(InputData)[2 + I * 3]) and 63);
        X4 := EnCodeTab[XT];
      end
      else if DataByteLen >= (2 + I * 3) then
      begin
        X1 := EnCodeTab[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        XT := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        XT := XT or (Ord(PAnsiChar(InputData)[1 + I * 3]) shr 4);
        X2 := EnCodeTab[XT];
        XT := (Ord(PAnsiChar(InputData)[1 + I * 3]) shl 2) and 60;
        X3 := EnCodeTab[XT ];
        X4 := '=';
      end
      else
      begin
        X1 := EnCodeTab[(Ord(PAnsiChar(InputData)[I * 3]) shr 2)];
        XT := (Ord(PAnsiChar(InputData)[I * 3]) shl 4) and 48;
        X2 := EnCodeTab[XT];
        X3 := '=';
        X4 := '=';
      end;
      OutputData[I shl 2 + 1] := Char(X1);
      OutputData[I shl 2 + 2] := Char(X2);
      OutputData[I shl 2 + 3] := Char(X3);
      OutputData[I shl 2 + 4] := Char(X4);
    end;
  end;

  OutputData := Trim(OutputData);
  if URL then
  begin
    // ɾ�� OutputData β���� = �ַ����������������
    if (Length(OutputData) > 0) and (OutputData[Length(OutputData)] = '=') then
    begin
      Delete(OutputData, Length(OutputData), 1);
      if (Length(OutputData) > 0) and (OutputData[Length(OutputData)] = '=') then
      begin
        Delete(OutputData, Length(OutputData), 1);
        if (Length(OutputData) > 0) and (OutputData[Length(OutputData)] = '=') then
          Delete(OutputData, Length(OutputData), 1);
      end;
    end;
  end;
  Result := ECN_BASE64_OK;
end;

function Base64Encode(const InputData: AnsiString; var OutputData: string; URL: Boolean): Integer;
begin
  if InputData <> '' then
    Result := Base64Encode(@InputData[1], Length(InputData), OutputData, URL)
  else
    Result := ECN_BASE64_LENGTH;
end;

function Base64Encode(InputData: TBytes; var OutputData: string; URL: Boolean): Integer;
begin
  if Length(InputData) > 0 then
    Result := Base64Encode(@InputData[0], Length(InputData), OutputData, URL)
  else
    Result := ECN_BASE64_LENGTH;
end;

function Base64Decode(const InputData: string; OutputData: TStream; FixZero: Boolean): Integer;
var
  Data: TBytes;
begin
  Result := Base64Decode(InputData, Data, FixZero);
  if (Result = ECN_BASE64_OK) and (Length(Data) > 0) then
  begin
    OutputData.Size := Length(Data);
    OutputData.Position := 0;
    OutputData.Write(Data[0], Length(Data));
  end;
end;

function Base64Decode(const InputData: string; out OutputData: TBytes;
  FixZero: Boolean): Integer;
var
  SrcLen, DstLen, Times, I: Integer;
  X1, X2, X3, X4, XT: Byte;
  C, ToDec: Integer;
  Data: AnsiString;

  function FilterLine(const Source: AnsiString): AnsiString;
  var
    P, PP: PAnsiChar;
    I, FL: Integer;
  begin
    FL := Length(Source);
    if FL > 0 then
    begin
      GetMem(P, FL);                   // һ�η��������ڴ�,����һ�δ��ַ������,һ�δ��ͷŷ����ڴ�
      PP := P;
      FillChar(P^, FL, 0);
      for I := 1 to FL do
      begin
        if Source[I] in ['0'..'9', 'A'..'Z', 'a'..'z', '+', '/', '=', '-', '_'] then
        begin
          PP^ := Source[I];
          Inc(PP);
        end;
      end;
      SetString(Result, P, PP - P);        // ��ȡ��Ч����
      FreeMem(P);
    end;
  end;

begin
  if InputData = '' then
  begin
    Result := ECN_BASE64_OK;
    Exit;
  end;
  OutPutData := nil;

  // �� D5 �²�֪����ô�Ĳ����� AnsiString(InputData)�����ܻ���ڴ�����������ֿ���
  if FilterDecodeInput then
  begin
{$IFDEF UNICODE}
    Data := FilterLine(AnsiString(InputData));
{$ELSE}
    Data := FilterLine(InputData);
{$ENDIF}
  end
  else
  begin
{$IFDEF UNICODE}
    Data := AnsiString(InputData);
{$ELSE}
    Data := InputData;
{$ENDIF}
  end;

  // ����� Base64URL ����Ľ��ȥ����β���� =������Ҫ���ݳ����Ƿ��� 4 �ı���������
  if (Length(Data) and $03) <> 0 then
    Data := Data + StringOfChar(AnsiChar('='), 4 - (Length(Data) and $03));

  SrcLen := Length(Data);
  DstLen := SrcLen * 3 div 4;
  ToDec := 0;

  // β����һ���Ⱥ���ζ��ԭʼ���ݲ��˸� #0�������Ⱥ���ζ�Ų������� #0����Ҫȥ��Ҳ�������̳���
  // ע���ⲻ��ͬ��ԭʼ���ݵ�β���� #0 ���������������ȥ��
  if Data[SrcLen] = '=' then
  begin
    Inc(ToDec);
    if (SrcLen > 1) and (Data[SrcLen - 1] = '=') then
      Inc(ToDec);
  end;

  SetLength(OutputData, DstLen);  // һ�η��������ڴ�,����һ�δ��ַ������,һ�δ��ͷŷ����ڴ�
  Times := SrcLen div 4;
  C := 0;

  for I := 0 to Times - 1 do
  begin
    X1 := DecodeTable[Data[1 + I shl 2]];
    X2 := DecodeTable[Data[2 + I shl 2]];
    X3 := DecodeTable[Data[3 + I shl 2]];
    X4 := DecodeTable[Data[4 + I shl 2]];
    X1 := X1 shl 2;
    XT := X2 shr 4;
    X1 := X1 or XT;
    X2 := X2 shl 4;
    OutputData[C] := X1;
    Inc(C);
    if X3 = 64 then
      Break;
    XT := X3 shr 2;
    X2 := X2 or XT;
    X3 := X3 shl 6;
    OutputData[C] := X2;
    Inc(C);
    if X4 = 64 then
      Break;
    X3 := X3 or X4;
    OutputData[C] := X3;
    Inc(C);
  end;

  // ���ݲ��ĵȺ���Ŀ�����Ƿ�ɾ��β�� #0
  while (ToDec > 0) and (OutputData[DstLen - 1] = 0) do
  begin
    Dec(ToDec);
    Dec(DstLen);
  end;
  SetLength(OutputData, DstLen);

  // �ٸ����ⲿҪ��ɾ��β���� #0����ʵ��̫���ʵ��������
  if FixZero then
  begin
    while (DstLen > 0) and (OutputData[DstLen - 1] = 0) do
      Dec(DstLen);
    SetLength(OutputData, DstLen);
  end;

  Result := ECN_BASE64_OK;
end;

function Base64Decode(const InputData: string; var OutputData: AnsiString; FixZero: Boolean): Integer;
var
  Data: TBytes;
begin
  Result := Base64Decode(InputData, Data, FixZero);
  if (Result = ECN_BASE64_OK) and (Length(Data) > 0) then
  begin
    SetLength(OutputData, Length(Data));
    Move(Data[0], OutputData[1], Length(Data));
  end;
end;

function Base64Decode(const InputData: string; OutputData: Pointer;
  DataByteLen: Integer; FixZero: Boolean): Integer;
var
  Data: TBytes;
begin
  Result := Base64Decode(InputData, Data, FixZero);
  if (Result = ECN_BASE64_OK) and (Length(Data) > 0) then
  begin
    if OutputData = nil then
    begin
      Result := Length(Data);
      Exit;
    end;

    if DataByteLen < Length(Data) then
    begin
      Result := ECN_BASE64_LENGTH;
      Exit;
    end;

    Move(Data[0], OutPutData^, Length(Data));
  end;
end;

end.
