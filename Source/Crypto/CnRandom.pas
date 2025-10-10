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

unit CnRandom;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��������䵥Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫ��װ�� Windows ƽ̨�� MacOS/Linux ƽ̨�µİ�ȫ�����������
*           �������ṩ��ȫ�������书�ܡ�
* ����ƽ̨��Win7 + Delphi 5.0
* ���ݲ��ԣ�Win32/Win64/MacOS/Linux + Unicode/NonUnicode
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2023.01.15 V1.3
*               �� Windows ��ȫ���� urandom ��֧�� Linux
*           2023.01.08 V1.2
*               ���� Win64 �� API �����������������
*           2022.08.22 V1.1
*               ����ʹ�ò���ϵͳ�ṩ�������������
*           2020.03.27 V1.0
*               ������Ԫ���� CnPrime �ж�������
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils {$IFDEF MSWINDOWS}, Windows {$ENDIF}, Classes, CnNative;

type
  ECnRandomAPIError = class(Exception);
  {* ���������쳣}

function RandomUInt64: TUInt64;
{* ���� UInt64 ��Χ�ڵ���������ڲ�֧�� UInt64 ��ƽ̨���� Int64 ���档

   ������
     ���ޣ�

   ����ֵ��TUInt64                        - ���� UInt64 ��Χ�ڵ������
}

function RandomUInt64LessThan(HighValue: TUInt64): TUInt64;
{* ���ش��ڵ��� 0 ��С��ָ�� UInt64 ֵ���������

   ������
     HighValue: TUInt64                   - ָ�� UInt64 �����������

   ����ֵ��TUInt64                        - ���ش��ڵ��� 0 ��С��ָ�� HighValue �������
}

function RandomInt64: Int64;
{* ���ش��ڵ��� 0 ��С�� Int64 ���޵��������

   ������
     ���ޣ�

   ����ֵ��Int64                          - ���ش��ڵ��� 0 ��С�� Int64 ���޵������
}

function RandomInt64LessThan(HighValue: Int64): Int64;
{* ���ش��ڵ��� 0 ��С��ָ�� Int64 ֵ��������������������б�֤ HighValue ���� 0��

   ������
     HighValue: Int64                     - ָ�� Int64 �����������

   ����ֵ��Int64                          - ���ش��ڵ��� 0 ��С��ָ�� HighValue �������
}

function RandomUInt32: Cardinal;
{* ���� UInt32 ��Χ�ڵ��������

   ������
     ���ޣ�

   ����ֵ��Cardinal                       - ���� UInt32 ��Χ�ڵ������
}

function RandomUInt32LessThan(HighValue: Cardinal): Cardinal;
{* ���ش��ڵ��� 0 ��С��ָ�� UInt32 ֵ���������

   ������
     HighValue: Cardinal                  - ָ�� UInt32 �����������

   ����ֵ��Cardinal                       - ���ش��ڵ��� 0 ��С��ָ�� HighValue �������
}

function RandomInt32: Integer;
{* ���ش��ڵ��� 0 ��С�� Int32 ���޵��������

   ������
     ���ޣ�

   ����ֵ��Integer                        - ���ش��ڵ��� 0 ��С�� Int32 ���޵������
}

function RandomInt32LessThan(HighValue: Integer): Integer;
{* ���ش��ڵ��� 0 ��С��ָ�� Int32 ��������������������б�֤ HighValue ���� 0��

   ������
     HighValue: Integer                   - ָ�� Int32 �����������

   ����ֵ��Integer                        - ���ش��ڵ��� 0 ��С��ָ�� HighValue �������
}

function CnKnuthShuffle(ArrayBase: Pointer; ElementByteSize: Integer;
  ElementCount: Integer): Boolean;
{* �ߵ���ϴ���㷨���� ArrayBase ��ָ��Ԫ�سߴ�Ϊ ElementSize �� ElementCount ��Ԫ�ؾ���ϴ�ơ�

   ������
     ArrayBase: Pointer                   - ��ϴ�Ƶ��ڴ���ַ
     ElementByteSize: Integer             - ��ϴ�Ƶ�ÿ���ڴ�Ԫ��Ҳ����ÿһ���Ƶ��ֽڴ�С
     ElementCount: Integer                - �ڴ�Ԫ��Ҳ�����Ƶ�����

   ����ֵ��Boolean                        - ����ϴ���Ƿ�ɹ�
}

function CnRandomFillBytes(Buf: PAnsiChar; BufByteLen: Integer): Boolean;
{* ʹ�� Windows API �� /dev/random �豸ʵ�����������䣬�ڲ����γ�ʼ����������沢�ͷš�

   ������
     Buf: PAnsiChar                       - �������ڴ���ַ
     BufByteLen: Integer                  - �������ڴ����ֽڳ���

   ����ֵ��Boolean                        - �����������Ƿ�ɹ�
}

function CnRandomFillBytes2(Buf: PAnsiChar; BufByteLen: Integer): Boolean;
{* ʹ�� Windows API �� /dev/urandom �豸ʵ�����������䣬
   Windows ��ʹ����Ԥ�ȳ�ʼ���õ����������١�

   ������
     Buf: PAnsiChar                       - �������ڴ���ַ
     BufByteLen: Integer                  - �������ڴ����ֽڳ���

   ����ֵ��Boolean                        - �����������Ƿ�ɹ�
}

function CnRandomBytes(ByteLen: Integer): TBytes;
{* ʹ�� Windows API �� /dev/random �豸��䲢����ָ�����ȵ�����ֽ����顣

   ������
     ByteLen: Integer                     - �����ɵ�����ֽ�������ֽڳ���

   ����ֵ��TBytes                         - ��������ֽ�����
}

implementation

{$IFDEF MSWINDOWS}

const
  ADVAPI32 = 'advapi32.dll';

  CRYPT_VERIFYCONTEXT = $F0000000;
  CRYPT_NEWKEYSET = $8;
  CRYPT_DELETEKEYSET = $10;

  PROV_RSA_FULL = 1;
  NTE_BAD_KEYSET = $80090016;

function CryptAcquireContext(phProv: PHandle; pszContainer: PAnsiChar;
  pszProvider: PAnsiChar; dwProvType: LongWord; dwFlags: LongWord): BOOL;
  stdcall; external ADVAPI32 name 'CryptAcquireContextA';

function CryptReleaseContext(hProv: THandle; dwFlags: LongWord): BOOL;
  stdcall; external ADVAPI32 name 'CryptReleaseContext';

function CryptGenRandom(hProv: THandle; dwLen: LongWord; pbBuffer: PAnsiChar): BOOL;
  stdcall; external ADVAPI32 name 'CryptGenRandom';

var
  FHProv: THandle = 0;

{$ELSE}

const
  DEV_FILE = '/dev/urandom';

{$ENDIF}

function CnRandomFillBytes(Buf: PAnsiChar; BufByteLen: Integer): Boolean;
var
{$IFDEF MSWINDOWS}
  HProv: THandle;
  Res: DWORD;
  B: Boolean;
{$ELSE}
  F: TFileStream;
{$ENDIF}
begin
  Result := False;
{$IFDEF MSWINDOWS}
  // ʹ�� Windows API ʵ������������
  HProv := 0;
  B := CryptAcquireContext(@HProv, nil, nil, PROV_RSA_FULL, 0);
  if not B then
    B := CryptAcquireContext(@HProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT);

  if not B then
  begin
    Res := GetLastError;
    if Res = NTE_BAD_KEYSET then // KeyContainer �����ڣ����½��ķ�ʽ
    begin
      if not CryptAcquireContext(@HProv, nil, nil, PROV_RSA_FULL, CRYPT_NEWKEYSET) then
        raise ECnRandomAPIError.CreateFmt('Error CryptAcquireContext NewKeySet $%8.8x', [GetLastError]);
    end
    else
        raise ECnRandomAPIError.CreateFmt('Error CryptAcquireContext $%8.8x', [Res]);
  end;

  if HProv <> 0 then
  begin
    try
      Result := CryptGenRandom(HProv, BufByteLen, Buf);
      if not Result then
        raise ECnRandomAPIError.CreateFmt('Error CryptGenRandom $%8.8x', [GetLastError]);
    finally
      CryptReleaseContext(HProv, 0);
    end;
  end;
{$ELSE}
  // MacOS/Linux �µ�������ʵ�֣����ö�ȡ /dev/urandom ���ݵķ�ʽ��������
  F := nil;
  try
    F := TFileStream.Create(DEV_FILE, fmOpenRead);
    Result := F.Read(Buf^, BufByteLen) = BufByteLen;
  finally
    F.Free;
  end;
{$ENDIF}
end;

function CnRandomFillBytes2(Buf: PAnsiChar; BufByteLen: Integer): Boolean;
{$IFNDEF MSWINDOWS}
var
  F: TFileStream;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := CryptGenRandom(FHProv, BufByteLen, Buf);
{$ELSE}
  // MacOS/Linux �µ�������ʵ�֣����ö�ȡ /dev/urandom ���ݵķ�ʽ��������
  F := nil;
  try
    F := TFileStream.Create(DEV_FILE, fmOpenRead);
    Result := F.Read(Buf^, BufByteLen) = BufByteLen;
  finally
    F.Free;
  end;
{$ENDIF}
end;

function CnRandomBytes(ByteLen: Integer): TBytes;
begin
  if ByteLen > 0 then
  begin
    SetLength(Result, ByteLen);
    CnRandomFillBytes2(@Result[0], ByteLen);
  end;
end;

function RandomUInt64: TUInt64;
var
  HL: array[0..1] of Cardinal;
begin
  // ������ϵͳ�������������
  if not CnRandomFillBytes2(@HL[0], SizeOf(TUInt64)) then
  begin
    // ֱ�� Random * High(TUInt64) ���ܻᾫ�Ȳ������� Lo ȫ FF����˷ֿ�����
    Randomize;
    HL[0] := Trunc(Random * High(Cardinal) - 1) + 1;
    HL[1] := Trunc(Random * High(Cardinal) - 1) + 1;
  end;

  Result := (TUInt64(HL[0]) shl 32) + HL[1];
end;

function RandomUInt64LessThan(HighValue: TUInt64): TUInt64;
begin
  Result := UInt64Mod(RandomUInt64, HighValue);
end;

function RandomInt64LessThan(HighValue: Int64): Int64;
var
  HL: array[0..1] of Cardinal;
begin
  // ������ϵͳ�������������
  if not CnRandomFillBytes2(@HL[0], SizeOf(Int64)) then
  begin
    // ֱ�� Random * High(Int64) ���ܻᾫ�Ȳ������� Lo ȫ FF����˷ֿ�����
    Randomize;
    HL[0] := Trunc(Random * High(Integer) - 1) + 1;   // Int64 ���λ������ 1�����⸺��
    HL[1] := Trunc(Random * High(Cardinal) - 1) + 1;
  end
  else
    HL[0] := HL[0] mod (Cardinal(High(Integer)) + 1);    // Int64 ���λ������ 1�����⸺��

  Result := (Int64(HL[0]) shl 32) + HL[1];
  Result := Result mod HighValue; // δ���� HighValue С�ڵ��� 0 ������
end;

function RandomInt64: Int64;
begin
  Result := RandomInt64LessThan(High(Int64));
end;

function RandomUInt32: Cardinal;
var
  D: Cardinal;
begin
  // ������ϵͳ�������������
  if not CnRandomFillBytes2(@D, SizeOf(Cardinal)) then
  begin
    Randomize;
    D := Trunc(Random * High(Cardinal) - 1) + 1;
  end;

  Result := D;
end;

function RandomUInt32LessThan(HighValue: Cardinal): Cardinal;
begin
  Result := RandomUInt32 mod HighValue;
end;

function RandomInt32: Integer;
begin
  Result := RandomInt32LessThan(High(Integer));
end;

function RandomInt32LessThan(HighValue: Integer): Integer;
var
  D: Cardinal;
begin
  // ������ϵͳ�������������
  if not CnRandomFillBytes2(@D, SizeOf(Cardinal)) then
  begin
    Randomize;
    D := Trunc(Random * High(Integer) - 1) + 1;
  end
  else
    D := D mod (Cardinal(High(Integer)) + 1);

  Result := Integer(Int64(D) mod Int64(HighValue)); // δ���� HighValue С�ڵ��� 0 ������
end;

function CnKnuthShuffle(ArrayBase: Pointer; ElementByteSize: Integer;
  ElementCount: Integer): Boolean;
var
  I, R: Integer;
  B1, B2: Pointer;
begin
  Result := False;
  if (ArrayBase = nil) or (ElementByteSize <= 0) or (ElementCount < 0) then // ����������Ȳ�����
    Exit;

  Result := True;
  if ElementCount <= 1 then // ûԪ�ػ�ֻ��һ��Ԫ��ʱ����ϴ
    Exit;

  for I := ElementCount - 1 downto 0 do
  begin
    R := RandomInt32LessThan(I + 1);  // 0 �� I ����������ڵ����������������Ҫ�� 1
    B1 := Pointer(TCnNativeUInt(ArrayBase) + TCnNativeUInt(I * ElementByteSize));
    B2 := Pointer(TCnNativeUInt(ArrayBase) + TCnNativeUInt(R * ElementByteSize));
    MemorySwap(B1, B2, ElementByteSize);
  end;
  Result := True;
end;

{$IFDEF MSWINDOWS}

procedure StartRandom;
var
  Res: DWORD;
  B: Boolean;
begin
  FHProv := 0;
  B := CryptAcquireContext(@FHProv, nil, nil, PROV_RSA_FULL, 0);
  if not B then
    B := CryptAcquireContext(@FHProv, nil, nil, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT);

  if not B then
  begin
    Res := GetLastError;
    if Res = NTE_BAD_KEYSET then // KeyContainer �����ڣ����½��ķ�ʽ
    begin
      if not CryptAcquireContext(@FHProv, nil, nil, PROV_RSA_FULL, CRYPT_NEWKEYSET) then
        raise ECnRandomAPIError.CreateFmt('Error CryptAcquireContext NewKeySet $%8.8x', [GetLastError]);
    end
    else
        raise ECnRandomAPIError.CreateFmt('Error CryptAcquireContext $%8.8x', [Res]);
  end;
end;

procedure StopRandom;
begin
  if FHProv <> 0 then
  begin
    CryptReleaseContext(FHProv, 0);
    FHProv := 0;
  end;
end;

initialization
  StartRandom;

finalization
  StopRandom;

{$ENDIF}

end.
