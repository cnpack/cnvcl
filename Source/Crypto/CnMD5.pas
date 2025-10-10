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

{******************************************************************************}
{                                                                              }
{                                 MD5 Message-Digest for Delphi 4              }
{                                                                              }
{                                 Delphi 4 Unit implementing the               }
{                      RSA Data Security, Inc. MD5 Message-Digest Algorithm    }
{                                                                              }
{                          Implementation of Ronald L. Rivest's RFC 1321       }
{                                                                              }
{                      Copyright ?1997-1999 Medienagentur Fichtner & Meyer     }
{                                  Written by Matthias Fichtner                }
{                                                                              }
{ -----------------------------------------------------------------------------}
{        See RFC 1321 for RSA Data Security's copyright and license notice!    }
{ -----------------------------------------------------------------------------}
{        The latest release of md5.pas will always be available from           }
{        the distribution site at: http://www.fichtner.net/delphi/md5/         }
{ -----------------------------------------------------------------------------}
{                       Please send questions, bug reports and suggestions     }
{                      regarding this code to: mfichtner@fichtner-meyer.com    }
{ -----------------------------------------------------------------------------}
{                        This code is provided "as is" without express or      }
{                     implied warranty of any kind. Use it at your own risk.   }
{******************************************************************************}

unit CnMD5;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�MD5 �Ӵ��㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ����壨QSoft�� hq.com@263.net; http://qsoft.51.net
*           ���� Ronald L. Rivest �� MD5.pas ��д������ԭʼ����
* ��    ע������Ԫʵ���� MD5 �Ӵ��㷨����Ӧ�� HMAC �㷨��
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.12.12 V1.4
*               ֧�� TBytes
*           2019.04.15 V1.3
*               ֧�� Win32/Win64/MacOS
*           2014.11.14 V1.2
*               ����л��� Pascal ��֧�ֿ�ƽ̨
*           2003.09.18 V1.1
*               �ò������ҵ��˸õ�Ԫԭ���ߵİ�Ȩ����
*           2003.09.18 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnConsts, CnNative {$IFDEF MSWINDOWS}, Windows {$ENDIF};

type
  PMD5Digest = ^TCnMD5Digest;
  {* MD5 �Ӵս��ָ��}
  TCnMD5Digest = array[0..15] of Byte;
  {* MD5 �Ӵս����16 �ֽ�}

  TCnMD5Count = array[0..1] of Cardinal;
  {* MD5 �ڲ������ṹ}
  TCnMD5State = array[0..3] of Cardinal;
  {* MD5 �ڲ�״̬�ṹ}
  TCnMD5Block = array[0..15] of Cardinal;
  {* MD5 �ڲ���ṹ}

  TCnMD5Buffer = array[0..63] of Byte;
  {* MD5 �ڲ��������ṹ}

  TCnMD5Context = packed record
  {* MD5 �������Ľṹ}
    State   : TCnMD5State;
    Count   : TCnMD5Count;
    Buffer  : TCnMD5Buffer;
    Ipad    : array[0..63] of Byte;      {!< HMAC: inner padding        }
    Opad    : array[0..63] of Byte;      {!< HMAC: outer padding        }
  end;

  TCnMD5CalcProgressFunc = procedure (ATotal, AProgress: Int64;
    var Cancel: Boolean) of object;
  {* ���Ȼص��¼���������}

//----------------------------------------------------------------
// �û� API ��������
//----------------------------------------------------------------

function MD5(Input: PAnsiChar; ByteLength: Cardinal): TCnMD5Digest;
{* �����ݿ���� MD5 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

function MD5Buffer(const Buffer; Count: Cardinal): TCnMD5Digest;
{* �����ݿ���� MD5 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

function MD5Bytes(Data: TBytes): TCnMD5Digest;
{* ���ֽ�������� MD5 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

function MD5String(const Str: string): TCnMD5Digest;
{* �� String �������ݽ��� MD5 ���㡣ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣


   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

function MD5StringA(const Str: AnsiString): TCnMD5Digest;
{* �� AnsiString �������ݽ��� MD5 ���㣬ֱ�Ӽ����ڲ����ݣ��ޱ��봦��

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

function MD5StringW(const Str: WideString): TCnMD5Digest;
{* �� WideString �����ַ�������ת�������� MD5 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

{$IFDEF UNICODE}

function MD5UnicodeString(const Str: string): TCnMD5Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� MD5 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

{$ELSE}

function MD5UnicodeString(const Str: WideString): TCnMD5Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� MD5 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����


   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

{$ENDIF}

function MD5File(const FileName: string;
  CallBack: TCnMD5CalcProgressFunc = nil): TCnMD5Digest;
{* ��ָ���ļ����ݽ��� MD5 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnMD5CalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

function MD5Stream(Stream: TStream;
  CallBack: TCnMD5CalcProgressFunc = nil): TCnMD5Digest;
{* ��ָ�������ݽ��� MD5 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnMD5CalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnMD5Digest                   - ���ص� MD5 �Ӵ�ֵ
}

// �����������������ⲿ���������ݽ�����ɢ�� MD5 ���㣬MD5Update �ɶ�α�����

procedure MD5Init(var Context: TCnMD5Context);
{* ��ʼ��һ�� MD5 ���������ģ�׼������ MD5 �����

   ������
     var Context: TCnMD5Context           - ����ʼ���� MD5 ������

   ����ֵ�����ޣ�
}

procedure MD5Update(var Context: TCnMD5Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� MD5 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnMD5Context           - MD5 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure MD5Final(var Context: TCnMD5Context; var Digest: TCnMD5Digest);
{* �������ּ��㣬�� MD5 ��������� Digest �С�

   ������
     var Context: TCnMD5Context           - MD5 ������
     var Digest: TCnMD5Digest             - ���ص� MD5 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

function MD5Print(const Digest: TCnMD5Digest): string;
{* ��ʮ�����Ƹ�ʽ��� MD5 �Ӵ�ֵ��

   ������
     const Digest: TCnMD5Digest           - ָ���� MD5 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function MD5Match(const D1: TCnMD5Digest; const D2: TCnMD5Digest): Boolean;
{* �Ƚ����� MD5 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnMD5Digest               - ���Ƚϵ� MD5 �Ӵ�ֵһ
     const D2: TCnMD5Digest               - ���Ƚϵ� MD5 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function MD5DigestToStr(const Digest: TCnMD5Digest): string;
{* MD5 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnMD5Digest           - ��ת���� MD5 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

procedure MD5Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnMD5Digest);
{* ���� MD5 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ MD5 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ MD5 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnMD5Digest             - ���ص� MD5 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

implementation

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  HMAC_MD5_BLOCK_SIZE_BYTE = 64;
  HMAC_MD5_OUTPUT_LENGTH_BYTE = 16;

type
  TMD5CBits = array[0..7] of Byte;

var
  PADDING: TCnMD5Buffer = (
    $80, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
  );

function F(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and Y) or ((not X) and Z);
end;

function G(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and Z) or (Y and (not Z));
end;

function H(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := X xor Y xor Z;
end;

function I(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Y xor (X or (not Z));
end;

procedure ROT(var X: Cardinal; N: BYTE); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  X := (X shl N) or (X shr (32 - N));
end;

procedure FF(var A: Cardinal; B, C, D, X: Cardinal; S: BYTE; AC: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Inc(A, F(B, C, D) + X + AC);
  ROT(A, S);
  Inc(A, B);
end;

procedure GG(var A: Cardinal; B, C, D, X: Cardinal; S: BYTE; AC: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Inc(A, G(B, C, D) + X + AC);
  ROT(A, S);
  Inc(A, B);
end;

procedure HH(var A: Cardinal; B, C, D, X: Cardinal; S: BYTE; AC: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Inc(A, H(B, C, D) + X + AC);
  ROT(A, S);
  Inc(A, B);
end;

procedure II(var A: Cardinal; B, C, D, X: Cardinal; S: BYTE; AC: Cardinal); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Inc(A, I(B, C, D) + X + AC);
  ROT(A, S);
  Inc(A, B);
end;

// Encode Count bytes at Source into (Count / 4) DWORDs at Target
procedure Encode(Source, Target: Pointer; Count: Cardinal);
var
  S: PByte;
  T: PCardinal;
  I: Cardinal;
begin
  S := Source;
  T := Target;
  for I := 1 to Count div 4 do
  begin
    T^ := S^;
    Inc(S);
    T^ := T^ or (S^ shl 8);
    Inc(S);
    T^ := T^ or (S^ shl 16);
    Inc(S);
    T^ := T^ or (S^ shl 24);
    Inc(S);
    Inc(T);
  end;
end;

// Decode Count DWORDs at Source into (Count * 4) Bytes at Target
procedure Decode(Source, Target: Pointer; Count: Cardinal);
var
  S: PCardinal;
  T: PByte;
  I: Cardinal;
begin
  S := Source;
  T := Target;
  for I := 1 to Count do
  begin
    T^ := S^ and $ff;
    Inc(T);
    T^ := (S^ shr 8) and $ff;
    Inc(T);
    T^ := (S^ shr 16) and $ff;
    Inc(T);
    T^ := (S^ shr 24) and $ff;
    Inc(T);
    Inc(S);
  end;
end;

// Transform State according to first 64 bytes at Buffer
procedure Transform(Buffer: Pointer; var State: TCnMD5State);
var
  A, B, C, D: Cardinal;
  Block: TCnMD5Block;
begin
  Encode(Buffer, @Block, 64);
  A := State[0];
  B := State[1];
  C := State[2];
  D := State[3];
  FF (A, B, C, D, Block[ 0],  7, $d76aa478);
  FF (D, A, B, C, Block[ 1], 12, $e8c7b756);
  FF (C, D, A, B, Block[ 2], 17, $242070db);
  FF (B, C, D, A, Block[ 3], 22, $c1bdceee);
  FF (A, B, C, D, Block[ 4],  7, $f57c0faf);
  FF (D, A, B, C, Block[ 5], 12, $4787c62a);
  FF (C, D, A, B, Block[ 6], 17, $a8304613);
  FF (B, C, D, A, Block[ 7], 22, $fd469501);
  FF (A, B, C, D, Block[ 8],  7, $698098d8);
  FF (D, A, B, C, Block[ 9], 12, $8b44f7af);
  FF (C, D, A, B, Block[10], 17, $ffff5bb1);
  FF (B, C, D, A, Block[11], 22, $895cd7be);
  FF (A, B, C, D, Block[12],  7, $6b901122);
  FF (D, A, B, C, Block[13], 12, $fd987193);
  FF (C, D, A, B, Block[14], 17, $a679438e);
  FF (B, C, D, A, Block[15], 22, $49b40821);
  GG (A, B, C, D, Block[ 1],  5, $f61e2562);
  GG (D, A, B, C, Block[ 6],  9, $c040b340);
  GG (C, D, A, B, Block[11], 14, $265e5a51);
  GG (B, C, D, A, Block[ 0], 20, $e9b6c7aa);
  GG (A, B, C, D, Block[ 5],  5, $d62f105d);
  GG (D, A, B, C, Block[10],  9,  $2441453);
  GG (C, D, A, B, Block[15], 14, $d8a1e681);
  GG (B, C, D, A, Block[ 4], 20, $e7d3fbc8);
  GG (A, B, C, D, Block[ 9],  5, $21e1cde6);
  GG (D, A, B, C, Block[14],  9, $c33707d6);
  GG (C, D, A, B, Block[ 3], 14, $f4d50d87);
  GG (B, C, D, A, Block[ 8], 20, $455a14ed);
  GG (A, B, C, D, Block[13],  5, $a9e3e905);
  GG (D, A, B, C, Block[ 2],  9, $fcefa3f8);
  GG (C, D, A, B, Block[ 7], 14, $676f02d9);
  GG (B, C, D, A, Block[12], 20, $8d2a4c8a);
  HH (A, B, C, D, Block[ 5],  4, $fffa3942);
  HH (D, A, B, C, Block[ 8], 11, $8771f681);
  HH (C, D, A, B, Block[11], 16, $6d9d6122);
  HH (B, C, D, A, Block[14], 23, $fde5380c);
  HH (A, B, C, D, Block[ 1],  4, $a4beea44);
  HH (D, A, B, C, Block[ 4], 11, $4bdecfa9);
  HH (C, D, A, B, Block[ 7], 16, $f6bb4b60);
  HH (B, C, D, A, Block[10], 23, $bebfbc70);
  HH (A, B, C, D, Block[13],  4, $289b7ec6);
  HH (D, A, B, C, Block[ 0], 11, $eaa127fa);
  HH (C, D, A, B, Block[ 3], 16, $d4ef3085);
  HH (B, C, D, A, Block[ 6], 23,  $4881d05);
  HH (A, B, C, D, Block[ 9],  4, $d9d4d039);
  HH (D, A, B, C, Block[12], 11, $e6db99e5);
  HH (C, D, A, B, Block[15], 16, $1fa27cf8);
  HH (B, C, D, A, Block[ 2], 23, $c4ac5665);
  II (A, B, C, D, Block[ 0],  6, $f4292244);
  II (D, A, B, C, Block[ 7], 10, $432aff97);
  II (C, D, A, B, Block[14], 15, $ab9423a7);
  II (B, C, D, A, Block[ 5], 21, $fc93a039);
  II (A, B, C, D, Block[12],  6, $655b59c3);
  II (D, A, B, C, Block[ 3], 10, $8f0ccc92);
  II (C, D, A, B, Block[10], 15, $ffeff47d);
  II (B, C, D, A, Block[ 1], 21, $85845dd1);
  II (A, B, C, D, Block[ 8],  6, $6fa87e4f);
  II (D, A, B, C, Block[15], 10, $fe2ce6e0);
  II (C, D, A, B, Block[ 6], 15, $a3014314);
  II (B, C, D, A, Block[13], 21, $4e0811a1);
  II (A, B, C, D, Block[ 4],  6, $f7537e82);
  II (D, A, B, C, Block[11], 10, $bd3af235);
  II (C, D, A, B, Block[ 2], 15, $2ad7d2bb);
  II (B, C, D, A, Block[ 9], 21, $eb86d391);
  Inc(State[0], A);
  Inc(State[1], B);
  Inc(State[2], C);
  Inc(State[3], D);
end;

// Initialize given Context
procedure MD5Init(var Context: TCnMD5Context);
begin
  with Context do
  begin
    State[0] := $67452301;
    State[1] := $EFCDAB89;
    State[2] := $98BADCFE;
    State[3] := $10325476;
    Count[0] := 0;
    Count[1] := 0;
    // ZeroMemory(@Buffer, SizeOf(TMD5Buffer));
    FillChar(Buffer, SizeOf(TCnMD5Buffer), 0);
  end;
end;

// Update given Context to include Length bytes of Input
procedure MD5Update(var Context: TCnMD5Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  Index: Cardinal;
  PartLen: Cardinal;
  I: Cardinal;
begin
  with Context do
  begin
    Index := (Count[0] shr 3) and $3F;
    Inc(Count[0], ByteLength shl 3);
    if Count[0] < (ByteLength shl 3) then Inc(Count[1]);
    Inc(Count[1], ByteLength shr 29);
  end;

  PartLen := 64 - Index;
  if ByteLength >= PartLen then
  begin
    Move(Input^, Context.Buffer[Index], PartLen);
    Transform(@Context.Buffer, Context.State);
    I := PartLen;
    while I + 63 < ByteLength do
    begin
      Transform(@Input[I], Context.State);
      Inc(I, 64);
    end;
    Index := 0;
  end
  else
    I := 0;

  Move(Input[I], Context.Buffer[Index], ByteLength - I);
end;

procedure MD5UpdateW(var Context: TCnMD5Context; Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  pContent: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(pContent, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // ����ҳĬ���� 0
      PAnsiChar(pContent), CharLength * SizeOf(WideChar), nil, nil);
    MD5Update(Context, pContent, iLen);
  finally
    FreeMem(pContent);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  MD5Update(Context, @A[1], Length(A));
{$ENDIF}
end;

// Finalize given Context, create Digest
procedure MD5Final(var Context: TCnMD5Context; var Digest: TCnMD5Digest);
var
  Bits: TMD5CBits;
  Index: Cardinal;
  PadLen: Cardinal;
begin
  Decode(@Context.Count, @Bits, 2);
  Index := (Context.Count[0] shr 3) and $3f;
  if Index < 56 then
    PadLen := 56 - Index
  else
    PadLen := 120 - Index;
  MD5Update(Context, @PADDING, PadLen);
  MD5Update(Context, @Bits, 8);
  Decode(@Context.State, @Digest, 4);
end;

function InternalMD5Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TCnMD5Digest; CallBack: TCnMD5CalcProgressFunc): Boolean;
var
  Context: TCnMD5Context;
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;
begin
  Result := False;
  Size := Stream.Size;
  if Size = 0 then
    Exit;

  SavePos := Stream.Position;
  TotalBytes := 0;

  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  MD5Init(Context);
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        MD5Update(Context, Buf, ReadBytes);
        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    MD5Final(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// �����ݿ���� MD5 ����
function MD5(Input: PAnsiChar; ByteLength: Cardinal): TCnMD5Digest;
var
  Context: TCnMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, Input, ByteLength);
  MD5Final(Context, Result);
end;

// �����ݿ���� MD5 ����
function MD5Buffer(const Buffer; Count: Cardinal): TCnMD5Digest;
var
  Context: TCnMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(Buffer), Count);
  MD5Final(Context, Result);
end;

function MD5Bytes(Data: TBytes): TCnMD5Digest;
var
  Context: TCnMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(@Data[0]), Length(Data));
  MD5Final(Context, Result);
end;

// �� String �������ݽ��� MD5 ����
function MD5String(const Str: string): TCnMD5Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := MD5StringA(AStr);
end;

// �� AnsiString �������ݽ��� MD5 ����
function MD5StringA(const Str: AnsiString): TCnMD5Digest;
var
  Context: TCnMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(Str), Length(Str));
  MD5Final(Context, Result);
end;

// �� WideString �������ݽ��� MD5 ����
function MD5StringW(const Str: WideString): TCnMD5Digest;
var
  Context: TCnMD5Context;
begin
  MD5Init(Context);
  MD5UpdateW(Context, PWideChar(Str), Length(Str));
  MD5Final(Context, Result);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� MD5 ���㣬������ת��
{$IFDEF UNICODE}
function MD5UnicodeString(const Str: string): TCnMD5Digest;
{$ELSE}
function MD5UnicodeString(const Str: WideString): TCnMD5Digest;
{$ENDIF}
var
  Context: TCnMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  MD5Final(Context, Result);
end;

// ��ָ���ļ����ݽ��� MD5 ����
function MD5File(const FileName: string;
  CallBack: TCnMD5CalcProgressFunc): TCnMD5Digest;
var
{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: TCnMD5Context;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  function FileSizeIsLargeThanMaxOrCanNotMap(const AFileName: string; out IsEmpty: Boolean): Boolean;
{$IFDEF MSWINDOWS}
  var
    H: THandle;
    Info: BY_HANDLE_FILE_INFORMATION;
    Rec : Int64Rec;
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
    Result := False;
    IsEmpty := False;
    H := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if H = INVALID_HANDLE_VALUE then Exit;
    try
      if not GetFileInformationByHandle(H, Info) then Exit;
    finally
      CloseHandle(H);
    end;
    Rec.Lo := Info.nFileSizeLow;
    Rec.Hi := Info.nFileSizeHigh;
    Result := (Rec.Hi > 0) or (Rec.Lo > MAX_FILE_SIZE);
    IsEmpty := (Rec.Hi = 0) and (Rec.Lo = 0);
{$ELSE}
    Result := True; // �� Windows ƽ̨���� True����ʾ�� Mapping
{$ENDIF}
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���� Windows ƽ̨����������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalMD5Stream(Stream, 4096 * 1024, Result, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    MD5Init(Context);
    FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
                  FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
                  FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if FileHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
        if MapHandle <> 0 then
        begin
          try
            ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
            if ViewPointer <> nil then
            begin
              try
                MD5Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
              finally
                UnmapViewOfFile(ViewPointer);
              end;
            end
            else
            begin
              raise Exception.Create(SCnErrorMapViewOfFile + IntToStr(GetLastError));
            end;
          finally
            CloseHandle(MapHandle);
          end;
        end
        else
        begin
          if not FileIsZeroSize then
            raise Exception.Create(SCnErrorCreateFileMapping + IntToStr(GetLastError));
        end;
      finally
        CloseHandle(FileHandle);
      end;
    end;
    MD5Final(Context, Result);
{$ENDIF}
  end;
end;

// ��ָ�������� MD5 ����
function MD5Stream(Stream: TStream;
  CallBack: TCnMD5CalcProgressFunc): TCnMD5Digest;
begin
  InternalMD5Stream(Stream, 4096 * 1024, Result, CallBack);
end;

// ��ʮ�����Ƹ�ʽ��� MD5 �Ӵ�ֵ
function MD5Print(const Digest: TCnMD5Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnMD5Digest));
end;

// �Ƚ����� MD5 �Ӵ�ֵ�Ƿ����
function MD5Match(const D1, D2: TCnMD5Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnMD5Digest));
end;

// MD5 �Ӵ�ֵת string
function MD5DigestToStr(const Digest: TCnMD5Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnMD5Digest));
end;

procedure MD5HmacInit(var Ctx: TCnMD5Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnMD5Digest;
begin
  if KeyLength > HMAC_MD5_BLOCK_SIZE_BYTE then
  begin
    Sum := MD5Buffer(Key, KeyLength);
    KeyLength := HMAC_MD5_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Ctx.Ipad, HMAC_MD5_BLOCK_SIZE_BYTE, $36);
  FillChar(Ctx.Opad, HMAC_MD5_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Ctx.Ipad[I] := Byte(Ctx.Ipad[I] xor Byte(Key[I]));
    Ctx.Opad[I] := Byte(Ctx.Opad[I] xor Byte(Key[I]));
  end;

  MD5Init(Ctx);
  MD5Update(Ctx, @(Ctx.Ipad[0]), HMAC_MD5_BLOCK_SIZE_BYTE);
end;

procedure MD5HmacUpdate(var Ctx: TCnMD5Context; Input: PAnsiChar; Length: Cardinal);
begin
  MD5Update(Ctx, Input, Length);
end;

procedure MD5HmacFinal(var Ctx: TCnMD5Context; var Output: TCnMD5Digest);
var
  Len: Integer;
  TmpBuf: TCnMD5Digest;
begin
  Len := HMAC_MD5_OUTPUT_LENGTH_BYTE;
  MD5Final(Ctx, TmpBuf);
  MD5Init(Ctx);
  MD5Update(Ctx, @(Ctx.Opad[0]), HMAC_MD5_BLOCK_SIZE_BYTE);
  MD5Update(Ctx, @(TmpBuf[0]), Len);
  MD5Final(Ctx, Output);
end;

procedure MD5Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnMD5Digest);
var
  Ctx: TCnMD5Context;
begin
  MD5HmacInit(Ctx, Key, KeyByteLength);
  MD5HmacUpdate(Ctx, Input, ByteLength);
  MD5HmacFinal(Ctx, Output);
end;

end.
