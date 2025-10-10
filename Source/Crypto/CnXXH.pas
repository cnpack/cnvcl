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

unit CnXXH;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�XXHash �Ӵ��㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע������Ԫʵ���� XXHash ϵ���Ӵ��㷨������ XXH32/XXH64��
*           Seed �൱�� Key ���߳�ʼ��������ֱ��Ӱ�����ռ�������
*           ע���㷨�ڴ���ʹ�� UInt64 �˷�����������������Ѱ� UInt64 �ضϴ���
*           ���⣬32 λ�� 64 λ�������������������ֽ�˳�򣬺�������С���޹ء�
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWinXP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.09.23 V1.0
*               ������Ԫ��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnNative, CnConsts;

type
  PCnXXH32Digest = ^TCnXXH32Digest;
  {* XXH32 �Ӵս��ָ��}
  TCnXXH32Digest = array[0..3] of Byte;
  {* XXH32 �Ӵս����32 λ 4 �ֽ�}

  PCnXXH64Digest = ^TCnXXH64Digest;
  {* XXH64 �Ӵս��ָ��}
  TCnXXH64Digest = array[0..7] of Byte;
  {* XXH64 �Ӵս����64 λ 8 �ֽ�}

  TCnXXH32Context = packed record
  {* XXH32 �������Ľṹ}
    TotalLen: TUInt64;
    V1, V2, V3, V4: Cardinal;
    Mem: array[0..15] of Cardinal;
    MemSize: Cardinal;
    Seed: Cardinal;
  end;

  TCnXXH64Context = packed record
  {* XXH64 �������Ľṹ}
    TotalLen: TUInt64;
    V1, V2, V3, V4: TUInt64;
    Mem: array[0..31] of Byte;
    MemSize: Cardinal;
    Seed: TUInt64;
  end;

  TCnXXHCalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* ���� XXH ϵ���Ӵս��Ȼص��¼���������}

function XXH32(Input: PAnsiChar; ByteLength: Cardinal; Seed: Cardinal = 0): TCnXXH32Digest;
{* �����ݿ���� XXH32 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH64(Input: PAnsiChar; ByteLength: Cardinal; Seed: TUInt64 = 0): TCnXXH64Digest;
{* �����ݿ���� XXH64 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

function XXH32Buffer(const Buffer; Count: Cardinal; Seed: Cardinal = 0): TCnXXH32Digest;
{* �����ݿ���� XXH32 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH32Digest                - ���ص� XXH32 �Ӵ�ֵ
}

function XXH64Buffer(const Buffer; Count: Cardinal; Seed: TUInt64 = 0): TCnXXH64Digest;
{* �����ݿ���� XXH64 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

function XXH32Bytes(Data: TBytes; Seed: Cardinal = 0): TCnXXH32Digest;
{* ���ֽ�������� XXH32 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH64Bytes(Data: TBytes; Seed: TUInt64 = 0): TCnXXH64Digest;
{* ���ֽ�������� XXH64 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

function XXH32String(const Str: string; Seed: Cardinal = 0): TCnXXH32Digest;
{* �� String �������ݽ��� XXH32 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH64String(const Str: string; Seed: TUInt64 = 0): TCnXXH64Digest;
{* �� String �������ݽ��� XXH64 ���㣬ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

function XXH32StringA(const Str: AnsiString; Seed: Cardinal = 0): TCnXXH32Digest;
{* �� AnsiString �������ݽ��� XXH32 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH32StringW(const Str: WideString; Seed: Cardinal = 0): TCnXXH32Digest;
{* �� WideString �������ݽ��� XXH32 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH64StringA(const Str: AnsiString; Seed: TUInt64 = 0): TCnXXH64Digest;
{* �� AnsiString �������ݽ��� XXH64 ���㡣

   ������
     const Str: AnsiString                - ��������ַ���
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

function XXH64StringW(const Str: WideString; Seed: TUInt64 = 0): TCnXXH64Digest;
{* �� WideString �������ݽ��� XXH64 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

{$IFDEF UNICODE}

function XXH32UnicodeString(const Str: string; Seed: Cardinal = 0): TCnXXH32Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� XXH32 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH64UnicodeString(const Str: string; Seed: TUInt64 = 0): TCnXXH64Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� XXH64 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH64Digest                - ���ص� XXH64 �Ӵ�ֵ
}

{$ELSE}

function XXH32UnicodeString(const Str: WideString; Seed: Cardinal = 0): TCnXXH32Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� XXH32 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH64UnicodeString(const Str: WideString; Seed: TUInt64 = 0): TCnXXH64Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� XXH64 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

{$ENDIF}

function XXH32File(const FileName: string; Seed: Cardinal = 0; CallBack: TCnXXHCalcProgressFunc =
  nil): TCnXXH32Digest;
{* ��ָ���ļ����ݽ��� XXH32 ���㡣

   ������
     const FileName: string               - ��������ļ���
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0
     CallBack: TCnXXHCalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH32Stream(Stream: TStream; Seed: Cardinal = 0; CallBack: TCnXXHCalcProgressFunc = nil):
  TCnXXH32Digest;
{* ��ָ�������ݽ��� XXH32 ���㡣

   ������
     Stream: TStream                      - �������������
     Seed: Cardinal                       - ����ֵ��Ĭ��Ϊ 0
     CallBack: TCnXXHCalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnXXH32Digest                 - ���ص� XXH32 �Ӵ�ֵ
}

function XXH64File(const FileName: string; Seed: TUInt64 = 0; CallBack: TCnXXHCalcProgressFunc =
  nil): TCnXXH64Digest;
{* ��ָ���ļ����ݽ��� XXH64 ���㡣

   ������
     const FileName: string               - ��������ļ���
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0
     CallBack: TCnXXHCalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

function XXH64Stream(Stream: TStream; Seed: TUInt64 = 0; CallBack: TCnXXHCalcProgressFunc = nil):
  TCnXXH64Digest;
{* ��ָ�������ݽ��� XXH64 ���㡣

   ������
     Stream: TStream                      - �������������
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0
     CallBack: TCnXXHCalcProgressFunc     - ���Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnXXH64Digest                 - ���ص� XXH64 �Ӵ�ֵ
}

// �����������������ⲿ���������ݽ�����ɢ�� XXH32 ���㣬XXH32Update �ɶ�α�����

procedure XXH32Init(var Context: TCnXXH32Context; Seed: Cardinal = 0);
{* ��ʼ��һ�� XXH32 ���������ģ�׼������ XXH32 �����

   ������
     var Context: TCnXXH32Context        - ����ʼ���� XXH32 ������
     Seed: Cardinal                      - ����ֵ��Ĭ��Ϊ 0

   ����ֵ�����ޣ�
}

procedure XXH32Update(var Context: TCnXXH32Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� XXH32 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnXXH32Context         - XXH32 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure XXH32Final(var Context: TCnXXH32Context; var Digest: TCnXXH32Digest);
{* �������ּ��㣬�� XXH32 ��������� Digest �С�

   ������
     var Context: TCnXXH32Context        - XXH32 ������
     var Digest: TCnXXH32Digest          - ���ص� XXH32 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

// �����������������ⲿ���������ݽ�����ɢ�� XXH64 ���㣬XXH64Update �ɶ�α�����

procedure XXH64Init(var Context: TCnXXH64Context; Seed: TUInt64 = 0);
{* ��ʼ��һ�� XXH64 ���������ģ�׼������ XXH64 �����

   ������
     var Context: TCnXXH64Context         - ����ʼ���� XXH64 ������
     Seed: TUInt64                        - ����ֵ��Ĭ��Ϊ 0

   ����ֵ�����ޣ�
}

procedure XXH64Update(var Context: TCnXXH64Context; Input: PAnsiChar; ByteLength: Cardinal);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� XXH64 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnXXH64Context         - XXH64 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure XXH64Final(var Context: TCnXXH64Context; var Digest: TCnXXH64Digest);
{* �������ּ��㣬�� XXH64 ��������� Digest �С�

   ������
     var Context: TCnXXH64Context        - XXH64 ������
     var Digest: TCnXXH64Digest          - ���ص� XXH64 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

function XXH32Print(const Digest: TCnXXH32Digest): string;
{* ��ʮ�����Ƹ�ʽ��� XXH32 �Ӵ�ֵ��

   ������
     const Digest: TCnXXH32Digest         - ָ���� XXH32 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function XXH64Print(const Digest: TCnXXH64Digest): string;
{* ��ʮ�����Ƹ�ʽ��� XXH64 �Ӵ�ֵ��

   ������
     const Digest: TCnXXH64Digest         - ָ���� XXH64 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function XXH32Match(const D1: TCnXXH32Digest; const D2: TCnXXH32Digest): Boolean;
{* �Ƚ����� XXH32 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnXXH32Digest             - ���Ƚϵ� XXH32 �Ӵ�ֵһ
     const D2: TCnXXH32Digest             - ���Ƚϵ� XXH32 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function XXH64Match(const D1: TCnXXH64Digest; const D2: TCnXXH64Digest): Boolean;
{* �Ƚ����� XXH64 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnXXH64Digest             - ���Ƚϵ� XXH64 �Ӵ�ֵһ
     const D2: TCnXXH64Digest             - ���Ƚϵ� XXH64 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function XXH32DigestToStr(const Digest: TCnXXH32Digest): string;
{* XXH32 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnXXH32Digest         - ��ת���� XXH32 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

function XXH64DigestToStr(const Digest: TCnXXH64Digest): string;
{* XXH64 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnXXH64Digest         - ��ת���� XXH64 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

implementation

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  CN_XXH32_BLOCK_SIZE  = 16;
  CN_XXH64_BLOCK_SIZE  = 32;

  CN_XXH32_PRIME32_1 = 2654435761;
  CN_XXH32_PRIME32_2 = 2246822519;
  CN_XXH32_PRIME32_3 = 3266489917;
  CN_XXH32_PRIME32_4 = 668265263;
  CN_XXH32_PRIME32_5 = 374761393;

  CN_XXH64_PRIME64_1: TUInt64 = $9E3779B185EBCA87; // 11400714785074694791;
  CN_XXH64_PRIME64_2: TUInt64 = $C2B2AE3D27D4EB4F; // 14029467366897019727;
  CN_XXH64_PRIME64_3: TUInt64 = $165667B19E3779F9; // 1609587929392839161;
  CN_XXH64_PRIME64_4: TUInt64 = $85EBCA77C2B2AE63; // 9650029242287828579;
  CN_XXH64_PRIME64_5: TUInt64 = $27D4EB2F165667C5; // 2870177450012600261;

type
  TCnXXHType = (xtXXH32, xtXXH64);

  TCnXXHGeneralDigest = TCnXXH64Digest; // ���

function RolDWord(Value: Cardinal; Shift: Byte): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (Value shl Shift) or (Value shr (32 - Shift));
end;

function RolQWord(Value: TUInt64; Shift: Byte): TUInt64; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (Value shl Shift) or (Value shr (64 - Shift));
end;

procedure XXH32Init(var Context: TCnXXH32Context; Seed: Cardinal);
begin
  FillChar(Context, SizeOf(Context), 0);
  Context.Seed := Seed;
  Context.V1 := Seed + CN_XXH32_PRIME32_1 + CN_XXH32_PRIME32_2;
  Context.V2 := Seed + CN_XXH32_PRIME32_2;
  Context.V3 := Seed + 0;
  Context.V4 := Seed - CN_XXH32_PRIME32_1;
end;

procedure XXH32Update(var Context: TCnXXH32Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  P: PByte;
  Len: Cardinal;
  V1, V2, V3, V4: Cardinal;
  P32: PCardinal;
begin
  if (Input = nil) or (ByteLength = 0) then
    Exit;

  P := PByte(Input);
  Len := ByteLength;
  Context.TotalLen := Context.TotalLen + UInt64(Len);

  // �������������е�����
  if Context.MemSize > 0 then
  begin
    if Context.MemSize + Len < CN_XXH32_BLOCK_SIZE then
    begin
      Move(P^, Context.Mem[Context.MemSize], Len);
      Context.MemSize := Context.MemSize + Len;
      Exit;
    end;

    // ��仺������ 16 �ֽ�
    Move(P^, Context.Mem[Context.MemSize], CN_XXH32_BLOCK_SIZE - Context.MemSize);
    P32 := @Context.Mem[0];

    // ���� 16 �ֽڿ�
    Context.V1 := RolDWord(Context.V1 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    Context.V2 := RolDWord(Context.V2 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    Context.V3 := RolDWord(Context.V3 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    Context.V4 := RolDWord(Context.V4 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;

    Inc(P, CN_XXH32_BLOCK_SIZE - Context.MemSize);
    Dec(Len, CN_XXH32_BLOCK_SIZE - Context.MemSize);
    Context.MemSize := 0;
  end;

  // ���������� 16 �ֽڿ�
  V1 := Context.V1;
  V2 := Context.V2;
  V3 := Context.V3;
  V4 := Context.V4;

  while Len >= CN_XXH32_BLOCK_SIZE do
  begin
    P32 := PCardinal(P);
    V1 := RolDWord(V1 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    V2 := RolDWord(V2 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    V3 := RolDWord(V3 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;
    Inc(P32);
    V4 := RolDWord(V4 + P32^ * CN_XXH32_PRIME32_2, 13) * CN_XXH32_PRIME32_1;

    Inc(P, CN_XXH32_BLOCK_SIZE);
    Dec(Len, CN_XXH32_BLOCK_SIZE);
  end;

  Context.V1 := V1;
  Context.V2 := V2;
  Context.V3 := V3;
  Context.V4 := V4;

  // ����ʣ�����ݵ�������
  if Len > 0 then
  begin
    Move(P^, Context.Mem[0], Len);
    Context.MemSize := Len;
  end;
end;

procedure XXH32UpdateW(var Context: TCnXXH32Context; Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // ����ҳĬ���� 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    XXH32Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  XXH32Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure XXH32Final(var Context: TCnXXH32Context; var Digest: TCnXXH32Digest);
var
  Hash: Cardinal;
  P: PByte;
  P32: PCardinal;
begin
  if Context.TotalLen >= CN_XXH32_BLOCK_SIZE then
  begin
    Hash := RolDWord(Context.V1, 1) + RolDWord(Context.V2, 7) + 
      RolDWord(Context.V3, 12) + RolDWord(Context.V4, 18);
  end
  else
    Hash := Context.Seed + CN_XXH32_PRIME32_5;

  Hash := Hash + Cardinal(Context.TotalLen);

  // ����������ʣ�������
  P := @Context.Mem[0];
  while Context.MemSize >= 4 do
  begin
    Hash := Hash + PCardinal(P)^ * CN_XXH32_PRIME32_3;
    Hash := RolDWord(Hash, 17) * CN_XXH32_PRIME32_4;
    Inc(P, 4);
    Dec(Context.MemSize, 4);
  end;

  while Context.MemSize > 0 do
  begin
    Hash := Hash + P^ * CN_XXH32_PRIME32_5;
    Hash := RolDWord(Hash, 11) * CN_XXH32_PRIME32_1;
    Inc(P);
    Dec(Context.MemSize);
  end;

  // ���ջ��
  Hash := Hash xor (Hash shr 15);
  Hash := Hash * CN_XXH32_PRIME32_2;
  Hash := Hash xor (Hash shr 13);
  Hash := Hash * CN_XXH32_PRIME32_3;
  Hash := Hash xor (Hash shr 16);

  // ���������Digest
  P32 := @Digest[0];
  P32^ := UInt32HostToNetwork(Hash);
end;

procedure XXH64Init(var Context: TCnXXH64Context; Seed: TUInt64);
begin
  FillChar(Context, SizeOf(Context), 0);
  Context.Seed := Seed;
  Context.V1 := Seed + CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_2;
  Context.V2 := Seed + CN_XXH64_PRIME64_2;
  Context.V3 := Seed + 0;
  Context.V4 := Seed - CN_XXH64_PRIME64_1;
end;

procedure XXH64Update(var Context: TCnXXH64Context; Input: PAnsiChar; ByteLength: Cardinal);
var
  P: PByte;
  Len: Cardinal;
  V1, V2, V3, V4: TUInt64;
  P64: PUInt64;
begin
  if (Input = nil) or (ByteLength = 0) then
    Exit;

  P := PByte(Input);
  Len := ByteLength;
  Context.TotalLen := Context.TotalLen + Len;

  // �������������е�����
  if Context.MemSize > 0 then
  begin
    if Context.MemSize + Len < CN_XXH64_BLOCK_SIZE then
    begin
      Move(P^, Context.Mem[Context.MemSize], Len);
      Context.MemSize := Context.MemSize + Len;
      Exit;
    end;

    // ��仺������ 32 �ֽ�
    Move(P^, Context.Mem[Context.MemSize], CN_XXH64_BLOCK_SIZE - Context.MemSize);
    P64 := @Context.Mem[0];

    // ���� 32 �ֽڿ�
    Context.V1 := RolQWord(Context.V1 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    Context.V2 := RolQWord(Context.V2 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    Context.V3 := RolQWord(Context.V3 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    Context.V4 := RolQWord(Context.V4 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;

    Inc(P, CN_XXH64_BLOCK_SIZE - Context.MemSize);
    Dec(Len, CN_XXH64_BLOCK_SIZE - Context.MemSize);
    Context.MemSize := 0;
  end;

  // ���������� 32 �ֽڿ�
  V1 := Context.V1;
  V2 := Context.V2;
  V3 := Context.V3;
  V4 := Context.V4;

  while Len >= CN_XXH64_BLOCK_SIZE do
  begin
    P64 := PUInt64(P);
    V1 := RolQWord(V1 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    V2 := RolQWord(V2 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    V3 := RolQWord(V3 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Inc(P64);
    V4 := RolQWord(V4 + P64^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;

    Inc(P, CN_XXH64_BLOCK_SIZE);
    Dec(Len, CN_XXH64_BLOCK_SIZE);
  end;

  Context.V1 := V1;
  Context.V2 := V2;
  Context.V3 := V3;
  Context.V4 := V4;

  // ����ʣ�����ݵ�������
  if Len > 0 then
  begin
    Move(P^, Context.Mem[0], Len);
    Context.MemSize := Len;
  end;
end;

procedure XXH64UpdateW(var Context: TCnXXH64Context; Input: PWideChar; CharLength: Cardinal);
var
{$IFDEF MSWINDOWS}
  Content: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // ������ UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(Content, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // ����ҳĬ���� 0
      PAnsiChar(Content), CharLength * SizeOf(WideChar), nil, nil);
    XXH64Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  XXH64Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure XXH64Final(var Context: TCnXXH64Context; var Digest: TCnXXH64Digest);
var
  Hash, V1, V2, V3, V4: TUInt64;
  P: PByte;
  P64: PUInt64;
begin
  if Context.TotalLen >= CN_XXH64_BLOCK_SIZE then
  begin
    V1 := Context.V1;
    V2 := Context.V2;
    V3 := Context.V3;
    V4 := Context.V4;
    Hash := RolQWord(V1, 1) + RolQWord(V2, 7) + RolQWord(V3, 12) + RolQWord(V4, 18);

    V1 := RolQWord(V1 * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Hash := (Hash xor V1) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;

    V2 := RolQWord(V2 * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Hash := (Hash xor V2) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;

    V3 := RolQWord(V3 * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Hash := (Hash xor V3) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;

    V4 := RolQWord(V4 * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1;
    Hash := (Hash xor V4) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;
  end
  else
    Hash := Context.Seed + CN_XXH64_PRIME64_5;

  Hash := Hash + Context.TotalLen;

  // ����������ʣ�������
  P := @Context.Mem[0];
  while Context.MemSize >= 8 do
  begin
    Hash := Hash xor (RolQWord(PUInt64(P)^ * CN_XXH64_PRIME64_2, 31) * CN_XXH64_PRIME64_1);
    Hash := RolQWord(Hash, 27) * CN_XXH64_PRIME64_1 + CN_XXH64_PRIME64_4;
    Inc(P, 8);
    Dec(Context.MemSize, 8);
  end;

  while Context.MemSize >= 4 do
  begin
    Hash := Hash xor (PCardinal(P)^ * CN_XXH64_PRIME64_1);
    Hash := RolQWord(Hash, 23) * CN_XXH64_PRIME64_2 + CN_XXH64_PRIME64_3;
    Inc(P, 4);
    Dec(Context.MemSize, 4);
  end;

  while Context.MemSize > 0 do
  begin
    Hash := Hash xor (P^ * CN_XXH64_PRIME64_5);
    Hash := RolQWord(Hash, 11) * CN_XXH64_PRIME64_1;
    Inc(P);
    Dec(Context.MemSize);
  end;

  // ���ջ��
  Hash := Hash xor (Hash shr 33);
  Hash := Hash * CN_XXH64_PRIME64_2;
  Hash := Hash xor (Hash shr 29);
  Hash := Hash * CN_XXH64_PRIME64_3;
  Hash := Hash xor (Hash shr 32);

  // ��������� Digest
  P64 := @Digest[0];
  P64^ := UInt64HostToNetwork(Hash);
end;

// �����ݿ���� XXH32 ����
function XXH32(Input: PAnsiChar; ByteLength: Cardinal; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, Input, ByteLength);
  XXH32Final(Context, Result);
end;

// �����ݿ���� XXH64 ����
function XXH64(Input: PAnsiChar; ByteLength: Cardinal; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, Input, ByteLength);
  XXH64Final(Context, Result);
end;

// �����ݿ���� XXH32 ����
function XXH32Buffer(const Buffer; Count: Cardinal; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, PAnsiChar(Buffer), Count);
  XXH32Final(Context, Result);
end;

// �����ݿ���� XXH64 ����
function XXH64Buffer(const Buffer; Count: Cardinal; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, PAnsiChar(Buffer), Count);
  XXH64Final(Context, Result);
end;

// ���ֽ�������� XXH32 ����
function XXH32Bytes(Data: TBytes; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, PAnsiChar(@Data[0]), Length(Data));
  XXH32Final(Context, Result);
end;

// ���ֽ�������� XXH64 ����
function XXH64Bytes(Data: TBytes; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, PAnsiChar(@Data[0]), Length(Data));
  XXH64Final(Context, Result);
end;

// �� String �������ݽ��� XXH32 ����
function XXH32String(const Str: string; Seed: Cardinal): TCnXXH32Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := XXH32StringA(AStr, Seed);
end;

// �� String �������ݽ��� XXH64 ����
function XXH64String(const Str: string; Seed: TUInt64): TCnXXH64Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := XXH64StringA(AStr, Seed);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� XXH32 ���㣬������ת��
{$IFDEF UNICODE}
function XXH32UnicodeString(const Str: string; Seed: Cardinal): TCnXXH32Digest;
{$ELSE}
function XXH32UnicodeString(const Str: WideString; Seed: Cardinal): TCnXXH32Digest;
{$ENDIF}
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  XXH32Final(Context, Result);
end;

// �� UnicodeString �������ݽ���ֱ�ӵ� XXH64 ���㣬������ת��
{$IFDEF UNICODE}
function XXH64UnicodeString(const Str: string; Seed: TUInt64): TCnXXH64Digest;
{$ELSE}
function XXH64UnicodeString(const Str: WideString; Seed: TUInt64): TCnXXH64Digest;
{$ENDIF}
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  XXH64Final(Context, Result);
end;

// �� AnsiString �������ݽ��� XXH32 ����
function XXH32StringA(const Str: AnsiString; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32Update(Context, PAnsiChar(Str), Length(Str));
  XXH32Final(Context, Result);
end;

// �� WideString �������ݽ��� XXH32 ����
function XXH32StringW(const Str: WideString; Seed: Cardinal): TCnXXH32Digest;
var
  Context: TCnXXH32Context;
begin
  XXH32Init(Context, Seed);
  XXH32UpdateW(Context, PWideChar(Str), Length(Str));
  XXH32Final(Context, Result);
end;

// �� AnsiString �������ݽ��� XXH64 ����
function XXH64StringA(const Str: AnsiString; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64Update(Context, PAnsiChar(Str), Length(Str));
  XXH64Final(Context, Result);
end;

// �� WideString �������ݽ��� XXH64 ����
function XXH64StringW(const Str: WideString; Seed: TUInt64): TCnXXH64Digest;
var
  Context: TCnXXH64Context;
begin
  XXH64Init(Context, Seed);
  XXH64UpdateW(Context, PWideChar(Str), Length(Str));
  XXH64Final(Context, Result);
end;

function InternalXXHStream(Stream: TStream; const BufSize: Cardinal; Seed: TUInt64; var D:
  TCnXXHGeneralDigest; XXHType: TCnXXHType; CallBack: TCnXXHCalcProgressFunc): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
  CancelCalc: Boolean;

  Context32: TCnXXH32Context;
  Context64: TCnXXH64Context;
  Dig32: TCnXXH32Digest;
  Dig64: TCnXXH64Digest;

  procedure _XXHInit;
  begin
    case XXHType of
      xtXXH32:
        XXH32Init(Context32);
      xtXXH64:
        XXH64Init(Context64);
    end;
  end;

  procedure _XXHUpdate;
  begin
    case XXHType of
      xtXXH32:
        XXH32Update(Context32, Buf, ReadBytes);
      xtXXH64:
        XXH64Update(Context64, Buf, ReadBytes);
    end;
  end;

  procedure _XXHFinal;
  begin
    case XXHType of
      xtXXH32:
        XXH32Final(Context32, Dig32);
      xtXXH64:
        XXH64Final(Context64, Dig64);
    end;
  end;

  procedure _CopyResult;
  begin
    case XXHType of
      xtXXH32:
        Move(Dig32[0], D[0], SizeOf(TCnXXH32Digest));
      xtXXH64:
        Move(Dig64[0], D[0], SizeOf(TCnXXH64Digest));
    end;
  end;

begin
  Result := False;
  Size := Stream.Size;
  SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then
    Exit;
  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  _XXHInit;
 
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        _XXHUpdate;

        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    _XXHFinal;
    _CopyResult;
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// ��ָ�������� XXH32 ����
function XXH32Stream(Stream: TStream; Seed: Cardinal; CallBack: TCnXXHCalcProgressFunc):
  TCnXXH32Digest;
var
  Dig: TCnXXHGeneralDigest;
begin
  InternalXXHStream(Stream, 4096 * 1024, Seed, Dig, xtXXH32, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnXXH32Digest));
end;

// ��ָ�������� XXH64 ����
function XXH64Stream(Stream: TStream; Seed: TUInt64; CallBack: TCnXXHCalcProgressFunc):
  TCnXXH64Digest;
var
  Dig: TCnXXHGeneralDigest;
begin
  InternalXXHStream(Stream, 4096 * 1024, Seed, Dig, xtXXH64, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnXXH64Digest));
end;

function FileSizeIsLargeThanMaxOrCanNotMap(const AFileName: string; out IsEmpty: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  H: THandle;
  Info: BY_HANDLE_FILE_INFORMATION;
  Rec: Int64Rec;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := False;
  IsEmpty := False;
  H := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, 0, 0);
  if H = INVALID_HANDLE_VALUE then
    Exit;
  try
    if not GetFileInformationByHandle(H, Info) then
      Exit;
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

function InternalXXHFile(const FileName: string; Seed: TUInt64; XXHType: TCnXXHType;
  CallBack: TCnXXHCalcProgressFunc): TCnXXHGeneralDigest;
var
  Context32: TCnXXH32Context;
  Context64: TCnXXH64Context;
  Dig32: TCnXXH32Digest;
  Dig64: TCnXXH64Digest;

{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
{$ENDIF}
  Stream: TStream;
  FileIsZeroSize: Boolean;

  procedure _XXHInit;
  begin
    case XXHType of
      xtXXH32:
        XXH32Init(Context32);
      xtXXH64:
        XXH64Init(Context64);
    end;
  end;

{$IFDEF MSWINDOWS}
  procedure _XXHUpdate;
  begin
    case XXHType of
      xtXXH32:
        XXH32Update(Context32, ViewPointer, GetFileSize(FileHandle, nil));
      xtXXH64:
        XXH64Update(Context64, ViewPointer, GetFileSize(FileHandle, nil));
    end;
  end;
{$ENDIF}

  procedure _XXHFinal;
  begin
    case XXHType of
      xtXXH32:
        XXH32Final(Context32, Dig32);
      xtXXH64:
        XXH64Final(Context64, Dig64);
    end;
  end;

  procedure _CopyResult(var D: TCnXXHGeneralDigest);
  begin
    case XXHType of
      xtXXH32:
        Move(Dig32[0], D[0], SizeOf(TCnXXH32Digest));
      xtXXH64:
        Move(Dig64[0], D[0], SizeOf(TCnXXH64Digest));
    end;
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // ���� 2G ���ļ����� Map ʧ�ܣ���� Windows ƽ̨����������ʽѭ������
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalXXHStream(Stream, 4096 * 1024, Seed, Result, XXHType, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    _XXHInit;
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
                _XXHUpdate;
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
    _XXHFinal;
    _CopyResult(Result);
{$ENDIF}
  end;
end;

// ��ָ���ļ����ݽ��� XXH32 ����
function XXH32File(const FileName: string; Seed: Cardinal; CallBack: TCnXXHCalcProgressFunc):
  TCnXXH32Digest;
var
  Dig: TCnXXHGeneralDigest;
begin
  Dig := InternalXXHFile(FileName, Seed, xtXXH32, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnXXH32Digest));
end;

// ��ָ���ļ����ݽ��� XXH64 ����
function XXH64File(const FileName: string; Seed: TUInt64; CallBack: TCnXXHCalcProgressFunc):
  TCnXXH64Digest;
var
  Dig: TCnXXHGeneralDigest;
begin
  Dig := InternalXXHFile(FileName, Seed, xtXXH64, CallBack);
  Move(Dig[0], Result[0], SizeOf(TCnXXH64Digest));
end;

// ��ʮ�����Ƹ�ʽ��� XXH32 �Ӵ�ֵ
function XXH32Print(const Digest: TCnXXH32Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnXXH32Digest));
end;

// ��ʮ�����Ƹ�ʽ��� XXH64 �Ӵ�ֵ
function XXH64Print(const Digest: TCnXXH64Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnXXH64Digest));
end;

// �Ƚ����� XXH32 �Ӵ�ֵ�Ƿ����
function XXH32Match(const D1, D2: TCnXXH32Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnXXH32Digest));
end;

// �Ƚ����� XXH64 �Ӵ�ֵ�Ƿ����
function XXH64Match(const D1, D2: TCnXXH64Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnXXH64Digest));
end;

// XXH32 �Ӵ�ֵת string
function XXH32DigestToStr(const Digest: TCnXXH32Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnXXH32Digest));
end;

// XXH64 �Ӵ�ֵת string
function XXH64DigestToStr(const Digest: TCnXXH64Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnXXH64Digest));
end;

end.
