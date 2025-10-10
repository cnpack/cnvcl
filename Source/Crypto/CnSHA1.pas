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

unit CnSHA1;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�SHA1 �Ӵ��㷨ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
*           ������/����������ֲ���������䲿�ֹ��ܡ�
* ��    ע������Ԫʵ���� SHA1 �Ӵ��㷨����Ӧ�� HMAC �㷨��
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2022.04.26 V1.5
*               �޸� LongWord �� Integer ��ַת����֧�� MacOS64
*           2019.12.12 V1.4
*               ֧�� TBytes
*           2019.04.15 V1.3
*               ֧�� Win32/Win64/MacOS32
*           2015.08.14 V1.2
*               ����л��� Pascal ��֧�ֿ�ƽ̨
*           2014.10.22 V1.1
*               ���� HMAC ����
*           2010.07.14 V1.0
*               ������Ԫ��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF}, CnNative, CnConsts;

type
  PCnSHA1Digest = ^TCnSHA1Digest;
  {* SHA1 �Ӵս��ָ��}
  TCnSHA1Digest = array[0..19] of Byte;
  {* SHA1 �Ӵս����20 �ֽ�}

  TCnSHA1Context = packed record
  {* SHA1 �������Ľṹ}
    Hash: array[0..4] of Cardinal;
    Hi, Lo: Cardinal;
    Buffer: array[0..63] of Byte;
    Index: Integer;
    Ipad: array[0..63] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..63] of Byte;      {!< HMAC: outer padding        }
  end;

  TCnSHA1CalcProgressFunc = procedure (ATotal, AProgress: Int64;
    var Cancel: Boolean) of object;
  {* SHA1 �Ӵս��Ȼص��¼���������}

function SHA1(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA1Digest;
{* �����ݿ���� SHA1 ���㡣

   ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

function SHA1Buffer(const Buffer; Count: Cardinal): TCnSHA1Digest;
{* �����ݿ���� SHA1 ���㡣

   ������
     const Buffer                         - ����������ݿ��ַ
     Count: Cardinal                      - ����������ݿ��ֽڳ���

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

function SHA1Bytes(Data: TBytes): TCnSHA1Digest;
{* ���ֽ�������� SHA1 ���㡣

   ������
     Data: TBytes                         - ��������ֽ�����

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

function SHA1String(const Str: string): TCnSHA1Digest;
{* �� String �������ݽ��� SHA1 ���㡣ע�� D2009 �����ϰ汾�� string Ϊ UnicodeString��
   �����лὫ��ǿ��ת���� AnsiString ���м��㡣

   ������
     const Str: string                    - ��������ַ���

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

function SHA1StringA(const Str: AnsiString): TCnSHA1Digest;
{* �� AnsiString �����ַ������� SHA1 ���㣬ֱ�Ӽ����ڲ����ݣ��ޱ��봦��

   ������
     const Str: AnsiString                - ��������ַ���

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

function SHA1StringW(const Str: WideString): TCnSHA1Digest;
{* �� WideString �����ַ�������ת�������� SHA1 ���㡣
   ����ǰ Windows �»���� WideCharToMultyByte ת��Ϊ AnsiString ���ͣ�
   ����ƽ̨��ֱ��ת��Ϊ AnsiString ���ͣ��ٽ��м��㡣

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

{$IFDEF UNICODE}

function SHA1UnicodeString(const Str: string): TCnSHA1Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA1 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: string                    - ������Ŀ��ַ���

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

{$ELSE}

function SHA1UnicodeString(const Str: WideString): TCnSHA1Digest;
{* �� UnicodeString �������ݽ���ֱ�ӵ� SHA1 ���㣬ֱ�Ӽ����ڲ� UTF16 ���ݣ�������ת����

   ������
     const Str: WideString                - ������Ŀ��ַ���

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

{$ENDIF}

function SHA1File(const FileName: string;
  CallBack: TCnSHA1CalcProgressFunc = nil): TCnSHA1Digest;
{* ��ָ���ļ����ݽ��� SHA1 ���㡣

   ������
     const FileName: string               - ��������ļ���
     CallBack: TCnSHA1CalcProgressFunc    - ������Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

function SHA1Stream(Stream: TStream;
  CallBack: TCnSHA1CalcProgressFunc = nil): TCnSHA1Digest;
{* ��ָ�������ݽ��� SHA1 ���㡣

   ������
     Stream: TStream                      - �������������
     CallBack: TCnSHA1CalcProgressFunc    - ������Ȼص�������Ĭ��Ϊ��

   ����ֵ��TCnSHA1Digest                  - ���ص� SHA1 �Ӵ�ֵ
}

// �����������������ⲿ���������ݽ�����ɢ�� SHA1 ���㣬SHA1Update �ɶ�α�����

procedure SHA1Init(var Context: TCnSHA1Context);
{* ��ʼ��һ�� SHA1 ���������ģ�׼������ SHA1 �����

   ������
     var Context: TCnSHA1Context          - ����ʼ���� SHA1 ������

   ����ֵ�����ޣ�
}

procedure SHA1Update(var Context: TCnSHA1Context; Input: PAnsiChar; ByteLength: Integer);
{* �Գ�ʼ����������Ķ�һ�����ݽ��� SHA1 ���㡣
   �ɶ�ε������������㲻ͬ�����ݿ飬���轫��ͬ�����ݿ�ƴ�����������ڴ��С�

   ������
     var Context: TCnSHA1Context          - SHA1 ������
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Integer                  - ����������ݿ���ֽڳ���

   ����ֵ�����ޣ�
}

procedure SHA1Final(var Context: TCnSHA1Context; var Digest: TCnSHA1Digest);
{* �������ּ��㣬�� SHA1 ��������� Digest �С�

   ������
     var Context: TCnSHA1Context          - SHA1 ������
     var Digest: TCnSHA1Digest            - ���ص� SHA1 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

function SHA1Print(const Digest: TCnSHA1Digest): string;
{* ��ʮ�����Ƹ�ʽ��� SHA1 �Ӵ�ֵ��

   ������
     const Digest: TCnSHA1Digest          - ָ���� SHA1 �Ӵ�ֵ

   ����ֵ��string                         - ����ʮ�������ַ���
}

function SHA1Match(const D1: TCnSHA1Digest; const D2: TCnSHA1Digest): Boolean;
{* �Ƚ����� SHA1 �Ӵ�ֵ�Ƿ���ȡ�

   ������
     const D1: TCnSHA1Digest              - ���Ƚϵ� SHA1 �Ӵ�ֵһ
     const D2: TCnSHA1Digest              - ���Ƚϵ� SHA1 �Ӵ�ֵ��

   ����ֵ��Boolean                        - �����Ƿ����
}

function SHA1DigestToStr(const Digest: TCnSHA1Digest): string;
{* SHA1 �Ӵ�ֵ����ֱ��ת string��ÿ�ֽڶ�Ӧһ�ַ���

   ������
     const Digest: TCnSHA1Digest          - ��ת���� SHA1 �Ӵ�ֵ

   ����ֵ��string                         - ���ص��ַ���
}

procedure SHA1Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA1Digest);
{* ���� SHA1 �� HMAC��Hash-based Message Authentication Code�����㣬
   ����ͨ���ݵļ����ϼ�����Կ�ĸ��Ҳ�м��Ρ�

   ������
     Key: PAnsiChar                       - ������ SHA1 �������Կ���ݿ��ַ
     KeyByteLength: Integer               - ������ SHA1 �������Կ���ݿ��ֽڳ���
     Input: PAnsiChar                     - ����������ݿ��ַ
     ByteLength: Cardinal                 - ����������ݿ��ֽڳ���
     var Output: TCnSHA1Digest            - ���ص� SHA1 �Ӵ�ֵ

   ����ֵ�����ޣ�
}

implementation

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  HMAC_SHA1_BLOCK_SIZE_BYTE = 64;
  HMAC_SHA1_OUTPUT_LENGTH_BYTE = 20;

function LRot32(X: Cardinal; C: Integer): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := X shl (C and 31) + X shr (32 - C and 31);
end;

function F1(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Z xor (X and (Y xor Z));
end;

function F2(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := X xor Y xor Z;
end;

function F3(X, Y, Z: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (X and Y) or (Z and (X or Y));
end;

function RB(A: Cardinal): Cardinal; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

procedure SHA1Compress(var Data: TCnSHA1Context);
var
  A, B, C, D, E, T: Cardinal;
  W: array[0..79] of Cardinal;
  I: Integer;
begin
  Move(Data.Buffer, W, Sizeof(Data.Buffer));
  for I := 0 to 15 do
    W[I] := RB(W[I]);
  for I := 16 to 79 do
    W[I] := LRot32(W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16], 1);
  A := Data.Hash[0];
  B := Data.Hash[1];
  C := Data.Hash[2];
  D := Data.Hash[3];
  E := Data.Hash[4];
  for I := 0 to 19 do
  begin
    T := LRot32(A, 5) + F1(B, C, D) + E + W[I] + $5A827999;
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for I := 20 to 39 do
  begin
    T := LRot32(A, 5) + F2(B, C, D) + E + W[I] + $6ED9EBA1;
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for I := 40 to 59 do
  begin
    T := LRot32(A, 5) + F3(B, C, D) + E + W[I] + $8F1BBCDC;
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for I := 60 to 79 do
  begin
    T := LRot32(A, 5) + F2(B, C, D) + E + W[I] + $CA62C1D6;
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  Data.Hash[0] := Data.Hash[0] + A;
  Data.Hash[1] := Data.Hash[1] + B;
  Data.Hash[2] := Data.Hash[2] + C;
  Data.Hash[3] := Data.Hash[3] + D;
  Data.Hash[4] := Data.Hash[4] + E;
  FillChar(W, Sizeof(W), 0);
  FillChar(Data.Buffer, Sizeof(Data.Buffer), 0);
end;

procedure SHA1Init(var Context: TCnSHA1Context);
begin
  Context.Hi := 0;
  Context.Lo := 0;
  Context.Index := 0;
  FillChar(Context.Buffer, Sizeof(Context.Buffer), 0);
  Context.Hash[0] := $67452301;
  Context.Hash[1] := $EFCDAB89;
  Context.Hash[2] := $98BADCFE;
  Context.Hash[3] := $10325476;
  Context.Hash[4] := $C3D2E1F0;
end;

procedure SHA1UpdateLen(var Context: TCnSHA1Context; Len: Integer);
var
  I: Cardinal;
  K: Integer;
begin
  for K := 0 to 7 do
  begin
    I := Context.Lo;
    Inc(Context.Lo, Len);
    if Context.Lo < I then
      Inc(Context.Hi);
  end;
end;

procedure SHA1Update(var Context: TCnSHA1Context; Input: PAnsiChar; ByteLength: Integer);
var
  B: Integer;
begin
  SHA1UpdateLen(Context, ByteLength);
  while ByteLength > 0 do
  begin
    if 64 - Context.Index > ByteLength then
      B := ByteLength
    else
      B := 64 - Context.Index;

    Move(Input^, Context.Buffer[Context.Index], B);
    Inc(PByte(Input), B);
    Inc(Context.Index, B);
    Dec(ByteLength, B);

    if Context.Index = 64 then
    begin
      Context.Index := 0;
      SHA1Compress(Context);
    end;
  end;
end;

procedure SHA1UpdateW(var Context: TCnSHA1Context; Input: PWideChar; CharLength: Cardinal);
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
    SHA1Update(Context, pContent, iLen);
  finally
    FreeMem(pContent);
  end;
{$ELSE}  // MacOS ��ֱ�Ӱ� UnicodeString ת�� AnsiString ���㣬��֧�ַ� Windows �� Unicode ƽ̨
  S := StrNew(Input);
  A := AnsiString(S);
  SHA1Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure SHA1Final(var Context: TCnSHA1Context; var Digest: TCnSHA1Digest);
type
  PDWord = ^Cardinal;
begin
  Context.Buffer[Context.Index] := $80;
  if Context.Index >= 56 then
    SHA1Compress(Context);
  PDWord(@Context.Buffer[56])^ := RB(Context.Hi);
  PDWord(@Context.Buffer[60])^ := RB(Context.Lo);
  SHA1Compress(Context);
  Context.Hash[0] := RB(Context.Hash[0]);
  Context.Hash[1] := RB(Context.Hash[1]);
  Context.Hash[2] := RB(Context.Hash[2]);
  Context.Hash[3] := RB(Context.Hash[3]);
  Context.Hash[4] := RB(Context.Hash[4]);
  Move(Context.Hash, Digest, Sizeof(Digest));
end;

// �����ݿ���� SHA1 ����
function SHA1(Input: PAnsiChar; ByteLength: Cardinal): TCnSHA1Digest;
var
  Context: TCnSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, Input, ByteLength);
  SHA1Final(Context, Result);
end;

// �����ݿ���� SHA1 ����
function SHA1Buffer(const Buffer; Count: Cardinal): TCnSHA1Digest;
var
  Context: TCnSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(Buffer), Count);
  SHA1Final(Context, Result);
end;

function SHA1Bytes(Data: TBytes): TCnSHA1Digest;
var
  Context: TCnSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA1Final(Context, Result);
end;

// �� String �������ݽ��� SHA1 ����
function SHA1String(const Str: string): TCnSHA1Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA1StringA(AStr);
end;

// �� AnsiString �������ݽ��� SHA1 ����
function SHA1StringA(const Str: AnsiString): TCnSHA1Digest;
var
  Context: TCnSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(Str), Length(Str));
  SHA1Final(Context, Result);
end;

// �� WideString �������ݽ��� SHA1 ����
function SHA1StringW(const Str: WideString): TCnSHA1Digest;
var
  Context: TCnSHA1Context;
begin
  SHA1Init(Context);
  SHA1UpdateW(Context, PWideChar(Str), Length(Str));
  SHA1Final(Context, Result);
end;

{$IFDEF UNICODE}
function SHA1UnicodeString(const Str: string): TCnSHA1Digest;
{$ELSE}
function SHA1UnicodeString(const Str: WideString): TCnSHA1Digest;
{$ENDIF}
var
  Context: TCnSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA1Final(Context, Result);
end;

function InternalSHA1Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TCnSHA1Digest; CallBack: TCnSHA1CalcProgressFunc): Boolean;
var
  Context: TCnSHA1Context;
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
  SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then Exit;
  if Size < BufSize then BufLen := Size
  else BufLen := BufSize;

  CancelCalc := False;
  SHA1Init(Context);
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        SHA1Update(Context, Buf, ReadBytes);
        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    SHA1Final(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// ��ָ�������� SHA1 ����
function SHA1Stream(Stream: TStream;
  CallBack: TCnSHA1CalcProgressFunc): TCnSHA1Digest;
begin
  InternalSHA1Stream(Stream, 4096 * 1024, Result, CallBack);
end;

// ��ָ���ļ����ݽ��� SHA1 ����
function SHA1File(const FileName: string;
  CallBack: TCnSHA1CalcProgressFunc): TCnSHA1Digest;
var
{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: TCnSHA1Context;
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
      InternalSHA1Stream(Stream, 4096 * 1024, Result, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    SHA1Init(Context);
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
                SHA1Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
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
    SHA1Final(Context, Result);
{$ENDIF}
  end;
end;

// ��ʮ�����Ƹ�ʽ��� SHA1 �Ӵ�ֵ
function SHA1Print(const Digest: TCnSHA1Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnSHA1Digest));
end;

// �Ƚ����� SHA1 �Ӵ�ֵ�Ƿ����
function SHA1Match(const D1, D2: TCnSHA1Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 20) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// SHA1 �Ӵ�ֵת string
function SHA1DigestToStr(const Digest: TCnSHA1Digest): string;
begin
  Result := MemoryToString(@Digest[0], SizeOf(TCnSHA1Digest));
end;

procedure SHA1HmacInit(var Ctx: TCnSHA1Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TCnSHA1Digest;
begin
  if KeyLength > HMAC_SHA1_BLOCK_SIZE_BYTE then
  begin
    Sum := SHA1Buffer(Key, KeyLength);
    KeyLength := HMAC_SHA1_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Ctx.Ipad, HMAC_SHA1_BLOCK_SIZE_BYTE, $36);
  FillChar(Ctx.Opad, HMAC_SHA1_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Ctx.Ipad[I] := Byte(Ctx.Ipad[I] xor Byte(Key[I]));
    Ctx.Opad[I] := Byte(Ctx.Opad[I] xor Byte(Key[I]));
  end;

  SHA1Init(Ctx);
  SHA1Update(Ctx, @(Ctx.Ipad[0]), HMAC_SHA1_BLOCK_SIZE_BYTE);
end;

procedure SHA1HmacUpdate(var Ctx: TCnSHA1Context; Input: PAnsiChar; Length: Cardinal);
begin
  SHA1Update(Ctx, Input, Length);
end;

procedure SHA1HmacFinal(var Ctx: TCnSHA1Context; var Output: TCnSHA1Digest);
var
  Len: Integer;
  TmpBuf: TCnSHA1Digest;
begin
  Len := HMAC_SHA1_OUTPUT_LENGTH_BYTE;
  SHA1Final(Ctx, TmpBuf);
  SHA1Init(Ctx);
  SHA1Update(Ctx, @(Ctx.Opad[0]), HMAC_SHA1_BLOCK_SIZE_BYTE);
  SHA1Update(Ctx, @(TmpBuf[0]), Len);
  SHA1Final(Ctx, Output);
end;

procedure SHA1Hmac(Key: PAnsiChar; KeyByteLength: Integer; Input: PAnsiChar;
  ByteLength: Cardinal; var Output: TCnSHA1Digest);
var
  Ctx: TCnSHA1Context;
begin
  SHA1HmacInit(Ctx, Key, KeyByteLength);
  SHA1HmacUpdate(Ctx, Input, ByteLength);
  SHA1HmacFinal(Ctx, Output);
end;

end.
