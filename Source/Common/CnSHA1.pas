{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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

unit CnSHA1;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：SHA1 算法实现单元
* 单元作者：刘啸（Liu Xiao）
* 备    注：
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2019.12.12 V1.4
*               支持 TBytes
*           2019.04.15 V1.3
*               支持 Win32/Win64/MacOS
*           2015.08.14 V1.2
*               汇编切换至 Pascal 以支持跨平台
*           2014.10.22 V1.1
*               加入 HMAC 方法
*           2010.07.14 V1.0
*               创建单元。从网上佚名代码移植而来，加入部分功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF};

type
  TSHA1Digest = array[0..19] of Byte;

  TSHA1Context = record
    Hash: array[0..4] of LongWord;
    Hi, Lo: LongWord;
    Buffer: array[0..63] of Byte;
    Index: Integer;
    Ipad: array[0..63] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..63] of Byte;      {!< HMAC: outer padding        }
  end;

  TSHA1CalcProgressFunc = procedure (ATotal, AProgress: Int64;
    var Cancel: Boolean) of object;
  {* 进度回调事件类型声明}

function SHA1Buffer(const Buffer; Count: LongWord): TSHA1Digest;
{* 对数据块进行 SHA1 计算
 |<PRE>
   const Buffer     - 要计算的数据块
   Count: LongWord  - 数据块长度
 |</PRE>}

{$IFDEF TBYTES_DEFINED}

function SHA1Bytes(Data: TBytes): TSHA1Digest;
{* 对 TBytes 进行 SHA1 计算
 |<PRE>
   Data     - 要计算的字节数组
 |</PRE>}

{$ENDIF}

function SHA1String(const Str: string): TSHA1Digest;
{* 对 String 类型数据进行 SHA1 计算，注意 D2009 或以上版本的 string 为 UnicodeString，
   代码中会将其转换成 AnsiString 进行计算
 |<PRE>
   Str: string       - 要计算的字符串
 |</PRE>}

function SHA1StringA(const Str: AnsiString): TSHA1Digest;
{* 对 AnsiString 类型数据进行 SHA1 计算
 |<PRE>
   Str: AnsiString       - 要计算的字符串
 |</PRE>}

function SHA1StringW(const Str: WideString): TSHA1Digest;
{* 对 WideString 类型数据进行 SHA1 计算，计算前会调用 WideCharToMultyByte 进行转换
 |<PRE>
   Str: WideString       - 要计算的字符串
 |</PRE>}

function SHA1UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA1Digest;
{* 对 UnicodeString 类型数据进行直接的 SHA1 计算，不进行转换
 |<PRE>
   Str: UnicodeString/WideString       - 要计算的宽字符串
 |</PRE>}

function SHA1File(const FileName: string;
  CallBack: TSHA1CalcProgressFunc = nil): TSHA1Digest;
{* 对指定文件内容进行 SHA1 计算
 |<PRE>
   FileName: string  - 要计算的文件名
   CallBack: TSHA1CalcProgressFunc - 进度回调函数，默认为空
 |</PRE>}

function SHA1Stream(Stream: TStream;
  CallBack: TSHA1CalcProgressFunc = nil): TSHA1Digest;
{* 对指定流数据进行 SHA1 计算
 |<PRE>
   Stream: TStream  - 要计算的流内容
   CallBack: TSHA1CalcProgressFunc - 进度回调函数，默认为空
 |</PRE>}

procedure SHA1Init(var Context: TSHA1Context);

procedure SHA1Update(var Context: TSHA1Context; Buffer: Pointer; Len: Integer);

procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);

function SHA1Print(const Digest: TSHA1Digest): string;
{* 以十六进制格式输出 SHA1 计算值
 |<PRE>
   Digest: TSHA1Digest  - 指定的 SHA1 计算值
 |</PRE>}

function SHA1Match(const D1, D2: TSHA1Digest): Boolean;
{* 比较两个 SHA1 计算值是否相等
 |<PRE>
   D1: TSHA1Digest   - 需要比较的 SHA1 计算值
   D2: TSHA1Digest   - 需要比较的 SHA1 计算值
 |</PRE>}

function SHA1DigestToStr(aDig: TSHA1Digest): string;
{* SHA1 计算值转 string
 |<PRE>
   aDig: TSHA1Digest   - 需要转换的 SHA1 计算值
 |</PRE>}

procedure SHA1Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA1Digest);

{* Hash-based Message Authentication Code (based on SHA1) }

implementation

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  HMAC_SHA1_BLOCK_SIZE_BYTE = 64;
  HMAC_SHA1_OUTPUT_LENGTH_BYTE = 20;

{$R-}

function LRot16(X: Word; c: Integer): Word; assembler;
begin
  Result := X shl (c and 15) + X shr (16 - c and 15);
//        mov     ecx, &c
//        mov     ax, &X
//        rol     ax, cl
//        mov     &Result, ax
end;

function RRot16(X: Word; c: Integer): Word; assembler;
begin
  Result := X shr (c and 15) + X shl (16 - c and 15);
//        mov     ecx, &c
//        mov     ax, &X
//        ror     ax, cl
//        mov     &Result, ax
end;

function LRot32(X: LongWord; c: Integer): LongWord; register; assembler;
begin
  Result := X shl (c and 31) + X shr (32 - c and 31);
//        mov     ecx, edx
//        rol     eax, cl
end;

function RRot32(X: LongWord; c: Integer): LongWord; register; assembler;
begin
  Result := X shr (c and 31) + X shl (32 - c and 31);
//        mov     ecx, edx
//        ror     eax, cl
end;

procedure XorBlock(I1, I2, O1: PByteArray; Len: Integer);
var
  I: Integer;
begin
  for I := 0 to Len - 1 do
    O1[I] := I1[I] xor I2[I];
end;

procedure IncBlock(P: PByteArray; Len: Integer);
begin
  Inc(P[Len - 1]);
  if (P[Len - 1] = 0) and (Len > 1) then
    IncBlock(P, Len - 1);
end;

function F1(x, y, z: LongWord): LongWord;
begin
  Result := z xor (x and (y xor z));
end;

function F2(x, y, z: LongWord): LongWord;
begin
  Result := x xor y xor z;
end;

function F3(x, y, z: LongWord): LongWord;
begin
  Result := (x and y) or (z and (x or y));
end;

function RB(A: LongWord): LongWord;
begin
  Result := (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

procedure SHA1Compress(var Data: TSHA1Context);
var
  A, B, C, D, E, T: LongWord;
  W: array[0..79] of LongWord;
  i: Integer;
begin
  Move(Data.Buffer, W, Sizeof(Data.Buffer));
  for i := 0 to 15 do
    W[i] := RB(W[i]);
  for i := 16 to 79 do
    W[i] := LRot32(W[i - 3] xor W[i - 8] xor W[i - 14] xor W[i - 16], 1);
  A := Data.Hash[0];
  B := Data.Hash[1];
  C := Data.Hash[2];
  D := Data.Hash[3];
  E := Data.Hash[4];
  for i := 0 to 19 do
  begin
    T := LRot32(A, 5) + F1(B, C, D) + E + W[i] + $5A827999;
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for i := 20 to 39 do
  begin
    T := LRot32(A, 5) + F2(B, C, D) + E + W[i] + $6ED9EBA1;
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for i := 40 to 59 do
  begin
    T := LRot32(A, 5) + F3(B, C, D) + E + W[i] + $8F1BBCDC;
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for i := 60 to 79 do
  begin
    T := LRot32(A, 5) + F2(B, C, D) + E + W[i] + $CA62C1D6;
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

procedure SHA1Init(var Context: TSHA1Context);
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

procedure SHA1UpdateLen(var Context: TSHA1Context; Len: Integer);
var
  i, k: LongWord;
begin
  for k := 0 to 7 do
  begin
    i := Context.Lo;
    Inc(Context.Lo, Len);
    if Context.Lo < i then
      Inc(Context.Hi);
  end;
end;

procedure SHA1Update(var Context: TSHA1Context; Buffer: Pointer; Len: Integer);
type
  PByte = ^Byte;
begin
  SHA1UpdateLen(Context, Len);
  while Len > 0 do
  begin
    Context.Buffer[Context.Index] := PByte(Buffer)^;
    Inc(PByte(Buffer));
    Inc(Context.Index);
    Dec(Len);
    if Context.Index = 64 then
    begin
      Context.Index := 0;
      SHA1Compress(Context);
    end;
  end;
end;

procedure SHA1UpdateW(var Context: TSHA1Context; Input: PWideChar; CharLength: LongWord);
var
{$IFDEF MSWINDOWS}
  pContent: PAnsiChar;
  iLen: Cardinal;
{$ELSE}
  S: string; // 必须是 UnicodeString
  A: AnsiString;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(pContent, CharLength * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, CharLength, // 代码页默认用 0
      PAnsiChar(pContent), CharLength * SizeOf(WideChar), nil, nil);
    SHA1Update(Context, pContent, iLen);
  finally
    FreeMem(pContent);
  end;
{$ELSE}  // MacOS 下直接把 UnicodeString 转成 AnsiString 计算，不支持非 Windows 非 Unicode 平台
  S := StrNew(Input);
  A := AnsiString(S);
  SHA1Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);
type
  PDWord = ^LongWord;
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

// 对数据块进行 SHA1 计算
function SHA1Buffer(const Buffer; Count: Longword): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(Buffer), Count);
  SHA1Final(Context, Result);
end;

{$IFDEF TBYTES_DEFINED}

function SHA1Bytes(Data: TBytes): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SHA1Final(Context, Result);
end;

{$ENDIF}

// 对 String 类型数据进行 SHA1 计算
function SHA1String(const Str: string): TSHA1Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SHA1StringA(AStr);
end;

// 对 AnsiString 类型数据进行 SHA1 计算
function SHA1StringA(const Str: AnsiString): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(Str), Length(Str));
  SHA1Final(Context, Result);
end;

// 对 WideString 类型数据进行 SHA1 计算
function SHA1StringW(const Str: WideString): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1UpdateW(Context, PWideChar(Str), Length(Str));
  SHA1Final(Context, Result);
end;

function SHA1UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SHA1Final(Context, Result);
end;

function InternalSHA1Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TSHA1Digest; CallBack: TSHA1CalcProgressFunc = nil): Boolean;
var
  Context: TSHA1Context;
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
    Stream.Seek(0, soFromBeginning);
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

// 对指定流进行 SHA1 计算
function SHA1Stream(Stream: TStream;
  CallBack: TSHA1CalcProgressFunc = nil): TSHA1Digest;
begin
  InternalSHA1Stream(Stream, 4096 * 1024, Result, CallBack);
end;

// 对指定文件数据进行 SHA1 计算
function SHA1File(const FileName: string;
  CallBack: TSHA1CalcProgressFunc): TSHA1Digest;
var
{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: TSHA1Context;
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
    Result := True; // 非 Windows 平台返回 True，表示不 Mapping
{$ENDIF}
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMaxOrCanNotMap(FileName, FileIsZeroSize) then
  begin
    // 大于 2G 的文件可能 Map 失败，或非 Windows 平台，采用流方式循环处理
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
              raise Exception.Create('MapViewOfFile Failed. ' + IntToStr(GetLastError));
            end;
          finally
            CloseHandle(MapHandle);
          end;
        end
        else
        begin
          if not FileIsZeroSize then
            raise Exception.Create('CreateFileMapping Failed. ' + IntToStr(GetLastError));
        end;
      finally
        CloseHandle(FileHandle);
      end;
    end;
    SHA1Final(Context, Result);
{$ENDIF}
  end;
end;

// 以十六进制格式输出 SHA1 计算值
function SHA1Print(const Digest: TSHA1Digest): string;
var
  I: Byte;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  for I := 0 to 19 do
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(Digest[I] shr 4) and $0f] +
      Digits[Digest[I] and $0f]);
end;

// 比较两个 SHA1 计算值是否相等
function SHA1Match(const D1, D2: TSHA1Digest): Boolean;
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

// SHA1 计算值转 string
function SHA1DigestToStr(aDig: TSHA1Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 20);
  for I := 1 to 20 do
    Result[I] := Chr(aDig[I - 1]);
end;

procedure SHA1HmacInit(var Ctx: TSHA1Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TSHA1Digest;
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

procedure SHA1HmacUpdate(var Ctx: TSHA1Context; Input: PAnsiChar; Length: LongWord);
begin
  SHA1Update(Ctx, Input, Length);
end;

procedure SHA1HmacFinal(var Ctx: TSHA1Context; var Output: TSHA1Digest);
var
  Len: Integer;
  TmpBuf: TSHA1Digest;
begin
  Len := HMAC_SHA1_OUTPUT_LENGTH_BYTE;
  SHA1Final(Ctx, TmpBuf);
  SHA1Init(Ctx);
  SHA1Update(Ctx, @(Ctx.Opad[0]), HMAC_SHA1_BLOCK_SIZE_BYTE);
  SHA1Update(Ctx, @(TmpBuf[0]), Len);
  SHA1Final(Ctx, Output);
end;

procedure SHA1Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA1Digest);
var
  Ctx: TSHA1Context;
begin
  SHA1HmacInit(Ctx, Key, KeyLength);
  SHA1HmacUpdate(Ctx, Input, Length);
  SHA1HmacFinal(Ctx, Output);
end;

end.
