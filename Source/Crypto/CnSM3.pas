{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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

unit CnSM3;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：国产散列算法 SM3 实现单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 备    注：参考国密算法公开文档《SM3 Cryptographic Hash Algorith》
*           http://www.oscca.gov.cn/UpFile/20101222141857786.pdf
*           并参考移植 goldboar 的 C 代码
* 开发平台：Windows 7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2019.12.12 V1.2
*               支持 TBytes
*           2019.04.15 V1.1
*               支持 Win32/Win64/MacOS
*           2014.09.23 V1.0
*               移植并创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNativeDecl {$IFDEF MSWINDOWS}, Windows {$ENDIF};

type
  TSM3Context = packed record
    Total: array[0..1] of LongWord;     {!< number of bytes processed  }
    State: array[0..8] of LongWord;     {!< intermediate digest state  }
    Buffer: array[0..63] of Byte;    {!< data block being processed }
    Ipad: array[0..63] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..63] of Byte;      {!< HMAC: outer padding        }
  end;
  PSM3Context = ^TSM3Context;

  PSM3Digest = ^TSM3Digest;
  TSM3Digest = array[0..31] of Byte;

  TSM3CalcProgressFunc = procedure (ATotal, AProgress: Int64;
    var Cancel: Boolean) of object;

procedure SM3Start(var Ctx: TSM3Context);

procedure SM3Update(var Ctx: TSM3Context; Input: PAnsiChar; CharLength: LongWord);

procedure SM3Finish(var Ctx: TSM3Context; var Output: TSM3Digest);

function SM3(Input: PAnsiChar; Length: LongWord): TSM3Digest;
{* 对数据块进行 SM3 计算
 |<PRE>
   Input: PAnsiChar  - 要计算的数据块
   Length: LongWord  - 数据块长度
 |</PRE>}

//procedure SM3HmacStarts(var Ctx: TSM3Context; Key: PAnsiChar; KeyLength: Integer);
//
//procedure SM3HmacUpdate(var Ctx: TSM3Context; Input: PAnsiChar; Length: LongWord);
//
//procedure SM3HmacFinish(var Ctx: TSM3Context; var Output: TSM3Digest);

procedure SM3Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSM3Digest);

{* Hash-based Message Authentication Code (based on SM3) }

function SM3Buffer(const Buffer; Count: LongWord): TSM3Digest;
{* 对数据块进行 SM3 计算
 |<PRE>
   const Buffer     - 要计算的数据块，一般传个地址
   Count: LongWord  - 数据块长度
 |</PRE>}

function SM3Bytes(Data: TBytes): TSM3Digest;
{* 对 TBytes 进行 MD5 计算
 |<PRE>
   Data     - 要计算的字节数组
 |</PRE>}

function SM3String(const Str: string): TSM3Digest;
{* 对 String 类型数据进行 SM3 计算，注意 D2009 或以上版本的 string 为 UnicodeString，
   代码中会将其转换成 AnsiString 进行计算
 |<PRE>
   Str: string       - 要计算的字符串
 |</PRE>}

function SM3StringA(const Str: AnsiString): TSM3Digest;
{* 对 AnsiString 类型数据进行 SM3 计算
 |<PRE>
   Str: AnsiString       - 要计算的字符串
 |</PRE>}

function SM3StringW(const Str: WideString): TSM3Digest;
{* 对 WideString 类型数据进行 SM3 计算，内部转换为 AnsiString
 |<PRE>
   Str: WideString       - 要计算的字符串
 |</PRE>}

function SM3UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSM3Digest;
{* 对 UnicodeString 类型数据进行直接的 SM3 计算，不进行转换
 |<PRE>
   Str: UnicodeString/WideString       - 要计算的宽字符串
 |</PRE>}

function SM3File(const FileName: string; CallBack: TSM3CalcProgressFunc = nil): TSM3Digest;
{* 对指定文件内容进行 SM3 计算
 |<PRE>
   FileName: string  - 要计算的文件名
   CallBack: TSM3PgressFunc - 进度回调函数，默认为空
 |</PRE>}

function SM3Stream(Stream: TStream; CallBack: TSM3CalcProgressFunc = nil): TSM3Digest;
{* 对指定流数据进行 SM3 计算
 |<PRE>
   Stream: TStream  - 要计算的流内容
   CallBack: TSM3CalcProgressFunc - 进度回调函数，默认为空
 |</PRE>}

function SM3Print(const Digest: TSM3Digest): string;
{* 以十六进制格式输出 SM3 计算值
 |<PRE>
   Digest: TSM3Digest  - 指定的 SM3 计算值
 |</PRE>}

function SM3Match(const D1, D2: TSM3Digest): Boolean;
{* 比较两个 SM3 计算值是否相等
 |<PRE>
   D1: TSM3Digest   - 需要比较的 SM3 计算值
   D2: TSM3Digest   - 需要比较的 SM3 计算值
 |</PRE>}

implementation

const
  SM3Padding: array[0..63] of Byte =
    (
      $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    );

  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  HMAC_SM3_BLOCK_SIZE_BYTE = 64;
  HMAC_SM3_OUTPUT_LENGTH_BYTE = 32;

type
  TSM3ProcessData = array[0..63] of Byte;

procedure GetULongBe(var N: LongWord; B: PAnsiChar; I: Integer);
var
  D: LongWord;
begin
  D := (LongWord(B[I]) shl 24) or (LongWord(B[I + 1]) shl 16) or
    (LongWord(B[I + 2]) shl 8) or (LongWord(B[I + 3]));
  N := D;
end;

procedure PutULongBe(N: LongWord; B: PAnsiChar; I: Integer);
begin
  B[I] := AnsiChar(N shr 24);
  B[I + 1] := AnsiChar(N shr 16);
  B[I + 2] := AnsiChar(N shr 8);
  B[I + 3] := AnsiChar(N);
end;

function FF0(X, Y, Z: LongWord): LongWord;
begin
  Result := X xor Y xor Z;
end;

function FF1(X, Y, Z: LongWord): LongWord;
begin
  Result := (X and Y) or (Y and Z) or (X and Z);
end;

function GG0(X, Y, Z: LongWord): LongWord;
begin
  Result := X xor Y xor Z;
end;

function GG1(X, Y, Z: LongWord): LongWord;
begin
  Result := (X and Y) or ((not X) and Z);
end;

function SM3Shl(X: LongWord; N: Integer): LongWord;
begin
  Result := (X and $FFFFFFFF) shl N;
end;

function ROTL(X: LongWord; N: Integer): LongWord;
begin
  Result := SM3Shl(X, N) or (X shr (32 - N));
end;

function P0(X: LongWord): LongWord;
begin
  Result := X xor ROTL(X, 9) xor ROTL(X, 17);
end;

function P1(X: LongWord): LongWord;
begin
  Result := X xor ROTL(X, 15) xor ROTL(X, 23);
end;

procedure SM3Start(var Ctx: TSM3Context);
begin
  Ctx.Total[0] := 0;
  Ctx.Total[1] := 0;

  Ctx.State[0] := $7380166F;
  Ctx.State[1] := $4914B2B9;
  Ctx.State[2] := $172442D7;
  Ctx.State[3] := $DA8A0600;
  Ctx.State[4] := $A96F30BC;
  Ctx.State[5] := $163138AA;
  Ctx.State[6] := $E38DEE4D;
  Ctx.State[7] := $B0FB0E4E;

  FillChar(Ctx.Buffer, SizeOf(Ctx.Buffer), 0);
end;

// 一次处理 64byte 也就是512bit 数据块
procedure SM3Process(var Ctx: TSM3Context; Data: PAnsiChar);
var
  SS1, SS2, TT1, TT2: LongWord;
  W: array[0..67] of LongWord;
  W1: array[0..63] of LongWord;
  T: array[0..63] of LongWord;
  A, B, C, D, E, F, G, H: LongWord;
  Temp1, Temp2, Temp3, Temp4, Temp5: LongWord;
  J: Integer;
begin
  for J := 0 to 15 do
    T[J] := $79CC4519;
  for J := 16 to 63 do
    T[J] := $7A879D8A;

  GetULongBe(W[ 0], Data,  0);
  GetULongBe(W[ 1], Data,  4);
  GetULongBe(W[ 2], Data,  8);
  GetULongBe(W[ 3], Data, 12);
  GetULongBe(W[ 4], Data, 16);
  GetULongBe(W[ 5], Data, 20);
  GetULongBe(W[ 6], Data, 24);
  GetULongBe(W[ 7], Data, 28);
  GetULongBe(W[ 8], Data, 32);
  GetULongBe(W[ 9], Data, 36);
  GetULongBe(W[10], Data, 40);
  GetULongBe(W[11], Data, 44);
  GetULongBe(W[12], Data, 48);
  GetULongBe(W[13], Data, 52);
  GetULongBe(W[14], Data, 56);
  GetULongBe(W[15], Data, 60);

  for J := 16 to 67 do
  begin
    Temp1 := W[J - 16] xor W[J - 9];
    Temp2 := ROTL(W[J - 3], 15);
    Temp3 := Temp1 xor Temp2;
    Temp4 := P1(Temp3);
    Temp5 := ROTL(W[J - 13],7 ) xor W[J - 6];
    W[J] := Temp4 xor Temp5;
  end;

  for J := 0 to 63 do
    W1[J] := W[J] xor W[J + 4];

  // 已经处理好俩数组W/W1的值。

  A := Ctx.State[0];
  B := Ctx.State[1];
  C := Ctx.State[2];
  D := Ctx.State[3];
  E := Ctx.State[4];
  F := Ctx.State[5];
  G := Ctx.State[6];
  H := Ctx.State[7];

  for J := 0 to 15 do
  begin
    SS1 := ROTL((ROTL(A, 12) + E + ROTL(T[J], J)), 7);
    SS2 := SS1 xor ROTL(A, 12);
    TT1 := FF0(A, B, C) + D + SS2 + W1[J];
    TT2 := GG0(E, F, G) + H + SS1 + W[J];
    D := C;
    C := ROTL(B, 9);
    B := A;
    A := TT1;
    H := G;
    G := ROTL(F, 19);
    F := E;
    E := P0(TT2);
  end;

  for J := 16 to 63 do
  begin
    SS1 := ROTL((ROTL(A, 12) + E + ROTL(T[J], J)), 7);
    SS2 := SS1 xor ROTL(A, 12);
    TT1 := FF1(A, B, C) + D + SS2 + W1[J];
    TT2 := GG1(E, F, G) + H + SS1 + W[J];
    D := C;
    C := ROTL(B,9);
    B := A;
    A := TT1;
    H := G;
    G := ROTL(F,19);
    F := E;
    E := P0(TT2);
  end;

  Ctx.State[0] := Ctx.State[0] xor A;
  Ctx.State[1] := Ctx.State[1] xor B;
  Ctx.State[2] := Ctx.State[2] xor C;
  Ctx.State[3] := Ctx.State[3] xor D;
  Ctx.State[4] := Ctx.State[4] xor E;
  Ctx.State[5] := Ctx.State[5] xor F;
  Ctx.State[6] := Ctx.State[6] xor G;
  Ctx.State[7] := Ctx.State[7] xor H;

  // 本轮无误
end;

procedure SM3UpdateW(var Context: TSM3Context; Input: PWideChar; CharLength: LongWord);
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
    SM3Update(Context, pContent, iLen);
  finally
    FreeMem(pContent);
  end;
{$ELSE}  // MacOS 下直接把 UnicodeString 转成 AnsiString 计算，不支持非 Windows 非 Unicode 平台
  S := StrNew(Input);
  A := AnsiString(S);
  SM3Update(Context, @A[1], Length(A));
{$ENDIF}
end;

procedure SM3Update(var Ctx: TSM3Context; Input: PAnsiChar; CharLength: LongWord);
var
  Fill, Left: LongWord;
begin
  if (Input = nil) or (CharLength <= 0) then
    Exit;

  Left := Ctx.Total[0] and $3F;
  Fill := 64 - Left;

  Ctx.Total[0] := Ctx.Total[0] + CharLength;
  Ctx.Total[0] := Ctx.Total[0] and $FFFFFFFF;

  if Ctx.Total[0] < LongWord(CharLength) then
    Ctx.Total[1] := Ctx.Total[1] + 1;

  if (Left <> 0) and (CharLength >= Fill) then
  begin
    Move(Input^, Ctx.Buffer[Left], Fill);
    SM3Process(Ctx, @(Ctx.Buffer[0]));
    Input := Input + Fill;
    CharLength := CharLength - Fill;
    Left := 0;
  end;

  while CharLength >= 64 do
  begin
    SM3Process(Ctx, Input);
    Input := Input + 64;
    CharLength := CharLength - 64;
  end;

  if CharLength > 0 then
    Move(Input^, Ctx.Buffer[Left], CharLength);
end;

procedure SM3Finish(var Ctx: TSM3Context; var Output: TSM3Digest);
var
  Last, Padn: LongWord;
  High, Low: LongWord;
  MsgLen: array[0..7] of Byte;
begin
  High := (Ctx.Total[0] shr 29) or (Ctx.Total[1] shl 3);
  Low := Ctx.Total[0] shl 3;

  PutULongBe(High, @(MsgLen[0]), 0);
  PutULongBe(Low, @(MsgLen[0]), 4);

  Last := Ctx.Total[0] and $3F;
  if Last < 56 then
    Padn := 56 - Last
  else
    Padn := 120 - Last;

  SM3Update(Ctx, @(SM3Padding[0]), Padn);
  SM3Update(Ctx, @(MsgLen[0]), 8);

  PutULongBe(Ctx.State[0], @Output,  0);
  PutULongBe(Ctx.State[1], @Output,  4);
  PutULongBe(Ctx.State[2], @Output,  8);
  PutULongBe(Ctx.State[3], @Output, 12);
  PutULongBe(Ctx.State[4], @Output, 16);
  PutULongBe(Ctx.State[5], @Output, 20);
  PutULongBe(Ctx.State[6], @Output, 24);
  PutULongBe(Ctx.State[7], @Output, 28);
end;

function SM3(Input: PAnsiChar; Length: LongWord): TSM3Digest;
var
  Ctx: TSM3Context;
begin
  SM3Start(Ctx);
  SM3Update(Ctx, Input, Length);
  SM3Finish(Ctx, Result);
end;

procedure SM3HmacStarts(var Ctx: TSM3Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TSM3Digest;
begin
  if KeyLength > HMAC_SM3_BLOCK_SIZE_BYTE then
  begin
    Sum := SM3(Key, KeyLength);
    KeyLength := HMAC_SM3_OUTPUT_LENGTH_BYTE;
    Key := @(Sum[0]);
  end;

  FillChar(Ctx.Ipad, HMAC_SM3_BLOCK_SIZE_BYTE, $36);
  FillChar(Ctx.Opad, HMAC_SM3_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Ctx.Ipad[I] := Byte(Ctx.Ipad[I] xor Byte(Key[I]));
    Ctx.Opad[I] := Byte(Ctx.Opad[I] xor Byte(Key[I]));
  end;

  SM3Start(Ctx);
  SM3Update(Ctx, @(Ctx.Ipad[0]), HMAC_SM3_BLOCK_SIZE_BYTE);
end;

procedure SM3HmacUpdate(var Ctx: TSM3Context; Input: PAnsiChar; Length: LongWord);
begin
  SM3Update(Ctx, Input, Length);
end;

procedure SM3HmacFinish(var Ctx: TSM3Context; var Output: TSM3Digest);
var
  Len: Integer;
  TmpBuf: TSM3Digest;
begin
  Len := HMAC_SM3_OUTPUT_LENGTH_BYTE;
  SM3Finish(Ctx, TmpBuf);
  SM3Start(Ctx);
  SM3Update(Ctx, @(Ctx.Opad[0]), HMAC_SM3_BLOCK_SIZE_BYTE);
  SM3Update(Ctx, @(TmpBuf[0]), Len);
  SM3Finish(Ctx, Output);
end;

procedure SM3Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSM3Digest);
var
  Ctx: TSM3Context;
begin
  SM3HmacStarts(Ctx, Key, KeyLength);
  SM3HmacUpdate(Ctx, Input, Length);
  SM3HmacFinish(Ctx, Output);
end;

function SM3Buffer(const Buffer; Count: LongWord): TSM3Digest;
var
  Context: TSM3Context;
begin
  SM3Start(Context);
  SM3Update(Context, PAnsiChar(Buffer), Count);
  SM3Finish(Context, Result);
end;

function SM3Bytes(Data: TBytes): TSM3Digest;
var
  Context: TSM3Context;
begin
  SM3Start(Context);
  SM3Update(Context, PAnsiChar(@Data[0]), Length(Data));
  SM3Finish(Context, Result);
end;

// 对 String 类型数据进行 SM3 转换
function SM3String(const Str: string): TSM3Digest;
var
  AStr: AnsiString;
begin
  AStr := AnsiString(Str);
  Result := SM3StringA(AStr);
end;

// 对 AnsiString 类型数据进行 SM3 转换
function SM3StringA(const Str: AnsiString): TSM3Digest;
var
  Context: TSM3Context;
begin
  SM3Start(Context);
  SM3Update(Context, PAnsiChar(Str), Length(Str));
  SM3Finish(Context, Result);
end;

// 对 WideString 类型数据进行 SM3 转换
function SM3StringW(const Str: WideString): TSM3Digest;
var
  Context: TSM3Context;
begin
  SM3Start(Context);
  SM3UpdateW(Context, PWideChar(Str), Length(Str));
  SM3Finish(Context, Result);
end;

// 对 UnicodeString 类型数据进行直接的 SM3 计算，不进行转换
function SM3UnicodeString(const Str: {$IFDEF UNICODE} string {$ELSE} WideString {$ENDIF}): TSM3Digest;
var
  Context: TSM3Context;
begin
  SM3Start(Context);
  SM3Update(Context, PAnsiChar(@Str[1]), Length(Str) * SizeOf(WideChar));
  SM3Finish(Context, Result);
end;

function InternalSM3Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TSM3Digest; CallBack: TSM3CalcProgressFunc = nil): Boolean;
var
  Context: TSM3Context;
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
  SM3Start(Context);
  GetMem(Buf, BufLen);
  try
    Stream.Position := 0;
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        SM3Update(Context, Buf, ReadBytes);
        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    SM3Finish(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// 对指定文件数据进行SM3转换
function SM3File(const FileName: string;
  CallBack: TSM3CalcProgressFunc): TSM3Digest;
var
{$IFDEF MSWINDOWS}
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: TSM3Context;
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
    // 大于 2G 的文件可能 Map 失败，或非 Windows 平台，，采用流方式循环处理
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalSM3Stream(Stream, 4096 * 1024, Result, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
{$IFDEF MSWINDOWS}
    SM3Start(Context);
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
                SM3Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
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
    SM3Finish(Context, Result);
{$ENDIF}
  end;
end;

// 对指定流进行 SM3 计算
function SM3Stream(Stream: TStream;
  CallBack: TSM3CalcProgressFunc = nil): TSM3Digest;
begin
  InternalSM3Stream(Stream, 4096 * 1024, Result, CallBack);
end;

function SM3Print(const Digest: TSM3Digest): string;
var
  I: Integer;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  for I := 0 to 31 do
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(Digest[I] shr 4) and $0f] +
              Digits[Digest[I] and $0F]);
end;

function SM3Match(const D1, D2: TSM3Digest): Boolean;
var
  I: Integer;
begin
  I := 0;
  Result := True;
  while Result and (I < 32) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

end.
