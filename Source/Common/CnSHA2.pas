{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
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

unit CnSHA2;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：SHA2(SHA256)算法单元
* 单元作者：刘啸（Liu Xiao）
* 备    注：
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWinXP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnSHA2.pas 426 2016-09-27 07:01:49Z liuxiao $
* 修改记录：2016.09.27 V1.0
*               创建单元。从网上佚名 C 代码与 Pascal 代码混合移植而来
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

uses
  SysUtils, Windows, Classes;

type
  TSHA256Digest = array[0..31] of Byte;

  TSHA256Context = record
    DataLen: DWORD;
    Data: array[0..63] of Byte;
    BitLen: Int64;
    State: array[0..7] of DWORD;
    Ipad: array[0..63] of Byte;      {!< HMAC: inner padding        }
    Opad: array[0..63] of Byte;      {!< HMAC: outer padding        }
  end;

  TSHA256CalcProgressFunc = procedure(ATotal, AProgress: Int64; var Cancel:
    Boolean) of object;
  {* 进度回调事件类型声明}

function SHA256Buffer(const Buffer; Count: LongWord): TSHA256Digest;
{* 对数据块进行SHA256转换
 |<PRE>
   const Buffer     - 要计算的数据块
   Count: LongWord  - 数据块长度
 |</PRE>}

function SHA256String(const Str: string): TSHA256Digest;
{* 对String类型数据进行SHA256转换，注意D2009或以上版本的string为UnicodeString，
   因此对同一个字符串的计算结果，和D2007或以下版本的会不同，使用时请注意
 |<PRE>
   Str: string       - 要计算的字符串
 |</PRE>}

function SHA256StringA(const Str: AnsiString): TSHA256Digest;
{* 对AnsiString类型数据进行SHA256转换
 |<PRE>
   Str: AnsiString       - 要计算的字符串
 |</PRE>}

function SHA256StringW(const Str: WideString): TSHA256Digest;
{* 对 WideString类型数据进行SHA256转换
 |<PRE>
   Str: WideString       - 要计算的字符串
 |</PRE>}

function SHA256File(const FileName: string; CallBack: TSHA256CalcProgressFunc =
  nil): TSHA256Digest;
{* 对指定文件数据进行SHA256转换
 |<PRE>
   FileName: string  - 要计算的文件名
   CallBack: TSHA256CalcProgressFunc - 进度回调函数，默认为空
 |</PRE>}

function SHA256Stream(Stream: TStream; CallBack: TSHA256CalcProgressFunc = nil):
  TSHA256Digest;
{* 对指定流数据进行SHA256转换
 |<PRE>
   Stream: TStream  - 要计算的流内容
   CallBack: TSHA256CalcProgressFunc - 进度回调函数，默认为空
 |</PRE>}

procedure SHA256Init(var Context: TSHA256Context);

procedure SHA256Update(var Context: TSHA256Context; Buffer: PAnsiChar; Len: Cardinal);

procedure SHA256Final(var Context: TSHA256Context; var Digest: TSHA256Digest);

function SHA256Print(const Digest: TSHA256Digest): string;
{* 以十六进制格式输出SHA256计算值
 |<PRE>
   Digest: TSHA256Digest  - 指定的SHA256计算值
 |</PRE>}

function SHA256Match(const D1, D2: TSHA256Digest): Boolean;
{* 比较两个SHA256计算值是否相等
 |<PRE>
   D1: TSHA256Digest   - 需要比较的SHA256计算值
   D2: TSHA256Digest   - 需要比较的SHA256计算值
 |</PRE>}

function SHA256DigestToStr(aDig: TSHA256Digest): string;
{* SHA256计算值转 string
 |<PRE>
   aDig: TSHA256Digest   - 需要转换的SHA256计算值
 |</PRE>}

procedure SHA256HmacInit(var Context: TSHA256Context; Key: PAnsiChar; KeyLength: Integer);

procedure SHA256HmacUpdate(var Context: TSHA256Context; Input: PAnsiChar; Length:
  LongWord);

procedure SHA256HmacFinal(var Context: TSHA256Context; var Output: TSHA256Digest);

procedure SHA256Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA256Digest);
{* Hash-based Message Authentication Code (based on SHA256) }

implementation

const
  MAX_FILE_SIZE = 512 * 1024 * 1024;
  // If file size <= this size (bytes), using Mapping, else stream

  KEYS: array[0..63] of DWORD = ($428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5,
    $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5, $D807AA98, $12835B01, $243185BE,
    $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174, $E49B69C1, $EFBE4786,
    $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA, $983E5152,
    $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E,
    $92722C85, $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624,
    $F40E3585, $106AA070, $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3,
    $4ED8AA4A, $5B9CCA4F, $682E6FF3, $748F82EE, $78A5636F, $84C87814, $8CC70208,
    $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2);
{$R-}

function ROTLeft256(A, B: DWORD): DWORD;
begin
  Result := (A shl B) or (A shr (32 - B));
end;

function ROTRight256(A, B: DWORD): DWORD;
begin
  Result := (A shr B) or (A shl (32 - B));
end;

function CH256(X, Y, Z: DWORD): DWORD;
begin
  Result := (X and Y) xor ((not X) and Z);
end;

function MAJ256(X, Y, Z: DWORD): DWORD;
begin
  Result := (X and Y) xor (X and Z) xor (Y and Z);
end;

function EP0256(X: DWORD): DWORD;
begin
  Result := ROTRight256(X, 2) xor ROTRight256(X, 13) xor ROTRight256(X, 22);
end;

function EP1256(X: DWORD): DWORD;
begin
  Result := ROTRight256(X, 6) xor ROTRight256(X, 11) xor ROTRight256(X, 25);
end;

function SIG0256(X: DWORD): DWORD;
begin
  Result := ROTRight256(X, 7) xor ROTRight256(X, 18) xor (X shr 3);
end;

function SIG1256(X: DWORD): DWORD;
begin
  Result := ROTRight256(X, 17) xor ROTRight256(X, 19) xor (X shr 10);
end;

procedure SHA256Transform(var Context: TSHA256Context; Data: PAnsiChar);
var
  A, B, C, D, E, F, G, H, T1, T2: DWORD;
  M: array[0..63] of DWORD;
  I, J: Integer;
begin
  I := 0;
  J := 0;
  while I < 16 do
  begin
    M[I] := (DWORD(Data[J]) shl 24) or (DWORD(Data[J + 1]) shl 16) or (DWORD(Data
      [J + 2]) shl 8) or DWORD(Data[J + 3]);
    Inc(I);
    Inc(J, 4);
  end;

  while I < 64 do
  begin
    M[I] := SIG1256(M[I - 2]) + M[I - 7] + SIG0256(M[I - 15]) + M[I - 16];
    Inc(I);
  end;

  A := Context.State[0];
  B := Context.State[1];
  C := Context.State[2];
  D := Context.State[3];
  E := Context.State[4];
  F := Context.State[5];
  G := Context.State[6];
  H := Context.State[7];

  I := 0;
  while I < 64 do
  begin
    T1 := H + EP1256(E) + CH256(E, F, G) + KEYS[I] + M[I];
    T2 := EP0256(A) + MAJ256(A, B, C);
    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
    Inc(I);
  end;

  Context.State[0] := Context.State[0] + A;
  Context.State[1] := Context.State[1] + B;
  Context.State[2] := Context.State[2] + C;
  Context.State[3] := Context.State[3] + D;
  Context.State[4] := Context.State[4] + E;
  Context.State[5] := Context.State[5] + F;
  Context.State[6] := Context.State[6] + G;
  Context.State[7] := Context.State[7] + H;
end;

procedure SHA256Init(var Context: TSHA256Context);
begin
  Context.DataLen := 0;
  Context.BitLen := 0;
  Context.State[0] := $6A09E667;
  Context.State[1] := $BB67AE85;
  Context.State[2] := $3C6EF372;
  Context.State[3] := $A54FF53A;
  Context.State[4] := $510E527F;
  Context.State[5] := $9B05688C;
  Context.State[6] := $1F83D9AB;
  Context.State[7] := $5BE0CD19;
  FillChar(Context.Data, SizeOf(Context.Data), 0);
end;

procedure SHA256Update(var Context: TSHA256Context; Buffer: PAnsiChar; Len: Cardinal);
var
  I: Integer;
begin
  for I := 0 to Len - 1 do
  begin
    Context.Data[Context.DataLen] := Byte(Buffer[I]);
    Inc(Context.DataLen);
    if Context.DataLen = 64 then
    begin
      SHA256Transform(Context, @Context.Data[0]);
      Context.BitLen := Context.BitLen + 512;
      Context.DataLen := 0;
    end;
  end;
end;

procedure SHA256UpdateW(var Context: TSHA256Context; Buffer: PWideChar; Len: LongWord);
var
  Content: PAnsiChar;
  iLen: Cardinal;
begin
  GetMem(Content, Len * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Buffer, Len, // 代码页默认用 0
      PAnsiChar(Content), Len * SizeOf(WideChar), nil, nil);
    SHA256Update(Context, Content, iLen);
  finally
    FreeMem(Content);
  end;
end;

procedure SHA256Final(var Context: TSHA256Context; var Digest: TSHA256Digest);
var
  I: Integer;
begin
  I := Context.DataLen;
  if Context.Datalen < 56 then
  begin
    Context.Data[I] := $80;
    Inc(I);
    while I < 56 do
    begin
      Context.Data[I] := 0;
      Inc(I);
    end;
  end
  else
  begin
    Context.Data[I] := $80;
    Inc(I);
    while I < 64 do
    begin
      Context.Data[I] := 0;
      Inc(I);
    end;

    SHA256Transform(Context, @(Context.Data[0]));
    FillChar(Context.Data, 56, 0);
  end;

  Context.BitLen := Context.BitLen + Context.DataLen * 8;
  Context.Data[63] := Context.Bitlen;
  Context.Data[62] := Context.Bitlen shr 8;
  Context.Data[61] := Context.Bitlen shr 16;
  Context.Data[60] := Context.Bitlen shr 24;
  Context.Data[59] := Context.Bitlen shr 32;
  Context.Data[58] := Context.Bitlen shr 40;
  Context.Data[57] := Context.Bitlen shr 48;
  Context.Data[56] := Context.Bitlen shr 56;
  SHA256Transform(Context, @(Context.Data[0]));

  for I := 0 to 3 do
  begin
    Digest[I] := (Context.State[0] shr (24 - I * 8)) and $000000FF;
    Digest[I + 4] := (Context.State[1] shr (24 - I * 8)) and $000000FF;
    Digest[I + 8] := (Context.State[2] shr (24 - I * 8)) and $000000FF;
    Digest[I + 12] := (Context.State[3] shr (24 - I * 8)) and $000000FF;
    Digest[I + 16] := (Context.State[4] shr (24 - I * 8)) and $000000FF;
    Digest[I + 20] := (Context.State[5] shr (24 - I * 8)) and $000000FF;
    Digest[I + 24] := (Context.State[6] shr (24 - I * 8)) and $000000FF;
    Digest[I + 28] := (Context.State[7] shr (24 - I * 8)) and $000000FF;
  end;
end;

// 对数据块进行SHA256转换
function SHA256Buffer(const Buffer; Count: Longword): TSHA256Digest;
var
  Context: TSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, PAnsiChar(Buffer), Count);
  SHA256Final(Context, Result);
end;

// 对String类型数据进行SHA256转换
function SHA256String(const Str: string): TSHA256Digest;
var
  Context: TSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, PAnsiChar({$IFDEF UNICODE}AnsiString{$ENDIF}(Str)),
    Length(Str) * SizeOf(Char));
  SHA256Final(Context, Result);
end;

// 对AnsiString类型数据进行SHA256转换
function SHA256StringA(const Str: AnsiString): TSHA256Digest;
var
  Context: TSHA256Context;
begin
  SHA256Init(Context);
  SHA256Update(Context, PAnsiChar(Str), Length(Str));
  SHA256Final(Context, Result);
end;

// 对WideString类型数据进行SHA256转换
function SHA256StringW(const Str: WideString): TSHA256Digest;
var
  Context: TSHA256Context;
begin
  SHA256Init(Context);
  SHA256UpdateW(Context, PWideChar(Str), Length(Str));
  SHA256Final(Context, Result);
end;

function InternalSHA256Stream(Stream: TStream; const BufSize: Cardinal; var D:
  TSHA256Digest; CallBack: TSHA256CalcProgressFunc = nil): Boolean;
var
  Context: TSHA256Context;
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
  if Size = 0 then
    Exit;
  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  SHA256Init(Context);
  GetMem(Buf, BufLen);
  try
    Stream.Seek(0, soFromBeginning);
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        SHA256Update(Context, Buf, ReadBytes);
        if Assigned(CallBack) then
        begin
          CallBack(Size, TotalBytes, CancelCalc);
          if CancelCalc then
            Exit;
        end;
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    SHA256Final(Context, D);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// 对指定流进行SHA256计算
function SHA256Stream(Stream: TStream; CallBack: TSHA256CalcProgressFunc = nil):
  TSHA256Digest;
begin
  InternalSHA256Stream(Stream, 4096 * 1024, Result, CallBack);
end;

// 对指定文件数据进行SHA256转换
function SHA256File(const FileName: string; CallBack: TSHA256CalcProgressFunc):
  TSHA256Digest;
var
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: TSHA256Context;
  Stream: TStream;
  FileIsZeroSize: Boolean;

  function FileSizeIsLargeThanMax(const AFileName: string; out IsEmpty: Boolean): Boolean;
  var
    H: THandle;
    Info: BY_HANDLE_FILE_INFORMATION;
    Rec: Int64Rec;
  begin
    Result := False;
    IsEmpty := False;
    H := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
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
  end;

begin
  FileIsZeroSize := False;
  if FileSizeIsLargeThanMax(FileName, FileIsZeroSize) then
  begin
    // 大于 2G 的文件可能 Map 失败，采用流方式循环处理
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalSHA256Stream(Stream, 4096 * 1024, Result, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    SHA256Init(Context);
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
                SHA256Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
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
    SHA256Final(Context, Result);
  end;
end;

// 以十六进制格式输出SHA256计算值
function SHA256Print(const Digest: TSHA256Digest): string;
var
  I: Byte;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  for I := 0 to 31 do
    Result := Result + {$IFDEF UNICODE}string{$ENDIF}(Digits[(Digest[I] shr 4)
      and $0F] + Digits[Digest[I] and $0F]);
end;

// 比较两个SHA256计算值是否相等
function SHA256Match(const D1, D2: TSHA256Digest): Boolean;
var
  I: Byte;
begin
  I := 0;
  Result := TRUE;
  while Result and (I < 20) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

// SHA256计算值转 string
function SHA256DigestToStr(aDig: TSHA256Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 20);
  for I := 1 to 20 do
    Result[I] := Chr(aDig[I - 1]);
end;

procedure SHA256HmacInit(var Context: TSHA256Context; Key: PAnsiChar; KeyLength: Integer);
var
  I: Integer;
  Sum: TSHA256Digest;
begin
  if KeyLength > 64 then
  begin
    Sum := SHA256Buffer(Key, KeyLength);
    KeyLength := 32;
    Key := @(Sum[0]);
  end;

  FillChar(Context.Ipad, $36, 64);
  FillChar(Context.Opad, $5C, 64);

  for I := 0 to KeyLength - 1 do
  begin
    Context.Ipad[I] := Byte(Context.Ipad[I] xor Byte(Key[I]));
    Context.Opad[I] := Byte(Context.Opad[I] xor Byte(Key[I]));
  end;

  SHA256Init(Context);
  SHA256Update(Context, @(Context.Ipad[0]), 64);
end;

procedure SHA256HmacUpdate(var Context: TSHA256Context; Input: PAnsiChar; Length:
  LongWord);
begin
  SHA256Update(Context, Input, Length);
end;

procedure SHA256HmacFinal(var Context: TSHA256Context; var Output: TSHA256Digest);
var
  Len: Integer;
  TmpBuf: TSHA256Digest;
begin
  Len := 32;
  SHA256Final(Context, TmpBuf);
  SHA256Init(Context);
  SHA256Update(Context, @(Context.Opad[0]), 64);
  SHA256Update(Context, @(TmpBuf[0]), Len);
  SHA256Final(Context, Output);
end;

procedure SHA256Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord; var Output: TSHA256Digest);
var
  Context: TSHA256Context;
begin
  SHA256HmacInit(Context, Key, KeyLength);
  SHA256HmacUpdate(Context, Input, Length);
  SHA256HmacFinal(Context, Output);
end;

end.

