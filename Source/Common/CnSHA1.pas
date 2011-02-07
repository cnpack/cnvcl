{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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
* 单元名称：SHA1算法单元
* 单元作者：刘啸（Liu Xiao）
* 备    注：
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnSHA1.pas 426 2010-02-09 07:01:49Z liuxiao $
* 修改记录：2010.07.14 V1.0
*               创建单元。从网上佚名代码移植而来，加入部分功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

uses
  SysUtils, Windows, Classes;

type
  TSHA1Digest = array[0..19] of Byte;

  TSHA1Context = record
    Hash: array[0..4] of DWORD;
    Hi, Lo: DWORD;
    Buffer: array[0..63] of Byte;
    Index: Integer;
  end;

  TSHA1CalcProgressFunc = procedure (ATotal, AProgress: Int64;
    var Cancel: Boolean) of object;
  {* 进度回调事件类型声明}

function SHA1Buffer(const Buffer; Count: LongWord): TSHA1Digest;
{* 对数据块进行SHA1转换
 |<PRE>
   const Buffer     - 要计算的数据块
   Count: LongWord  - 数据块长度
 |</PRE>}

function SHA1String(const Str: string): TSHA1Digest;
{* 对String类型数据进行SHA1转换，注意D2009或以上版本的string为UnicodeString，
   因此对同一个字符串的计算结果，和D2007或以下版本的会不同，使用时请注意
 |<PRE>
   Str: string       - 要计算的字符串
 |</PRE>}

function SHA1StringA(const Str: AnsiString): TSHA1Digest;
{* 对AnsiString类型数据进行SHA1转换
 |<PRE>
   Str: AnsiString       - 要计算的字符串
 |</PRE>}

function SHA1StringW(const Str: WideString): TSHA1Digest;
{* 对 WideString类型数据进行SHA1转换
 |<PRE>
   Str: WideString       - 要计算的字符串
 |</PRE>}

function SHA1File(const FileName: string;
  CallBack: TSHA1CalcProgressFunc = nil): TSHA1Digest;
{* 对指定文件数据进行SHA1转换
 |<PRE>
   FileName: string  - 要计算的文件名
   CallBack: TSHA1CalcProgressFunc - 进度回调函数，默认为空
 |</PRE>}

function SHA1Stream(Stream: TStream;
  CallBack: TSHA1CalcProgressFunc = nil): TSHA1Digest;
{* 对指定流数据进行SHA1转换
 |<PRE>
   Stream: TStream  - 要计算的流内容
   CallBack: TSHA1CalcProgressFunc - 进度回调函数，默认为空
 |</PRE>}

procedure SHA1Init(var Context: TSHA1Context);

procedure SHA1Update(var Context: TSHA1Context; Buffer: Pointer; Len: Integer);

procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);

function SHA1Print(const Digest: TSHA1Digest): string;
{* 以十六进制格式输出SHA1计算值
 |<PRE>
   Digest: TSHA1Digest  - 指定的SHA1计算值
 |</PRE>}

function SHA1Match(const D1, D2: TSHA1Digest): Boolean;
{* 比较两个SHA1计算值是否相等
 |<PRE>
   D1: TSHA1Digest   - 需要比较的SHA1计算值
   D2: TSHA1Digest   - 需要比较的SHA1计算值
 |</PRE>}

function SHA1DigestToStr(aDig: TSHA1Digest): string;
{* SHA1计算值转 string
 |<PRE>
   aDig: TSHA1Digest   - 需要转换的SHA1计算值
 |</PRE>}

implementation

{$R-}

function LRot16(X: Word; c: Integer): Word; assembler;
asm
        mov     ecx, &c
        mov     ax, &X
        rol     ax, cl
        mov     &Result, ax
end;

function RRot16(X: Word; c: Integer): Word; assembler;
asm
        mov     ecx, &c
        mov     ax, &X
        ror     ax, cl
        mov     &Result, ax
end;

function LRot32(X: DWORD; c: Integer): DWORD; register; assembler;
asm
        mov     ecx, edx
        rol     eax, cl
end;

function RRot32(X: DWORD; c: Integer): DWORD; register; assembler;
asm
        mov     ecx, edx
        ror     eax, cl
end;

procedure XorBlock(I1, I2, O1: PByteArray; Len: Integer);
var
  i: Integer;
begin
  for i := 0 to Len - 1 do
    O1[i] := I1[i] xor I2[i];
end;

procedure IncBlock(P: PByteArray; Len: Integer);
begin
  Inc(P[Len - 1]);
  if (P[Len - 1] = 0) and (Len > 1) then
    IncBlock(P, Len - 1);
end;

function F1(x, y, z: DWORD): DWORD;
begin
  Result := z xor (x and (y xor z));
end;

function F2(x, y, z: DWORD): DWORD;
begin
  Result := x xor y xor z;
end;

function F3(x, y, z: DWORD): DWORD;
begin
  Result := (x and y) or (z and (x or y));
end;   
   
function RB(A: DWORD): DWORD;
begin
  Result := (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

procedure SHA1Compress(var Data: TSHA1Context);
var
  A, B, C, D, E, T: DWORD;
  W: array[0..79] of DWORD;
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
  i, k: DWORD;
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

procedure SHA1UpdateW(var Context: TSHA1Context; Input: PWideChar; Length: LongWord);
var
  pContent: PAnsiChar;
  iLen: Cardinal;
begin
  GetMem(pContent, Length * SizeOf(WideChar));
  try
    iLen := WideCharToMultiByte(0, 0, Input, Length, // 代码页默认用 0
      PAnsiChar(pContent), Length * SizeOf(WideChar), nil, nil);
    SHA1Update(Context, pContent, iLen);
  finally
    FreeMem(pContent);
  end;
end;

procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);
type
  PDWord = ^DWORD;
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
  FillChar(Context, Sizeof(Context), 0);
end;

// 对数据块进行SHA1转换
function SHA1Buffer(const Buffer; Count: Longword): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(Buffer), Count);
  SHA1Final(Context, Result);
end;

// 对String类型数据进行SHA1转换
function SHA1String(const Str: string): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar({$IFDEF DELPHI2009_UP}AnsiString{$ENDIF}(Str)), Length(Str) * SizeOf(Char));
  SHA1Final(Context, Result);
end;

// 对AnsiString类型数据进行SHA1转换
function SHA1StringA(const Str: AnsiString): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PAnsiChar(Str), Length(Str));
  SHA1Final(Context, Result);
end;

// 对WideString类型数据进行SHA1转换
function SHA1StringW(const Str: WideString): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1UpdateW(Context, PWideChar(Str), Length(Str));
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

// 对指定流进行SHA1计算
function SHA1Stream(Stream: TStream;
  CallBack: TSHA1CalcProgressFunc = nil): TSHA1Digest;
begin
  InternalSHA1Stream(Stream, 4096 * 1024, Result, CallBack);
end;

// 对指定文件数据进行SHA1转换
function SHA1File(const FileName: string;
  CallBack: TSHA1CalcProgressFunc): TSHA1Digest;
var
  FileHandle: THandle;
  MapHandle: THandle;
  ViewPointer: Pointer;
  Context: TSHA1Context;
  Stream: TStream;

  function FileSizeIsLargeThan2G(const AFileName: string): Boolean;
  var
    H: THandle;
    Info: BY_HANDLE_FILE_INFORMATION;
    Rec : Int64Rec;
  begin
    Result := False;
    H := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if H = INVALID_HANDLE_VALUE then Exit;
    if not GetFileInformationByHandle(H, Info) then Exit;
    CloseHandle(H);
    Rec.Lo := Info.nFileSizeLow;
    Rec.Hi := Info.nFileSizeHigh;
    Result := (Rec.Hi > 0) or (Rec.Lo > Cardinal(MaxInt));
  end;

begin
  if FileSizeIsLargeThan2G(FileName) then
  begin
    // 大于 2G 的文件可能 Map 失败，采用流方式循环处理
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      InternalSHA1Stream(Stream, 4096 * 1024, Result, CallBack);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    SHA1Init(Context);
    FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
                  FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
                  FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if FileHandle <> INVALID_HANDLE_VALUE then
      try
        MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
        if MapHandle <> 0 then
          try
            ViewPointer := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
            if ViewPointer <> nil then
              try
                SHA1Update(Context, ViewPointer, GetFileSize(FileHandle, nil));
              finally
                UnmapViewOfFile(ViewPointer);
              end;
          finally
            CloseHandle(MapHandle);
          end;
      finally
        CloseHandle(FileHandle);
      end;
    SHA1Final(Context, Result);
  end;
end;

// 以十六进制格式输出SHA1计算值
function SHA1Print(const Digest: TSHA1Digest): string;
var
  I: Byte;
const
  Digits: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  for I := 0 to 19 do
    Result := Result + {$IFDEF DELPHI12_UP}string{$ENDIF}(Digits[(Digest[I] shr 4) and $0f] +
      Digits[Digest[I] and $0f]);
end;

// 比较两个SHA1计算值是否相等
function SHA1Match(const D1, D2: TSHA1Digest): Boolean;
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

// SHA1计算值转 string
function SHA1DigestToStr(aDig: TSHA1Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 20);
  for I := 1 to 20 do
    Result[I] := Chr(aDig[I - 1]);
end;

end.
