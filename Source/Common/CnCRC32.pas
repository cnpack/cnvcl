{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2012 CnPack 开发组                       }
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

unit CnCRC32;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：CRC32循环冗余校验单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2009.08.21 V1.3
*               增加CRC64的支持
*           2009.07.31 V1.2
*               修正计算大文件CRC32不正确的问题，增加对大于4G文件的支持
*           2009.04.16 V1.1
*               修正一处计算有误的问题
*           2002.08.11 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils;

function CRC32Calc(const OrgCRC32: DWORD; const Data; Len: DWORD): DWORD;
{* 计算CRC32值
 |<PRE>
   OrgCRC32: DWORD  - 起始CRC32值，默认可传 0
   const Data       - 要计算的数据块
   Len: DWORD       - 数据块长度
   Result: DWORD    - 返回CRC32计算结果
 |</PRE>}

function StrCRC32(const OrgCRC32: DWORD; const Text: string): DWORD;
{* 计算字符串的CRC32值 }

function StrCRC32A(const OrgCRC32: DWORD; const Text: AnsiString): DWORD;
{* 计算 AnsiString 字符串的CRC32值 }

function FileCRC32(const FileName: string; var CRC: DWORD; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
{* 计算文件CRC32值，支持超过4G的大文件
 |<PRE>
   const FileName: string   - 目标文件名
   var CRC: DWORD           - CRC32值，变量参数，传入原始值，默认可为 0，输出计算值
   StartPos: Int64 = 0      - 文件起始位置，默认从头开始
   Len: Int64 = 0           - 计算长度，为零默认为整个文件
   Result: Boolean          - 返回成功标志，文件打开失败或指定长度无效时返回 False
 |</PRE>}

function CRC64Calc(const OrgCRC64: Int64; const Data; Len: DWORD): Int64;
{* 计算CRC64值
 |<PRE>
   OrgCRC64: Int64  - 起始CRC64值，默认可传 0
   const Data       - 要计算的数据块
   Len: DWORD       - 数据块长度
   Result: Int64    - 返回CRC64计算结果
 |</PRE>}

function StrCRC64(const OrgCRC64: Int64; const Text: string): Int64;
{* 计算字符串的CRC64值 }

function StrCRC64A(const OrgCRC64: Int64; const Text: AnsiString): Int64;
{* 计算 AnsiString 字符串的CRC64值 }

function FileCRC64(const FileName: string; var CRC: Int64; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
{* 计算文件CRC64值，支持超过4G的大文件
 |<PRE>
   const FileName: string   - 目标文件名
   var CRC: Int64           - CRC64值，变量参数，传入原始值，默认可为 0，输出计算值
   StartPos: Int64 = 0      - 文件起始位置，默认从头开始
   Len: Int64 = 0           - 计算长度，为零默认为整个文件
   Result: Boolean          - 返回成功标志，文件打开失败或指定长度无效时返回 False
 |</PRE>}

implementation

const
  csBuff_Size = 4096;
  csCRC64 = $C96C5795D7870F42;
  
type
  // 文件缓冲区
  PBuff = ^TBuff;
  TBuff = array[0..csBuff_Size - 1] of Byte;

  // CRC32表
  TCRC32Table = array[0..255] of DWORD;
  
  // CRC64表
  TCRC64Table = array[0..255] of Int64;
  
var
  CRC32Table: TCRC32Table;
  
  CRC64Table: TCRC64Table;

// 生成CRC32表
procedure Make_CRC32Table;
asm
        PUSH    EBX
        MOV     EDX, OFFSET CRC32Table

        XOR     EBX, EBX
@MakeCRC32Loop:
        CMP     EBX, $100
        JE      @MakeCRC32_Succ
        MOV     EAX, EBX
        MOV     ECX, 8
@MakeLoop:
        TEST    EAX, 1
        JZ      @MakeIsZero
        SHR     EAX, 1
        XOR     EAX, $EDB88320
        JMP     @MakeNext
@MakeIsZero:
        SHR     EAX, 1
@MakeNext:
        LOOP    @MakeLoop
        MOV     DWORD PTR [EDX], EAX
        ADD     EDX, 4
        INC     EBX
        JMP     @MakeCRC32Loop

@MakeCRC32_Succ:
        POP     EBX
        RET
end;

// 计算CRC32值
function DoCRC32Calc(const OrgCRC32: DWORD; const Data; Len: DWORD): DWORD;
asm
        OR      EDX, EDX   // Data = nil?
        JE      @Exit
        JECXZ   @Exit      // Len = 0?
        PUSH    ESI
        PUSH    EBX
        MOV     ESI, OFFSET CRC32Table
@Upd:
        MOVZX   EBX, AL    // CRC32
        XOR     BL, [EDX]
        SHR     EAX, 8
        AND     EAX, $00FFFFFF
        XOR     EAX, [EBX * 4 + ESI]
        INC     EDX
        LOOP    @Upd
        POP     EBX
        POP     ESI
@Exit:
        RET
end;

// 计算 CRC32 值
function CRC32Calc(const OrgCRC32: DWORD; const Data; Len: DWORD): DWORD;
begin
  Result := not OrgCRC32;
  Result := DoCRC32Calc(Result, Data, Len);
  Result := not Result;
end;

// 计算字符串的CRC32值
function StrCRC32(const OrgCRC32: DWORD; const Text: string): DWORD;
begin
  Result := CRC32Calc(OrgCRC32, PChar(Text)^, Length(Text) * SizeOf(Char));
end;

// 计算 AnsiString 字符串的CRC32值
function StrCRC32A(const OrgCRC32: DWORD; const Text: AnsiString): DWORD;
begin
  Result := CRC32Calc(OrgCRC32, PAnsiChar(Text)^, Length(Text));
end;

// 计算文件CRC值，参数分别为：文件名、CRC值、起始地址、计算长度
function FileCRC32(const FileName: string; var CRC: DWORD; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
var
  Handle: THandle;
  ReadCount: Integer;
  Size: Int64;
  Count: Int64;
  Buff: TBuff;
begin
  // 以共享读方式打开文件
  Handle := CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if Result then
  try
    Int64Rec(Size).Lo := GetFileSize(Handle, @Int64Rec(Size).Hi);
    if Size < StartPos + Len then
    begin
      Result := False;                  // 超过文件长度
      Exit;
    end;
    if Len > 0 then
      Count := Len
    else
      Count := Size - StartPos;         // 长度为零，计算到文件尾

    CRC := not CRC;
    SetFilePointer(Handle, Int64Rec(StartPos).Lo, @Int64Rec(StartPos).Hi, FILE_BEGIN);
    while Count > 0 do
    begin
      if Count > SizeOf(Buff) then
        ReadCount := SizeOf(Buff)
      else
        ReadCount := Count;
      ReadFile(Handle, Buff, ReadCount, LongWord(ReadCount), nil);
      CRC := DoCrc32Calc(CRC, Buff, ReadCount);
      Dec(Count, ReadCount);
    end;
    CRC := not CRC;
  finally
    CloseHandle(Handle);
  end;
end;

procedure Make_CRC64Table;
var
  I, J: Integer;
  Data: Int64;
begin
  for I := 0 to 255 do
  begin
    Data := I;
    for J := 0 to 7 do
    begin
      if (Data and 1) <> 0 then
        Data := Data shr 1 xor csCRC64
      else
        Data := Data shr 1;
      
      CRC64Table[I] := Data;   
    end;
  end;
end;

function DoCRC64Calc(const OrgCRC64: Int64; const Data; Len: DWORD): Int64;
var
  I: Integer;
  DataAddr: PByte;
begin
  DataAddr := @Data;
  Result := OrgCRC64;
  
  for I := 0 to Len - 1 do
  begin
    Result := Result shr 8 xor 
      CRC64Table[Cardinal(Result) and $FF xor DataAddr^]; 
    Inc(DataAddr);   
  end;
end;

// 计算 CRC64 值
function CRC64Calc(const OrgCRC64: Int64; const Data; Len: DWORD): Int64;
begin
  Result := not OrgCRC64;
  Result := DoCRC64Calc(Result, Data, Len);
  Result := not Result;
end;

// 计算字符串的CRC32值
function StrCRC64(const OrgCRC64: Int64; const Text: string): Int64;
begin
  Result := CRC64Calc(OrgCRC64, PChar(Text)^, Length(Text) * SizeOf(Char));
end;

// 计算 AnsiString 字符串的CRC32值
function StrCRC64A(const OrgCRC64: Int64; const Text: AnsiString): Int64;
begin
  Result := CRC64Calc(OrgCRC64, PAnsiChar(Text)^, Length(Text));
end;

// 计算文件CRC值，参数分别为：文件名、CRC值、起始地址、计算长度
function FileCRC64(const FileName: string; var CRC: Int64; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
var
  Handle: THandle;
  ReadCount: Integer;
  Size: Int64;
  Count: Int64;
  Buff: TBuff;
begin
  // 以共享读方式打开文件
  Handle := CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if Result then
  try
    Int64Rec(Size).Lo := GetFileSize(Handle, @Int64Rec(Size).Hi);
    if Size < StartPos + Len then
    begin
      Result := False;                  // 超过文件长度
      Exit;
    end;
    if Len > 0 then
      Count := Len
    else
      Count := Size - StartPos;         // 长度为零，计算到文件尾

    CRC := not CRC;
    SetFilePointer(Handle, Int64Rec(StartPos).Lo, @Int64Rec(StartPos).Hi, FILE_BEGIN);
    while Count > 0 do
    begin
      if Count > SizeOf(Buff) then
        ReadCount := SizeOf(Buff)
      else
        ReadCount := Count;
      ReadFile(Handle, Buff, ReadCount, LongWord(ReadCount), nil);
      CRC := DoCrc64Calc(CRC, Buff, ReadCount);
      Dec(Count, ReadCount);
    end;
    CRC := not CRC;
  finally
    CloseHandle(Handle);
  end;
end;

initialization
  Make_CRC32Table; // 初始化CRC32表
  
  Make_CRC64Table; // 初始化CRC64表

end.

