{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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
* 单元标识：$Id: CnCRC32.pas,v 1.8 2009/05/03 08:51:54 liuxiao Exp $
* 修改记录：2009.04.16 V1.1
*               修正一处计算有误的问题
*           2002.08.11 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows;

type
  TCRC32 = type DWORD;

function CRC32Calc(OrgCRC32: DWORD; const Data; Len: DWORD): DWORD;
{* 计算CRC32值
 |<PRE>
   OrgCRC32: DWORD  - 起始CRC32值
   const Data       - 要计算的数据块
   Len: DWORD       - 数据块长度
   Result: DWORD    - 返回CRC32计算结果
 |</PRE>}

function StrCRC32(OrgCRC32: DWORD; const Text: string): DWORD;
{* 计算字符串的CRC32值 }

function StrCRC32A(OrgCRC32: DWORD; const Text: AnsiString): DWORD;
{* 计算 AnsiString 字符串的CRC32值 }

function FileCRC32(const FileName: string; var CRC: TCRC32; StartPos: Integer = 0;
  Len: Integer = 0): Boolean;
{* 计算文件CRC32值
 |<PRE>
   const FileName: string   - 目标文件名
   var CRC: TCRC32          - CRC32值，变量参数，传入原始值，输出计算值
   StartPos: Integer = 0    - 文件起始位置，默认从头开始
   Len: Integer = 0         - 计算长度，为零默认为整个文件
   Result: Boolean          - 返回成功标志，文件打开失败或指定长度无效时返回False
 |</PRE>}

implementation

const
  csBuff_Size = 4096;
  
type
  // 文件缓冲区
  PBuff = ^TBuff;
  TBuff = array[0..csBuff_Size - 1] of Byte;

  // CRC32表
  TCRC32Table = array[0..255] of DWORD;

var
  CRC32Table: TCRC32Table;

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
function CRC32Calc(OrgCRC32: DWORD; const Data; Len: DWORD): DWORD;
asm
        OR      EDX, EDX   // Data = nil?
        JE      @Exit
        JECXZ   @Exit      // Len = 0?
        PUSH    ESI
        PUSH    EBX
        MOV     ESI, OFFSET CRC32Table
        XOR     EAX, -1
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
        XOR     EAX, -1
@Exit:
        RET
end;

// 计算字符串的CRC32值
function StrCRC32(OrgCRC32: DWORD; const Text: string): DWORD;
begin
  Result := CRC32Calc(OrgCRC32, PChar(Text)^, Length(Text) * SizeOf(Char));
end;

// 计算 AnsiString 字符串的CRC32值
function StrCRC32A(OrgCRC32: DWORD; const Text: AnsiString): DWORD;
begin
  Result := CRC32Calc(OrgCRC32, PAnsiChar(Text)^, Length(Text));
end;

// 计算文件CRC值，参数分别为：文件名、CRC值、起始地址、计算长度
function FileCRC32(const FileName: string; var CRC: TCRC32; StartPos: Integer = 0;
  Len: Integer = 0): Boolean;
var
  Handle: THandle;
  ReadCount: Integer;
  Size: Integer;
  Count: Integer;
  Buff: TBuff;
begin
  // 以共享读方式打开文件
  Handle := CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if Result then
  begin
    Size := GetFileSize(Handle, nil);
    if Size < StartPos + Len then
    begin
      Result := False;                  // 超过文件长度
      Exit;
    end;
    if Len > 0 then
      Count := Len
    else
      Count := Size - StartPos;         // 长度为零，计算到文件尾
    SetFilePointer(Handle, StartPos, nil, FILE_BEGIN);
    while Count > 0 do
    begin
      if Count > SizeOf(Buff) then
        ReadCount := SizeOf(Buff)
      else
        ReadCount := Count;
      ReadFile(Handle, Buff, ReadCount, LongWord(ReadCount), nil);
      CRC := Crc32Calc(CRC, Buff, ReadCount);
      Dec(Count, ReadCount);
    end;
    CloseHandle(Handle);
  end;
end;

initialization
  Make_CRC32Table; // 初始化CRC32表

end.

