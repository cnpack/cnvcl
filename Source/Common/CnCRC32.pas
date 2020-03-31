{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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
* 单元名称：CRC32/CRC64 循环冗余校验单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2019.12.12 V1.6
*               支持 TBytes
*           2019.04.15 V1.5
*               支持 Win32/Win64/MacOS
*           2015.06.12 V1.4
*               把汇编改写为 Pascal 以适应 64 位编译器
*           2009.08.21 V1.3
*               增加 CRC64 的支持
*           2009.07.31 V1.2
*               修正计算大文件 CRC32 不正确的问题，增加对大于 4G 文件的支持
*           2009.04.16 V1.1
*               修正一处计算有误的问题
*           2002.08.11 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF MSWINDOWS}, Windows {$ENDIF};

function CalcCRC32Byte(OrgCRC32: LongWord; B: Byte): LongWord;
{* CRC32 计算单个字节，供特殊需求使用}

function CRC32Calc(const OrgCRC32: LongWord; const Data; Len: LongWord): LongWord;
{* 计算 CRC32 值
 |<PRE>
   OrgCRC32: DWORD  - 起始 CRC32 值，默认可传 0
   const Data       - 要计算的数据块，一般不传地址
   Len: DWORD       - 数据块长度
   Result: DWORD    - 返回 CRC32 计算结果
 |</PRE>}

function StrCRC32(const OrgCRC32: LongWord; const Text: string): LongWord;
{* 计算字符串的 CRC32 值 }

function StrCRC32A(const OrgCRC32: LongWord; const Text: AnsiString): LongWord;
{* 计算 AnsiString 字符串的 CRC32 值 }

{$IFDEF TBYTES_DEFINED}

function BytesCRC32(const OrgCRC32: LongWord; Data: TBytes): LongWord;
{* 计算 TBytes 的 CRC32 值}

{$ENDIF}

function FileCRC32(const FileName: string; var CRC: LongWord; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
{* 计算文件 CRC32 值，支持超过 4G 的大文件
 |<PRE>
   const FileName: string   - 目标文件名
   var CRC: DWORD           - CRC32 值，变量参数，传入原始值，默认可为 0，输出计算值
   StartPos: Int64 = 0      - 文件起始位置，默认从头开始
   Len: Int64 = 0           - 计算长度，为零默认为整个文件
   Result: Boolean          - 返回成功标志，文件打开失败或指定长度无效时返回 False
 |</PRE>}

function CRC64Calc(const OrgCRC64: Int64; const Data; Len: LongWord): Int64;
{* 计算 CRC64 值
 |<PRE>
   OrgCRC64: Int64  - 起始 CRC64 值，默认可传 0
   const Data       - 要计算的数据块
   Len: DWORD       - 数据块长度
   Result: Int64    - 返回 CRC64 计算结果
 |</PRE>}

function StrCRC64(const OrgCRC64: Int64; const Text: string): Int64;
{* 计算字符串的 CRC64 值 }

function StrCRC64A(const OrgCRC64: Int64; const Text: AnsiString): Int64;
{* 计算 AnsiString 字符串的 CRC64 值 }

{$IFDEF TBYTES_DEFINED}

function BytesCRC64(const OrgCRC64: LongWord; Data: TBytes): LongWord;
{* 计算 TBytes 的 CRC64 值}

{$ENDIF}

function FileCRC64(const FileName: string; var CRC: Int64; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
{* 计算文件 CRC64 值，支持超过 4G 的大文件
 |<PRE>
   const FileName: string   - 目标文件名
   var CRC: Int64           - CRC64 值，变量参数，传入原始值，默认可为 0，输出计算值
   StartPos: Int64 = 0      - 文件起始位置，默认从头开始
   Len: Int64 = 0           - 计算长度，为零默认为整个文件
   Result: Boolean          - 返回成功标志，文件打开失败或指定长度无效时返回 False
 |</PRE>}

function CRC32Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord): LongWord;

function CRC64Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord): Int64;

{* Hash-based Message Authentication Code (based on CRC32/CRC64) }

implementation

const
  csBuff_Size = 4096;
  csCRC64 = $C96C5795D7870F42;

  HMAC_CRC32_BLOCK_SIZE_BYTE = 4;
  HMAC_CRC32_OUTPUT_LENGTH_BYTE = 4;

  HMAC_CRC64_BLOCK_SIZE_BYTE = 4;
  HMAC_CRC64_OUTPUT_LENGTH_BYTE = 4;

type
  // 文件缓冲区
  PBuff = ^TBuff;
  TBuff = array[0..csBuff_Size - 1] of Byte;

  // CRC32 表
  TCRC32Table = array[0..255] of LongWord;

  // CRC64 表
  TCRC64Table = array[0..255] of Int64;

var
  CRC32Table: TCRC32Table = (
    $00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
  );

  CRC64Table: TCRC64Table;

// 生成 CRC32 表，现用常量直接代替
//procedure Make_CRC32Table;
//asm
//        PUSH    EBX
//        MOV     EDX, OFFSET CRC32Table
//
//        XOR     EBX, EBX
//@MakeCRC32Loop:
//        CMP     EBX, $100
//        JE      @MakeCRC32_Succ
//        MOV     EAX, EBX
//        MOV     ECX, 8
//@MakeLoop:
//        TEST    EAX, 1
//        JZ      @MakeIsZero
//        SHR     EAX, 1
//        XOR     EAX, $EDB88320
//        JMP     @MakeNext
//@MakeIsZero:
//        SHR     EAX, 1
//@MakeNext:
//        LOOP    @MakeLoop
//        MOV     DWORD PTR [EDX], EAX
//        ADD     EDX, 4
//        INC     EBX
//        JMP     @MakeCRC32Loop
//
//@MakeCRC32_Succ:
//        POP     EBX
//        RET
//end;

function CalcCRC32Byte(OrgCRC32: LongWord; B: Byte): LongWord;
begin
  Result := ((OrgCRC32 shr 8) and $FFFFFF) xor CRC32Table[(OrgCRC32 and $FF) xor B];
end;

// 计算 CRC32 值
function DoCRC32Calc(const OrgCRC32: LongWord; const Data; Len: LongWord): LongWord;
var
  P: PByte;
begin
  Result := OrgCRC32;
  if (@Data = nil) or (Len = 0) then
    Exit;

  P := PByte(@Data);
  while Len > 0 do
  begin
    Result := ((Result shr 8) and $FFFFFF) xor CRC32Table[(Result and $FF) xor P^]; // CalcCRC32Byte(Result, P^);

    Inc(P);
    Dec(Len);
  end;
end;
//asm
//        OR      EDX, EDX   // Data = nil?
//        JE      @Exit
//        JECXZ   @Exit      // Len = 0?
//        PUSH    ESI
//        PUSH    EBX
//        MOV     ESI, OFFSET CRC32Table
//@Upd:
//        MOVZX   EBX, AL    // CRC32
//        XOR     BL, [EDX]
//        SHR     EAX, 8
//        AND     EAX, $00FFFFFF
//        XOR     EAX, [EBX * 4 + ESI]
//        INC     EDX
//        LOOP    @Upd
//        POP     EBX
//        POP     ESI
//@Exit:
//        RET
//end;

// 计算 CRC32 值
function CRC32Calc(const OrgCRC32: LongWord; const Data; Len: LongWord): LongWord;
begin
  Result := not OrgCRC32;
  Result := DoCRC32Calc(Result, Data, Len);
  Result := not Result;
end;

// 计算字符串的 CRC32 值
function StrCRC32(const OrgCRC32: LongWord; const Text: string): LongWord;
begin
  Result := CRC32Calc(OrgCRC32, PChar(Text)^, Length(Text) * SizeOf(Char));
end;

// 计算 AnsiString 字符串的 CRC32 值
function StrCRC32A(const OrgCRC32: LongWord; const Text: AnsiString): LongWord;
begin
  Result := CRC32Calc(OrgCRC32, PAnsiChar(Text)^, Length(Text));
end;

{$IFDEF TBYTES_DEFINED}

// 计算 TBytes 的 CRC32 值
function BytesCRC32(const OrgCRC32: LongWord; Data: TBytes): LongWord;
begin
  Result := CRC32Calc(OrgCRC32, PAnsiChar(Data[0])^, Length(Data));
end;

{$ENDIF}

function InternalCRC32Stream(Stream: TStream; const BufSize: Cardinal;
  var CRC: LongWord): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
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

  GetMem(Buf, BufLen);
  try
    Stream.Seek(0, soFromBeginning);
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        CRC := DoCrc32Calc(CRC, Buf^, ReadBytes);
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// 计算文件 CRC 值，参数分别为：文件名、CRC 值、起始地址、计算长度
function FileCRC32(const FileName: string; var CRC: LongWord; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
var
{$IFDEF MSWINDOWS}
  Handle: THandle;
  ReadCount: Integer;
  Size: Int64;
  Count: Int64;
  Buff: TBuff;
{$ELSE}
  Stream: TStream;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
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
{$ELSE} // 非 Windows 平台直接用文件流
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := InternalCRC32Stream(Stream, 4096 * 1024, CRC);
  finally
    Stream.Free;
  end;
{$ENDIF}
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

function DoCRC64Calc(const OrgCRC64: Int64; const Data; Len: LongWord): Int64;
var
  I: Integer;
  P: PByte;
begin
  Result := OrgCRC64;
  if (@Data = nil) or (Len = 0) then
    Exit;

  P := @Data;
  for I := 0 to Len - 1 do
  begin
    Result := Result shr 8 xor
      CRC64Table[Cardinal(Result) and $FF xor P^];
    Inc(P);
  end;
end;

// 计算 CRC64 值
function CRC64Calc(const OrgCRC64: Int64; const Data; Len: LongWord): Int64;
begin
  Result := not OrgCRC64;
  Result := DoCRC64Calc(Result, Data, Len);
  Result := not Result;
end;

// 计算字符串的 CRC64 值
function StrCRC64(const OrgCRC64: Int64; const Text: string): Int64;
begin
  Result := CRC64Calc(OrgCRC64, PChar(Text)^, Length(Text) * SizeOf(Char));
end;

// 计算 AnsiString 字符串的 CRC64 值
function StrCRC64A(const OrgCRC64: Int64; const Text: AnsiString): Int64;
begin
  Result := CRC64Calc(OrgCRC64, PAnsiChar(Text)^, Length(Text));
end;

{$IFDEF TBYTES_DEFINED}

// 计算 TBytes 的 CRC64 值
function BytesCRC64(const OrgCRC64: LongWord; Data: TBytes): LongWord;
begin
  Result := CRC64Calc(OrgCRC64, PAnsiChar(Data[0])^, Length(Data));
end;

{$ENDIF}

function InternalCRC64Stream(Stream: TStream; const BufSize: Cardinal;
  var CRC: Int64): Boolean;
var
  Buf: PAnsiChar;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  SavePos: Int64;
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

  GetMem(Buf, BufLen);
  try
    Stream.Seek(0, soFromBeginning);
    repeat
      ReadBytes := Stream.Read(Buf^, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        CRC := DoCrc64Calc(CRC, Buf^, ReadBytes);
      end;
    until (ReadBytes = 0) or (TotalBytes = Size);
    Result := True;
  finally
    FreeMem(Buf, BufLen);
    Stream.Position := SavePos;
  end;
end;

// 计算文件 CRC64 值，参数分别为：文件名、CRC 值、起始地址、计算长度
function FileCRC64(const FileName: string; var CRC: Int64; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
var
{$IFDEF MSWINDOWS}
  Handle: THandle;
  ReadCount: Integer;
  Size: Int64;
  Count: Int64;
  Buff: TBuff;
{$ELSE}
  Stream: TStream;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
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
{$ELSE}
  // 非 Windows 平台直接用文件流
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := InternalCRC64Stream(Stream, 4096 * 1024, CRC);
  finally
    Stream.Free;
  end;
{$ENDIF}
end;

function CRC32Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord): LongWord;
var
  I: Integer;
  Ipad, Opad: array[0..3] of Byte;
  Sum, Res: LongWord;
begin
  if KeyLength > HMAC_CRC32_BLOCK_SIZE_BYTE then
  begin
    Sum := CRC32Calc(0, Key^, KeyLength);
    KeyLength := HMAC_CRC32_OUTPUT_LENGTH_BYTE;
    Key := @Sum;
  end;

  FillChar(Ipad, HMAC_CRC32_BLOCK_SIZE_BYTE, $36);
  FillChar(Opad, HMAC_CRC32_BLOCK_SIZE_BYTE, $5C);
  
  for I := 0 to KeyLength - 1 do
  begin
    Ipad[I] := Byte(Ipad[I] xor Byte(Key[I]));
    Opad[I] := Byte(Opad[I] xor Byte(Key[I]));
  end;

  Res := $FFFFFFFF;
  Res := DoCRC32Calc(Res, Ipad[0], HMAC_CRC32_BLOCK_SIZE_BYTE);
  Res := DoCRC32Calc(Res, Input^, Length);
  Res := not Res;

  Result := $FFFFFFFF;
  Result := DoCRC32Calc(Result, Opad[0], HMAC_CRC32_BLOCK_SIZE_BYTE);
  Result := DoCRC32Calc(Result, Res, HMAC_CRC32_OUTPUT_LENGTH_BYTE);
  Result := not Result;
end;

function CRC64Hmac(Key: PAnsiChar; KeyLength: Integer; Input: PAnsiChar;
  Length: LongWord): Int64;
var
  I: Integer;
  Ipad, Opad: array[0..7] of Byte;
  Sum, Res: Int64;
begin
  if KeyLength > HMAC_CRC64_BLOCK_SIZE_BYTE then
  begin
    Sum := CRC64Calc(0, Key^, KeyLength);
    KeyLength := HMAC_CRC64_OUTPUT_LENGTH_BYTE;
    Key := @Sum;
  end;

  FillChar(Ipad, HMAC_CRC64_BLOCK_SIZE_BYTE, $36);
  FillChar(Opad, HMAC_CRC64_BLOCK_SIZE_BYTE, $5C);

  for I := 0 to KeyLength - 1 do
  begin
    Ipad[I] := Byte(Ipad[I] xor Byte(Key[I]));
    Opad[I] := Byte(Opad[I] xor Byte(Key[I]));
  end;

  Res := $FFFFFFFF;
  Res := DoCRC64Calc(Res, Ipad[0], HMAC_CRC64_BLOCK_SIZE_BYTE);
  Res := DoCRC64Calc(Res, Input^, Length);
  Res := not Res;

  Result := $FFFFFFFF;
  Result := DoCRC64Calc(Result, Opad[0], HMAC_CRC64_BLOCK_SIZE_BYTE);
  Result := DoCRC64Calc(Result, Res, HMAC_CRC64_OUTPUT_LENGTH_BYTE);
  Result := not Result;
end;

initialization
//  Make_CRC32Table; // 初始化CRC32表
  
  Make_CRC64Table; // 初始化CRC64表

end.

