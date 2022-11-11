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

unit CnGB18030;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：支持 GB18030 大字符集与 Unicode 的工具单元
* 单元作者：CnPack 开发组
* 备    注：GB18030 大字符集因为需兼容 GBK/GB2312，故此本质上是非等宽字符串，
*           字符长度有 ASCII 的一字节、普通汉字的二字节、生僻汉字的四字节三类
*           且均是按阅读习惯紧密排列，类似于 AnsiString
*           而 Delphi 的 WideString 和 UnicodeString 是 UTF16-LE，双字节编码有颠倒
*           比如“吃饭”两个字，
*           AnsiString 内存中是 B3D4B7B9，GB18030内码也是 B3D4 和 B7B9 符合阅读顺序
*           UnicodeString 内存中是 03546D99，但 Unicode 内码却是 5403 和 996D，有反置
*
*           GB18030 中，字符的编码值就是实际编码内容
*           UTF16 中，辅助平面内的编码值（超出二字节），和实际四字节编码方式不同
*
*           系统的 UtfEncode 函数能够正确处理四字节 UTF16-LE，注意 UTF8 转换的是
*           四字节 UTF16 字符的编码值，不是转换四字节本身，因而 UTF8-MB4 足够容纳
*
*           GB18030 的编码取值范围（十六进制）
*             注意 AABB~CCDD 的范围不是通常意义上的增到 FF 再进位，
*             而是代表前一个字节 AA 到 CC，且后一个字节 CC 到 DD，并不包括 AAFF 这种
*           单字节：00~7F
*           双字节：A1A9~A1FE                     1 区
*                   A840~A97E, A880~A9A0          5 区
*                   B0A1~F7FE                     2 区汉字
*                   8140~A07E, 8180~A0FE          3 区汉字
*                   AA40~FE7E, AA80~FEA0          4 区汉字
*                   AAA1~AFFE                     用户 1 区
*                   F8A1~FEFE                     用户 2 区
*                   A140~A77E, A180~A7A0          用户 3 区
*           四字节：
*
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.11.11
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// {$DEFINE UTF16_BE}

// Delphi 默认 UTF16-LE，如果要处理 UTF16-BE 字符串，需要定义 UTF16_BE

uses
  SysUtils, Classes, CnNative;

const
  CN_INVALID_CODEPOINT = $FFFFFFFF;

type
{$IFDEF SUPPORT_ANSISTRING_CODEPAGE}
  TCnGB18130String = RawByteString;
{$ELSE}
  TCnGB18130String = AnsiString;
{$ENDIF}
  {* GB18130 编码的字符串，内部用 RawByteString 也就是 AnsiString($FFFF) 表示}

  PCnGB18130String = ^TCnGB18130String;

  PCnGB18130StringPtr = PAnsiChar;
  {* GB18130 编码的字符指针，内部用 PAnsiChar 表示}

  TCnCodePoint = type Cardinal;

  TCn2CharRec = packed record
    P1: AnsiChar;
    P2: AnsiChar;
  end;
  PCn2CharRec = ^TCn2CharRec;

  TCn4CharRec = packed record
    P1: AnsiChar;
    P2: AnsiChar;
    P3: AnsiChar;
    P4: AnsiChar;
  end;
  PCn4CharRec = ^TCn4CharRec;

function GetCharLengthFromUtf8(Utf8Str: PAnsiChar): Integer;
{* 计算一 UTF8（可能是 UTF8MB4）字符串的字符数}

function GetCharLengthFromUtf16(Utf16Str: PWideChar): Integer;
{* 计算一 UTF16（可能混合 Unicode 扩展平面里的四字节字符）字符串的字符数}

function GetCharLengthFromGB18130(GB18130Str: PCnGB18130StringPtr): Integer;
{* 计算一 GB18130 字符串的字符数}

function GetByteWidthFromUtf8(Utf8Str: PAnsiChar): Integer;
{* 计算一 UTF8（可能是 UTF8MB4）字符串的当前字符占多少字节}

function GetByteWidthFromUtf16(Utf16Str: PWideChar): Integer;
{* 计算一 UTF16（可能混合 Unicode 扩展平面里的四字节字符）字符串的当前字符占多少字节}

function GetByteWidthFromGB18130(GB18130Str: PCnGB18130StringPtr): Integer;
{* 计算一 GB18130 字符串的当前字符占多少字节}

function Utf16ToGB18130(Utf16Str: PWideChar; GB18130Str: PCnGB18130StringPtr): Integer;
{* 将一 UTF16（可能混合 Unicode 扩展平面里的四字节字符）字符串转换为 GB18130 字符串
  GB18130Str 所指区域用来容纳转换的结果，如传 nil，则不进行转换
  返回值返回 GB18130Str 所需的比特长度或转换后的比特长度，不包括末尾的 #0}

function GB18130ToUtf16(GB18130Str: PCnGB18130StringPtr; Utf16Str: PWideChar): Integer;
{* 将一 GB18130 字符串转换为 UTF16（可能混合 Unicode 扩展平面里的四字节字符）字符串
  UniStr 所指区域用来容纳转换的结果，如传 nil，则不进行转换
  返回值返回 UniStr 所需的双字节字符长度或转换后的双字节字符长度，不包括末尾的宽字符 #0}

function GetGB18130FromUtf16(Utf16Str: PWideChar): TCnGB18130String;
{* 返回一 Unicode 字符串对应的 GB18130 字符串}

{$IFDEF UNICODE}

function GetUtf16FromGB18130(GB18130Str: TCnGB18130String): string;
{* 返回一 GB18130 字符串对应的 Utf16 字符串}

{$ELSE}

function GetUtf16FromGB18130(GB18130Str: TCnGB18130String): WideString;
{* 返回一 GB18130 字符串对应的 Utf16 字符串}

{$ENDIF}

function GetCodePointFromUtf16Char(Utf16Str: PWideChar): TCnCodePoint;
{* 计算一个 Utf16 字符的编码值（也叫代码位置），注意 Utf16Str 可能指向一个双字节字符，也可能指向一个四字节字符}

function GetCodePointFromUtf164Char(PtrTo4Char: Pointer): TCnCodePoint;
{* 计算一个四字节 Utf16 字符的编码值（也叫代码位置）}

function GetUtf16CharFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
{* 计算一个 Unicode 编码值的二字节或四字节表示，结果放在 PtrTo4Char 所指的二字节或四字节区域
  调用者在 CP 超过 $FFFF 时须保证 PtrToChars 所指的区域至少四字节，反之二字节即可
  返回 1 或 2，分别表示处理的是二字节或四字节}

function GetCodePointFromGB18030Char(PtrToGB18030Chars: PCnGB18130StringPtr): TCnCodePoint;
{* 计算一个 GB18030 字符的编码值（也叫代码位置），注意 PtrToGB18030Chars 可能指向一个单、双、四字节字符}

function GetGB18030CharsFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
{* 计算一个 GB18030 编码值的一字节或二字节或四字节表示，如果 PtrToChars 指向的位置不为空则将转换后的内容放里头
   返回值是转换的字节数，1 或 2 或 4}

function GetUtf16HighByte(Rec: PCn2CharRec): Byte; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 得到一个 UTF 16 双字节字符的高位字节值}

function GetUtf16LowByte(Rec: PCn2CharRec): Byte; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 得到一个 UTF 16 双字节字符的低位字节值}

procedure SetUtf16HighByte(B: Byte; Rec: PCn2CharRec); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 设置一个 UTF 16 双字节字符的高位字节值}

procedure SetUtf16LowByte(B: Byte; Rec: PCn2CharRec); {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* 设置一个 UTF 16 双字节字符的低位字节值}

implementation

const
  CN_UTF16_4CHAR_PREFIX1_LOW  = $D8;
  CN_UTF16_4CHAR_PREFIX1_HIGH = $DC;
  CN_UTF16_4CHAR_PREFIX2_LOW  = $DC;
  CN_UTF16_4CHAR_PREFIX2_HIGH = $E0;

  CN_UTF16_4CHAR_HIGH_MASK    = $3;
  CN_UTF16_4CHAR_SPLIT_MASK   = $3FF;

  CN_UTF16_EXT_BASE           = $10000;

  CN_GB18030_BOM: array[0..3] of Byte = ($84, $31, $95, $33);

function GetUtf16HighByte(Rec: PCn2CharRec): Byte;
begin
{$IFDEF UTF16_BE}
  Result := Byte(Rec^.P1);
{$ELSE}
  Result := Byte(Rec^.P2); // UTF16-LE 的高低位会置换
{$ENDIF}
end;

function GetUtf16LowByte(Rec: PCn2CharRec): Byte;
begin
{$IFDEF UTF16_BE}
  Result := Byte(Rec^.P2);
{$ELSE}
  Result := Byte(Rec^.P1); // UTF16-LE 的高低位会置换
{$ENDIF}
end;

procedure SetUtf16HighByte(B: Byte; Rec: PCn2CharRec);
begin
{$IFDEF UTF16_BE}
  Rec^.P1 := AnsiChar(B);
{$ELSE}
  Rec^.P2 := AnsiChar(B); // UTF16-LE 的高低位会置换
{$ENDIF}
end;

procedure SetUtf16LowByte(B: Byte; Rec: PCn2CharRec);
begin
{$IFDEF UTF16_BE}
  Rec^.P2 := AnsiChar(B);
{$ELSE}
  Rec^.P1 := AnsiChar(B); // UTF16-LE 的高低位会置换
{$ENDIF}
end;

function GetCharLengthFromUtf8(Utf8Str: PAnsiChar): Integer;
var
  L: Integer;
begin
  Result := 0;
  while Utf8Str^ <> #0 do
  begin
    L := GetByteWidthFromUtf8(Utf8Str);
    Inc(Utf8Str, L);
    Inc(Result);
  end;
end;

function GetCharLengthFromUtf16(Utf16Str: PWideChar): Integer;
var
  L: Integer;
begin
  Result := 0;
  while Utf16Str^ <> #0 do
  begin
    L := GetByteWidthFromUtf16(Utf16Str);
    Utf16Str := PWideChar(TCnNativeInt(Utf16Str) + L);
    Inc(Result);
  end;
end;

function GetCharLengthFromGB18130(GB18130Str: PCnGB18130StringPtr): Integer;
var
  L: Integer;
begin
  Result := 0;
  while GB18130Str^ <> #0 do
  begin
    L := GetByteWidthFromGB18130(GB18130Str);
    Inc(GB18130Str, L);
    Inc(Result);
  end;
end;

function GetByteWidthFromUtf8(Utf8Str: PAnsiChar): Integer;
var
  B: Byte;
begin
  B := Byte(Utf8Str^);
  if B >= $FC then        // 6 个 1，1 个 0，先不考虑七或八 1 的情况
    Result := 6
  else if B >= $F8 then   // 5 个 1，1 个 0
    Result := 5
  else if B >= $F0 then   // 4 个 1，1 个 0
    Result := 4
  else if B >= $E0 then   // 3 个 1，1 个 0
    Result := 3
  else if B >= $B0 then   // 2 个 1，1 个 0
    Result := 2
  else                    // 其他
    Result := 1;
end;

function GetByteWidthFromUtf16(Utf16Str: PWideChar): Integer;
var
  P: PCn2CharRec;
  B1, B2: Byte;
begin
  Result := 2;

  P := PCn2CharRec(Utf16Str);
  B1 := GetUtf16HighByte(P);

  if (B1 >= CN_UTF16_4CHAR_PREFIX1_LOW) and (B1 < CN_UTF16_4CHAR_PREFIX1_HIGH) then
  begin
    // 如果两个单字节字符，其值分别在 $D800 到 $DBFF 之间
    Inc(P);
    B2 := GetUtf16HighByte(P);

    // 那么紧跟在后面的两个单字节字符应该在 $DC00 到 $DFFF 之间，
    if (B2 >= CN_UTF16_4CHAR_PREFIX2_LOW) and (B2 < CN_UTF16_4CHAR_PREFIX2_HIGH) then
      Result := 4;

    // 这四个字节组成一个四字节 Unicode 字符，但并非该值的编码值
  end;
end;

function GetByteWidthFromGB18130(GB18130Str: PCnGB18130StringPtr): Integer;
var
  B1, B2, B3, B4: Byte;
begin
  Result := 1;
  B1 := Byte(GB18130Str^);
  if B1 <= $7F then
    Exit;

  Inc(GB18130Str);
  B2 := Byte(GB18130Str^);

  if (B1 >= $81) and (B1 <= $FE) then
  begin
    if ((B2 >= $40) and (B2 <= $7E)) or
      ((B2 >= $80) and (B2 <= $FE)) then
      Result := 2
    else if (B2 >= $30) and (B2 <= $39) then
    begin
      Inc(GB18130Str);
      B3 := Byte(GB18130Str^);
      Inc(GB18130Str);
      B4 := Byte(GB18130Str^);

      if ((B3 >= $81) and (B3 <= $FE)) or
      ((B4 >= $30) and (B4 <= $39)) then
        Result := 4;
    end;
  end;
end;

function Utf16ToGB18130(Utf16Str: PWideChar; GB18130Str: PCnGB18130StringPtr): Integer;
begin

end;

function GB18130ToUtf16(GB18130Str: PCnGB18130StringPtr; Utf16Str: PWideChar): Integer;
begin

end;

function GetGB18130FromUtf16(Utf16Str: PWideChar): TCnGB18130String;
var
  L: Integer;
begin
  L := Utf16ToGB18130(Utf16Str, nil);
  if L > 0 then
  begin
    SetLength(Result, L);
    Utf16ToGB18130(Utf16Str, @Result[1]);
  end;
end;

{$IFDEF UNICODE}

function GetUtf16FromGB18130(GB18130Str: TCnGB18130String): string;
var
  L: Integer;
begin
  L := GB18130ToUtf16(PCnGB18130StringPtr(GB18130Str), nil);
  if L > 0 then
  begin
    SetLength(Result, L);
    GB18130ToUtf16(PCnGB18130StringPtr(GB18130Str), @Result[1]);
  end;
end;

{$ELSE}

function GetUtf16FromGB18130(GB18130Str: TCnGB18130String): WideString;
var
  L: Integer;
begin
  L := GB18130ToUtf16(PCnGB18130StringPtr(GB18130Str), nil);
  if L > 0 then
  begin
    SetLength(Result, L);
    GB18130ToUtf16(PCnGB18130StringPtr(GB18130Str), @Result[1]);
  end;
end;

{$ENDIF}

function GetCodePointFromUtf16Char(Utf16Str: PWideChar): TCnCodePoint;
var
  R: Word;
  C2: PCn2CharRec;
begin
  if GetByteWidthFromUtf16(Utf16Str) = 4 then // 四字节字符
    Result := GetCodePointFromUtf164Char(PAnsiChar(Utf16Str))
  else  // 普通双字节字符
  begin
    C2 := PCn2CharRec(Utf16Str);
    R := Byte(C2^.P1) shl 8 + Byte(C2^.P2);       // 双字节字符，值本身就是编码值

{$IFDEF UTF16_BE}
    Result := TCnCodePoint(R);
{$ELSE}
    Result := TCnCodePoint(UInt16ToBigEndian(R)); // UTF16-LE 要交换值
{$ENDIF}
  end;
end;

function GetCodePointFromUtf164Char(PtrTo4Char: Pointer): TCnCodePoint;
var
  TH, TL: Word;
  C2: PCn2CharRec;
begin
  C2 := PCn2CharRec(PtrTo4Char);

  // 第一个字节，去掉高位的 110110；第二个字节留着，共 2 + 8 = 10 位
  TH := (GetUtf16HighByte(C2) and CN_UTF16_4CHAR_HIGH_MASK) shl 8 + GetUtf16LowByte(C2);
  Inc(C2);

  // 第三个字节，去掉高位的 110111，第四个字节留着，共 2 + 8 = 10 位
  TL := (GetUtf16HighByte(C2) and CN_UTF16_4CHAR_HIGH_MASK) shl 8 + GetUtf16LowByte(C2);

  // 高 10 位拼低 10 位
  Result := TH shl 10 + TL + CN_UTF16_EXT_BASE;
  // 码点减去 $10000 后的值，前 10 位映射到 $D800 到 $DBFF 之间，后 10 位映射到 $DC00 到 $DFFF 之间
end;

function GetUtf16CharFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
var
  C2: PCn2CharRec;
  L, H: Byte;
  LW, HW: Word;
begin
  if CP >= CN_UTF16_EXT_BASE then
  begin
    CP := CP - CN_UTF16_EXT_BASE;
    // 拆出高 10 位放前两字节，拆出低 10 位放后两字节

    LW := CP and CN_UTF16_4CHAR_SPLIT_MASK;          // 低 10 位，放三、四字节
    HW := (CP shr 10) and CN_UTF16_4CHAR_SPLIT_MASK; // 高 10 位，放一、二字节

    L := HW and $FF;
    H := (HW shr 8) and CN_UTF16_4CHAR_HIGH_MASK;
    H := H or CN_UTF16_4CHAR_PREFIX1_LOW;              // 1101 1000
    C2 := PCn2CharRec(PtrToChars);

    SetUtf16LowByte(L, C2);
    SetUtf16HighByte(H, C2);

    L := LW and $FF;
    H := (LW shr 8) and CN_UTF16_4CHAR_HIGH_MASK;
    H := H or CN_UTF16_4CHAR_PREFIX1_HIGH;              // 1101 1100
    Inc(C2);

    SetUtf16LowByte(L, C2);
    SetUtf16HighByte(H, C2);
    Result := 2;
  end
  else
  begin
    C2 := PCn2CharRec(PtrToChars);
    SetUtf16LowByte(Byte(CP and $00FF), C2);
    SetUtf16HighByte(Byte(CP shr 8), C2);
    Result := 1;
  end;
end;

function GetCodePointFromGB18030Char(PtrToGB18030Chars: PCnGB18130StringPtr): TCnCodePoint;
var
  C1, C2, C3, C4: Byte;
begin
  Result := 0;
  C1 := Byte(PtrToGB18030Chars^);
  if C1 < $80 then
    Result := C1                                // 单字节
  else if (C1 >= $81) and (C1 <= $FE) then
  begin
    Inc(PtrToGB18030Chars);
    C2 := Byte(PtrToGB18030Chars^);
    if ((C2 >= $40) and (C2 <= $7E)) or ((C2 >= $90) and (C2 <= $FE)) then 
      Result := C1 shl 8 + C2                   // 双字节
    else if (C2 >= $30) and (C2 <= $39) then    // 四字节
    begin
      Inc(PtrToGB18030Chars);
      C3 := Byte(PtrToGB18030Chars^);
      Inc(PtrToGB18030Chars);                   // 不判断三字节的 81 到 F3 以及四字节的 30 到 39 了
      C4 := Byte(PtrToGB18030Chars^);

      Result := C1 shl 24 + C2 shl 16 + C3 shl 8 + C4;
    end;
  end;
end;

function GetGB18030CharsFromCodePoint(CP: TCnCodePoint; PtrToChars: Pointer): Integer;
var
  P: PByte;
  C1, C2, C3, C4: Byte;
begin
  Result := 0;
  P := PByte(PtrToChars);
  if CP < $80 then
  begin
    if P <> nil then
      P^ := Byte(CP);
    Result := 1;
  end
  else
  begin
    C1 := CP and $FF000000 shr 24;
    C2 := CP and $00FF0000 shr 16;
    C3 := CP and $0000FF00 shr 8;
    C4 := CP and $000000FF;

    if (C1 = 0) and (C2 = 0) and ((C3 >= $81) and (C3 <= $FE)) and
      (((C4 >= $40) and (C4 <= $7E)) or ((C4 >= $80) and (C4 <= $FE))) then
    begin
      // 是两字节字符
      if P <> nil then
      begin
        P^ := C3;
        Inc(P);
        P^ := C4;
      end;
      Result := 2;
    end
    else if ((C1 >= $81) and (C1 <= $FE)) and ((C2 >= $30) and (C2 <= $39)) then
    begin
      // 是四字节字符，暂不判断 C3 和 C4
      if P <> nil then
      begin
        P^ := C1;
        Inc(P);
        P^ := C2;
        Inc(P);
        P^ := C3;
        Inc(P);
        P^ := C4;
      end;
      Result := 4;
    end;
  end;
end;

end.

