program TestString;
// 注意本源文件必须是 UTF-8 编码

uses
  SysUtils;

// 打印内存块的十六进制值
procedure PrintHexDump(P: Pointer; ByteLen: Integer);
var
  i: Integer;
  PB: PByte;
begin
  if (P = nil) or (ByteLen <= 0) then
  begin
    Writeln('Invalid parameters');
    Exit;
  end;

  PB := PByte(P);
  // Write('Hex Dump (', ByteLen, ' bytes): ');

  for i := 0 to ByteLen - 1 do
  begin
    // 输出每个字节的十六进制值
    Write(IntToHex(PB^, 2), ' ');
    Inc(PB);

    // 每16字节换行
    if (i + 1) mod 16 = 0 then
    begin
      Writeln;
      Write('  ');
    end;
  end;

  Writeln;
end;

var
  Raw: RawByteString;
  Ansi, A1: AnsiString;
  Utf8: UTF8String;
  Utf16: UnicodeString;
  Str: string;
begin
  Writeln(DefaultSystemCodePage);
  // 936

  Raw := '吃饭';
  Writeln('Raw Length ' + IntToStr(Length(Raw)));
  PrintHexDump(@Raw[1], Length(Raw));
  // 6 字节的 UTF-8 编码

  Ansi := '吃饭';
  Writeln('Ansi Length ' + IntToStr(Length(Ansi)));
  PrintHexDump(@Ansi[1], Length(Ansi));
  // 6 字节的 UTF-8 编码

  Utf8 := '吃饭';
  Writeln('Utf8 Length ' + IntToStr(Length(Utf8)));
  PrintHexDump(@Utf8[1], Length(Utf8));
  // 6 字节的 UTF-8 编码再次被 UTF-8 编码，共 12 字节

  Utf16 := '吃饭';
  Writeln('Utf16 Length ' + IntToStr(Length(Utf16) * SizeOf(WideChar)));
  PrintHexDump(@Utf16[1], Length(Utf16) * SizeOf(WideChar));
  // 6 字节的 UTF-8 编码，每个字节被扩展成双字节，共 12 字节

  Str := '吃饭';
  Writeln('String Length ' + IntToStr(Length(Str)));
  PrintHexDump(@Str[1], Length(Str));
  // 6 字节的 UTF-8 编码

  Ansi := Utf8ToAnsi(Raw);
  PrintHexDump(@Ansi[1], Length(Ansi));
  // 936 代码页得到 B3 D4 B7 B9 否则 3F 3F

  A1 := #$B3#$D4#$B7#$B9;
  A1 := AnsiToUtf8(A1);
  PrintHexDump(@A1[1], Length(A1));
  // 936 代码页得到 E5 90 83 E9 A5 AD 否则 C2 B3 C3 94 C2 B7 C2 B9

  Readln;
end.

