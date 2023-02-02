{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

{******************************************************************************}
{      该单元大部分内容基于 Stefan Reuther 的 BDiff / BPatch C 代码翻译而来。  }
{      下面是 BDiff / BPatch 的声明:                                           }
{ -----------------------------------------------------------------------------}
{(c) copyright 1999 by Stefan Reuther <Streu@gmx.de>. Copying this program is  }
{allowed, as long as you include source code and document changes you made in a}
{user-visible way so people know they're using your version, not mine.         }
{This program is distributed in the hope that it will be useful, but without   }
{warranties of any kind, be they explicit or implicit.                         }
{ -----------------------------------------------------------------------------}

unit CnBinaryDiffPatch;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：简易的二进制差分以及补丁算法单元
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：该单元是简易的二进制差分以及补丁算法实现。
*           大部分基于 Stefan Reuther 的 BDiff / BPatch C 代码翻译而来。
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2016.08.16 V1.2
*               实现目录的批量 Diff/Patch 功能
*           2016.08.08 V1.1
*               实现 Patch 功能
*           2016.08.05 V1.0
*               创建单元，实现 Diff 功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, {$IFDEF MSWINDOWS} Windows, {$ENDIF} CnCommon;

const
  CN_BINARY_DIFF_NAME_VER: AnsiString = 'CnBDiff1';

type
  ECnPatchFormatError = class(Exception);

function BinaryDiffStream(OldStream, NewStream, PatchStream: TMemoryStream): Boolean;
{* 二进制差分比较新旧内存块（流），差分结果放入 PatchStream 中，Patch 可输出二进制与文本等格式}

function BinaryPatchStream(OldStream, PatchStream, NewStream: TMemoryStream): Boolean;
{* 二进制差分补丁旧内存块（流），合成结果放入 NewStream 中，只支持二进制格式的 Patch}

function BinaryDiffFile(const OldFile, NewFile, PatchFile: string): Boolean;
{* 二进制差分比较新旧文件，差分结果存入 PatchFile 中}

function BinaryPatchFile(const OldFile, PatchFile, NewFile: string): Boolean;
{* 二进制差分补丁旧文件，合成结果存入 NewFile 中}

function BinaryDiffDirectory(const OldDir, NewDir, PatchDir: string): Boolean;
{* 二进制差分比较新旧目录，在 PatchDir 中输出形如“旧文件名.patch”的二进制补丁，
   如无旧文件，则以新文件.patch 为补丁名，不支持子目录}

function BinaryPatchDirectory(const OldDir, PatchDir, NewDir: string): Boolean;
{* 二进制差分补丁旧目录，并把新内容输出至新目录}

implementation

uses
  CnNative;

resourcestring
  SCnErrorCopyRead = 'Copy Content Read Fail.';
  SCnErrorCopyWrite = 'Copy Content Write Fail.';
  SCnErrorCheckSum = 'Patch Checksum Fail.';
  SCnErrorPatchHeader = 'Patch Header Missing.';
  SCnErrorPatchNameVersion = 'Patch Header Name/Version Mismatch.';
  SCnErrorPatchCopyArea = 'Patch Copy Area Mismatch.';
  SCnErrorPatchCopySize = 'Patch Copy Size Mismatch.';
  SCnErrorPatchAddArea = 'Patch Add Area Mismatch.';
  SCnErrorPatchOperatorFmt = 'Patch Operator Unknown %c.';
  SCnErrorPatchLength = 'Patch Length Mismatch.';

const
  CN_MIN_LENGTH = 24;
  ADD_CHAR: AnsiChar = '+';
  COPY_CHAR: AnsiChar = '@';
  DOT_CHAR: AnsiChar = '.';
  CRLF: AnsiString = #13#10;
  PATCH_SUFFIX = '.patch';

type
  TCardinalArray = array[0..65535] of Cardinal;
  PCardinalArray = ^TCardinalArray;

  TCnMatchRec = packed record
    OldPos: Cardinal;
    NewPos: Cardinal;
    Len: Cardinal;
  end;
  PCnMatchRec = ^TCnMatchRec;

  TCnDiffOutputType = (dotBinary, dotFiltered, dotQuoted);

var
  CnDiffOutputType: TCnDiffOutputType = dotBinary; // 默认生成二进制方式的 Patch

function BlockSortCompare(BytePosA, BytePosB: Cardinal; Data: PByte; DataLen: Cardinal): Integer;
var
  Pa, Pb: PShortInt;
  Len: Cardinal;
begin
  Pa := PShortInt(TCnNativeUInt(Data) + BytePosA);
  Pb := PShortInt(TCnNativeUInt(Data) + BytePosB);
  Len := DataLen - BytePosA;
  if DataLen - BytePosB < Len then
    Len := DataLen - BytePosB;

  while (Len <> 0) and (Pa^ = Pb^) do
  begin
    Inc(Pa);
    Inc(Pb);
    Dec(Len);
  end;

  if Len = 0 then
    Result := BytePosA - BytePosB
  else
    Result := Pa^ - Pb^;
end;

procedure BlockSortSink(LeftPos, RightPos: Cardinal; Block: PInteger;
  Data: PByte; DataLen: Cardinal);
var
  I, J, X: Cardinal;
  BlockIntArray: PCardinalArray;
begin
  I := LeftPos;
  BlockIntArray := PCardinalArray(Block);
  X := BlockIntArray^[I];
  while True do
  begin
    J := 2 * I + 1;
    if J >= RightPos then
      Break;
    if J < RightPos - 1 then
      if BlockSortCompare(BlockIntArray^[J], BlockIntArray^[J + 1], Data, DataLen) < 0 then
        Inc(J);
    if BlockSortCompare(X, BlockIntArray^[J], Data, DataLen) > 0 then
      Break;

    BlockIntArray^[I] := BlockIntArray^[J];
    I := J;
  end;
  BlockIntArray^[I] := X;
end;

function BlockSort(Data: PByte; DataLen: Cardinal): PInteger;
var
  Block: PInteger;
  I, X, LeftPos, RightPos: Cardinal;
  BlockIntArray: PCardinalArray;
begin
  Result := nil;
  if DataLen <= 0 then
    Exit;

  Block := PInteger(GetMemory(SizeOf(Cardinal) * DataLen));
  if Block = nil then
    Exit;

  BlockIntArray := PCardinalArray(Block);
  for I := 0 to DataLen - 1 do
    BlockIntArray^[I] := I;

  LeftPos := DataLen div 2;
  RightPos := DataLen;

  while LeftPos > 0 do
  begin
    Dec(LeftPos);
    BlockSortSink(LeftPos, RightPos, Block, Data, DataLen);
  end;

  while RightPos > 0 do
  begin
    X := BlockIntArray^[LeftPos];
    BlockIntArray^[LeftPos] := BlockIntArray^[RightPos - 1];
    BlockIntArray^[RightPos - 1] := X;
    Dec(RightPos);
    BlockSortSink(LeftPos, RightPos, Block, Data, DataLen);
  end;
  Result := Block;
end;

function FindString(Data: PByte; Block: PInteger; DataLen: Cardinal; Sub: PByte;
  MaxLen: Cardinal; var Index: Cardinal): Cardinal;
var
  First, Last, Mid, FoundSize, L: Cardinal;
  Pm, Sm: PShortInt;
  BlockIntArray: PCardinalArray;
begin
  Result := 0;
  Index := 0;
  if DataLen = 0 then
    Exit;

  First := 0;
  Last := DataLen - 1;

  BlockIntArray := PCardinalArray(Block);
  while First <= Last do
  begin
    Mid := (First + Last) div 2;
    Pm := PShortInt(TCnNativeUInt(Data) + BlockIntArray^[Mid]);
    Sm := PShortInt(Sub);

    L := DataLen - BlockIntArray^[Mid];
    if L > MaxLen then
      L := MaxLen;

    FoundSize := 0;
    while (FoundSize < L) and (Pm^ = Sm^) do
    begin
      Inc(FoundSize);
      Inc(Pm);
      Inc(Sm);
    end;

    if FoundSize > Result then
    begin
      Result := FoundSize;
      Index := BlockIntArray^[Mid];
    end;

    if (FoundSize = L) or (Pm^ < Sm^) then
      First := Mid + 1
    else
    begin
      Last := Mid;
      if Last <> 0 then
        Dec(Last)
      else
        Break;
    end;
  end;
end;

procedure PackLong(P: PByte; L: Cardinal);
begin
  P^ := L and $FF;
  Inc(P);
  P^ := (L shr 8) and $FF;
  Inc(P);
  P^ := (L shr 16) and $FF;
  Inc(P);
  P^ := (L shr 24) and $FF;
end;

function GetLong(P: PByte): Cardinal;
begin
  Result := P^;
  Inc(P);
  Result := Result + 256 * P^;
  Inc(P);
  Result := Result + 65536 * P^;
  Inc(P);
  Result := Result + 16777216 * P^;
end;

function CheckSum(Data: PByte; DataLen: Cardinal; InitialSum: Cardinal = 0): Cardinal;
begin
  Result := InitialSum;
  while DataLen > 0 do
  begin
    Result := ((Result shr 30) and 3) or (Result shl 2);
    Result := Result xor Data^;

    Dec(DataLen);
    Inc(Data);
  end;
end;

procedure BsFindMaxMatch(Ret: PCnMatchRec; Data: PByte; Sort: PInteger; Len: Cardinal;
  Text: PByte; TextLen: Cardinal);
var
  FoundPos, FoundLen: Cardinal;
begin
  Ret^.Len := 0;
  Ret^.NewPos := 0;
  while TextLen <> 0 do
  begin
    FoundLen := FindString(Data, Sort, Len, Text, TextLen, FoundPos);
    if FoundLen >= CN_MIN_LENGTH then
    begin
      Ret^.OldPos := FoundPos;
      Ret^.Len := FoundLen;
      Exit;
    end;
    Inc(Text);
    Dec(TextLen);
    Inc(Ret^.NewPos);
  end;
end;

procedure CopyStreamData(OldStream, NewStream: TStream; ASize: Cardinal;
  ACheckSum: Cardinal; FromPatch: Boolean);
const
  BUF_SIZE = 4096;
var
  ChkRes: Cardinal;
  Buf: array[0..BUF_SIZE - 1] of AnsiChar;
  ToReadSize: Cardinal;
begin
  if (OldStream = nil) or (NewStream = nil) or (ASize = 0) then
    Exit;

  ChkRes := 0;
  while ASize > 0 do
  begin
    if ASize > BUF_SIZE then
      ToReadSize := BUF_SIZE
    else
      ToReadSize := ASize;

    if Cardinal(OldStream.Read(Buf[0], ToReadSize)) <> ToReadSize then
      raise ECnPatchFormatError.Create(SCnErrorCopyRead);

    if Cardinal(NewStream.Write(Buf[0], ToReadSize)) <> ToReadSize then
      raise ECnPatchFormatError.Create(SCnErrorCopyWrite);

    ChkRes := CheckSum(@Buf[0], ToReadSize, ChkRes);
    Dec(ASize, ToReadSize);
  end;

  if not FromPatch and (ChkRes <> ACheckSum) then
    raise ECnPatchFormatError.Create(SCnErrorCheckSum);
end;

procedure WriteHeader(OutStream: TStream; OldSize, NewSize: Cardinal);
var
  Buf: array[0..7] of Byte;
  S: AnsiString;
begin
  if OutStream <> nil then
  begin
    case CnDiffOutputType of
      dotBinary:
        begin
          OutStream.Write(CN_BINARY_DIFF_NAME_VER[1], Length(CN_BINARY_DIFF_NAME_VER));
          PackLong(@Buf[0], OldSize);
          PackLong(@Buf[4], NewSize);
          OutStream.Write(Buf[0], SizeOf(Buf));
        end;
      dotFiltered, dotQuoted:
        begin
          S := AnsiString(Format('%% --- Old (%d bytes)' + #13#10 + '%% +++ New (%d bytes)' + #13#10, [OldSize, NewSize]));
          OutStream.Write(S[1], Length(S));
        end;
    end;
  end;
end;

function IsPrintableChar(AChar: Byte): Boolean;
begin
  Result := AChar in [32..127];
end;

procedure WriteFilteredOrQuotedData(OutStream: TStream; Data: PByte;
  DataLen: Cardinal; IsFiltered: Boolean);
var
  S: AnsiString;
begin
  if IsFiltered then
  begin
    while DataLen > 0 do
    begin
      if IsPrintableChar(Data^) and (Chr(Data^) <> '\') then
        OutStream.Write(Data^, 1)
      else
      begin
        S := AnsiString(Format('#$%2.2x', [Data^]));
        OutStream.Write(S[1], Length(S));
      end;

      Inc(Data);
      Dec(DataLen);
    end;
  end
  else
  begin
    while DataLen > 0 do
    begin
      if IsPrintableChar(Data^) then
        OutStream.Write(Data^, 1)
      else
        OutStream.Write(DOT_CHAR, 1);
      Inc(Data);
      Dec(DataLen);
    end;
  end;
end;

procedure WriteAddContent(OutStream: TStream; Data: PByte; DataLen: Cardinal);
var
  Buf: array[0..3] of Byte;
begin
  if OutStream <> nil then
  begin
    if CnDiffOutputType = dotBinary then
    begin
      OutStream.Write(ADD_CHAR, 1);
      PackLong(@Buf[0], DataLen);
      OutStream.Write(Buf[0], SizeOf(Buf));
      OutStream.Write(Data^, DataLen);
    end
    else
    begin
      OutStream.Write(ADD_CHAR, 1);
      WriteFilteredOrQuotedData(OutStream, Data, DataLen, CnDiffOutputType = dotFiltered);
      OutStream.Write(CRLF[1], Length(CRLF));
    end;
  end;
end;

procedure WriteCopyContent(OutStream: TStream; NewBase: PByte; NewPos: Cardinal;
  OldBase: PByte; OldPos: Cardinal; DataLen: Cardinal);
var
  Buf: array[0..11] of Byte;
  S: AnsiString;
begin
  if OutStream <> nil then
  begin
    if CnDiffOutputType = dotBinary then
    begin
      OutStream.Write(COPY_CHAR, 1);
      PackLong(@Buf[0], OldPos);
      PackLong(@Buf[4], DataLen);
      PackLong(@Buf[8], CheckSum(PByte(TCnNativeUInt(NewBase) + NewPos), DataLen));
      OutStream.Write(Buf[0], SizeOf(Buf));
    end
    else
    begin
      S := AnsiString(Format('@ -[%d] => +[%d] %d bytes' + #13#10, [OldPos, NewPos, DataLen]));
      OutStream.Write(S[1], Length(S));
      WriteFilteredOrQuotedData(OutStream, PByte(TCnNativeUInt(NewBase) + NewPos), DataLen,
        CnDiffOutputType = dotFiltered);
      OutStream.Write(CRLF[1], Length(CRLF));
    end;
  end;
end;

// 二进制差分比较新旧内存块（流），差分结果放入 PatchStream 中
function BinaryDiffStream(OldStream, NewStream, PatchStream: TMemoryStream): Boolean;
var
  Sort: PInteger;
  Todo, Nofs: Cardinal;
  Match: TCnMatchRec;
begin
  Result := False;
  if (OldStream = nil) or (NewStream = nil) or (PatchStream = nil) then
    Exit;

  Sort := BlockSort(OldStream.Memory, OldStream.Size);
  if (Sort = nil) and (OldStream.Size > 0) then
    Exit;

  WriteHeader(PatchStream, OldStream.Size, NewStream.Size);

  Todo := NewStream.Size;
  Nofs := 0;
  while Todo > 0 do
  begin
    BsFindMaxMatch(@Match, OldStream.Memory, Sort, OldStream.Size,
      PByte(TCnNativeUInt(NewStream.Memory) + Nofs), Todo);

    if Match.Len <> 0 then
    begin
      WriteAddContent(PatchStream, PByte(TCnNativeUInt(NewStream.Memory) + Nofs), Match.NewPos);

      Inc(Nofs, Match.NewPos);
      Dec(Todo, Match.NewPos);

      WriteCopyContent(PatchStream, NewStream.Memory, Nofs, OldStream.Memory, Match.OldPos, Match.Len);

      Inc(Nofs, Match.Len);
      Dec(Todo, Match.Len);
    end
    else
    begin
      WriteAddContent(PatchStream, PByte(TCnNativeUInt(NewStream.Memory) + Nofs), Todo);
      Break;
    end;
  end;
  FreeMemory(Sort);
  Result := True;
end;

// 二进制差分补丁旧内存块（流），合成结果放入 NewStream 中
function BinaryPatchStream(OldStream, PatchStream, NewStream: TMemoryStream): Boolean;
var
  Buf: array[0..15] of Byte;
  SrcLen, DstLen, ASize, AnOffset: Cardinal;
  AnOperator: AnsiChar;
begin
  Result := False;
  if (OldStream = nil) or (PatchStream = nil) or (NewStream = nil) then
    Exit;

  if PatchStream.Read(Buf[0], 16) <> 16 then
    raise ECnPatchFormatError.Create(SCnErrorPatchHeader);

  if not CompareMem(@CN_BINARY_DIFF_NAME_VER[1], @Buf[0], Length(CN_BINARY_DIFF_NAME_VER)) then
    raise ECnPatchFormatError.Create(SCnErrorPatchNameVersion);

  SrcLen := GetLong(@Buf[8]);
  DstLen := GetLong(@Buf[12]);

  AnOperator := #0;
  while True do
  begin
    if PatchStream.Read(AnOperator, 1) <> 1 then // 流末尾
      Break;

    case AnOperator of
    '@':
      begin
        if PatchStream.Read(Buf[0], 12) <> 12 then
          raise ECnPatchFormatError.Create(SCnErrorPatchCopyArea);

        ASize := GetLong(@Buf[4]);
        AnOffset := GetLong(@Buf[0]);

        if (AnOffset > SrcLen) or (ASize > SrcLen) or (ASize + AnOffset > SrcLen) then
          raise ECnPatchFormatError.Create(SCnErrorPatchCopySize);
{$IFDEF MSWINDOWS}
        OldStream.Seek(AnOffset, soFromBeginning);
{$ELSE}
        OldStream.Seek(LongInt(AnOffset), soFromBeginning);
{$ENDIF}
        CopyStreamData(OldStream, NewStream, ASize, GetLong(@Buf[8]), False);
        Dec(DstLen, ASize);
      end;
    '+':
      begin
        if PatchStream.Read(Buf[0], 4) <> 4 then
          raise ECnPatchFormatError.Create(SCnErrorPatchAddArea);
        ASize := GetLong(@Buf[0]);

        CopyStreamData(PatchStream, NewStream, ASize, 0, True);
        Dec(DstLen, ASize);
      end;
    else
      raise ECnPatchFormatError.CreateFmt(SCnErrorPatchOperatorFmt, [AnOperator]);
    end;
  end;

  if DstLen <> 0 then
    raise ECnPatchFormatError.Create(SCnErrorPatchLength);

  Result := True;
end;

// 二进制差分比较新旧文件，差分结果存入 PatchFile 中
function BinaryDiffFile(const OldFile, NewFile, PatchFile: string): Boolean;
var
  OldStream, NewStream, PatchStream: TMemoryStream;
begin
  OldStream := nil;
  NewStream := nil;
  PatchStream := nil;

  try
    OldStream := TMemoryStream.Create;
    if OldFile <> '' then // 旧文件不存在也行
      OldStream.LoadFromFile(OldFile);
    NewStream := TMemoryStream.Create;
    if NewFile <> '' then // 新文件不存在也行，表示删除
      NewStream.LoadFromFile(NewFile);

    PatchStream := TMemoryStream.Create;
    Result := BinaryDiffStream(OldStream, NewStream, PatchStream);
    PatchStream.SaveToFile(PatchFile);
  finally
    PatchStream.Free;
    NewStream.Free;
    OldStream.Free;
  end;
end;

// 二进制差分补丁旧文件，合成结果存入 NewFile 中
function BinaryPatchFile(const OldFile, PatchFile, NewFile: string): Boolean;
var
  OldStream, NewStream, PatchStream: TMemoryStream;
begin
  OldStream := nil;
  PatchStream := nil;
  NewStream := nil;

  try
    OldStream := TMemoryStream.Create;
    if OldFile <> '' then // 旧文件不存在也行
      OldStream.LoadFromFile(OldFile);
    PatchStream := TMemoryStream.Create;
    PatchStream.LoadFromFile(PatchFile);

    NewStream := TMemoryStream.Create;
    Result := BinaryPatchStream(OldStream, PatchStream, NewStream);
    if NewStream.Size > 0 then // 新文件如果无内容则不存
      NewStream.SaveToFile(NewFile)
    else
      DeleteFile(PChar(NewFile));
  finally
    NewStream.Free;
    PatchStream.Free;
    OldStream.Free;
  end;
end;

// 二进制差分比较新旧目录，在 PatchDir 中输出形如“旧文件名.patch”的二进制补丁，
// 如无旧文件，则以新文件.patch 为补丁名，不支持子目录
function BinaryDiffDirectory(const OldDir, NewDir, PatchDir: string): Boolean;
var
  OldFiles, NewFiles: TStrings;
  OldFile: string;
  I, Idx: Integer;
begin
  Result := False;
  if not DirectoryExists(OldDir) or not DirectoryExists(NewDir) then
    Exit;

  ForceDirectories(PatchDir);
  OldFiles := nil;
  NewFiles := nil;
  try
    OldFiles := TStringList.Create;
    NewFiles := TStringList.Create;

    GetDirFiles(OldDir, OldFiles);
    GetDirFiles(NewDir, NewFiles);

    for I := 0 to OldFiles.Count - 1 do
    begin
      OldFile := OldFiles[I];
      Idx := NewFiles.IndexOf(OldFile);
      if Idx >= 0 then // 新旧文件都存在
      begin
        if not BinaryDiffFile(AddDirSuffix(OldDir) + OldFile, AddDirSuffix(NewDir) + OldFile,
          AddDirSuffix(PatchDir) + OldFile + PATCH_SUFFIX) then
          Exit;
        NewFiles.Delete(Idx);
      end
      else // 新文件不存在
      begin
        if not BinaryDiffFile(AddDirSuffix(OldDir) + OldFile, '',
          AddDirSuffix(PatchDir) + OldFile + PATCH_SUFFIX) then
          Exit;
      end;
    end;

    for I := 0 to NewFiles.Count - 1 do
    begin
      // 旧文件不存在
      if not BinaryDiffFile('', AddDirSuffix(NewDir) + NewFiles[I],
        AddDirSuffix(PatchDir) + NewFiles[I] + PATCH_SUFFIX) then
        Exit;
    end;
    Result := True;
  finally
    OldFiles.Free;
    NewFiles.Free;
  end;
end;

// 二进制差分补丁旧目录，并把新内容输出至新目录
function BinaryPatchDirectory(const OldDir, PatchDir, NewDir: string): Boolean;
var
  OldFiles, PatchFiles: TStrings;
  PatchFile, FileName, OldFile: string;
  I: Integer;
begin
  Result := False;
  if not DirectoryExists(OldDir) or not DirectoryExists(PatchDir) then
    Exit;

  ForceDirectories(NewDir);
  OldFiles := nil;
  PatchFiles := nil;
  try
    OldFiles := TStringList.Create;
    PatchFiles := TStringList.Create;

    GetDirFiles(OldDir, OldFiles);
    GetDirFiles(PatchDir, PatchFiles);

    for I := 0 to PatchFiles.Count - 1 do
    begin
      PatchFile := PatchFiles[I];
      if StrRight(PatchFile, Length(PATCH_SUFFIX)) <> PATCH_SUFFIX then
        Continue;  // .patch 结尾的才认为是补丁

      FileName := PatchFile;
      Delete(FileName, Pos(PATCH_SUFFIX, FileName), MaxInt);

      OldFile := AddDirSuffix(OldDir) + FileName;
      if not FileExists(OldFile) then // 允许源文件不存在
        OldFile := '';

      if not BinaryPatchFile(OldFile, AddDirSuffix(PatchDir) + PatchFile,
        AddDirSuffix(NewDir) + FileName) then
        Exit;
    end;
    Result := True;
  finally
    OldFiles.Free;
    PatchFiles.Free;
  end;

end;

end.
