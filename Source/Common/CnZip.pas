{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnZip;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：CnPack 组件包 Zip 实现单元
* 单元作者：CnPack开发组 Liu Xiao
* 备    注：只实现存储方式，未实现压缩
* 开发平台：PWinXP + Delphi 5
* 兼容测试：PWinXP/7 + Delphi 5 ~ XE
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2018.08.07 V1.0
*                使用 ZLib 实现加压解压但用 Zip 解压时仍有问题
*           2018.08.05 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Contnrs, FileCtrl, CnCommon, CnCRC32, ZLib;

const
  SIGNATURE_ZIPENDOFHEADER: LongWord = $06054B50;
  SIGNATURE_CENTRALHEADER:  LongWord = $02014B50;
  SIGNATURE_LOCALHEADER:    LongWord = $04034B50;

  LOCALHEADERSIZE = 26;
  CENTRALHEADERSIZE = 42;

type
  ECnZipException = class(Exception);

  TCnZipCompressionMethod = (
    zcStored,
    zcShrunk,
    zcReduce1,
    zcReduce2,
    zcReduce3,
    zcReduce4,
    zcImplode,
    zcTokenize,
    zcDeflate,
    zcDeflate64,
    zcPKImplode,
    zcReserved11,
    zcBZIP2,
    zcReserved13,
    zcLZMA,
    zcReserved15,
    zcReserved16,
    zcReserved17,
    zcTERSE,
    zcLZ77
//    zcWavePack = 97,
//    zcPPMdI1
  );

  TCnZipHeader = packed record
    MadeByVersion:      Word;     // Start of Central Header
    RequiredVersion:    Word;     // Start of Local Header
    Flag:               Word;
    CompressionMethod:  Word;
    ModifiedDateTime:   LongWord;
    CRC32:              LongWord;
    CompressedSize:     LongWord;
    UncompressedSize:   LongWord;
    FileNameLength:     Word;
    ExtraFieldLength:   Word;     // End of Local Header
    FileCommentLength:  Word;
    DiskNumberStart:    Word;
    InternalAttributes: Word;
    ExternalAttributes: LongWord;
    LocalHeaderOffset:  LongWord; // End of Central Header
    FileName:           AnsiString;
    ExtraField:         AnsiString;
    FileComment:        AnsiString;
  end;
  PCnZipHeader = ^TCnZipHeader;

  TCnZipEndOfCentralHeader = packed record
    DiskNumber:          Word;
    CentralDirStartDisk: Word;
    NumEntriesThisDisk:  Word;
    CentralDirEntries:   Word;
    CentralDirSize:      LongWord;
    CentralDirOffset:    LongWord;
    CommentLength:       Word;
    {Comment: RawByteString}
  end;
  PCnZipEndOfCentralHeader = ^TCnZipEndOfCentralHeader;

  TCnZipAbstractCompressionHandler = class(TObject)
  {* 压缩类型的实现基类}
  private

  public
    class function CanHandleCompressionMethod(AMethod: TCnZipCompressionMethod): Boolean; virtual; abstract;
    class function CreateCompressionStream(AMethod: TCnZipCompressionMethod;
      InStream: TStream; const Item: PCnZipHeader): TStream; virtual; abstract;
    class function CreateDecompressionStream(AMethod: TCnZipCompressionMethod;
      InStream: TStream; const Item: PCnZipHeader): TStream; virtual; abstract;
  end;

  TCnZipCompressionHandlerClass = class of TCnZipAbstractCompressionHandler;

  TCnZipBase = class(TObject)
  {* Zip 工具类基类}
  private
    FUtf8: Boolean;
    FFileList: TList;
    FComment: AnsiString;
    procedure SetUtf8(const Value: Boolean);
    function GetComment: string;
    function GetFileComment(Index: Integer): string;
    function GetFileCount: Integer;
    function GetFileInfo(Index: Integer): PCnZipHeader;
    function GetFileName(Index: Integer): string;
    procedure SetComment(const Value: string);
    procedure SetFileComment(Index: Integer; const Value: string);
  protected
    FStartFileData: Int64;
    FEndFileData: Int64;
    procedure ClearFiles;
    function RawToString(Raw: AnsiString): string;
    function StringToRaw(Str: string): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(const FileName: string): Integer;

    property FileCount: Integer read GetFileCount;
    {* 该 Zip 文件包含的文件个数}
    property FileName[Index: Integer]: string read GetFileName;
    {* 该 Zip 文件包含的文件名}
    property FileInfo[Index: Integer]: PCnZipHeader read GetFileInfo;
    {* 该 Zip 文件包含的文件信息，从中央目录读出的}
    property FileComment[Index: Integer]: string read GetFileComment write SetFileComment;
    {* 该 Zip 文件包含的文件注释}
    property Comment: string read GetComment write SetComment;
    {* 该 Zip 文件包含的注释}
    property Utf8: Boolean read FUtf8 write SetUtf8;
    {* 该 Zip 文件是否支持 Utf8}
  end;

  TCnZipReader = class(TCnZipBase)
  {* 用来打开 Zip 文件可解压的工具类}
  private
    FInStream: TStream;
    procedure OpenZipStream;
    procedure ReadCentralHeader;
    function PrepareStream(Index: Integer; LocalHeader: PCnZipHeader): TStream;
  protected
    function SearchEndOfCentralHeader(Stream: TStream;
      Header: PCnZipEndOfCentralHeader): Boolean;
  public
    destructor Destroy; override;

    procedure OpenZipFile(const ZipFileName: string);
    {* 打开一个 Zip 文件}
    procedure ExtractAllTo(const Path: string);
    {* 将打开的 Zip 文件全部解压至指定目录}
    procedure ExtractTo(Index: Integer; const Path: string; CreateSubdirs: Boolean = True);
    {* 解压指定序号的单个文件至指定目录}
    procedure ExtractByFileName(const FileName: string; const Path: string; CreateSubdirs: Boolean = True);
    {* 解压指定文件至指定目录}
    procedure Close;
    {* 关闭该 Zip 文件}
  end;

  TCnZipWriter = class(TCnZipBase)
  {* 用来生成 Zip 文件的工具类}
  private
    FOutStream: TStream;
    FRemovePath: Boolean;
    FDirFiles: TStrings;
    procedure FindFileCallback(const FileName: string; const Info: TSearchRec;
      var Abort: Boolean);
  protected
    procedure AddStream(Data: TStream; LocalHeader: PCnZipHeader);
  public
    destructor Destroy; override;

    procedure CreateZipFile(const ZipFileName: string);
    {* 创建一个空白的 Zip 文件}
    procedure AddFile(const FileName: string; const ArchiveFileName: string = '';
      Compression: TCnZipCompressionMethod = zcDeflate);
    {* 向 Zip 文件中添加指定内容}
    procedure AddDirectory(const DirName: string; Compression: TCnZipCompressionMethod = zcDeflate);
    {* 向 Zip 文件中添加指定目录下的所有文件}
    procedure Save;
    {* 将压缩内容保存至 Zip 文件}
    procedure Close;

    property RemovePath: Boolean read FRemovePath write FRemovePath;
    {* 是否去除每个文件的路径信息只留文件名信息}
  end;

procedure RegisterCompressionHandlerClass(AClass: TCnZipCompressionHandlerClass);
{* 供外界提供对新的压缩方式的支持}

function CnZipFileIsValid(const FileName: string): Boolean;
{* 判断 Zip 文件是否合法}

function CnZipDirectory(const DirName: string; const FileName: string;
  Compression: TCnZipCompressionMethod = zcDeflate): Boolean;
{* 将指定目录压缩为一个 Zip 文件}

function CnZipExtractTo(const FileName: string; const DirName: string): Boolean;
{* 将指定 Zip 文件解压缩到指定目录}

implementation

resourcestring
  SZipErrorRead = 'Error Reading Zip File';
  SZipErrorWrite = 'Error Writing Zip File';
  SZipInvalidLocalHeader   = 'Invalid Zip Local Header';
  SZipInvalidCentralHeader = 'Invalid Zip Central Header';
  SFileNotFound = 'Error Finding File';
  SZipNotSupport = 'Zip Compression Method NOT Support';

var
  FZipCompressionHandlers: TClassList = nil;

type
  TCnZipDefaultCompressionHandler = class(TCnZipAbstractCompressionHandler)
  private

  public
    class function CanHandleCompressionMethod(AMethod: TCnZipCompressionMethod): Boolean; override;
    class function CreateCompressionStream(AMethod: TCnZipCompressionMethod;
      InStream: TStream; const Item: PCnZipHeader): TStream; override;
    class function CreateDecompressionStream(AMethod: TCnZipCompressionMethod;
      InStream: TStream; const Item: PCnZipHeader): TStream; override;
  end;

  TCnStoredStream = class(TStream)
  private
    FStream: TStream;
    FPos: Int64;
  protected
    function GetSize: Int64; // override;
  public
    constructor Create(Stream: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

procedure RegisterCompressionHandlerClass(AClass: TCnZipCompressionHandlerClass);
begin
  if FZipCompressionHandlers.IndexOf(AClass) < 0 then
    FZipCompressionHandlers.Add(AClass);
end;

// 是否支持指定的压缩方式
function SupportCompressionMethod(AMethod: TCnZipCompressionMethod): Boolean;
var
  I: Integer;
  AComp: TCnZipCompressionHandlerClass;
begin
  Result := False;
  for I := 0 to FZipCompressionHandlers.Count - 1 do
  begin
    AComp := TCnZipCompressionHandlerClass(FZipCompressionHandlers[I]);
    if AComp <> nil then
    begin
      if AComp.CanHandleCompressionMethod(AMethod) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function CreateCompressStreamFromHandler(AMethod: TCnZipCompressionMethod;
  InStream: TStream; const Item: PCnZipHeader): TStream;
var
  I: Integer;
  AComp: TCnZipCompressionHandlerClass;
begin
  Result := nil;
  for I := 0 to FZipCompressionHandlers.Count - 1 do
  begin
    AComp := TCnZipCompressionHandlerClass(FZipCompressionHandlers[I]);
    if AComp <> nil then
    begin
      if AComp.CanHandleCompressionMethod(AMethod) then
      begin
        Result := AComp.CreateCompressionStream(AMethod, InStream, Item);
        Exit;
      end;
    end;
  end;
end;

function CreateDecompressStreamFromHandler(AMethod: TCnZipCompressionMethod;
  InStream: TStream; const Item: PCnZipHeader): TStream;
var
  I: Integer;
  AComp: TCnZipCompressionHandlerClass;
begin
  Result := nil;
  for I := 0 to FZipCompressionHandlers.Count - 1 do
  begin
    AComp := TCnZipCompressionHandlerClass(FZipCompressionHandlers[I]);
    if AComp <> nil then
    begin
      if AComp.CanHandleCompressionMethod(AMethod) then
      begin
        Result := AComp.CreateDecompressionStream(AMethod, InStream, Item);
        Exit;
      end;
    end;
  end;
end;

function CnZipFileIsValid(const FileName: string): Boolean;
var
  Z: TCnZipReader;
  Stream: TStream;
  Header: TCnZipEndOfCentralHeader;
begin
  Result := False;
  try
    Stream := nil;
    Z := nil;
    try
      Z := TCnZipReader.Create;
      Stream := TFileStream.Create(FileName, fmOpenRead);
      Result := Z.SearchEndOfCentralHeader(Stream, @Header);
    finally
      Stream.Free;
      Z.Free;
    end;
  except on E: EStreamError do
    ;
  end;
end;

function CnZipDirectory(const DirName: string; const FileName: string;
  Compression: TCnZipCompressionMethod): Boolean;
var
  Zip: TCnZipWriter;
begin
  Result := False;
  if not DirectoryExists(DirName) then
    Exit;

  Zip := TCnZipWriter.Create;

  try
    Zip.CreateZipFile(FileName);
    Zip.AddDirectory(DirName);
    Zip.Save;
    Result := True;
  finally
    Zip.Free;
  end;
end;

function CnZipExtractTo(const FileName: string; const DirName: string): Boolean;
var
  Zip: TCnZipReader;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;

  Zip := TCnZipReader.Create;
  try
    Zip.OpenZipFile(FileName);
    Zip.ExtractAllTo(DirName);
    Result := True;
  finally
    Zip.Free;
  end;
end;

procedure VerifyRead(Stream: TStream; var Buffer; Count: Integer);
begin
  if Stream.Read(Buffer, Count) <> Count then
    raise ECnZipException.CreateRes(@SZipErrorRead);
end;

procedure VerifyWrite(Stream: TStream; var Buffer; Count: Integer);
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise ECnZipException.CreateRes(@SZipErrorWrite);
end;

{ TCnZipBase }

procedure TCnZipBase.ClearFiles;
var
  I: Integer;
begin
  for I := FFileList.Count - 1 downto 0 do
    Dispose(FFileList[I]);
  FFileList.Clear;
end;

constructor TCnZipBase.Create;
begin
  inherited;
  FFileList := TList.Create;
  FUtf8 := True;
end;

destructor TCnZipBase.Destroy;
begin
  ClearFiles;
  FFileList.Free;
  inherited;
end;

function TCnZipBase.GetComment: string;
begin
  Result := RawToString(FComment);
end;

function TCnZipBase.GetFileComment(Index: Integer): string;
begin
  Result := RawToString(FileInfo[Index]^.FileComment);
end;

function TCnZipBase.GetFileCount: Integer;
begin
  Result := FFileList.Count;
end;

function TCnZipBase.GetFileInfo(Index: Integer): PCnZipHeader;
begin
  Result := PCnZipHeader(FFileList[Index]);
end;

function TCnZipBase.GetFileName(Index: Integer): string;
begin
  Result := RawToString(FileInfo[Index]^.FileName);
end;

function TCnZipBase.IndexOf(const FileName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FFileList.Count - 1 do
  begin
    if SameText(RawToString(FileInfo[I].FileName), FileName) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TCnZipBase.RawToString(Raw: AnsiString): string;
begin
  if FUtf8 then
    Result := CnUtf8ToAnsi2(string(Raw))
  else
    Result := string(Raw);
end;

procedure TCnZipBase.SetComment(const Value: string);
begin
  FComment := StringToRaw(Value);
end;

procedure TCnZipBase.SetFileComment(Index: Integer; const Value: string);
begin
  FileInfo[Index]^.FileComment := StringToRaw(Value);
end;

procedure TCnZipBase.SetUtf8(const Value: Boolean);
begin
  FUtf8 := Value;
end;

function TCnZipBase.StringToRaw(Str: string): AnsiString;
begin
  if FUtf8 then
    Result := CnAnsiToUtf82(Str)
  else
    Result := AnsiString(Str);
end;

{ TCnZipReader }

procedure TCnZipReader.Close;
begin
  ClearFiles;
  FreeAndNil(FInStream);
end;

function TCnZipReader.PrepareStream(Index: Integer;
  LocalHeader: PCnZipHeader): TStream;
var
  Sig: LongWord;
begin
  if (Index < 0) or (Index > FileCount) then
    raise ECnZipException.CreateRes(@SFileNotFound);

  LocalHeader^.MadeByVersion := 0;
  SetLength(LocalHeader^.FileComment, 0);
  LocalHeader^.FileCommentLength  := 0;
  LocalHeader^.DiskNumberStart    := 0;
  LocalHeader^.InternalAttributes := 0;
  LocalHeader^.ExternalAttributes := 0;
  LocalHeader^.LocalHeaderOffset  := 0;

  FInStream.Position := FileInfo[Index].LocalHeaderOffset + FStartFileData;
  FInStream.Read(Sig, Sizeof(Sig));
  if Sig <> SIGNATURE_LOCALHEADER then
    raise ECnZipException.CreateRes(@SZipInvalidLocalHeader);

  FInStream.Read(LocalHeader^.RequiredVersion,    Sizeof(Word));
  FInStream.Read(LocalHeader^.Flag,               Sizeof(Word));
  FInStream.Read(LocalHeader^.CompressionMethod,  Sizeof(Word));
  FInStream.Read(LocalHeader^.ModifiedDateTime,   Sizeof(LongWord));
  FInStream.Read(LocalHeader^.CRC32,              Sizeof(LongWord));
  FInStream.Read(LocalHeader^.CompressedSize,     Sizeof(LongWord));
  FInStream.Read(LocalHeader^.UncompressedSize,   Sizeof(LongWord));
  FInStream.Read(LocalHeader^.FileNameLength,     Sizeof(Word));
  FInStream.Read(LocalHeader^.ExtraFieldLength,   Sizeof(Word));

  SetLength(LocalHeader^.FileName, LocalHeader^.FileNameLength);
  FInStream.Read(LocalHeader^.FileName[1], LocalHeader^.FileNameLength);
  if LocalHeader^.ExtraFieldLength > 0 then
  begin
    SetLength(LocalHeader^.ExtraField, LocalHeader^.ExtraFieldLength);
    FInStream.Read(LocalHeader^.ExtraField[1], LocalHeader^.ExtraFieldLength);
  end;

  Result := CreateDecompressStreamFromHandler(TCnZipCompressionMethod(LocalHeader^.CompressionMethod),
    FInStream, LocalHeader);
end;

destructor TCnZipReader.Destroy;
begin
  FreeAndNil(FInStream);
  inherited;
end;

procedure TCnZipReader.ExtractAllTo(const Path: string);
var
  I: Integer;
begin
  for I := 0 to FFileList.Count - 1 do
    ExtractTo(I, Path);
end;

procedure TCnZipReader.ExtractByFileName(const FileName, Path: string;
  CreateSubdirs: Boolean);
begin
  ExtractTo(IndexOf(FileName), Path, CreateSubdirs);
end;

procedure TCnZipReader.ExtractTo(Index: Integer; const Path: string;
  CreateSubdirs: Boolean);
var
  CompressionStream, OutStream: TStream;
  LocalHeader: TCnZipHeader;
  Dir, AFileName: string;
begin
  CompressionStream := PrepareStream(Index, @LocalHeader);
  if CompressionStream = nil then
    raise ECnZipException.CreateRes(@SZipNotSupport);

  try
    AFileName := RawToString(FileInfo[Index].FileName);
    if AFileName = '' then
      Exit;

{$IFDEF MSWINDOWS}
    AFileName := StringReplace(AFileName, '/', '\', [rfReplaceAll]);
{$ENDIF}

    if CreateSubdirs then
      AFileName := MakePath(Path) + AFileName
    else
      AFileName := MakePath(Path) + ExtractFileName(AFileName);

    Dir := ExtractFileDir(AFileName);
    if CreateSubdirs and (Dir <> '') then
      ForceDirectories(Dir);

    if AFileName[Length(AFileName) - 1] in ['\', '/'] then
      Exit;

    OutStream := TFileStream.Create(AFileName, fmCreate);
    try
      if (LocalHeader.Flag and (1 shl 3)) = 0 then
      begin
        if FileInfo[Index].UncompressedSize > 0 then
          OutStream.CopyFrom(CompressionStream, FileInfo[Index].UncompressedSize);
      end
      else
      begin
        OutStream.CopyFrom(CompressionStream, FileInfo[Index].UncompressedSize);
      end;
    finally
      OutStream.Free;
    end;
  finally
    CompressionStream.Free;
  end;
end;

procedure TCnZipReader.OpenZipFile(const ZipFileName: string);
begin
  Close;

  FInStream := TFileStream.Create(ZipFileName, fmOpenRead);
  try
    OpenZipStream;
  except
    FreeAndNil(FInStream);
    raise;
  end;
end;

procedure TCnZipReader.OpenZipStream;
begin
  FStartFileData := FInStream.Position;
  ReadCentralHeader;
end;

procedure TCnZipReader.ReadCentralHeader;
var
  I: Integer;
  Signature: LongWord;
  EndHeader: TCnZipEndOfCentralHeader;
  Header: PCnZipHeader;
begin
  ClearFiles;
  if FInStream.Size = 0 then
    Exit;

  if not SearchEndOfCentralHeader(FInStream, @EndHeader) then
    raise ECnZipException.CreateRes(@SZipErrorRead);

  FInStream.Position := EndHeader.CentralDirOffset;
  FEndFileData := EndHeader.CentralDirOffset;

  for I := 0 to EndHeader.CentralDirEntries - 1 do
  begin
    FInStream.Read(Signature, Sizeof(Signature));
    if Signature <> SIGNATURE_CENTRALHEADER then
      raise ECnZipException.CreateRes(@SZipInvalidCentralHeader);

    New(Header);
    try
      VerifyRead(FInStream, Header^.MadeByVersion,      Sizeof(Word));
      VerifyRead(FInStream, Header^.RequiredVersion,    Sizeof(Word));
      VerifyRead(FInStream, Header^.Flag,               Sizeof(Word));
      VerifyRead(FInStream, Header^.CompressionMethod,  Sizeof(Word));
      VerifyRead(FInStream, Header^.ModifiedDateTime,   Sizeof(LongWord));
      VerifyRead(FInStream, Header^.CRC32,              Sizeof(LongWord));
      VerifyRead(FInStream, Header^.CompressedSize,     Sizeof(LongWord));
      VerifyRead(FInStream, Header^.UncompressedSize,   Sizeof(LongWord));
      VerifyRead(FInStream, Header^.FileNameLength,     Sizeof(Word));
      VerifyRead(FInStream, Header^.ExtraFieldLength,   Sizeof(Word));
      VerifyRead(FInStream, Header^.FileCommentLength,  Sizeof(Word));
      VerifyRead(FInStream, Header^.DiskNumberStart,    Sizeof(Word));
      VerifyRead(FInStream, Header^.InternalAttributes, Sizeof(Word));
      VerifyRead(FInStream, Header^.ExternalAttributes, Sizeof(LongWord));
      VerifyRead(FInStream, Header^.LocalHeaderOffset,  Sizeof(LongWord));

      if Header^.FileNameLength > 0 then
      begin
        SetLength(Header^.FileName, Header^.FileNameLength);
        VerifyRead(FInStream, Header^.FileName[1], Header^.FileNameLength);
      end;
      if Header^.ExtraFieldLength > 0 then
      begin
        SetLength(Header^.ExtraField, Header^.ExtraFieldLength);
        VerifyRead(FInStream, Header^.ExtraField[1], Header^.ExtraFieldLength);
      end;
      if Header^.FileCommentLength > 0 then
      begin
        SetLength(Header^.FileComment, Header^.FileCommentLength);
        VerifyRead(FInStream, Header^.FileComment[1], Header^.FileCommentLength);
      end;

      if (Header^.Flag and (1 shl 11)) = 0 then
        FUtf8 := False;
    except
      Dispose(Header);
    end;
    FFileList.Add(Header);
  end;
end;

function TCnZipReader.SearchEndOfCentralHeader(Stream: TStream;
  Header: PCnZipEndOfCentralHeader): Boolean;
var
  I: Integer;
  BackRead, ReadSize, MaxBack: Longint;
  BackBuf: array of Byte;
begin
  if Stream.Size < $FFFF then
    MaxBack := Stream.Size
  else
    MaxBack := $FFFF;

  BackRead := 4;
  SetLength(BackBuf, $404 - 1);
  while BackRead < MaxBack do
  begin
    if BackRead + Longint(Length(BackBuf) - 4) > MaxBack then
      BackRead := MaxBack
    else
      Inc(BackRead, Length(BackBuf) - 4);

    Stream.Position := Stream.Size - BackRead;
    if Length(BackBuf) < (Stream.Size - Stream.Position) then
      ReadSize := Length(BackBuf)
    else
      ReadSize := Stream.Size - Stream.Position;

    VerifyRead(Stream, BackBuf[0], ReadSize);
    for I := ReadSize - 4 downto 0 do
    begin
      if (BackBuf[I]     = ((SIGNATURE_ZIPENDOFHEADER       ) and $FF)) and
         (BackBuf[I + 1] = ((SIGNATURE_ZIPENDOFHEADER shr  8) and $FF)) and
         (BackBuf[I + 2] = ((SIGNATURE_ZIPENDOFHEADER shr 16) and $FF)) and
         (BackBuf[I + 3] = ((SIGNATURE_ZIPENDOFHEADER shr 24) and $FF)) then
      begin
        Move(BackBuf[I + 4], Header^, SizeOf(Header^));
        if Header^.CommentLength > 0 then
        begin
          Stream.Position := Stream.Size - BackRead + I + 4 + SizeOf(Header^);
          SetLength(FComment, Header^.CommentLength);
          Stream.Read(FComment[1], Header^.CommentLength);
        end
        else
          SetLength(FComment, 0);

        Result := True;
        Exit;
      end;
    end;
  end;
  Result := False;
end;

{ TCnZipDefaultCompressionHandler }

class function TCnZipDefaultCompressionHandler.CanHandleCompressionMethod(
  AMethod: TCnZipCompressionMethod): Boolean;
begin
  Result := AMethod in [zcStored, zcDeflate];
end;

class function TCnZipDefaultCompressionHandler.CreateCompressionStream(
  AMethod: TCnZipCompressionMethod; InStream: TStream; const Item: PCnZipHeader): TStream;
begin
  Result := nil;
  if AMethod = zcStored then
    Result := TCnStoredStream.Create(InStream)
  else if AMethod = zcDeflate then
    Result := TCompressionStream.Create(clDefault, InStream);
end;

class function TCnZipDefaultCompressionHandler.CreateDecompressionStream(
  AMethod: TCnZipCompressionMethod; InStream: TStream; const Item: PCnZipHeader): TStream;
begin
  Result := nil;
  if AMethod = zcStored then
    Result := TCnStoredStream.Create(InStream)
  else if AMethod = zcDeflate then
    Result := TDecompressionStream.Create(InStream);
end;

{ TCnStoredStream }

constructor TCnStoredStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FPos := FStream.Position;
end;

function TCnStoredStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TCnStoredStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TCnStoredStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := FStream.Seek(Offset, Origin);
end;

function TCnStoredStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

{ TCnZipWriter }

procedure TCnZipWriter.AddDirectory(const DirName: string;
  Compression: TCnZipCompressionMethod);
var
  I: Integer;
  Path, AFile: string;
begin
  if FDirFiles = nil then
    FDirFiles := TStringList.Create
  else
    FDirFiles.Free;

  FindFile(DirName, '*.*', FindFileCallback);

  for I := 0 to FDirFiles.Count - 1 do
  begin
    Path := MakePath(DirName);
{$IFDEF MSWINDOWS}
    AFile := StringReplace(Copy(FDirFiles[I], Length(Path) + 1, Length(FDirFiles[I])), '\', '/', [rfReplaceAll]);
{$ELSE}
    AFile := Copy(FDirFiles[I], Length(Path) + 1, Length(FDirFiles[I]));
{$ENDIF}
    AddFile(FDirFiles[I], AFile, Compression);
  end;
end;

procedure TCnZipWriter.AddFile(const FileName, ArchiveFileName: string;
  Compression: TCnZipCompressionMethod);
var
  InStream: TStream;
  LocalHeader: PCnZipHeader;
  Archive: string;
begin
  if Trim(FileName) = '' then
    Exit;

  if not SupportCompressionMethod(Compression) then
    raise ECnZipException.CreateRes(@SZipNotSupport);

  New(LocalHeader);
  FillChar(LocalHeader^, SizeOf(LocalHeader^), 0);
  LocalHeader^.Flag := 0;
  InStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LocalHeader^.Flag := 0;
    LocalHeader^.CompressionMethod := Word(Compression);
    LocalHeader^.ModifiedDateTime := DateTimeToFileDate(GetFileDateTime(FileName) );
    LocalHeader^.UncompressedSize := InStream.Size;
    LocalHeader^.InternalAttributes := 0;
    LocalHeader^.ExternalAttributes := 0;
    if ArchiveFileName <> '' then
      Archive := ArchiveFileName
    else if FRemovePath then
      Archive := ExtractFileName(FileName)
    else
      Archive := FileName;

    if FUtf8 then
      LocalHeader^.Flag := LocalHeader^.Flag or (1 shl 11);
    LocalHeader^.FileName := StringToRaw(Archive);
    LocalHeader^.FileNameLength := Length(LocalHeader^.FileName);

    LocalHeader^.ExtraFieldLength := 0;
    AddStream(InStream, LocalHeader);
  finally
    InStream.Free;
  end;
end;

procedure TCnZipWriter.AddStream(Data: TStream; LocalHeader: PCnZipHeader);
var
  DataStart: Int64;
  CompressStream: TStream;
  Signature: LongWord;
  LStartPos: Int64;
  C: Integer;
  Buffer: array of Byte;
begin
  FOutStream.Position := FEndFileData;
  LocalHeader^.LocalHeaderOffset := FEndFileData;

  if LocalHeader^.MadeByVersion < 20 then
    LocalHeader^.MadeByVersion := 20;
  if LocalHeader^.RequiredVersion < 20 then
    LocalHeader^.RequiredVersion := 20;

  LocalHeader^.FileNameLength   := Length(LocalHeader^.FileName);
  LocalHeader^.ExtraFieldLength := Length(LocalHeader^.ExtraField);

  Signature := SIGNATURE_LOCALHEADER;
  VerifyWrite(FOutStream, Signature, SizeOf(Signature));

  VerifyWrite(FOutStream, LocalHeader^.RequiredVersion,    Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.Flag,               Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.CompressionMethod,  Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ModifiedDateTime,   Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.CRC32,              Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.CompressedSize,     Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.UncompressedSize,   Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.FileNameLength,     Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ExtraFieldLength,   Sizeof(Word));

  VerifyWrite(FOutStream, LocalHeader^.FileName, LocalHeader^.FileNameLength);
  if LocalHeader^.ExtraFieldLength > 0 then
    VerifyWrite(FOutStream, LocalHeader^.ExtraField, LocalHeader^.ExtraFieldLength);

  LStartPos := FOutStream.Position;
  DataStart := Data.Position;
  LocalHeader^.UncompressedSize := Data.Size - DataStart;

  CompressStream := CreateCompressStreamFromHandler(TCnZipCompressionMethod(LocalHeader^.CompressionMethod), FOutStream, LocalHeader);
  try
    CompressStream.CopyFrom(Data, LocalHeader^.UncompressedSize);
  finally
    CompressStream.Free;
  end;

  LocalHeader^.CompressedSize := FOutStream.Position - LStartPos;
  Data.Position := DataStart;
  SetLength(Buffer, $4000);

  while Data.Position < Longint(LocalHeader^.UncompressedSize) do
  begin
    C := Data.Read(Buffer[0], Length(Buffer));
    LocalHeader^.CRC32 := CRC32Calc(LocalHeader^.CRC32, Buffer[0], C);
  end;

  FEndFileData := FOutStream.Position;
  FOutStream.Position := LocalHeader^.LocalHeaderOffset + SizeOf(LongWord);
  VerifyWrite(FOutStream, LocalHeader^.RequiredVersion,    Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.Flag,               Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.CompressionMethod,  Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ModifiedDateTime,   Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.CRC32,              Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.CompressedSize,     Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.UncompressedSize,   Sizeof(LongWord));
  VerifyWrite(FOutStream, LocalHeader^.FileNameLength,     Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ExtraFieldLength,   Sizeof(Word));

  FFileList.Add(LocalHeader);
end;

procedure TCnZipWriter.Close;
begin
  ClearFiles;
  FreeAndNil(FOutStream);
end;

procedure TCnZipWriter.CreateZipFile(const ZipFileName: string);
begin
  Close;

  FOutStream := TFileStream.Create(ZipFileName, fmCreate);
  FStartFileData := FOutStream.Position;
end;

destructor TCnZipWriter.Destroy;
begin
  FreeAndNil(FOutStream);
  FDirFiles.Free;
  inherited;
end;

procedure TCnZipWriter.FindFileCallback(const FileName: string;
  const Info: TSearchRec; var Abort: Boolean);
begin
  if (FileName <> '.') and (FileName <> '..') then
    FDirFiles.Add(FileName);
end;

procedure TCnZipWriter.Save;
var
  Header: PCnZipHeader;
  EndOfHeader: TCnZipEndOfCentralHeader;
  I: Integer;
  Sig: LongWord;
begin
  FOutStream.Position := FEndFileData;
  Sig := SIGNATURE_CENTRALHEADER;

  for I := 0 to FileCount - 1 do
  begin
    Header := FileInfo[I];
    VerifyWrite(FOutStream, Sig, SizeOf(Sig));
    VerifyWrite(FOutStream, Header^.MadeByVersion,      Sizeof(Word));
    VerifyWrite(FOutStream, Header^.RequiredVersion,    Sizeof(Word));
    VerifyWrite(FOutStream, Header^.Flag,               Sizeof(Word));
    VerifyWrite(FOutStream, Header^.CompressionMethod,  Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ModifiedDateTime,   Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.CRC32,              Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.CompressedSize,     Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.UncompressedSize,   Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.FileNameLength,     Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ExtraFieldLength,   Sizeof(Word));
    VerifyWrite(FOutStream, Header^.FileCommentLength,  Sizeof(Word));
    VerifyWrite(FOutStream, Header^.DiskNumberStart,    Sizeof(Word));
    VerifyWrite(FOutStream, Header^.InternalAttributes, Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ExternalAttributes, Sizeof(LongWord));
    VerifyWrite(FOutStream, Header^.LocalHeaderOffset,  Sizeof(LongWord));

    if Header^.FileNameLength <> 0 then
      VerifyWrite(FOutStream, Header^.FileName[1], Header^.FileNameLength);
    if Header^.ExtraFieldLength <> 0 then
      VerifyWrite(FOutStream, Header^.ExtraField[1], Header^.ExtraFieldLength);
    if Header^.FileCommentLength <> 0 then
      VerifyWrite(FOutStream, Header^.FileComment[1], Header^.FileCommentLength);
  end;

  FillChar(EndOfHeader, Sizeof(EndOfHeader), 0);
  EndOfHeader.CentralDirEntries := FileCount;
  EndOfHeader.NumEntriesThisDisk := FileCount;
  EndOfHeader.CentralDirSize := FOutStream.Position - FEndFileData;
  EndOfHeader.CentralDirOffset := FEndFileData;

  if Length(FComment) > $FFFF then
    SetLength(FComment, $FFFF);
  EndOfHeader.CommentLength := Length(FComment);

  Sig := SIGNATURE_ZIPENDOFHEADER;
  VerifyWrite(FOutStream, Sig, SizeOf(Sig));
  VerifyWrite(FOutStream, EndOfHeader.DiskNumber,          SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirStartDisk, SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.NumEntriesThisDisk,  SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirEntries,   SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirSize,      SizeOf(LongWord));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirOffset,    SizeOf(LongWord));
  VerifyWrite(FOutStream, EndOfHeader.CommentLength,       SizeOf(Word));

  if EndOfHeader.CommentLength > 0 then
    VerifyWrite(FOutStream, FComment[1], EndOfHeader.CommentLength);
end;

initialization
  FZipCompressionHandlers := TClassList.Create;
  RegisterCompressionHandlerClass(TCnZipDefaultCompressionHandler);

finalization
  FZipCompressionHandlers.Free;

end.
