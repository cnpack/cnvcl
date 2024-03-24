{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnZip;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：CnPack 组件包 Zip 实现单元
* 单元作者：CnPack 开发组 Liu Xiao
* 备    注：使用 Delphi 自带的 Zlib 实现压缩解压与传统密码支持。
*           但 XE2 以上的 Zlib 才支持 WindowBits 参数，才兼容传统的 ZIP 软件
* 开发平台：PWinXP + Delphi 5
* 兼容测试：PWinXP/7 + Delphi 5 ~ XE
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2024.02.17 V1.5
*                新增流压缩解压缩函数，注意需 SUPPORT_ZLIB_WINDOWBITS 才兼容标准 Deflate
*           2022.03.30 V1.4
*                支持删除 Zip 包中的指定文件
*           2018.08.26 V1.3
*                存储/Deflate 模式下支持 Zip 传统的密码压缩解压缩算法
*           2018.08.22 V1.2
*                存储模式下支持 Zip 传统的密码压缩解压缩算法，但 Deflate 模式仍不支持密码
*           2018.08.07 V1.1
*                使用 ZLib 实现加压解压但 XE2 以下用 Zip 解压时仍有问题，XE2 或以上可兼容 Zip
*           2018.08.05 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF SUPPORT_ZLIB_WINDOWBITS}
//  {$MESSAGE WARN 'NOT Compatable with WinZip/WinRAR etc.'}
{$ENDIF}

// {$DEFINE DEBUGZIP}

uses
  SysUtils, Classes, Windows, Contnrs, FileCtrl, CnCRC32, CnNative, ZLib
  {$IFNDEF DISABLE_DIRECTORY_SUPPORT}, CnCommon {$ENDIF}
  {$IFNDEF COMPILER6_UP} , CnWideStrings  {$ENDIF};
  // D5 下需要用到 CnWideStrings 单元做 UTF8 支持

type
  ECnZipException = class(Exception);
  {* Zip 相关异常}

  TCnZipCompressionMethod = (
  {* Zip 压缩类型}
    zcStored, zcShrunk, zcReduce1, zcReduce2, zcReduce3, zcReduce4, zcImplode,
    zcTokenize, zcDeflate, zcDeflate64, zcPKImplode, zcReserved11, zcBZIP2,
    zcReserved13, zcLZMA, zcReserved15, zcReserved16, zcReserved17, zcTERSE,
    zcLZ77
//    zcWavePack = 97,
//    zcPPMdI1
  );

  TCnZipHeader = packed record
  {* Zip 文件头结构}
    MadeByVersion:      Word;     // Start of Central Header
    RequiredVersion:    Word;     // Start of Local Header
    Flag:               Word;
    CompressionMethod:  Word;
    ModifiedDateTime:   Cardinal;
    CRC32:              Cardinal;
    CompressedSize:     Cardinal;
    UncompressedSize:   Cardinal;
    FileNameLength:     Word;
    ExtraFieldLength:   Word;     // End of Local Header
    FileCommentLength:  Word;
    DiskNumberStart:    Word;
    InternalAttributes: Word;
    ExternalAttributes: Cardinal;
    LocalHeaderOffset:  Cardinal; // End of Central Header
    FileName:           AnsiString;
    ExtraField:         AnsiString;
    FileComment:        AnsiString;
  end;
  PCnZipHeader = ^TCnZipHeader;

  TCnZipEndOfCentralHeader = packed record
  {* Zip Central 头结束部分结构}
    DiskNumber:          Word;
    CentralDirStartDisk: Word;
    NumEntriesThisDisk:  Word;
    CentralDirEntries:   Word;
    CentralDirSize:      Cardinal;
    CentralDirOffset:    Cardinal;
    CommentLength:       Word;
    {Comment: RawByteString}
  end;
  PCnZipEndOfCentralHeader = ^TCnZipEndOfCentralHeader;

  TCnZipBase = class(TObject)
  {* Zip 工具类基类}
  private
    FUtf8: Boolean;
    FFileList: TList;
    FComment: AnsiString;
    FPassword: AnsiString;
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
    function GetHasPassword: Boolean; virtual;
    procedure SetPassword(const Value: AnsiString); virtual;

    property Password: AnsiString read FPassword write SetPassword;
    {* 该 Zip 文件的密码}
    property HasPassword: Boolean read GetHasPassword;
    {* 该 Zip 文件是否有密码}
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}
    function IndexOf(const FileName: string): Integer;
    {* 在该 Zip 文件中查找指定文件名，返回其顺序索引}
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

  TCnZipAbstractCompressionHandler = class(TObject)
  {* 压缩类型的实现基类}
  private

  public
    class function CanHandleCompressionMethod(AMethod: TCnZipCompressionMethod): Boolean; virtual; abstract;
    class function CreateCompressionStream(AMethod: TCnZipCompressionMethod;
      OutStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream; virtual; abstract;
    class function CreateDecompressionStream(AMethod: TCnZipCompressionMethod;
      InStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream; virtual; abstract;
  end;

  TCnZipCompressionHandlerClass = class of TCnZipAbstractCompressionHandler;

  TCnZipReader = class(TCnZipBase)
  {* 用来打开 Zip 文件可解压的工具类}
  private
    FInStream: TStream;
    procedure OpenZipStream;
    procedure ReadCentralHeader;
    function PrepareStream(Index: Integer; LocalHeader: PCnZipHeader): TStream;
  protected
    function GetHasPassword: Boolean; override;
    function SearchEndOfCentralHeader(Stream: TStream;
      Header: PCnZipEndOfCentralHeader): Boolean;

    procedure SetPassword(const Value: AnsiString); override;
  public
    constructor Create; override;
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

    property Password;
    {* 解压该 Zip 文件所需的密码}
    property HasPassword;
    {* 该 Zip 文件头里的标志是否需要密码才能解压}
  end;

  TCnZipWriter = class(TCnZipBase)
  {* 用来生成 Zip 文件的工具类}
  private
    FOutStream: TStream;
    FRemovePath: Boolean;
{$IFNDEF DISABLE_DIRECTORY_SUPPORT}
    FDirFiles: TStrings;
    procedure FindFileCallback(const FileName: string; const Info: TSearchRec;
      var Abort: Boolean);
{$ENDIF}
  protected
    procedure AddStream(Data: TStream; LocalHeader: PCnZipHeader);
  public
    destructor Destroy; override;

    procedure CreateZipFile(const ZipFileName: string);
    {* 创建一个空白的 Zip 文件}
    procedure AddFile(const FileName: string; const ArchiveFileName: string = '';
      Compression: TCnZipCompressionMethod = zcDeflate);
    {* 向 Zip 文件中添加指定内容，FileName 为具体文件，ArchiveFileName 为要写入
      Zip 内部的文件名}
    function RemoveFile(const FileName: string): Boolean;
    {* 从 Zip 文件内删除一个文件，返回删除是否成功
      文件名参数需根据 RemovePath 的值以对应是否包含路径}
    function RemoveFileByIndex(FileIndex: Integer): Boolean;
    {* 从 Zip 文件内删除一个指定序号的文件，返回删除是否成功}
{$IFNDEF DISABLE_DIRECTORY_SUPPORT}
    procedure AddDirectory(const DirName: string; Compression: TCnZipCompressionMethod = zcDeflate);
    {* 向 Zip 文件中添加指定目录下的所有文件}
{$ENDIF}    
    procedure Save;
    {* 将压缩内容保存至 Zip 文件}
    procedure Close;
    {* 关闭压缩文件}

    property RemovePath: Boolean read FRemovePath write FRemovePath;
    {* 是否去除每个文件的路径信息只留文件名信息，只在 AddFile 中 ArchiveFileName
      为空的情况下有效}

    property Password;
    {* 设置该 Zip 文件的压缩密码，保存后才生效}
    property HasPassword;
    {* 是否设置了密码，等于判断 Password 属性是否不为空}
  end;

procedure RegisterCompressionHandlerClass(AClass: TCnZipCompressionHandlerClass);
{* 供外界提供对新的压缩方式的支持}

function CnZipFileIsValid(const FileName: string): Boolean;
{* 判断 Zip 文件是否合法}

{$IFNDEF DISABLE_DIRECTORY_SUPPORT}

function CnZipDirectory(const DirName: string; const FileName: string;
  Compression: TCnZipCompressionMethod = zcDeflate; const Password: string = ''): Boolean;
{* 将指定目录压缩为一个 Zip 文件}

{$ENDIF}

function CnZipExtractTo(const FileName: string; const DirName: string;
  const Password: string = ''): Boolean;
{* 将指定 Zip 文件解压缩到指定目录}

procedure CnZipCompressStream(InStream, OutZipStream: TStream;
  CompressionLevel: TCompressionLevel = clDefault);
{* 将 InStream 中的内容压缩并输出至 OutZipStream
  注意，如果 Delphi 版本过低导致 CnPack.inc 中未定义 SUPPORT_ZLIB_WINDOWBITS
  压缩出的内容可能和标准 Deflate 不兼容}

procedure CnZipUncompressStream(InZipStream, OutStream: TStream);
{* 将 InZipStream 中的压缩的内容解压缩并输出至 OutStream
  注意，如果 Delphi 版本过低导致 CnPack.inc 中未定义 SUPPORT_ZLIB_WINDOWBITS
  则可能和标准 Deflate 不兼容，解压内容可能失败
  另外，解压缩时会从 InZipStream 的 Position 读起，宜按需设为 0}

implementation

{$IFDEF DEBUGZIP}
uses
  CnDebug;
{$ENDIF}

const
  CN_SIGNATURE_ZIPENDOFHEADER: Cardinal = $06054B50;
  CN_SIGNATURE_CENTRALHEADER:  Cardinal = $02014B50;
  CN_SIGNATURE_LOCALHEADER:    Cardinal = $04034B50;

  CN_KEY0_INIT: Cardinal = 305419896;
  CN_KEY1_INIT: Cardinal = 591751049;
  CN_KEY2_INIT: Cardinal = 878082192;
  CN_KEY_UPDATE: Cardinal = 134775813;

  CN_LOCAL_HEADERSIZE = 26;
  CN_CENTRAL_HEADERSIZE = 42;
  CN_UTF8_MASK = $0800;  // 1 shl 11
  CN_ZIP_CRYPT_HEAD_SIZE = 12;

resourcestring
  SZipErrorRead = 'Error Reading Zip File';
  SZipErrorWrite = 'Error Writing Zip File';
  SZipInvalidLocalHeader   = 'Invalid Zip Local Header';
  SZipInvalidCentralHeader = 'Invalid Zip Central Header';
  SFileNotFound = 'Error Finding File';
  SZipNotSupport = 'Zip Compression Method NOT Support';
  SZipInvalidPassword = 'Invalid Password';
  SZipNotImplemented = 'Feature NOT Implemented';
  SZipUtf8NotSupport = 'UTF8 NOT Support';

var
  FZipCompressionHandlers: TClassList = nil;

type
  TCnZipDefaultCompressionHandler = class(TCnZipAbstractCompressionHandler)
  {* 默认实现了 Stored 与 Deflate 模式并支持传统加解密的处理类}
  private

  public
    class function CanHandleCompressionMethod(AMethod: TCnZipCompressionMethod): Boolean; override;
    {* 是否支持特定的压缩方法}
    class function CreateCompressionStream(AMethod: TCnZipCompressionMethod;
      OutStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream; override;
    {* 创建针对特定输入流的压缩流。压缩流的概念是，压缩流有个输出流，当朝压缩流写入数据时，
      将自动把压缩后的数据写入输出流。所以压缩流要实现 Write 方法写明文，内部压缩加密后写输出流}
    class function CreateDecompressionStream(AMethod: TCnZipCompressionMethod;
      InStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream; override;
    {* 创建针对特定输入流的解压缩流。解压缩流的概念是，解压缩流有个输入流，当从解压缩流读数据时，
      将自动把解压缩后的数据提供出来到 Buffer。所以解压缩流要实现 Read 方法返回明文，内部从输入流读并解压缩解密之类的}
  end;

  TCnStoredStream = class(TStream)
  {* 存储方式的压缩流与解压缩流}
  private
    FStream: TStream;
  protected
{$IFDEF COMPILER7_UP}
    function GetSize: Int64; override;
{$ENDIF}
  public
    constructor Create(Stream: TStream);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TCnZipCryptKeys = class(TObject)
  {* 用来给 Zip 内容流加密解密的工具类}
  private
    FKey0, FKey1, FKey2: Cardinal;
  protected
    function CalcDecryptByte: Byte;
  public
    procedure InitKeys(const Password: AnsiString);
    procedure UpdateKeys(C: Byte);

    procedure DecryptByte(var Value: Byte);
    procedure EncryptByte(var Value: Byte);
  end;

  TCnEncryptStoredStream = class(TStream)
  {* 存储也就是非压缩的加密流的实现，Write 时加密}
  private
    FKeys: TCnZipCryptKeys;
    FPassword: AnsiString;
    FOutStream: TStream;
    FZipHeader: PCnZipHeader;
  protected

  public
    constructor Create(OutStream: TStream; const APassword: AnsiString; const AZipHeader: PCnZipHeader);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;    // 可无需实现
    function Seek(Offset: Longint; Origin: Word): Longint; override; // 可无需实现
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

  TCnDecryptStoredStream = class(TStream)
  {* 存储也就是非压缩的解密流的实现，Read 时解密}
  private
    FZip: TStream;
    FKeys: TCnZipCryptKeys;
    FPassword: AnsiString;
    FStream: TStream;
    FZipHeader: PCnZipHeader;
  protected

  public
    constructor Create(AStream: TStream; const APassword: AnsiString;
      const AZipHeader: PCnZipHeader);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override; // 可无需实现
    function Write(const Buffer; Count: Integer): Integer; override; // 可无需实现
  end;

  TCnEncryptZipCompressStream = class(TStream)
  {* Deflate 压缩并且标准 Zip 加密的压缩流实现，使用内存流压缩，超大文件可能会 OOM
    实现顺序：在外界写入时先压缩至 FZipped，析构时再将 FZipped 逐字节加密写出}
  private
    FZip: TStream;
    FZipped: TMemoryStream; // 存储压缩后但未加密的临时内容
    FKeys: TCnZipCryptKeys;
    FPassword: AnsiString;
    FOutStream: TStream;
    FZipHeader: PCnZipHeader;
  public
    constructor Create(OutStream: TStream; const APassword: AnsiString;
      const AZipHeader: PCnZipHeader);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;    // 可无需实现
    function Seek(Offset: Longint; Origin: Word): Longint; override; // 可无需实现
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

  TCnDecryptZipCompressStream = class(TStream)
  {* Deflate 压缩并且标准 Zip 加密的解压缩流实现，使用内存流解压缩，超大文件可能会 OOM
    实现顺序：在创建时先解密内容至 FDecrypted，外界读取时再将 FDecrypted 解压缩供读出}
  private
    FUnzip: TStream;
    FDecrypted: TMemoryStream; // 存储预先逐字节解密出来的压缩内容
    FKeys: TCnZipCryptKeys;
    FPassword: AnsiString;
    FInStream: TStream;
    FZipHeader: PCnZipHeader;
  public
    constructor Create(InStream: TStream; const APassword: AnsiString;
      const AZipHeader: PCnZipHeader);
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override; // 可无需实现
    function Write(const Buffer; Count: Integer): Integer; override; // 可无需实现
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
  OutStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream;
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
        Result := AComp.CreateCompressionStream(AMethod, OutStream, Item, Zip);
        Exit;
      end;
    end;
  end;
end;

function CreateDecompressStreamFromHandler(AMethod: TCnZipCompressionMethod;
  InStream: TStream; const Item: PCnZipHeader; Zip: TCnZipBase): TStream;
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
        Result := AComp.CreateDecompressionStream(AMethod, InStream, Item, Zip);
        Exit;
      end;
    end;
  end;
end;

function ZipUtf8ToString(const Text: AnsiString): string;
begin
{$IFDEF UNICODE}
  Result := UTF8ToUnicodeString(PAnsiChar(Text));
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := Utf8ToAnsi(Text);
  {$ELSE}
  // raise ECnZipException.CreateRes(@SZipUtf8NotSupport);
  Result := AnsiString(CnUtf8DecodeToWideString(Text));
  {$ENDIF}
{$ENDIF}
end;

function ZipStringToUtf8(const Text: string): AnsiString;
begin
{$IFDEF UNICODE}
  Result := Utf8Encode(Text);
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := AnsiToUtf8(Text);
  {$ELSE}
  // raise ECnZipException.CreateRes(@SZipUtf8NotSupport);
  Result := CnUtf8EncodeWideString(WideString(Text));
  {$ENDIF}
{$ENDIF}
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
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      Result := Z.SearchEndOfCentralHeader(Stream, @Header);
    finally
      Stream.Free;
      Z.Free;
    end;
  except on E: EStreamError do
    ;
  end;
end;

{$IFNDEF DISABLE_DIRECTORY_SUPPORT}

function CnZipDirectory(const DirName: string; const FileName: string;
  Compression: TCnZipCompressionMethod; const Password: string): Boolean;
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

{$ENDIF}

function CnZipExtractTo(const FileName: string; const DirName: string;
  const Password: string): Boolean;
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

procedure CnZipCompressStream(InStream, OutZipStream: TStream;
  CompressionLevel: TCompressionLevel);
var
  Zip: TCompressionStream;
begin
  // 不能 Read 只能 Write，Write 时自动压缩并向创建时指定的关联流里写
  Zip := TCompressionStream.Create(CompressionLevel, OutZipStream);
  try
    Zip.CopyFrom(InStream, 0);
  finally
    Zip.Free;
  end;
end;

procedure CnZipUncompressStream(InZipStream, OutStream: TStream);
var
{$IFDEF ZLIB_STREAM_NOSIZE}
  InpBuf, OutBuf: Pointer;
  OutBytes, Cnt: Integer;
{$ELSE}
  UnZip: TDecompressionStream;
{$ENDIF}
begin
{$IFDEF ZLIB_STREAM_NOSIZE}
  InpBuf := nil;
  OutBuf := nil;

  // 低版本 TDecompressionStream 类不支持内部的 Seek 到 soEnd 操作，无法获取解压后的 Size
  // 只能全部读入内存后调 ZLIB 的 DecompressBuf 解压
  Cnt := InZipStream.Size - InZipStream.Position;
  if Cnt > 0 then
  begin
    try
      GetMem(InpBuf, Cnt);
      InZipStream.Read(InpBuf^, Cnt);
      DecompressBuf(InpBuf, Cnt, 0, OutBuf, OutBytes);
      OutStream.Write(OutBuf^, OutBytes);
    finally
      if InpBuf <> nil then
        FreeMem(InpBuf);
      if OutBuf <> nil then
        FreeMem(OutBuf);
    end;
  end;
{$ELSE}
  // 不能 Write 只能 Read，Read 时读出的是关联流里解压缩了的内容
  UnZip := TDecompressionStream.Create(InZipStream);
  try
    OutStream.CopyFrom(UnZip, 0);
    // 注意这里会用到 UnZip.Size，高版本 TDecompressionStream 类才可用
  finally
    UnZip.Free;
  end;
{$ENDIF}
  OutStream.Position := 0;
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
  FUtf8 := False;
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

function TCnZipBase.GetHasPassword: Boolean;
begin
  Result := FPassword <> '';
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
    Result := ZipUtf8ToString(Raw)
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

procedure TCnZipBase.SetPassword(const Value: AnsiString);
begin
  FPassword := Value;
end;

procedure TCnZipBase.SetUtf8(const Value: Boolean);
begin
  FUtf8 := Value;
end;

function TCnZipBase.StringToRaw(Str: string): AnsiString;
begin
  if FUtf8 then
    Result := ZipStringToUtf8(Str)
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
  Sig: Cardinal;
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
  if Sig <> CN_SIGNATURE_LOCALHEADER then
    raise ECnZipException.CreateRes(@SZipInvalidLocalHeader);

  FInStream.Read(LocalHeader^.RequiredVersion,    Sizeof(Word));
  FInStream.Read(LocalHeader^.Flag,               Sizeof(Word));
  FInStream.Read(LocalHeader^.CompressionMethod,  Sizeof(Word));
  FInStream.Read(LocalHeader^.ModifiedDateTime,   Sizeof(Cardinal));
  FInStream.Read(LocalHeader^.CRC32,              Sizeof(Cardinal));
  FInStream.Read(LocalHeader^.CompressedSize,     Sizeof(Cardinal));
  FInStream.Read(LocalHeader^.UncompressedSize,   Sizeof(Cardinal));
  FInStream.Read(LocalHeader^.FileNameLength,     Sizeof(Word));
  FInStream.Read(LocalHeader^.ExtraFieldLength,   Sizeof(Word));

  SetLength(LocalHeader^.FileName, LocalHeader^.FileNameLength);
  FInStream.Read(LocalHeader^.FileName[1], LocalHeader^.FileNameLength);
  if LocalHeader^.ExtraFieldLength > 0 then
  begin
    SetLength(LocalHeader^.ExtraField, LocalHeader^.ExtraFieldLength);
    FInStream.Read(LocalHeader^.ExtraField[1], LocalHeader^.ExtraFieldLength);
  end;

{$IFDEF DEBUGZIP}
  CnDebugger.LogMsg('Reader: Dump a Local Header for FileName: ' + LocalHeader^.FileName);
  CnDebugger.LogFmt('  RequiredVersion: %4.4d', [LocalHeader^.RequiredVersion]);
  CnDebugger.LogFmt('  Flag: $%4.4x', [LocalHeader^.Flag]);
  CnDebugger.LogFmt('  CompressionMethod: %4.4d', [LocalHeader^.CompressionMethod]);
  CnDebugger.LogFmt('  ModifiedDateTime: $%8.8x', [LocalHeader^.ModifiedDateTime]);
  CnDebugger.LogFmt('  CRC32: $%8.8x', [LocalHeader^.CRC32]);
  CnDebugger.LogFmt('  CompressedSize: %d', [LocalHeader^.CompressedSize]);
  CnDebugger.LogFmt('  UncompressedSize: %d', [LocalHeader^.UncompressedSize]);
  CnDebugger.LogFmt('  FileNameLength: %d', [LocalHeader^.FileNameLength]);
  CnDebugger.LogFmt('  ExtraFieldLength: %d', [LocalHeader^.ExtraFieldLength]);
  CnDebugger.LogFmt('  FileCommentLength: %d', [LocalHeader^.FileCommentLength]);
  CnDebugger.LogFmt('  DiskNumberStart: %d', [LocalHeader^.DiskNumberStart]);
  CnDebugger.LogFmt('  InternalAttributes: %d', [LocalHeader^.InternalAttributes]);
  CnDebugger.LogFmt('  ExternalAttributes: %d', [LocalHeader^.ExternalAttributes]);
  CnDebugger.LogFmt('  LocalHeaderOffset:  %8.8x', [LocalHeader^.LocalHeaderOffset]);
{$ENDIF}

  Result := CreateDecompressStreamFromHandler(TCnZipCompressionMethod(LocalHeader^.CompressionMethod),
    FInStream, LocalHeader, Self);
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
      AFileName := IncludeTrailingBackslash(Path) + AFileName
    else
      AFileName := IncludeTrailingBackslash(Path) + ExtractFileName(AFileName);

    Dir := ExtractFileDir(AFileName);
    if CreateSubdirs and (Dir <> '') then
      ForceDirectories(Dir);

{$WARNINGS OFF}
    if AFileName[Length(AFileName) - 1] in ['\', '/'] then
      Exit;
{$WARNINGS ON}

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

  FInStream := TFileStream.Create(ZipFileName, fmOpenRead or fmShareDenyWrite);
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
  Signature: Cardinal;
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
    if Signature <> CN_SIGNATURE_CENTRALHEADER then
      raise ECnZipException.CreateRes(@SZipInvalidCentralHeader);

    New(Header);
    try
      VerifyRead(FInStream, Header^.MadeByVersion,      Sizeof(Word));
      VerifyRead(FInStream, Header^.RequiredVersion,    Sizeof(Word));
      VerifyRead(FInStream, Header^.Flag,               Sizeof(Word));
      VerifyRead(FInStream, Header^.CompressionMethod,  Sizeof(Word));
      VerifyRead(FInStream, Header^.ModifiedDateTime,   Sizeof(Cardinal));
      VerifyRead(FInStream, Header^.CRC32,              Sizeof(Cardinal));
      VerifyRead(FInStream, Header^.CompressedSize,     Sizeof(Cardinal));
      VerifyRead(FInStream, Header^.UncompressedSize,   Sizeof(Cardinal));
      VerifyRead(FInStream, Header^.FileNameLength,     Sizeof(Word));
      VerifyRead(FInStream, Header^.ExtraFieldLength,   Sizeof(Word));
      VerifyRead(FInStream, Header^.FileCommentLength,  Sizeof(Word));
      VerifyRead(FInStream, Header^.DiskNumberStart,    Sizeof(Word));
      VerifyRead(FInStream, Header^.InternalAttributes, Sizeof(Word));
      VerifyRead(FInStream, Header^.ExternalAttributes, Sizeof(Cardinal));
      VerifyRead(FInStream, Header^.LocalHeaderOffset,  Sizeof(Cardinal));

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

      if (Header^.Flag and CN_UTF8_MASK) = 0 then
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
  BackBuf: TBytes;
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
      if (BackBuf[I]     = ((CN_SIGNATURE_ZIPENDOFHEADER       ) and $FF)) and
         (BackBuf[I + 1] = ((CN_SIGNATURE_ZIPENDOFHEADER shr  8) and $FF)) and
         (BackBuf[I + 2] = ((CN_SIGNATURE_ZIPENDOFHEADER shr 16) and $FF)) and
         (BackBuf[I + 3] = ((CN_SIGNATURE_ZIPENDOFHEADER shr 24) and $FF)) then
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

function TCnZipReader.GetHasPassword: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GetFileCount - 1 do
  begin
    if GetFileInfo(I)^.Flag and 1 = 1 then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TCnZipReader.SetPassword(const Value: AnsiString);
begin
  inherited;

end;

constructor TCnZipReader.Create;
begin
  inherited;

end;

{ TCnZipDefaultCompressionHandler }

class function TCnZipDefaultCompressionHandler.CanHandleCompressionMethod(
  AMethod: TCnZipCompressionMethod): Boolean;
begin
  Result := AMethod in [zcStored, zcDeflate];
end;

class function TCnZipDefaultCompressionHandler.CreateCompressionStream(
  AMethod: TCnZipCompressionMethod; OutStream: TStream; const Item: PCnZipHeader;
  Zip: TCnZipBase): TStream;
var
  HasPas: Boolean;
begin
  Result := nil;
  HasPas := (Item^.Flag and 1) = 1;

  if AMethod = zcStored then
  begin
    if HasPas then
      Result := TCnEncryptStoredStream.Create(OutStream, Zip.Password, Item)
    else
      Result := TCnStoredStream.Create(OutStream)
  end
  else if AMethod = zcDeflate then
  begin
    if HasPas then
    begin
      Result := TCnEncryptZipCompressStream.Create(OutStream, Zip.Password, Item);
    end
    else
    begin
{$IFDEF SUPPORT_ZLIB_WINDOWBITS}
      Result := TCompressionStream.Create(OutStream, zcDefault, -15);
{$ELSE}
      Result := TCompressionStream.Create(clDefault, OutStream);
{$ENDIF}
    end;
  end;
end;

class function TCnZipDefaultCompressionHandler.CreateDecompressionStream(
  AMethod: TCnZipCompressionMethod; InStream: TStream; const Item: PCnZipHeader;
  Zip: TCnZipBase): TStream;
var
  HasPas: Boolean;
begin
  Result := nil;
  HasPas := (Item^.Flag and 1) = 1;

  if AMethod = zcStored then
  begin
    if HasPas then
      Result := TCnDecryptStoredStream.Create(InStream, Zip.Password, Item)
    else
      Result := TCnStoredStream.Create(InStream);
  end
  else if AMethod = zcDeflate then
  begin
    if HasPas then
    begin
      Result := TCnDecryptZipCompressStream.Create(InStream, Zip.Password, Item);
    end
    else
    begin
{$IFDEF SUPPORT_ZLIB_WINDOWBITS}
      Result := TDecompressionStream.Create(InStream, -15);
{$ELSE}
      Result := TDecompressionStream.Create(InStream);
{$ENDIF}
    end;
  end;
end;

{ TCnStoredStream }

constructor TCnStoredStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
end;

{$IFDEF COMPILER7_UP}
function TCnStoredStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;
{$ENDIF}

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

{$IFNDEF DISABLE_DIRECTORY_SUPPORT}

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
    Path := IncludeTrailingBackslash(DirName);
{$IFDEF MSWINDOWS}
    AFile := StringReplace(Copy(FDirFiles[I], Length(Path) + 1, Length(FDirFiles[I])), '\', '/', [rfReplaceAll]);
{$ELSE}
    AFile := Copy(FDirFiles[I], Length(Path) + 1, Length(FDirFiles[I]));
{$ENDIF}
    AddFile(FDirFiles[I], AFile, Compression);
  end;
end;

{$ENDIF}

procedure TCnZipWriter.AddFile(const FileName, ArchiveFileName: string;
  Compression: TCnZipCompressionMethod);
var
  InStream: TStream;
  LocalHeader: PCnZipHeader;
  Archive: string;

  function GetFileDateTime(const FileName: string): TDateTime;
  var
    Handle: THandle;
    FindData: TWin32FindData;
    SystemTime: TSystemTime;
  begin
    Result := 0.0;
    Handle := FindFirstFile(PChar(FileName), FindData);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        FileTimeToLocalFileTime(FindData.ftLastWriteTime, FindData.ftLastWriteTime);
        FileTimeToSystemTime(FindData.ftLastWriteTime, SystemTime);
        with SystemTime do
          Result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute,
            wSecond, wMilliseconds);
      end;
    end;
  end;

begin
  if Trim(FileName) = '' then
    Exit;

  if not SupportCompressionMethod(Compression) then
    raise ECnZipException.CreateRes(@SZipNotSupport);

  New(LocalHeader);
  FillChar(LocalHeader^, SizeOf(LocalHeader^), 0);

  InStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
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
      LocalHeader^.Flag := LocalHeader^.Flag or CN_UTF8_MASK;
    if HasPassword then
      LocalHeader^.Flag := LocalHeader^.Flag or 1;

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
  Signature: Cardinal;
  LStartPos: Int64;
  C: Integer;
  Buffer: TBytes;
begin
  FOutStream.Position := FEndFileData;
  LocalHeader^.LocalHeaderOffset := FEndFileData;

  if LocalHeader^.MadeByVersion < 20 then
    LocalHeader^.MadeByVersion := 20;
  if LocalHeader^.RequiredVersion < 20 then
    LocalHeader^.RequiredVersion := 20;

  LocalHeader^.FileNameLength   := Length(LocalHeader^.FileName);
  LocalHeader^.ExtraFieldLength := Length(LocalHeader^.ExtraField);

  Signature := CN_SIGNATURE_LOCALHEADER;
  VerifyWrite(FOutStream, Signature, SizeOf(Signature));

  VerifyWrite(FOutStream, LocalHeader^.RequiredVersion,    Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.Flag,               Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.CompressionMethod,  Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ModifiedDateTime,   Sizeof(Cardinal));
  VerifyWrite(FOutStream, LocalHeader^.CRC32,              Sizeof(Cardinal));
  VerifyWrite(FOutStream, LocalHeader^.CompressedSize,     Sizeof(Cardinal));
  VerifyWrite(FOutStream, LocalHeader^.UncompressedSize,   Sizeof(Cardinal));
  VerifyWrite(FOutStream, LocalHeader^.FileNameLength,     Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ExtraFieldLength,   Sizeof(Word));

  VerifyWrite(FOutStream, LocalHeader^.FileName[1], LocalHeader^.FileNameLength);
  if LocalHeader^.ExtraFieldLength > 0 then
    VerifyWrite(FOutStream, LocalHeader^.ExtraField[1], LocalHeader^.ExtraFieldLength);

  LStartPos := FOutStream.Position;
  DataStart := Data.Position;
  LocalHeader^.UncompressedSize := Data.Size - DataStart;

  // 计算原始的 CRC32 值
  SetLength(Buffer, $4000);
  while Data.Position < Longint(LocalHeader^.UncompressedSize) do
  begin
    C := Data.Read(Buffer[0], Length(Buffer));
    LocalHeader^.CRC32 := CRC32Calc(LocalHeader^.CRC32, Buffer[0], C);
  end;

  // 重新回到原位，压缩
  Data.Position := DataStart;
  CompressStream := CreateCompressStreamFromHandler(TCnZipCompressionMethod(LocalHeader^.CompressionMethod),
    FOutStream, LocalHeader, Self);
  try
    CompressStream.CopyFrom(Data, LocalHeader^.UncompressedSize);
  finally
    CompressStream.Free;
  end;
  LocalHeader^.CompressedSize := FOutStream.Position - LStartPos;

  FEndFileData := FOutStream.Position;
  FOutStream.Position := LocalHeader^.LocalHeaderOffset + SizeOf(Cardinal);
  VerifyWrite(FOutStream, LocalHeader^.RequiredVersion,    Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.Flag,               Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.CompressionMethod,  Sizeof(Word));
  VerifyWrite(FOutStream, LocalHeader^.ModifiedDateTime,   Sizeof(Cardinal));
  VerifyWrite(FOutStream, LocalHeader^.CRC32,              Sizeof(Cardinal));
  VerifyWrite(FOutStream, LocalHeader^.CompressedSize,     Sizeof(Cardinal));
  VerifyWrite(FOutStream, LocalHeader^.UncompressedSize,   Sizeof(Cardinal));
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
{$IFNDEF DISABLE_DIRECTORY_SUPPORT}
  FDirFiles.Free;
{$ENDIF}
  inherited;
end;

{$IFNDEF DISABLE_DIRECTORY_SUPPORT}

procedure TCnZipWriter.FindFileCallback(const FileName: string;
  const Info: TSearchRec; var Abort: Boolean);
begin
  if (FileName <> '.') and (FileName <> '..') then
    FDirFiles.Add(FileName);
end;

{$ENDIF}

function TCnZipWriter.RemoveFile(const FileName: string): Boolean;
var
  Idx: Integer;
  H: PCnZipHeader;
begin
  Result := False;
  Idx := IndexOf(FileName);
  if Idx >= 0 then
  begin
    H := PCnZipHeader(FFileList[Idx]);
    FFileList.Delete(Idx);
    Dispose(H);
    Result := True;
  end;
end;

function TCnZipWriter.RemoveFileByIndex(FileIndex: Integer): Boolean;
var
  H: PCnZipHeader;
begin
  Result := False;
  if (FileIndex >= 0) and (FileIndex < FileCount) then
  begin
    H := PCnZipHeader(FFileList[FileIndex]);
    FFileList.Delete(FileIndex);
    Dispose(H);
    Result := True;
  end;
end;

procedure TCnZipWriter.Save;
var
  Header: PCnZipHeader;
  EndOfHeader: TCnZipEndOfCentralHeader;
  I: Integer;
  Sig: Cardinal;
begin
  FOutStream.Position := FEndFileData;
  Sig := CN_SIGNATURE_CENTRALHEADER;

  for I := 0 to FileCount - 1 do
  begin
    Header := FileInfo[I];
    VerifyWrite(FOutStream, Sig, SizeOf(Sig));
    VerifyWrite(FOutStream, Header^.MadeByVersion,      Sizeof(Word));
    VerifyWrite(FOutStream, Header^.RequiredVersion,    Sizeof(Word));
    VerifyWrite(FOutStream, Header^.Flag,               Sizeof(Word));
    VerifyWrite(FOutStream, Header^.CompressionMethod,  Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ModifiedDateTime,   Sizeof(Cardinal));
    VerifyWrite(FOutStream, Header^.CRC32,              Sizeof(Cardinal));
    VerifyWrite(FOutStream, Header^.CompressedSize,     Sizeof(Cardinal));
    VerifyWrite(FOutStream, Header^.UncompressedSize,   Sizeof(Cardinal));
    VerifyWrite(FOutStream, Header^.FileNameLength,     Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ExtraFieldLength,   Sizeof(Word));
    VerifyWrite(FOutStream, Header^.FileCommentLength,  Sizeof(Word));
    VerifyWrite(FOutStream, Header^.DiskNumberStart,    Sizeof(Word));
    VerifyWrite(FOutStream, Header^.InternalAttributes, Sizeof(Word));
    VerifyWrite(FOutStream, Header^.ExternalAttributes, Sizeof(Cardinal));
    VerifyWrite(FOutStream, Header^.LocalHeaderOffset,  Sizeof(Cardinal));

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

  Sig := CN_SIGNATURE_ZIPENDOFHEADER;
  VerifyWrite(FOutStream, Sig, SizeOf(Sig));
  VerifyWrite(FOutStream, EndOfHeader.DiskNumber,          SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirStartDisk, SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.NumEntriesThisDisk,  SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirEntries,   SizeOf(Word));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirSize,      SizeOf(Cardinal));
  VerifyWrite(FOutStream, EndOfHeader.CentralDirOffset,    SizeOf(Cardinal));
  VerifyWrite(FOutStream, EndOfHeader.CommentLength,       SizeOf(Word));

  if EndOfHeader.CommentLength > 0 then
    VerifyWrite(FOutStream, FComment[1], EndOfHeader.CommentLength);
end;

{ TCnZipCryptKeys }

function TCnZipCryptKeys.CalcDecryptByte: Byte;
var
  T: Word;
begin
  T := FKey2 or 2;
  Result := Word(T * (T xor 1)) shr 8;
end;

procedure TCnZipCryptKeys.DecryptByte(var Value: Byte);
begin
  Value := Value xor CalcDecryptByte;
  UpdateKeys(Value);
end;

procedure TCnZipCryptKeys.EncryptByte(var Value: Byte);
var
  T: Byte;
begin
  T := CalcDecryptByte;
  UpdateKeys(Value);
  Value := Value xor T;
end;

procedure TCnZipCryptKeys.InitKeys(const Password: AnsiString);
var
  I: Integer;
begin
  FKey0 := CN_KEY0_INIT;
  FKey1 := CN_KEY1_INIT;
  FKey2 := CN_KEY2_INIT;

  for I := 1 to Length(Password) do
    UpdateKeys(Ord(Password[I]));
end;

procedure TCnZipCryptKeys.UpdateKeys(C: Byte);
begin
  FKey0 := CalcCRC32Byte(FKey0, C);
  FKey1 := FKey1 + (FKey0 and $FF);
  FKey1 := FKey1 * CN_KEY_UPDATE + 1;
  FKey2 := CalcCRC32Byte(FKey2, FKey1 shr 24);
end;

{ TCnDecryptStoredStream }

constructor TCnDecryptStoredStream.Create(AStream: TStream;
  const APassword: AnsiString; const AZipHeader: PCnZipHeader);
var
  I: Integer;
  C: Byte;
  H: array [0..CN_ZIP_CRYPT_HEAD_SIZE - 1] of Byte;
begin
  inherited Create;
  FStream := AStream;
  FPassword := APassword;
  FZipHeader := AZipHeader;
  FKeys := TCnZipCryptKeys.Create;
  FKeys.InitKeys(FPassword);

  // 读 12 字节头并解出来比对 CRC 以判断密码是否正确，注意有些 zip 文件不比对 CRC
  FStream.Read(H, Sizeof(H));

  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 1 do
  begin
    C := H[I] xor FKeys.CalcDecryptByte;
    FKeys.UpdateKeys(C);
    H[I] := C;
  end;

  if H[CN_ZIP_CRYPT_HEAD_SIZE - 1] <> (FZipHeader^.CRC32 shr 24) then
    raise ECnZipException.CreateRes(@SZipInvalidPassword);
end;

destructor TCnDecryptStoredStream.Destroy;
begin
  FZip.Free;
  FKeys.Free;
  inherited;
end;

function TCnDecryptStoredStream.Read(var Buffer; Count: Integer): Integer;
var
  P: PByte;
  I: Integer;
begin
  Result := FStream.Read(Buffer, Count);
  P := @Buffer;
  for I := 1 to Result do
  begin
    FKeys.DecryptByte(P^);
    Inc(P);
  end;
end;

function TCnDecryptStoredStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnDecryptStoredStream.Write(const Buffer; Count: Integer): Integer;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

{ TCnEnryptStoredStream }

constructor TCnEncryptStoredStream.Create(OutStream: TStream;
  const APassword: AnsiString; const AZipHeader: PCnZipHeader);
var
  H: array[0..CN_ZIP_CRYPT_HEAD_SIZE - 1] of Byte;
  I: Integer;
begin
  inherited Create;
  FOutStream := OutStream;
  FPassword := APassword;
  FZipHeader := AZipHeader;
  FKeys := TCnZipCryptKeys.Create;
  FKeys.InitKeys(FPassword);

  // 随机凑 12 个字节的头
  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 2 do
    H[I] := Random(256);
  H[CN_ZIP_CRYPT_HEAD_SIZE - 1] := (FZipHeader^.CRC32 shr 24);

  // 加密并写入
  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 1 do
    FKeys.EncryptByte(H[I]);
  FOutStream.Write(H, Sizeof(H));
end;

destructor TCnEncryptStoredStream.Destroy;
begin
  FKeys.Free;
  inherited;
end;

function TCnEncryptStoredStream.Read(var Buffer; Count: Integer): Integer;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnEncryptStoredStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnEncryptStoredStream.Write(const Buffer; Count: Integer): Integer;
const
  MaxBufSize = $F000;
var
  B: TBytes;
  C, I: Integer;
  P: PByte;
begin
  Result := 0;
  if Count < MaxBufSize then
    SetLength(B, Count)
  else
    SetLength(B, MaxBufSize);

  P := @Buffer;
  while Count > 0 do
  begin
    C := Length(B);
    if Count < C then
      C := Count;

    Move(P^, B[0], C);
    Inc(P, C);
    for I := 0 to C - 1 do
      FKeys.EncryptByte(B[I]);

    Result := Result + FOutStream.Write(B[0], C);
    Count := Count - C;
  end;
end;

{ TCnEncryptZipCompressStream }

constructor TCnEncryptZipCompressStream.Create(OutStream: TStream;
  const APassword: AnsiString; const AZipHeader: PCnZipHeader);
var
  H: array[0..CN_ZIP_CRYPT_HEAD_SIZE - 1] of Byte;
  I: Integer;
begin
  inherited Create;
  FOutStream := OutStream;
  FPassword := APassword;
  FZipHeader := AZipHeader;
  FKeys := TCnZipCryptKeys.Create;
  FKeys.InitKeys(FPassword);

  FZipped := TMemoryStream.Create;
{$IFDEF SUPPORT_ZLIB_WINDOWBITS}
  FZip := TCompressionStream.Create(FZipped, zcDefault, -15);
{$ELSE}
  FZip := TCompressionStream.Create(clDefault, FZipped);
{$ENDIF}

  // 随机凑 12 个字节的头
  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 2 do
    H[I] := Random(256);
  H[CN_ZIP_CRYPT_HEAD_SIZE - 1] := (FZipHeader^.CRC32 shr 24);

  // 加密并写入
  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 1 do
    FKeys.EncryptByte(H[I]);
  FOutStream.Write(H, Sizeof(H));
end;

destructor TCnEncryptZipCompressStream.Destroy;
var
  I: Integer;
  P: PByte;
begin
  FZip.Free;

  // FZipped 现在是压缩内容，需要加密并写出至 FOutStream
  P := FZipped.Memory;
  if (P <> nil) and (FZipped.Size > 0) then
  begin
    for I := 0 to FZipped.Size - 1 do
    begin
      FKeys.EncryptByte(P^);
      FOutStream.Write(P^, 1);
      Inc(P);
    end;
  end;

  FZipped.Free;
  FKeys.Free;
  inherited;
end;

function TCnEncryptZipCompressStream.Read(var Buffer;
  Count: Integer): Integer;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnEncryptZipCompressStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnEncryptZipCompressStream.Write(const Buffer;
  Count: Integer): Integer;
begin
  // 外界写入的原始内容在这里要转写进 FZip 压缩流，压缩后内容则被 FZip 写进 FZipped
  Result := FZip.Write(Buffer, Count);
end;

{ TCnDecryptZipCompressStream }

constructor TCnDecryptZipCompressStream.Create(InStream: TStream;
  const APassword: AnsiString; const AZipHeader: PCnZipHeader);
var
  I: Integer;
  P: PByte;
  C: Byte;
  H: array [0..CN_ZIP_CRYPT_HEAD_SIZE - 1] of Byte;
begin
  inherited Create;
  FInStream := InStream;
  FPassword := APassword;
  FZipHeader := AZipHeader;
  FKeys := TCnZipCryptKeys.Create;
  FKeys.InitKeys(FPassword);

  // 读 12 字节头并解出来比对 CRC 以判断密码是否正确，注意有些 zip 文件不比对 CRC
  FInStream.Read(H, Sizeof(H));

  for I := 0 to CN_ZIP_CRYPT_HEAD_SIZE - 1 do
  begin
    C := H[I] xor FKeys.CalcDecryptByte;
    FKeys.UpdateKeys(C);
    H[I] := C;
  end;

  if H[CN_ZIP_CRYPT_HEAD_SIZE - 1] <> (FZipHeader^.CRC32 shr 24) then
    raise ECnZipException.CreateRes(@SZipInvalidPassword);

  // 先从 FInStream 密文流中读出内容到 FDecrypted 并解密得到压缩后内容
  FDecrypted := TMemoryStream.Create;
  FDecrypted.CopyFrom(FInStream, FZipHeader^.CompressedSize - CN_ZIP_CRYPT_HEAD_SIZE);
  P := FDecrypted.Memory;
  for I := 1 to FDecrypted.Size do
  begin
    FKeys.DecryptByte(P^);
    Inc(P);
  end;

  FDecrypted.Position := 0;
{$IFDEF SUPPORT_ZLIB_WINDOWBITS}
  FUnzip := TDecompressionStream.Create(FDecrypted, -15);
{$ELSE}
  FUnzip := TDecompressionStream.Create(FDecrypted);
{$ENDIF}
end;

destructor TCnDecryptZipCompressStream.Destroy;
begin
  FUnzip.Free;      // FUnzip 流释放时内部要访问其引用的 FDecrypted 因此必须先释放 FUnzip
  FDecrypted.Free;
  FKeys.Free;
  inherited;
end;

function TCnDecryptZipCompressStream.Read(var Buffer;
  Count: Integer): Integer;
begin
  // 外界 Read 时从 FDecrypted 读出解压缩后的内容并解密
  Result := FUnzip.Read(Buffer, Count);
end;

function TCnDecryptZipCompressStream.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

function TCnDecryptZipCompressStream.Write(const Buffer;
  Count: Integer): Integer;
begin
  raise ECnZipException.CreateRes(@SZipNotImplemented);
end;

initialization
  FZipCompressionHandlers := TClassList.Create;
  RegisterCompressionHandlerClass(TCnZipDefaultCompressionHandler);

finalization
  FZipCompressionHandlers.Free;

end.
