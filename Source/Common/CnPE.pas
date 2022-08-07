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

unit CnPE;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：解析 PE 文件的工具单元
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：该单元实现了部分 PE 格式解析
*           PE 文件的格式分类大概如下：
*           +------------------------------------------------------------------+
*           | IMAGE_DOS_HEADER  64 字节、MZ、e_lfanew 是 PE 头的文件偏移
*           +------------------------------------------------------------------+
*           | IMAGE_NT_HEADERS  -- Signature 4 字节
*           |                   -- IMAGE_FILE_HEADER 40 字节
*           |                      -- 包含 x86/x64、Section 数，属性与后面可选块的大小
*           |                   -- IMAGE_OPTIONAL_HEADER 32/64 位下 $E0/$F0 字节
*           |                      -- 包含基址、入口、版本号、多个数据目录项等
*           |                      -- 数据目录项有导出表、可能多个输入表、调试信息等
*           +------------------------------------------------------------------+
*           | IMAGE_SECTION_HEADER[] 数组，每个 40 字节
*           |                   -- 包含名字、基地址、大小、文件偏移、Section 属性等
*           +------------------------------------------------------------------+
*           | 空隙
*           +------------------------------------------------------------------+
*           | 各个 Section 排列
*           +------------------------------------------------------------------+
*           | 各个 Section 排列
*           +------------------------------------------------------------------+
* 开发平台：PWin7 + Delphi 5
* 兼容测试：Win32/Win64
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.08.07
*               创建单元,实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, CnNative;

type
  ECnPEException = class(Exception);

  TCnPEParseMode = (ppmInvalid, ppmFile, ppmMemoryModule);
  {* 解析 PE 的模式，非法、纯文件、内存模块（已加载入内存并重定位过的）}

  TCnPE = class
  {* 解析一个 PE 文件的类，兼容 32 位和 64 位 PE 文件，自身也要 32 位和 64 位都能跑}
  private
    FParseMode: TCnPEParseMode;
    FPEFile: string;
    FModule: HMODULE;
    FFileHandle: THandle;
    FMapHandle: THandle;
    FBaseAddress: Pointer;
    FDosHeader: PImageDosHeader;
    FNtHeaders: PImageNtHeaders;
    FFileHeader: PImageFileHeader;
    FOptionalHeader: Pointer;
    // 32 位 PE 文件时指向类型是 PImageOptionalHeader。64 位时指向类型是 PImageOptionalHeader64
    FSectionHeader: PImageSectionHeader; // 紧接着 OptionalHeader 后

    FOptionalMajorLinkerVersion: Byte;
    FOptionalMinorLinkerVersion: Byte;
    FOptionalCheckSum: DWORD;
    FOptionalSizeOfInitializedData: DWORD;
    FOptionalSizeOfStackReserve: DWORD;
    FOptionalSizeOfUninitializedData: DWORD;
    FOptionalBaseOfData: DWORD;
    FOptionalSizeOfHeapReserve: DWORD;
    FOptionalLoaderFlags: DWORD;
    FFileTimeDateStamp: DWORD;
    FOptionalSizeOfStackCommit: DWORD;
    FOptionalImageBase: DWORD;
    FOptionalAddressOfEntryPoint: DWORD;
    FOptionalNumberOfRvaAndSizes: DWORD;
    FOptionalSizeOfImage: DWORD;
    FFileNumberOfSymbols: DWORD;
    FOptionalSectionAlignment: DWORD;
    FOptionalSizeOfHeaders: DWORD;
    FOptionalSizeOfCode: DWORD;
    FOptionalSizeOfHeapCommit: DWORD;
    FOptionalBaseOfCode: DWORD;
    FFilePointerToSymbolTable: DWORD;
    FOptionalWin32VersionValue: DWORD;
    FOptionalFileAlignment: DWORD;
    FDosLfanew: LongInt;
    FDosSs: Word;
    FOptionalMagic: Word;
    FDosCblp: Word;
    FDosCrlc: Word;
    FDosSp: Word;
    FFileCharacteristics: Word;
    FDosOeminfo: Word;
    FDosMinalloc: Word;
    FDosMaxalloc: Word;
    FDosLfarlc: Word;
    FOptionalMinorOperatingSystemVersion: Word;
    FOptionalMinorSubsystemVersion: Word;
    FDosCs: Word;
    FOptionalMajorImageVersion: Word;
    FOptionalSubsystem: Word;
    FOptionalDllCharacteristics: Word;
    FFileSizeOfOptionalHeader: Word;
    FOptionalMinorImageVersion: Word;
    FDosIp: Word;
    FOptionalMajorOperatingSystemVersion: Word;
    FDosOvno: Word;
    FFileNumberOfSections: Word;
    FDosMagic: Word;
    FDosCparhdr: Word;
    FDosOemid: Word;
    FDosCp: Word;
    FFileMachine: Word;
    FDosCsum: Word;
    FOptionalMajorSubsystemVersion: Word;
    FSignature: DWORD;
    FOptionalSizeOfHeapCommit64: TUInt64;
    FOptionalSizeOfStackCommit64: TUInt64;
    FOptionalSizeOfStackReserve64: TUInt64;
    FOptionalSizeOfHeapReserve64: TUInt64;
    FOptionalImageBase64: TUInt64;
    function GetDataDirectorySize(Index: Integer): DWORD;
    function GetDataDirectory(Index: Integer): PImageDataDirectory;
    function GetDataDirectoryVirtualAddress(Index: Integer): DWORD;
    function GetSectionHeader(Index: Integer): PImageSectionHeader;
    function GetIsDll: Boolean;
    function GetIsExe: Boolean;
    function GetIsWin32: Boolean;
    function GetIsWin64: Boolean;
    function GetIsSys: Boolean;

  public
    constructor Create(const APEFileName: string); overload;
    constructor Create(AModuleHandle: HMODULE); overload;

    destructor Destroy; override;

    procedure ParsePE;

    // Dos 开头的属性表示是 DosHeader 中的
    property DosMagic: Word read FDosMagic;
    {* EXE标志，字符 MZ}
    property DosCblp: Word read FDosCblp;
    {* 最后一页中的字节数}
    property DosCp: Word read FDosCp;
    {* 文件中的页数}
    property DosCrlc: Word read FDosCrlc;
    {* 重定位表中的指针数}
    property DosCparhdr: Word read FDosCparhdr;
    {* 头部尺寸，以段为单位}
    property DosMinalloc: Word read FDosMinalloc;
    {* 所需的最小附加段}
    property DosMaxalloc: Word read FDosMaxalloc;
    {* 所需的最大附加段}
    property DosSs: Word read FDosSs;
    {* 初始的 SS 值（相对偏移量）}
    property DosSp: Word read FDosSp;
    {* 初始的 SP 值（相对偏移量）}
    property DosCsum: Word read FDosCsum;
    {* 校验和                         }
    property DosIp: Word read FDosIp;
    {* 初始的 IP 值}
    property DosCs: Word read FDosCs;
    {* 初始的 CS 值}
    property DosLfarlc: Word read FDosLfarlc;
    {* 重定位表的字节偏移量}
    property DosOvno: Word read FDosOvno;
    {* 覆盖号}
    // DosRes: array [0..3] of Word;    { Reserved words                   }
    property DosOemid: Word read FDosOemid;
    {* OEM 标识符}
    property DosOeminfo: Word read FDosOeminfo;
    {* OEM 信息}
    // DosRes2: array [0..9] of Word;   { Reserved words                   }
    property DosLfanew: LongInt read FDosLfanew;
    {* PE 头相对于文件的偏移地址，也就是指向 NtHeader}

    property Signature: DWORD read FSignature;
    {* PE 文件标识，PE00}

    // File 开头的属性表示是 NtHeader 中的 FileHeader 中的
    property FileMachine: Word read FFileMachine;
    {* 运行平台}
    property FileNumberOfSections: Word read FFileNumberOfSections;
    {* Section 的数量}
    property FileTimeDateStamp: DWORD read FFileTimeDateStamp;
    {* 文件创建日期和时间}
    property FilePointerToSymbolTable: DWORD read FFilePointerToSymbolTable;
    {* 指向符号表}
    property FileNumberOfSymbols: DWORD read FFileNumberOfSymbols;
    {* 符号表中的符号数量}
    property FileSizeOfOptionalHeader: Word read FFileSizeOfOptionalHeader;
    {* OptionalHeader 结构的长度}
    property FileCharacteristics: Word read FFileCharacteristics;
    {* 文件属性}

    // Optional 开头的属性表示是 NtHeader 中的 OptionalHeader 中的
    { Standard fields. }
    property OptionalMagic: Word read FOptionalMagic;
    property OptionalMajorLinkerVersion: Byte read FOptionalMajorLinkerVersion;
    property OptionalMinorLinkerVersion: Byte read FOptionalMinorLinkerVersion;
    property OptionalSizeOfCode: DWORD read FOptionalSizeOfCode;
    property OptionalSizeOfInitializedData: DWORD read FOptionalSizeOfInitializedData;
    property OptionalSizeOfUninitializedData: DWORD read FOptionalSizeOfUninitializedData;
    property OptionalAddressOfEntryPoint: DWORD read FOptionalAddressOfEntryPoint;
    property OptionalBaseOfCode: DWORD read FOptionalBaseOfCode;
    property OptionalBaseOfData: DWORD read FOptionalBaseOfData;
    { NT additional fields. }
    property OptionalImageBase: DWORD read FOptionalImageBase;
    property OptionalImageBase64: TUInt64 read FOptionalImageBase64;
    {* 64 位下是 UInt64}
    property OptionalSectionAlignment: DWORD read FOptionalSectionAlignment;
    property OptionalFileAlignment: DWORD read FOptionalFileAlignment;
    property OptionalMajorOperatingSystemVersion: Word read FOptionalMajorOperatingSystemVersion;
    property OptionalMinorOperatingSystemVersion: Word read FOptionalMinorOperatingSystemVersion;
    property OptionalMajorImageVersion: Word read FOptionalMajorImageVersion;
    property OptionalMinorImageVersion: Word read FOptionalMinorImageVersion;
    property OptionalMajorSubsystemVersion: Word read FOptionalMajorSubsystemVersion;
    property OptionalMinorSubsystemVersion: Word read FOptionalMinorSubsystemVersion;
    property OptionalWin32VersionValue: DWORD read FOptionalWin32VersionValue;
    property OptionalSizeOfImage: DWORD read FOptionalSizeOfImage;
    property OptionalSizeOfHeaders: DWORD read FOptionalSizeOfHeaders;
    property OptionalCheckSum: DWORD read FOptionalCheckSum;
    property OptionalSubsystem: Word read FOptionalSubsystem;
    property OptionalDllCharacteristics: Word read FOptionalDllCharacteristics;
    property OptionalSizeOfStackReserve: DWORD read FOptionalSizeOfStackReserve;
    property OptionalSizeOfStackReserve64: TUInt64 read FOptionalSizeOfStackReserve64;
    {* 64 位下是 UInt64}
    property OptionalSizeOfStackCommit: DWORD read FOptionalSizeOfStackCommit;
    property OptionalSizeOfStackCommit64: TUInt64 read FOptionalSizeOfStackCommit64;
    {* 64 位下是 UInt64}
    property OptionalSizeOfHeapReserve: DWORD read FOptionalSizeOfHeapReserve;
    property OptionalSizeOfHeapReserve64: TUInt64 read FOptionalSizeOfHeapReserve64;
    {* 64 位下是 UInt64}
    property OptionalSizeOfHeapCommit: DWORD read FOptionalSizeOfHeapCommit;
    property OptionalSizeOfHeapCommit64: TUInt64 read FOptionalSizeOfHeapCommit64;
    {* 64 位下是 UInt64}
    property OptionalLoaderFlags: DWORD read FOptionalLoaderFlags;
    property OptionalNumberOfRvaAndSizes: DWORD read FOptionalNumberOfRvaAndSizes;
    {* DataDirectory 的 Size，一般为 16}

    // 接下来是 DataDirectory
    property DataDirectory[Index: Integer]: PImageDataDirectory read GetDataDirectory;
    {* 第 Index 个 DataDirectory 的指针，0 到 15}
    property DataDirectoryVirtualAddress[Index: Integer]: DWORD read GetDataDirectoryVirtualAddress;
    {* 第 Index 个 DataDirectory 的具体地址}
    property DataDirectorySize[Index: Integer]: DWORD read GetDataDirectorySize;
    {* 第 Index 个 DataDirectory 的尺寸，单位字节}

    // 以及 Sections 信息
    property SectionHeader[Index: Integer]: PImageSectionHeader read GetSectionHeader;
    {* 第 Index 个 SectionHeader 的指针，0 开始}

    // 接下来细化一些特定的如 32 还是 64、属性、输入表、输出表、调试信息等
    property IsWin32: Boolean read GetIsWin32;
    property IsWin64: Boolean read GetIsWin64;
    property IsExe: Boolean read GetIsExe;
    property IsDll: Boolean read GetIsDll;
    property IsSys: Boolean read GetIsSys;
    
  end;

implementation

resourcestring
  SCnPEOpenErrorFmt = 'Can NOT Open File ''%s''';
  SCnPEFormatError = 'NOT a Valid PE File';
  SCnPEDataDirectoryIndexErrorFmt = 'Data Directory Out Of Index %d';
  SCnPESectionIndexErrorFmt = 'Section Out Of Index %d';

const
  IMAGE_FILE_MACHINE_IA64                  = $0200;  { Intel 64 }
  IMAGE_FILE_MACHINE_AMD64                 = $8664;  { AMD64 (K8) }

  IMAGE_NT_OPTIONAL_HDR32_MAGIC            = $010B;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC            = $020B;

type
  PImageOptionalHeader64 = ^TImageOptionalHeader64;
  TImageOptionalHeader64 = record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    { NT additional fields. }
    ImageBase: TUInt64;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: TUInt64;
    SizeOfStackCommit: TUInt64;
    SizeOfHeapReserve: TUInt64;
    SizeOfHeapCommit: TUInt64;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;

function MapFileToPointer(const FileName: string; out FileHandle, MapHandle: THandle;
  out Address: Pointer): Boolean;
begin
  // 打开文件、创建映射、映射地址
  Result := False;
  FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
                FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
                FILE_FLAG_SEQUENTIAL_SCAN, 0);

  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if MapHandle <> 0 then
    begin
      Address := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
      if Address <> nil then
      begin
        Result := True; // 成功返回时，三个值都是有效的
        Exit;
      end
      else // 如果创建映射成功，但地址映射失败，就需要关闭创建映射
      begin
        CloseHandle(MapHandle);
        MapHandle := INVALID_HANDLE_VALUE;
      end;
    end
    else // 如果打开文件成功，但创建映射失败，就需要关闭文件
    begin
      CloseHandle(FileHandle);
      MapHandle := INVALID_HANDLE_VALUE;
    end;
  end;
end;

function UnMapFileFromPointer(var FileHandle, MapHandle: THandle;
  var Address: Pointer): Boolean;
begin
  UnmapViewOfFile(Address);
  Address := nil;

  CloseHandle(MapHandle);
  MapHandle := INVALID_HANDLE_VALUE;

  CloseHandle(FileHandle);
  FileHandle := INVALID_HANDLE_VALUE;

  Result := True;
end;

{ TCnPE }

constructor TCnPE.Create(const APEFileName: string);
begin
  inherited Create;
  FFileHandle := INVALID_HANDLE_VALUE;
  FMapHandle := INVALID_HANDLE_VALUE;

  FPEFile := APEFileName;
  FParseMode := ppmFile;
end;

constructor TCnPE.Create(AModuleHandle: HMODULE);
begin
  inherited Create;
  FFileHandle := INVALID_HANDLE_VALUE;
  FMapHandle := INVALID_HANDLE_VALUE;

  FModule := AModuleHandle;
  FParseMode := ppmMemoryModule;
end;

destructor TCnPE.Destroy;
begin
  if FParseMode = ppmFile then
    UnMapFileFromPointer(FFileHandle, FMapHandle, FBaseAddress);
  inherited;
end;

function TCnPE.GetDataDirectory(Index: Integer): PImageDataDirectory;
begin
  if (Index < 0) or (DWORD(Index) >= FOptionalNumberOfRvaAndSizes) then
    raise ECnPEException.CreateFmt(SCnPEDataDirectoryIndexErrorFmt, [Index]);

  if IsWin32 then
    Result := @(PImageOptionalHeader(FOptionalHeader)^.DataDirectory[Index])
  else if IsWin64 then
    Result := @(PImageOptionalHeader64(FOptionalHeader)^.DataDirectory[Index])
  else
    Result := nil;
end;

function TCnPE.GetDataDirectoryVirtualAddress(Index: Integer): DWORD;
var
  P: PImageDataDirectory;
begin
  P := DataDirectory[Index];
  if P <> nil then
    Result := P^.VirtualAddress
  else
    Result := 0;
end;

function TCnPE.GetDataDirectorySize(Index: Integer): DWORD;
var
  P: PImageDataDirectory;
begin
  P := DataDirectory[Index];
  if P <> nil then
    Result := P^.Size
  else
    Result := 0;
end;

function TCnPE.GetIsDll: Boolean;
begin
  Result := (FFileHeader^.Characteristics and IMAGE_FILE_DLL) <> 0;
end;

function TCnPE.GetIsExe: Boolean;
begin
  Result := (FFileHeader^.Characteristics and IMAGE_FILE_EXECUTABLE_IMAGE) <> 0; // FIXME: 不对，这个标记指的是打包完毕了，没链接错误
end;

function TCnPE.GetIsSys: Boolean;
begin
  Result := (FFileHeader^.Characteristics and IMAGE_FILE_SYSTEM) <> 0;
end;

function TCnPE.GetIsWin32: Boolean;
begin
  Result := ((FFileHeader^.Machine and IMAGE_FILE_MACHINE_I386) <> 0) and
    (FOptionalMagic = IMAGE_NT_OPTIONAL_HDR32_MAGIC);
end;

function TCnPE.GetIsWin64: Boolean;
begin
  Result := ((FFileHeader^.Machine = IMAGE_FILE_MACHINE_IA64) or
    (FFileHeader^.Machine = IMAGE_FILE_MACHINE_AMD64)) and
    (FOptionalMagic = IMAGE_NT_OPTIONAL_HDR64_MAGIC);
end;

function TCnPE.GetSectionHeader(Index: Integer): PImageSectionHeader;
begin
  if (Index < 0) or (Index >= Integer(FFileNumberOfSections)) then
    raise ECnPEException.CreateFmt(SCnPESectionIndexErrorFmt, [Index]);

  Result := PImageSectionHeader(TCnNativeInt(FSectionHeader) + Index * SizeOf(TImageSectionHeader));
end;

procedure TCnPE.ParsePE;
var
  P: PByte;
  OH32: PImageOptionalHeader;
  OH64: PImageOptionalHeader64;
begin
  if FParseMode = ppmFile then
  begin
    if not MapFileToPointer(FPEFile, FFileHandle, FMapHandle, FBaseAddress) then
      raise ECnPEException.CreateFmt(SCnPEOpenErrorFmt, [FPEFile]);
  end
  else if FParseMode = ppmMemoryModule then
  begin
    FBaseAddress := Pointer(FModule);
  end;

  FDosHeader := PImageDosHeader(FBaseAddress);
  if FDosHeader^.e_magic <> IMAGE_DOS_SIGNATURE then
    raise ECnPEException.Create(SCnPEFormatError);

  P := PByte(FBaseAddress);
  Inc(P, FDosHeader^._lfanew);

  FNtHeaders := PImageNtHeaders(P);
  if FNtHeaders^.Signature <> IMAGE_NT_SIGNATURE then
    raise ECnPEException.Create(SCnPEFormatError);

  FFileHeader := @FNtHeaders^.FileHeader;
  FOptionalHeader := @FNtHeaders^.OptionalHeader;

  // 四个大 Header 都指好了，开始赋值，先是 DosHeader
  FDosMagic := FDosHeader^.e_magic;
  FDosCblp := FDosHeader^.e_cblp;
  FDosCp := FDosHeader^.e_cp;
  FDosCrlc := FDosHeader^.e_crlc;
  FDosCparhdr := FDosHeader^.e_cparhdr;
  FDosMinalloc := FDosHeader^.e_minalloc;
  FDosMaxalloc := FDosHeader^.e_maxalloc;
  FDosSs := FDosHeader^.e_ss;
  FDosSp := FDosHeader^.e_sp;
  FDosCsum := FDosHeader^.e_csum;
  FDosIp := FDosHeader^.e_ip;
  FDosCs := FDosHeader^.e_cs;
  FDosLfarlc := FDosHeader^.e_lfarlc;
  FDosOvno := FDosHeader^.e_ovno;
  FDosOemid := FDosHeader^.e_oemid;
  FDosOeminfo := FDosHeader^.e_oeminfo;
  FDosLfanew := FDosHeader^._lfanew;

  // Signature
  FSignature := FNtHeaders^.Signature;

  // 然后是 FileHeader
  FFileMachine := FFileHeader^.Machine;
  FFileNumberOfSections := FFileHeader^.NumberOfSections;
  FFileTimeDateStamp := FFileHeader^.TimeDateStamp;
  FFilePointerToSymbolTable := FFileHeader^.PointerToSymbolTable;
  FFileNumberOfSymbols := FFileHeader^.NumberOfSymbols;
  FFileSizeOfOptionalHeader := FFileHeader^.SizeOfOptionalHeader;
  FFileCharacteristics := FFileHeader^.Characteristics;

  // 然后是 OptionalHeader
  if FFileSizeOfOptionalHeader = SizeOf(TImageOptionalHeader) then // 32 位
  begin
    OH32 := PImageOptionalHeader(FOptionalHeader);

    FOptionalMagic := OH32^.Magic;
    FOptionalMajorLinkerVersion := OH32^.MajorLinkerVersion;
    FOptionalMinorLinkerVersion := OH32^.MinorLinkerVersion;
    FOptionalSizeOfCode := OH32^.SizeOfCode;
    FOptionalSizeOfInitializedData := OH32^.SizeOfInitializedData;
    FOptionalSizeOfUninitializedData := OH32^.SizeOfUninitializedData;
    FOptionalAddressOfEntryPoint := OH32^.AddressOfEntryPoint;
    FOptionalBaseOfCode := OH32^.BaseOfCode;
    FOptionalBaseOfData := OH32^.BaseOfData;

    FOptionalImageBase := OH32^.ImageBase;
    FOptionalSectionAlignment := OH32^.SectionAlignment;
    FOptionalFileAlignment := OH32^.FileAlignment;
    FOptionalMajorOperatingSystemVersion := OH32^.MajorOperatingSystemVersion;
    FOptionalMinorOperatingSystemVersion := OH32^.MinorOperatingSystemVersion;
    FOptionalMajorImageVersion := OH32^.MajorImageVersion;
    FOptionalMinorImageVersion := OH32^.MinorImageVersion;
    FOptionalMajorSubsystemVersion := OH32^.MajorSubsystemVersion;
    FOptionalMinorSubsystemVersion := OH32^.MinorSubsystemVersion;
    FOptionalWin32VersionValue := OH32^.Win32VersionValue;
    FOptionalSizeOfImage := OH32^.SizeOfImage;
    FOptionalSizeOfHeaders := OH32^.SizeOfHeaders;
    FOptionalCheckSum := OH32^.CheckSum;
    FOptionalSubsystem := OH32^.Subsystem;
    FOptionalDllCharacteristics := OH32^.DllCharacteristics;
    FOptionalSizeOfStackReserve := OH32^.SizeOfStackReserve;
    FOptionalSizeOfStackCommit := OH32^.SizeOfStackCommit;
    FOptionalSizeOfHeapReserve := OH32^.SizeOfHeapReserve;
    FOptionalSizeOfHeapCommit := OH32^.SizeOfHeapCommit;
    FOptionalLoaderFlags := OH32^.LoaderFlags;
    FOptionalNumberOfRvaAndSizes := OH32^.NumberOfRvaAndSizes;
  end
  else if FFileSizeOfOptionalHeader = SizeOf(TImageOptionalHeader64) then // 64 位
  begin
    OH64 := PImageOptionalHeader64(FOptionalHeader);

    FOptionalMagic := OH64^.Magic;
    FOptionalMajorLinkerVersion := OH64^.MajorLinkerVersion;
    FOptionalMinorLinkerVersion := OH64^.MinorLinkerVersion;
    FOptionalSizeOfCode := OH64^.SizeOfCode;
    FOptionalSizeOfInitializedData := OH64^.SizeOfInitializedData;
    FOptionalSizeOfUninitializedData := OH64^.SizeOfUninitializedData;
    FOptionalAddressOfEntryPoint := OH64^.AddressOfEntryPoint;
    FOptionalBaseOfCode := OH64^.BaseOfCode;
    FOptionalBaseOfData := 0;  // 64 位没有 OH64^.BaseOfData;

    FOptionalImageBase64 := OH64^.ImageBase;
    FOptionalSectionAlignment := OH64^.SectionAlignment;
    FOptionalFileAlignment := OH64^.FileAlignment;
    FOptionalMajorOperatingSystemVersion := OH64^.MajorOperatingSystemVersion;
    FOptionalMinorOperatingSystemVersion := OH64^.MinorOperatingSystemVersion;
    FOptionalMajorImageVersion := OH64^.MajorImageVersion;
    FOptionalMinorImageVersion := OH64^.MinorImageVersion;
    FOptionalMajorSubsystemVersion := OH64^.MajorSubsystemVersion;
    FOptionalMinorSubsystemVersion := OH64^.MinorSubsystemVersion;
    FOptionalWin32VersionValue := OH64^.Win32VersionValue;
    FOptionalSizeOfImage := OH64^.SizeOfImage;
    FOptionalSizeOfHeaders := OH64^.SizeOfHeaders;
    FOptionalCheckSum := OH64^.CheckSum;
    FOptionalSubsystem := OH64^.Subsystem;
    FOptionalDllCharacteristics := OH64^.DllCharacteristics;
    FOptionalSizeOfStackReserve64 := OH64^.SizeOfStackReserve;
    FOptionalSizeOfStackCommit64 := OH64^.SizeOfStackCommit;
    FOptionalSizeOfHeapReserve64 := OH64^.SizeOfHeapReserve;
    FOptionalSizeOfHeapCommit64 := OH64^.SizeOfHeapCommit;
    FOptionalLoaderFlags := OH64^.LoaderFlags;
    FOptionalNumberOfRvaAndSizes := OH64^.NumberOfRvaAndSizes;
  end;

  FSectionHeader := PImageSectionHeader(TCnNativeInt(FOptionalHeader) + FFileSizeOfOptionalHeader);
end;

end.
