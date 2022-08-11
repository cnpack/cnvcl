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
*           概念：文件中一个数据位置相对于文件头第一个字节的偏移称为相对地址（RA）
*                 加载进内存后，一个数据相对于程序开始处的偏移称为相对虚拟地址（RVA）
*           PE 文件中大部分偏移都是 RVA，少部分和加载无关的使用 RA
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
*           | 空隙（导致了 RA 和 RVA 的差异）
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

  TCnPEExportItem = packed record
  {* 代表一个输出表项，包括名字、序号与真实地址}
    Name: string;
    Ordinal: DWORD;
    Address: Pointer;
  end;
  PCnPEExportItem = ^TCnPEExportItem;

  TCnPE = class
  {* 解析一个 PE 文件的类，兼容 32 位和 64 位 PE 文件，自身也要 32 位和 64 位都能跑}
  private
    FMode: TCnPEParseMode;
    FPEFile: string;
    FModule: HMODULE;
    FFileHandle: THandle;
    FMapHandle: THandle;
    FBaseAddress: Pointer;
    // 文件模式时是被 Map 到内存的低地址。内存模式时是 HModule，是 PE 加载展开后的基地址

    FDosHeader: PImageDosHeader;
    FNtHeaders: PImageNtHeaders;
    FFileHeader: PImageFileHeader;
    FOptionalHeader: Pointer;
    // 32 位 PE 文件时指向类型是 PImageOptionalHeader。64 位时指向类型是 PImageOptionalHeader64
    FSectionHeader: PImageSectionHeader; // 紧接着 OptionalHeader 后

    FDirectoryExport: PImageExportDirectory; // 输出表的结构
    FExportItems: array of TCnPEExportItem;  // 所有输出表的内容

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
    FExportName: AnsiString;
    FExportNumberOfNames: DWORD;
    FExportNumberOfFunctions: DWORD;
    FExportBase: DWORD;
    function GetDataDirectorySize(Index: Integer): DWORD;
    function GetDataDirectory(Index: Integer): PImageDataDirectory;
    function GetDataDirectoryVirtualAddress(Index: Integer): DWORD;
    function GetDataDirectoryContent(Index: Integer): Pointer;
    function GetSectionHeader(Index: Integer): PImageSectionHeader;
    function GetIsDll: Boolean;
    function GetIsExe: Boolean;
    function GetIsWin32: Boolean;
    function GetIsWin64: Boolean;
    function GetIsSys: Boolean;
    function GetDataDirectoryCount: Integer;
    function GetSectionCount: Integer;
    function GetSectionCharacteristics(Index: Integer): DWORD;
    function GetSectionContent(Index: Integer): Pointer;
    function GetSectionVirtualSize(Index: Integer): DWORD;
    function GetSectionName(Index: Integer): AnsiString;
    function GetSectionNumberOfLinenumbers(Index: Integer): Word;
    function GetSectionNumberOfRelocations(Index: Integer): Word;
    function GetSectionPointerToLinenumbers(Index: Integer): DWORD;
    function GetSectionPointerToRawData(Index: Integer): DWORD;
    function GetSectionPointerToRelocations(Index: Integer): DWORD;
    function GetSectionSizeOfRawData(Index: Integer): DWORD;
    function GetSectionVirtualAddress(Index: Integer): DWORD;
    function GetSectionContentSize(Index: Integer): DWORD;
    function GetExportFunctionItem(Index: Integer): PCnPEExportItem;

  protected
    function RvaToActual(Rva: DWORD): Pointer;
    {* RVA 转换成内存中的真实地址，可以直接访问}
    function GetSectionHeaderFromRva(Rva: DWORD): PImageSectionHeader;
    {* 根据 RVA 找到它落在哪个 Section 里，返回其 SectionHeader}

    procedure ParseHeaders;
    procedure ParseExports;
  public
    constructor Create(const APEFileName: string); overload;
    constructor Create(AModuleHandle: HMODULE); overload;

    destructor Destroy; override;

    procedure Parse;
    {* 分析 PE 文件，调用成功后才能使用内部各属性}

    property Mode: TCnPEParseMode read FMode;
    {* PE 加载模式}

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
    // DosRes: array [0..3] of Word;    { Reserved words}
    property DosOemid: Word read FDosOemid;
    {* OEM 标识符}
    property DosOeminfo: Word read FDosOeminfo;
    {* OEM 信息}
    // DosRes2: array [0..9] of Word;   { Reserved words}
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

    // 接下来是 DataDirectory 信息
    property DataDirectoryCount: Integer read GetDataDirectoryCount;
    {* DataDirectory 的数量，内部是 NumberOfRvaAndSizes}
    property DataDirectory[Index: Integer]: PImageDataDirectory read GetDataDirectory;
    {* 第 Index 个 DataDirectory 的指针，0 到 15}
    property DataDirectoryContent[Index: Integer]: Pointer read GetDataDirectoryContent;
    {* 第 Index 个 DataDirectory 的实际地址，通过此地址可以直接访问其内容}

    property DataDirectoryVirtualAddress[Index: Integer]: DWORD read GetDataDirectoryVirtualAddress;
    {* 第 Index 个 DataDirectory 的偏移地址}
    property DataDirectorySize[Index: Integer]: DWORD read GetDataDirectorySize;
    {* 第 Index 个 DataDirectory 的尺寸，单位字节}

    // Sections 信息。其中 PE 加载器应该将 Section 的物理文件偏移 PointerToRawData 处的数据映射入内存的 VritualAddress 处
    property SectionCount: Integer read GetSectionCount;
    {* Section 的数量，内部是 NumberOfSections}
    property SectionHeader[Index: Integer]: PImageSectionHeader read GetSectionHeader;
    {* 第 Index 个 SectionHeader 的指针，0 开始}
    property SectionContent[Index: Integer]: Pointer read GetSectionContent;
    {* 第 Index 个 Section 的实际地址，通过此地址可以直接访问其内容，内部要区分文件模式还是内存加载模式}
    property SectionContentSize[Index: Integer]: DWORD read GetSectionContentSize;
    {* 第 Index 个 Section 的实际大小，取 VirtualSize 与 SizeOfRawData 中的较小者}

    property SectionName[Index: Integer]: AnsiString read GetSectionName;
    {* 第 Index 个 Section 的名称}
    property SectionVirtualSize[Index: Integer]: DWORD read GetSectionVirtualSize;
    {* 第 Index 个 Section 的 Misc 复用字段的内容，一般用 VirtualSize，
      指加载进内存后的实际大小}
    property SectionVirtualAddress[Index: Integer]: DWORD read GetSectionVirtualAddress;
    {* 第 Index 个 Section 的 RVA 偏移，也就是 PE 加载进内存后该节相对基址的偏移（RVA）}
    property SectionSizeOfRawData[Index: Integer]: DWORD read GetSectionSizeOfRawData;
    {* 第 Index 个 Section 在文件中的原始尺寸，一般被对齐过，可能比 VirtualSize 更大}
    property SectionPointerToRawData[Index: Integer]: DWORD read GetSectionPointerToRawData;
    {* 第 Index 个 Section 在文件中的偏移量（RA）}
    property SectionPointerToRelocations[Index: Integer]: DWORD read GetSectionPointerToRelocations;
    {* 第 Index 个 Section 的 PointerToRelocations}
    property SectionPointerToLinenumbers[Index: Integer]: DWORD read GetSectionPointerToLinenumbers;
    {* 第 Index 个 Section 的 PointerToLinenumbers}
    property SectionNumberOfRelocations[Index: Integer]: Word read GetSectionNumberOfRelocations;
    {* 第 Index 个 Section 的 NumberOfRelocations}
    property SectionNumberOfLinenumbers[Index: Integer]: Word read GetSectionNumberOfLinenumbers;
    {* 第 Index 个 Section 的 NumberOfLinenumbers}
    property SectionCharacteristics[Index: Integer]: DWORD read GetSectionCharacteristics;
    {* 第 Index 个 Section 的 Characteristics}

    // 接下来细化一些特定的如 32 还是 64、属性、输入表、输出表、调试信息等
    property IsWin32: Boolean read GetIsWin32;
    {* 本 PE 文件是否 Win32 格式}
    property IsWin64: Boolean read GetIsWin64;
    {* 本 PE 文件是否 Win64 格式}
    property IsExe: Boolean read GetIsExe;
    {* 本 PE 文件是否为独立运行的 EXE}
    property IsDll: Boolean read GetIsDll;
    {* 本 PE 文件是否 DLL}
    property IsSys: Boolean read GetIsSys;
    {* 本 PE 文件是否 SYS 文件}

    // 输出表信息
    property ExportName: AnsiString read FExportName;
    {* 输出表名称，一般是 DLL 文件名}
    property ExportBase: DWORD read FExportBase;
    {* 输出表的函数序号起始值}
    property ExportNumberOfFunctions: DWORD read FExportNumberOfFunctions;
    {* 输出的函数总数，包括有名字的和没名字的}
    property ExportNumberOfNames: DWORD read FExportNumberOfNames;
    {* 以有名字方式输出的函数总数}
    property ExportFunctionItem[Index: Integer]: PCnPEExportItem read GetExportFunctionItem;
    {* 获取第 Index 个输出函数的记录指针，Index 从 0 到 FExportNumberOfFunctions，注意 Index 不等于 Ordinal}
  end;

implementation

resourcestring
  SCnPEOpenErrorFmt = 'Can NOT Open File ''%s''';
  SCnPEFormatError = 'NOT a Valid PE File';
  SCnPEDataDirectoryIndexErrorFmt = 'Data Directory Out Of Index %d';
  SCnPESectionIndexErrorFmt = 'Section Out Of Index %d';
  SCnPEExportIndexErrorFmt = 'Export Item Out Of Index %d';

const
  IMAGE_FILE_MACHINE_IA64                  = $0200;  { Intel 64 }
  IMAGE_FILE_MACHINE_AMD64                 = $8664;  { AMD64 (K8) }

  IMAGE_NT_OPTIONAL_HDR32_MAGIC            = $010B;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC            = $020B;

type
{$IFDEF SUPPORT_32_AND_64}
  PImageOptionalHeader = PImageOptionalHeader32;
{$ENDIF}

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

function ExtractNewString(Ptr: Pointer; MaxLen: Integer = 0): AnsiString;
var
  L: Integer;
begin
  Result := '';
  if Ptr <> nil then
  begin
    L := StrLen(PAnsiChar(Ptr));
    if L > 0 then
      Result := StrNew(PAnsiChar(Ptr));
  end;
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
  FMode := ppmFile;
end;

constructor TCnPE.Create(AModuleHandle: HMODULE);
begin
  inherited Create;
  FFileHandle := INVALID_HANDLE_VALUE;
  FMapHandle := INVALID_HANDLE_VALUE;

  FModule := AModuleHandle;
  FMode := ppmMemoryModule;
end;

destructor TCnPE.Destroy;
begin
  if FMode = ppmFile then
    UnMapFileFromPointer(FFileHandle, FMapHandle, FBaseAddress);
  inherited;
end;

function TCnPE.GetIsDll: Boolean;
begin
  Result := (FFileHeader^.Characteristics and IMAGE_FILE_DLL) <> 0;
end;

function TCnPE.GetIsExe: Boolean;
begin
  Result := ((FFileHeader^.Characteristics and IMAGE_FILE_EXECUTABLE_IMAGE) <> 0) // 这个标记指的是打包完毕了，没链接错误，并不特指 EXE
    and not GetIsDll and not GetIsSys;
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

procedure TCnPE.Parse;
begin
  if FMode = ppmFile then
  begin
    if not MapFileToPointer(FPEFile, FFileHandle, FMapHandle, FBaseAddress) then
      raise ECnPEException.CreateFmt(SCnPEOpenErrorFmt, [FPEFile]);
  end
  else if FMode = ppmMemoryModule then
  begin
    FBaseAddress := Pointer(FModule);
  end;

  // 解析各头
  ParseHeaders;

  // 解析输出表
  ParseExports;
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

function TCnPE.GetDataDirectoryContent(Index: Integer): Pointer;
var
  D: DWORD;
begin
  D := GetDataDirectoryVirtualAddress(Index);
  Result := RvaToActual(D);
end;

function TCnPE.GetDataDirectoryCount: Integer;
begin
  Result := FOptionalNumberOfRvaAndSizes;
end;

function TCnPE.GetSectionCount: Integer;
begin
  Result := FFileNumberOfSections;
end;

function TCnPE.GetSectionCharacteristics(Index: Integer): DWORD;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.Characteristics
  else
    Result := 0;
end;

function TCnPE.GetSectionContent(Index: Integer): Pointer;
var
  D: DWORD;
begin
  Result := nil;
  if FMode = ppmFile then
  begin
    D := GetSectionPointerToRawData(Index); // 拿文件偏移
    if D = 0 then
      D := GetSectionVirtualAddress(Index); // 存在文件偏移为 0 的情况
    Result := RvaToActual(D);
  end
  else if FMode = ppmMemoryModule then
  begin
    D := GetSectionVirtualAddress(Index);   // 拿已加载展开后的内存偏移
    Result := RvaToActual(D);
  end;
end;

function TCnPE.GetSectionVirtualSize(Index: Integer): DWORD;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.Misc.VirtualSize
  else
    Result := 0;
end;

function TCnPE.GetSectionName(Index: Integer): AnsiString;
var
  P: PImageSectionHeader;
begin
  Result := '';
  P := SectionHeader[Index];
  if P <> nil then
    Result := ExtractNewString(@P^.Name[0]);
end;

function TCnPE.GetSectionNumberOfLinenumbers(Index: Integer): Word;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.NumberOfLinenumbers
  else
    Result := 0;
end;

function TCnPE.GetSectionNumberOfRelocations(Index: Integer): Word;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.NumberOfRelocations
  else
    Result := 0;
end;

function TCnPE.GetSectionPointerToLinenumbers(Index: Integer): DWORD;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.PointerToLinenumbers
  else
    Result := 0;
end;

function TCnPE.GetSectionPointerToRawData(Index: Integer): DWORD;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.PointerToRawData
  else
    Result := 0;
end;

function TCnPE.GetSectionPointerToRelocations(Index: Integer): DWORD;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.PointerToRelocations
  else
    Result := 0;
end;

function TCnPE.GetSectionSizeOfRawData(Index: Integer): DWORD;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.SizeOfRawData
  else
    Result := 0;
end;

function TCnPE.GetSectionVirtualAddress(Index: Integer): DWORD;
var
  P: PImageSectionHeader;
begin
  P := SectionHeader[Index];
  if P <> nil then
    Result := P^.VirtualAddress
  else
    Result := 0;
end;

function TCnPE.GetSectionContentSize(Index: Integer): DWORD;
var
  T: DWORD;
begin
  Result := GetSectionSizeOfRawData(Index);
  T := GetSectionVirtualSize(Index);
  if (T <> 0) and (Result <> 0) and (Result > T) then
    Result := T
  else if Result = 0 then
    Result := T;
end;

function TCnPE.RvaToActual(Rva: DWORD): Pointer;
var
  SH: PImageSectionHeader;
begin
  Result := nil;

  // PE 加载入内存展开后，全部符合此规则
  if FMode = ppmMemoryModule then
    Result := Pointer(TCnNativeUInt(FBaseAddress) + Rva)
  else if FMode = ppmFile then
  begin
    // 如果是文件模式，头部符合此规则；各节因有拉伸铺开，和文件内直接访问有差别
    SH := GetSectionHeaderFromRva(Rva);
    if SH <> nil then
    begin
      // 找到该 RVA 与 Section 头部 RVA 的距离，再加上 Section 头的文件偏移
      Result := Pointer(TCnNativeUInt(FBaseAddress) +
        (Rva - SH^.VirtualAddress + SH^.PointerToRawData));
    end;
  end;
end;

function TCnPE.GetSectionHeaderFromRva(Rva: DWORD): PImageSectionHeader;
var
  I: Integer;
  SH: PImageSectionHeader;
  ER: DWORD;
begin
  Result := nil;
  for I := 0 to SectionCount - 1 do
  begin
    SH := GetSectionHeader(I);
    if SH^.SizeOfRawData = 0 then
      ER := SH^.Misc.VirtualSize
    else
      ER := SH^.SizeOfRawData;
    Inc(ER, SH^.VirtualAddress);
    if (SH^.VirtualAddress <= Rva) and (ER >= Rva) then
    begin
      Result := SH;
      Break;
    end;
  end;
end;

procedure TCnPE.ParseExports;
var
  I, J, T: DWORD;
  O: WORD;
  PAddress, PName: PDWORD;
  POrd: PWORD;
begin
  FDirectoryExport := PImageExportDirectory(DataDirectoryContent[IMAGE_DIRECTORY_ENTRY_EXPORT]);
  if FDirectoryExport = nil then
    Exit;

  if FDirectoryExport^.Name <> 0 then
    FExportName := ExtractNewString(RvaToActual(FDirectoryExport^.Name));
  FExportBase := FDirectoryExport^.Base;
  FExportNumberOfNames := FDirectoryExport^.NumberOfNames;
  FExportNumberOfFunctions := FDirectoryExport^.NumberOfFunctions;

  SetLength(FExportItems, FExportNumberOfFunctions);
  if FExportNumberOfFunctions <= 0 then
    Exit;

{
  AddressOfFunctions: ^PDWORD;     指向地址数组，下标从 AddressOfNameOrdinals 中获取，一共 NumberOfFunctions 个
  AddressOfNames: ^PDWORD;         指向名字数组  下标与序号数组内的对应，值是名字字符串的 RVA，一共 NumberOfNames 个，
  AddressOfNameOrdinals: ^PWord;   指向序号数组，下标与名字数组内的对应，值是 AddressOfFunctions 里的下标，一共 NumberOfFunctions

  NumberOfFunctions 可能大于 NumberOfNames，多的部分是没名字、仅序号输出的函数。
  序号是 AddressOfNameOrdinals 里的值加 Base
}

  // 按照 AddressofFunctions 里的顺序排列，取出地址
  PAddress := PDWORD(RvaToActual(DWORD(FDirectoryExport^.AddressOfFunctions)));
  PName := PDWORD(RvaToActual(DWORD(FDirectoryExport^.AddressOfNames)));
  POrd := PWORD(RvaToActual(DWORD(FDirectoryExport^.AddressOfNameOrdinals)));

  I := 0;
  while I < FExportNumberOfNames do
  begin
    FExportItems[I].Name := ExtractNewString(RvaToActual(PName^));  // 取名字、序号和地址

    O := POrd^;
    FExportItems[I].Ordinal := O + FExportBase;

    T := PDWORD(TCnNativeUInt(PAddress) + O * SizeOf(DWORD))^;
    if T <> 0 then
      FExportItems[I].Address := RvaToActual(T)
    else
      FExportItems[I].Address := nil;

    Inc(PName);
    Inc(POrd);
    Inc(I);
  end;

  J := I;
  I := 0;
  while I < FExportNumberOfFunctions - FExportNumberOfNames do
  begin
    O := POrd^;                                                    // 没名字的，取序号和地址
    FExportItems[J + I].Ordinal := O + FExportBase;

    T := PDWORD(TCnNativeUInt(PAddress) + O * SizeOf(DWORD))^;
    if T <> 0 then
      FExportItems[J + I].Address := RvaToActual(T)
    else
      FExportItems[J + I].Address := nil;

    Inc(POrd);
    Inc(I);
  end;
end;

procedure TCnPE.ParseHeaders;
var
  P: PByte;
  OH32: PImageOptionalHeader;
  OH64: PImageOptionalHeader64;
begin
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

function TCnPE.GetExportFunctionItem(Index: Integer): PCnPEExportItem;
begin
  if (Index < 0) or (Index >= Length(FExportItems)) then
    raise ECnPEException.CreateFmt(SCnPEExportIndexErrorFmt, [Index]);

  Result := @FExportItems[Index];
end;

end.
