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
  SysUtils, Classes, Windows, Psapi, Contnrs, CnContainers, CnNative;

const
  CN_INVALID_LINENUMBER_OFFSET = -1;

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
    FExportItems: array of TCnPEExportItem;  // 所有输出表的内容，外部可能需要排序

    FDirectoryDebug: PImageDebugDirectory;   // 调试信息表的结构

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
    FDebugType: DWORD;
    FDebugPointerToRawData: DWORD;
    FDebugAddressOfRawData: DWORD;
    FDebugSizeOfData: DWORD;
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
    function GetDebugContent: Pointer;
    function GetIsDebug: Boolean;

  protected
    function RvaToActual(Rva: DWORD; ZeroIsNil: Boolean = True): Pointer;
    {* RVA 转换成内存中的真实地址，可以直接访问。ZeroIsNil 控制当 Rva 是 0 时返回 Base 还是 nil}
    function GetSectionHeaderFromRva(Rva: DWORD): PImageSectionHeader;
    {* 根据 RVA 找到它落在哪个 Section 里，返回其 SectionHeader}

    procedure ParseHeaders;
    procedure ParseExports;
    procedure ParseDebugData;
  public
    constructor Create(const APEFileName: string); overload; virtual;
    constructor Create(AModuleHandle: HMODULE); overload; virtual;

    destructor Destroy; override;

    procedure Parse;
    {* 分析 PE 文件，调用成功后才能使用内部各属性}

    procedure SortExports;
    {* 内部针对输出函数的地址进行排序}

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

    // 接下来是 DataDirectory 信息。注意这批只是 Directory，并非指向的实际内容
    property DataDirectoryCount: Integer read GetDataDirectoryCount;
    {* DataDirectory 的数量，内部是 NumberOfRvaAndSizes}
    property DataDirectory[Index: Integer]: PImageDataDirectory read GetDataDirectory;
    {* 第 Index 个 DataDirectory 的指针，0 到 15，始终都存在}
    property DataDirectoryContent[Index: Integer]: Pointer read GetDataDirectoryContent;
    {* 第 Index 个 DataDirectory 的实际地址，通过此地址可以直接访问其内容}

    property DataDirectoryVirtualAddress[Index: Integer]: DWORD read GetDataDirectoryVirtualAddress;
    {* 第 Index 个 DataDirectory 的偏移地址}
    property DataDirectorySize[Index: Integer]: DWORD read GetDataDirectorySize;
    {* 第 Index 个 DataDirectory 的尺寸，单位字节，似乎对于固定内容来说不重要}

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
    property IsDebug: Boolean read GetIsDebug;
    {* 本 PE 文件是否包含调试信息}

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
    {* 获取第 Index 个输出函数的记录指针，Index 从 0 到 FExportNumberOfFunctions - 1，注意 Index 不等于 Ordinal}

    // 调试信息
    property DebugType: DWORD read FDebugType;
    {* 调试信息类型}
    property DebugSizeOfData: DWORD read FDebugSizeOfData;
    {* 调试数据大小}
    property DebugAddressOfRawData: DWORD read FDebugAddressOfRawData;
    {* 调试数据在内存中的 RVA}
    property DebugPointerToRawData: DWORD read FDebugPointerToRawData;
    {* 调试数据在文件中的偏移}
    property DebugContent: Pointer read GetDebugContent;
    {* 调试数据的真实指针，可拿去直接解析，尺寸为 DebugSizeOfData}
  end;

  TCnModuleDebugInfo = class
  {* 描述本进程内一模块的调试信息的基类，默认实现根据输出表追踪的功能}
  private
    FModuleFile: string;
    FModuleHandle: HMODULE;
  protected
    FPE: TCnPE;
  public
    constructor Create(AModuleHandle: HMODULE); virtual;
    destructor Destroy; override;

    function Init: Boolean; virtual;
    {* 初始化动作，供子类重载以处理各种格式的调试信息如 map/tds/td32/DebugInfo 节等}

    function GetDebugInfoFromAddr(Address: Pointer; out OutModuleFile, OutUnitName, OutProcName: string;
      out OutLineNumber, OutOffsetLineNumber, OutOffsetProc: Integer): Boolean; virtual;
    {* 从当前进程内的虚拟地址返回其模块文件名（不包含路径）、单元名（如果有的话）、当前函数名、
      当前行号、离行号的字节位置、离当前函数开始的字节位置}

    function VAFromAddr(Address: Pointer): DWORD;
    {* 根据真实地址返回模块内偏移，其实是减去模块基地址再减去代码基地址}

    property ModuleHandle: HMODULE read FModuleHandle;
    {* 当前模块的 Handle}
    property ModuleFile: string read FModuleFile;
    {* 完整文件名包括路径}
  end;

  TCnTDSourceModule = class
  {* 描述一 TD 调试信息中源文件的类}
  private
    FSegmentCount: Integer;
    FName: string;
    FSegmentArray: PDWORD;  // 指向一对 Start/End 的双 DWORD 数组
    FNameIndex: Integer;
    function GetSegmentEnd(Index: Integer): DWORD;
    function GetSegmentStart(Index: Integer): DWORD;
  public
    function IsAddressInSource(Address: DWORD): Boolean;
    {* 返回一个地址是否在本模块内}

    property NameIndex: Integer read FNameIndex write FNameIndex;
    {* 本源文件的名字索引}
    property Name: string read FName write FName;
    {* 本源文件的名字}
    property SegmentCount: Integer read FSegmentCount write FSegmentCount;
    {* 本源文件的对应段数}

    property SegmentArray: PDWORD read FSegmentArray write FSegmentArray;
    {* 每个段的起始地址的数组指针}
    property SegmentStart[Index: Integer]: DWORD read GetSegmentStart;
    {* 每个段的起始地址}
    property SegmentEnd[Index: Integer]: DWORD read GetSegmentEnd;
    {* 每个段的结束地址}
  end;

  TCnTDProcedureSymbol = class
  {* 描述一 TD 调试信息中函数或过程的类}
  private
    FNameIndex: DWORD;
    FOffset: DWORD;
    FSize: DWORD;
    FName: string;
  public
    function IsAddressInProcedure(Address: DWORD): Boolean;
    {* 返回一个地址是否在本过程内}

    property Name: string read FName write FName;
    {* 过程名字}
    property NameIndex: DWORD read FNameIndex write FNameIndex;
    {* 过程名字索引}
    property Offset: DWORD read FOffset write FOffset;
    {* 过程起始地址的偏移量}
    property Size: DWORD read FSize write FSize;
    {* 过程本身的大小}
  end;

  TCnModuleDebugInfoTD32 = class(TCnModuleDebugInfo)
  {* 解析 TD32 Debugger Info 的调试信息类，注意只解析部分堆栈所需内容}
  private
    FData: Pointer;
    FSize: DWORD;
    FStream: TMemoryStream;
    FNames: TStringList;
    FSourceModuleNames: TStringList;
    FProcedureNames: TStringList;
    FOffsets: TCnIntegerList;
    FLineNumbers: TCnIntegerList;
    procedure SyncNames; // 把 NameIndex 转换为 Name
    procedure ParseSubSection(DSE: Pointer);
    function GetSourceModules(Index: Integer): TCnTDSourceModule;
    function GetSourceModuleCount: Integer;
    function GetProcedureCount: Integer;
    function GetProcedures(Index: Integer): TCnTDProcedureSymbol;
    function GetLineNumberCount: Integer;
    function GetOffsetCount: Integer;
    function GetLineNumbers(Index: Integer): Integer;
    function GetOffsets(Index: Integer): Integer;
  public
    constructor Create(AModuleHandle: HMODULE); override;
    destructor Destroy; override;

    function Init: Boolean; override;

    function GetDebugInfoFromAddr(Address: Pointer; out OutModuleFile, OutUnitName, OutProcName: string;
      out OutLineNumber, OutOffsetLineNumber, OutOffsetProc: Integer): Boolean; override;

    property Names: TStringList read FNames;
    {* 内部的名字列表}

    property SourceModuleNames: TStringList read FSourceModuleNames;
    {* 内部的源文件名列表}
    property SourceModuleCount: Integer read GetSourceModuleCount;
    {* 内部的源文件对象数量}
    property SourceModules[Index: Integer]: TCnTDSourceModule read GetSourceModules;
    {* 内部的源文件对象列表}

    property ProcedureNames: TStringList read FProcedureNames;
    {* 内部的函数过程名列表}
    property ProcedureCount: Integer read GetProcedureCount;
    {* 内部的函数过程对象数量}
    property Procedures[Index: Integer]: TCnTDProcedureSymbol read GetProcedures;
    {* 内部的函数过程对象列表}

    property LineNumberCount: Integer read GetLineNumberCount;
    {* 行号数量}
    property LineNumbers[Index: Integer]: Integer read GetLineNumbers;
    {* 行号}
    property OffsetCount: Integer read GetOffsetCount;
    {* 偏移量数量}
    property Offsets[Index: Integer]: Integer read GetOffsets;
    {* 偏移量，原始数据已从低到高排序好的}
  end;

  TCnMapSourceModule = class
  {* Map 文件中的模块文件描述类，包括文件名与一大堆内部行号和偏移量的对应关系，
    另有段起止地址，组织结构和 TCnTDSourceModule 不同，不宜复用}
  private
    FSegStarts: TCnIntegerList;
    FSegEnds: TCnIntegerList;
    FOffsets: TCnIntegerList;       // 内部偏移量，按此排序
    FLineNumbers: TCnIntegerList;
    FName: string;
    FFileName: string;
    function GetLineNumberCount: Integer;
    function GetLineNumbers(Index: Integer): Integer;
    function GetOffsetCount: Integer;
    function GetOffsets(Index: Integer): Integer;
    function GetSegmentEnd(Index: Integer): DWORD;
    function GetSegmentStart(Index: Integer): DWORD;
    function GetSegmentCount: Integer;   // 行号列表
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddLineOffset(ALineNumber: Integer; AnOffset: DWORD);
    {* 添加行号与偏移对应值}
    procedure AddSegment(ASegStart: DWORD; ASegEnd: DWORD);
    {* 添加一个新段的起始地址和结束地址}

    property Name: string read FName write FName;
    {* 本源文件的模块名字}
    property FileName: string read FFileName write FFileName;
    {* 本源文件的文件名（可能包含路径）}

    property LineNumberCount: Integer read GetLineNumberCount;
    {* 本源文件中的行号数量}
    property LineNumbers[Index: Integer]: Integer read GetLineNumbers;
    {* 本源文件中的行号}
    property OffsetCount: Integer read GetOffsetCount;
    {* 本源文件中的偏移量数量}
    property Offsets[Index: Integer]: Integer read GetOffsets;
    {* 本源文件中的偏移量，原始数据已从低到高排序好的}

    property SegmentCount: Integer read GetSegmentCount;
    {* 本源文件的对应段数}
    property SegmentStart[Index: Integer]: DWORD read GetSegmentStart;
    {* 每个段的起始地址}
    property SegmentEnd[Index: Integer]: DWORD read GetSegmentEnd;
    {* 每个段的结束地址}
  end;

  TCnModuleDebugInfoMap = class(TCnModuleDebugInfo)
  {* 解析 Map 文件中的调试信息的类}
  private
    // 一个源文件信息包含段与其内部行号和偏移量的对应关系（多组，需组合而来，按偏移量排序）
    // 一个函数信息包含段与其内部偏移量，与下一个偏移量之差就是本函数信息的尺寸
    FSourceModuleNames: TStringList; // 存储模块名字（不是完整文件名），其 Objects 是 TCnMapSourceModule
    FProcedureNames: TStringList;    // 其 Objects 是起始地址
    function GetSourceModuleCount: Integer;
    function GetSourceModules(Index: Integer): TCnMapSourceModule;
    function GetProcedureAddress(Index: Integer): DWORD;
    function GetProcedureCount: Integer;
    function GetProcedures(Index: Integer): string;
  protected
    function MakeSureModuleExists(const SourceName: string;
      const SourceFile: string = ''): TCnMapSourceModule;
  public
    constructor Create(AModuleHandle: HMODULE); override;
    destructor Destroy; override;

    function Init: Boolean; override;

    function GetDebugInfoFromAddr(Address: Pointer; out OutModuleFile, OutUnitName, OutProcName: string;
      out OutLineNumber, OutOffsetLineNumber, OutOffsetProc: Integer): Boolean; override;

    property SourceModuleNames: TStringList read FSourceModuleNames;
    {* 内部的源文件名列表}
    property SourceModuleCount: Integer read GetSourceModuleCount;
    {* 内部的源文件对象数量}
    property SourceModules[Index: Integer]: TCnMapSourceModule read GetSourceModules;
    {* 内部的源文件对象列表}

    property ProcedureNames: TStringList read FProcedureNames;
    {* 内部的函数过程名列表}
    property ProcedureCount: Integer read GetProcedureCount;
    {* 内部的函数过程对象数量}
    property Procedures[Index: Integer]: string read GetProcedures;
    {* 内部的函数过程名}
    property ProcedureAddress[Index: Integer]: DWORD read GetProcedureAddress;
    {* 内部的函数起始地址}
  end;

  TCnInProcessModuleList = class(TObjectList)
  {* 描述本进程内所有模块的调试信息列表类，内部持有 TCnModuleDebugInfo 及其子类}
  private
    function GetItem(Index: Integer): TCnModuleDebugInfo;
    procedure SetItem(Index: Integer; const Value: TCnModuleDebugInfo);

  protected
    function CreateDebugInfoFromModule(AModuleHandle: HMODULE): TCnModuleDebugInfo;
    {* 从某模块句柄创建各具体类的调试信息对象}
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    function GetDebugInfoFromAddress(Address: Pointer): TCnModuleDebugInfo;
    {* 从某地址返回该模块的调试信息}
    function GetDebugInfoFromModule(AModuleHandle: HMODULE): TCnModuleDebugInfo;
    {* 从某模块句柄返回该模块的调试信息}

    function CreateDebugInfoFromAddress(Address: Pointer): TCnModuleDebugInfo;
    {* 从某地址创建各具体类的调试信息对象并添加到列表中，如果已存在则不重复创建但仍返回}

    property Items[Index: Integer]: TCnModuleDebugInfo read GetItem write SetItem; default;
    {* 本进程内的调试信息列表}
  end;

function CreateInProcessAllModulesList: TCnInProcessModuleList;
{* 创建当前进程中所有模块的 TCnInProcessModuleList，调用者自行释放}

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

  { Turbo Debugger 调试信息头标记}
  TD_SIGNATURE_DELPHI = $39304246; // 'FB09'
  TD_SIGNATURE_BCB    = $41304246; // 'FB0A'

  { Turbo Debugger 调试信息 Entry 中的 Subsection Types}
  TD_SUBSECTION_TYPE_MODULE         = $120;
  TD_SUBSECTION_TYPE_TYPES          = $121;
  TD_SUBSECTION_TYPE_SYMBOLS        = $124;
  TD_SUBSECTION_TYPE_ALIGN_SYMBOLS  = $125;
  TD_SUBSECTION_TYPE_SOURCE_MODULE  = $127;
  TD_SUBSECTION_TYPE_GLOBAL_SYMBOLS = $129;
  TD_SUBSECTION_TYPE_GLOBAL_TYPES   = $12B;
  TD_SUBSECTION_TYPE_NAMES          = $130;

  { Turbo Debugger 调试信息 中的 Symbol type defines}
  SYMBOL_TYPE_COMPILE        = $0001; // Compile flags symbol
  SYMBOL_TYPE_REGISTER       = $0002; // Register variable
  SYMBOL_TYPE_CONST          = $0003; // Constant symbol
  SYMBOL_TYPE_UDT            = $0004; // User-defined Type
  SYMBOL_TYPE_SSEARCH        = $0005; // Start search
  SYMBOL_TYPE_END            = $0006; // End block, procedure, with, or thunk
  SYMBOL_TYPE_SKIP           = $0007; // Skip - Reserve symbol space
  SYMBOL_TYPE_CVRESERVE      = $0008; // Reserved for Code View internal use
  SYMBOL_TYPE_OBJNAME        = $0009; // Specify name of object file

  SYMBOL_TYPE_BPREL16        = $0100; // BP relative 16:16
  SYMBOL_TYPE_LDATA16        = $0101; // Local data 16:16
  SYMBOL_TYPE_GDATA16        = $0102; // Global data 16:16
  SYMBOL_TYPE_PUB16          = $0103; // Public symbol 16:16
  SYMBOL_TYPE_LPROC16        = $0104; // Local procedure start 16:16
  SYMBOL_TYPE_GPROC16        = $0105; // Global procedure start 16:16
  SYMBOL_TYPE_THUNK16        = $0106; // Thunk start 16:16
  SYMBOL_TYPE_BLOCK16        = $0107; // Block start 16:16
  SYMBOL_TYPE_WITH16         = $0108; // With start 16:16
  SYMBOL_TYPE_LABEL16        = $0109; // Code label 16:16
  SYMBOL_TYPE_CEXMODEL16     = $010A; // Change execution model 16:16
  SYMBOL_TYPE_VFTPATH16      = $010B; // Virtual function table path descriptor 16:16

  SYMBOL_TYPE_BPREL32        = $0200; // BP relative 16:32
  SYMBOL_TYPE_LDATA32        = $0201; // Local data 16:32
  SYMBOL_TYPE_GDATA32        = $0202; // Global data 16:32
  SYMBOL_TYPE_PUB32          = $0203; // Public symbol 16:32
  SYMBOL_TYPE_LPROC32        = $0204; // Local procedure start 16:32
  SYMBOL_TYPE_GPROC32        = $0205; // Global procedure start 16:32
  SYMBOL_TYPE_THUNK32        = $0206; // Thunk start 16:32
  SYMBOL_TYPE_BLOCK32        = $0207; // Block start 16:32
  SYMBOL_TYPE_WITH32         = $0208; // With start 16:32
  SYMBOL_TYPE_LABEL32        = $0209; // Label 16:32
  SYMBOL_TYPE_CEXMODEL32     = $020A; // Change execution model 16:32
  SYMBOL_TYPE_VFTPATH32      = $020B; // Virtual function table path descriptor 16:32

type
{$IFDEF SUPPORT_32_AND_64}
  PImageOptionalHeader = PImageOptionalHeader32;
{$ELSE}
  TImageOptionalHeader32 = TImageOptionalHeader;
{$ENDIF}

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
  PImageOptionalHeader64 = ^TImageOptionalHeader64;

// ==================== Turbo Debugger 调试信息相关结构声明 ====================

  TTDFileSignature = packed record
  { TD 调试信息最头部的结构}
    Signature: DWORD;
    Offset: DWORD;        // 指向 TTDDirectoryHeader，大块的调试信息头部，大概叫 Section
  end;
  PTDFileSignature = ^TTDFileSignature;

  TTDDirectoryEntry = packed record
  {* TD 调试信息的元素，属于一个大块调试信息头的后部数组}
    SubsectionType: Word; // SubSection 类型，对应 TD_SUBSECTION_TYPE_* 常量
    ModuleIndex: Word;    // Module index
    Offset: DWORD;        // Offset from the base offset lfoBase，指向具体的调试信息 SubSection
    Size: DWORD;          // Number of bytes in subsection
  end;
  PTDDirectoryEntry = ^TTDDirectoryEntry;

  TTDDirectoryHeader = packed record
  {* 每大块 TD 调试信息的 Section 的头部，后面紧跟 DirEntryCount 个 TTDDirectoryEntry，
    每个大小 DirEntrySize，也等于 SizeOf(TTDDirectoryEntry)
    然后 lfoNextDir 指向下一个调试信息 Section 的头}
    Size: Word;           // Length of this structure
    DirEntrySize: Word;   // Length of each directory entry
    DirEntryCount: DWORD; // Number of directory entries
    lfoNextDir: DWORD;    // Offset from lfoBase of next directory.
    Flags: DWORD;         // Flags describing directory and subsection tables.
    DirEntries: array [0..0] of TTDDirectoryEntry;
  end;
  PTDDirectoryHeader = ^TTDDirectoryHeader;

  TSymbolProcInfo = packed record
  {* TD 调试信息的 Subsection 中的过程类型的符号项结构}
    pParent: DWORD;
    pEnd: DWORD;
    pNext: DWORD;
    Size: DWORD;        // Length in bytes of this procedure
    DebugStart: DWORD;  // Offset in bytes from the start of the procedure to
                        // the point where the stack frame has been set up.
    DebugEnd: DWORD;    // Offset in bytes from the start of the procedure to
                        // the point where the  procedure is  ready to  return
                        // and has calculated its return value, if any.
                        // Frame and register variables an still be viewed.
    Offset: DWORD;      // Offset portion of  the segmented address of
                        // the start of the procedure in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the procedure in the code segment
    ProcType: DWORD;    // Type of the procedure type record
    NearFar: Byte;      // Type of return the procedure makes:
                        //   0       near
                        //   4       far
    Reserved: Byte;
    NameIndex: DWORD;   // Name index of procedure
  end;

  TTDSymbolInfo = packed record
  {* TD 调试信息的 Subsection 中的符号表结构}
    Size: Word;
    SymbolType: Word;
    case Word of
      SYMBOL_TYPE_LPROC32, SYMBOL_TYPE_GPROC32:
        (Proc: TSymbolProcInfo);
      // 后面的不解析，因而不使用
  end;
  PTDSymbolInfo = ^TTDSymbolInfo;

  TTDSymbolInfos = packed record
  {* TD 调试信息的 Subsection 中的符号表结构列表}
    Signature: DWORD;
    Symbols: array [0..0] of TTDSymbolInfo;
  end;
  PTDSymbolInfos = ^TTDSymbolInfos;

  TTDSourceModuleInfo = packed record
  {* TD 调试信息的 Subsection 中的源码文件头结构}
    FileCount: Word;    // The number of source file scontributing code to segments
    SegmentCount: Word; // The number of code segments receiving code from this module

    BaseSrcFiles: array [0..0] of DWORD;
  end;
  PTDSourceModuleInfo = ^TTDSourceModuleInfo;

  TTDSourceFileEntry = packed record
  {* TD 调试信息的 Subsection 中的单个源码文件结构}
    SegmentCount: Word; // Number of segments that receive code from this source file.
    NameIndex: DWORD;   // Name index of Source file name.
    BaseSrcLines: array [0..0] of DWORD; // 这里有 SegmentCount 个，0 到 SegmentCount - 1
    // BaseSrcLines[SegmentCount] 是 TTDOffsetPair 的数组，指示当前文件的代码在每个 Segment 中的偏移头尾
  end;
  PTDSourceFileEntry = ^TTDSourceFileEntry;

  TTDOffsetPair = packed record
  {* TD 调试信息中的代码段起始地址}
    StartOffset: DWORD;
    EndOffset: DWORD;
  end;
  PTDOffsetPairArray = ^TTDOffsetPairArray;
  TTDOffsetPairArray = array [0..32767] of TTDOffsetPair;

  TTDLineMappingEntry = packed record
  {* TD 调试信息中的行号与偏移量对应关系}
    SegmentIndex: Word;  // Segment index for this table
    PairCount: Word;     // Count of the number of source line pairs to follow
    Offsets: array [0..0] of DWORD; // PairCount 个 DWORD 表示偏移
    // LineNumbers: array [0..PairCount - 1] of Word; // PairCount 个 Word 表示行号（行号超过 65536咋办？)
  end;
  PTDLineMappingEntry = ^TTDLineMappingEntry;

function CreateInProcessAllModulesList: TCnInProcessModuleList;
var
  HP: array of THandle;
  I, L: Integer;
  Cnt: DWORD;
  Info: TCnModuleDebugInfo;
begin
  Result := nil;

  // 获取 Module 列表
  if not EnumProcessModules(GetCurrentProcess, nil, 0, Cnt) then
    Exit;

  if Cnt = 0 then
    Exit;

  L := Cnt div SizeOf(THandle);
  SetLength(HP, L);
  if EnumProcessModules(GetCurrentProcess, @HP[0], Cnt, Cnt) then
  begin
    Result := TCnInProcessModuleList.Create;

    for I := 0 to L - 1 do
    begin
      Info := Result.CreateDebugInfoFromModule(HP[I]);
      if Info <> nil then
        Result.Add(Info);
    end;
  end;
end;

function HexStrToDWord(const S: string): Cardinal;
var
  R: TBytes;
  T: array[0..3] of Byte;
begin
  R := HexToBytes(S);
  if Length(R) >= SizeOf(Cardinal) then
  begin
    // 把末四个复制进 T
    Move(R[Length(R) - SizeOf(Cardinal)], T[0], SizeOf(Cardinal));
  end
  else
  begin
    PDWORD(@T[0])^ := 0;                                    // 先清零
    Move(R[0], T[SizeOf(Cardinal) - Length(R)], Length(R)); // 再把内容往高对齐复制
  end;

  Result := UInt32NetworkToHost(PDWORD(@T[0])^);
  SetLength(R, 0);
end;

// 得到本进程某虚拟地址所属的 Module，也就是模块基地址。如不属于模块则返回 0
function ModuleFromAddr(const Addr: Pointer): HMODULE;
var
  MBI: TMemoryBasicInformation;
begin
  VirtualQuery(Addr, MBI, SizeOf(MBI));
  if MBI.State <> MEM_COMMIT then
    Result := 0
  else
    Result := HMODULE(MBI.AllocationBase);
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
var
  ModName: array[0..MAX_PATH] of Char;
begin
  if FMode = ppmFile then
  begin
    if not MapFileToPointer(FPEFile, FFileHandle, FMapHandle, FBaseAddress) then
      raise ECnPEException.CreateFmt(SCnPEOpenErrorFmt, [FPEFile]);
  end
  else if FMode = ppmMemoryModule then
  begin
    FBaseAddress := Pointer(FModule);
    SetString(FPEFile, ModName, GetModuleFileName(FModule, ModName, Length(ModName)));
  end;

  // 解析各头
  ParseHeaders;

  // 解析输出表
  ParseExports;

  // 解析内部调试信息
  ParseDebugData;
end;


procedure TCnPE.ParseDebugData;
begin
  FDirectoryDebug := nil;
  try
    FDirectoryDebug := PImageDebugDirectory(DataDirectoryContent[IMAGE_DIRECTORY_ENTRY_DEBUG]);
  except
    ;
  end;

  if FDirectoryDebug = nil then
    Exit;

  FDebugType := FDirectoryDebug^._Type;
  FDebugSizeOfData := FDirectoryDebug^.SizeOfData;
  FDebugAddressOfRawData := FDirectoryDebug^.AddressOfRawData;
  FDebugPointerToRawData := FDirectoryDebug^.PointerToRawData;
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

function TCnPE.GetDebugContent: Pointer;
var
  D: DWORD;
begin
  if FMode = ppmFile then
  begin
    D := FDebugPointerToRawData;   // 拿文件偏移
    if D = 0 then
      D := FDebugAddressOfRawData; // 可能存在文件偏移为 0 的情况？
    Result := RvaToActual(D);
  end
  else if FMode = ppmMemoryModule then
  begin
    D := FDebugAddressOfRawData;   // 拿已加载展开后的内存偏移
    Result := RvaToActual(D);
  end
  else
    Result := nil;
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

function TCnPE.RvaToActual(Rva: DWORD; ZeroIsNil: Boolean): Pointer;
var
  SH: PImageSectionHeader;
begin
  Result := nil;
  if (Rva = 0) and ZeroIsNil then
    Exit;

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
  I, T: DWORD;
  O: WORD;
  PAddress, PName: PDWORD;
  POrd: PWORD;
begin
  FDirectoryExport := nil;
  try
    FDirectoryExport := PImageExportDirectory(DataDirectoryContent[IMAGE_DIRECTORY_ENTRY_EXPORT]);
  except
    ;
  end;

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
  AddressOfNameOrdinals: ^PWord;   指向序号数组，下标与名字数组内的对应，值是 AddressOfFunctions 里的下标，一共 NumberOfNames

  也就是说：
  地址表第 0 个地址，是 base 序号函数的输出地址。可能有名字……
  地址表第 1 个地址，是 base + 1 序号函数的输出地址。可能有名字……
  地址表第 2 个地址，是 base + 2 序号函数的输出地址。可能有名字……
  地址表第 3 个地址，是 base + 3 序号函数的输出地址。可能有名字……
  地址表第 4 个地址，是 base + 4 序号函数的输出地址。可能有名字……

  NumberOfFunctions 可能大于 NumberOfNames，多的部分是没名字、仅序号输出的函数。
  序号是 AddressOfNameOrdinals 里的值加 Base
}

  // 按照 AddressofFunctions 里的顺序排列，取出地址
  PAddress := PDWORD(RvaToActual(DWORD(FDirectoryExport^.AddressOfFunctions)));
  PName := PDWORD(RvaToActual(DWORD(FDirectoryExport^.AddressOfNames)));
  POrd := PWORD(RvaToActual(DWORD(FDirectoryExport^.AddressOfNameOrdinals)));

  I := 0;
  while I < FExportNumberOfFunctions do  // 先取出所有函数序号与地址，其中 I 与 AddressOfFunctions 里的下标对应
  begin
    FExportItems[I].Ordinal := FExportBase + I; // 序号

    T := PDWORD(TCnNativeUInt(PAddress) + I * SizeOf(DWORD))^; // 地址
    FExportItems[I].Address := RvaToActual(T);

    Inc(I);
  end;

  I := 0;
  while I < FExportNumberOfNames do // 再补充名字
  begin
    O := POrd^; // 取出序号表中的内容，是 AddressOfFunctions 里的下标
    FExportItems[O].Name := ExtractNewString(RvaToActual(PName^));  // 取名字与序号对应的下标

    Inc(PName);
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

  // 然后是 OptionalHeader，注意 TImageOptionalHeader在 64 位编译器下具体是 TImageOptionalHeader64，此处需显式写明 32
  if FFileSizeOfOptionalHeader = SizeOf(TImageOptionalHeader32) then // 32 位
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

function ExportItemCompare(P1, P2: Pointer; ElementByteSize: Integer): Integer;
var
  E1, E2: PCnPEExportItem;
begin
  E1 := PCnPEExportItem(P1);
  E2 := PCnPEExportItem(P2);

  if TCnNativeUInt(E1^.Address) > TCnNativeUInt(E2^.Address) then
    Result := 1
  else if TCnNativeUInt(E1^.Address) < TCnNativeUInt(E2^.Address) then
    Result := -1
  else
    Result := 0;
end;

procedure TCnPE.SortExports;
begin
  // 根据输出地址排序
  MemoryQuickSort(@FExportItems[0], SizeOf(TCnPEExportItem), Length(FExportItems), ExportItemCompare);
end;

function TCnPE.GetIsDebug: Boolean;
begin
  Result := DataDirectoryContent[IMAGE_DIRECTORY_ENTRY_DEBUG] <> nil;
end;

{ TCnInProcessModuleList }

constructor TCnInProcessModuleList.Create;
begin
  inherited Create(True);
end;

function TCnInProcessModuleList.CreateDebugInfoFromAddress(
  Address: Pointer): TCnModuleDebugInfo;
var
  M: HMODULE;
begin
  M := ModuleFromAddr(Address);
  Result := GetDebugInfoFromModule(M);

  if Result = nil then
  begin
    Result := CreateDebugInfoFromModule(M);
    if Result <> nil then
      Add(Result);
  end;
end;

function TCnInProcessModuleList.CreateDebugInfoFromModule(AModuleHandle: HMODULE): TCnModuleDebugInfo;
begin
  // 根据各种情况创建不同的子类，后面加上 64 位、Map 和 tds 文件等
  Result := TCnModuleDebugInfoTD32.Create(AModuleHandle);
  if not Result.Init then
    FreeAndNil(Result)
  else
    Exit;

  Result := TCnModuleDebugInfoMap.Create(AModuleHandle);
  if not Result.Init then
    FreeAndNil(Result)
  else
    Exit;

  Result := TCnModuleDebugInfo.Create(AModuleHandle);
  if not Result.Init then
    FreeAndNil(Result);
end;

destructor TCnInProcessModuleList.Destroy;
begin

  inherited;
end;

function TCnInProcessModuleList.GetDebugInfoFromAddress(
  Address: Pointer): TCnModuleDebugInfo;
begin
  Result := GetDebugInfoFromModule(ModuleFromAddr(Address));
end;

function TCnInProcessModuleList.GetDebugInfoFromModule(
  AModuleHandle: HMODULE): TCnModuleDebugInfo;
var
  I: Integer;
  Info: TCnModuleDebugInfo;
begin
  for I := 0 to Count - 1 do
  begin
    Info := Items[I];
    if (Info <> nil) and (Info.ModuleHandle = AModuleHandle) then
    begin
      Result := Info;
      Exit;
    end;
  end;
  Result := nil;
end;

function TCnInProcessModuleList.GetItem(Index: Integer): TCnModuleDebugInfo;
begin
  Result := TCnModuleDebugInfo(inherited GetItem(Index));
end;

procedure TCnInProcessModuleList.SetItem(Index: Integer;
  const Value: TCnModuleDebugInfo);
begin
  inherited SetItem(Index, Value);
end;

{ TCnModuleDebugInfo }

constructor TCnModuleDebugInfo.Create(AModuleHandle: HMODULE);
var
  F: array[0..MAX_PATH - 1] of Char;
begin
  inherited Create;
  FModuleHandle := AModuleHandle;
  if GetModuleFileName(FModuleHandle, @F[0], SizeOf(F)) > 0 then;
    FModuleFile := StrNew(PChar(@F[0]));
end;

destructor TCnModuleDebugInfo.Destroy;
begin
  FPE.Free;
  inherited;
end;

function TCnModuleDebugInfo.GetDebugInfoFromAddr(Address: Pointer;
  out OutModuleFile, OutUnitName, OutProcName: string; out OutLineNumber, OutOffsetLineNumber,
  OutOffsetProc: Integer): Boolean;
var
  I: Integer;
  Item: PCnPEExportItem;
begin
  Result := False;
  OutModuleFile := ExtractFileName(FModuleFile);
  OutUnitName := '';

  for I := FPE.ExportNumberOfFunctions - 1 downto 0 do
  begin
    Item := FPE.ExportFunctionItem[I];
    if TCnNativeUInt(Item^.Address) < TCnNativeUInt(Address) then
    begin
      OutProcName := Item^.Name;
      OutOffsetProc := TCnNativeUInt(Address) - TCnNativeUInt(Item^.Address);

      OutLineNumber := CN_INVALID_LINENUMBER_OFFSET;
      OutOffsetLineNumber := CN_INVALID_LINENUMBER_OFFSET;

      Result := True;
      Exit;
    end;
  end;
end;

function TCnModuleDebugInfo.Init: Boolean;
begin
  Result := (FModuleHandle <> 0) and (FModuleHandle <> INVALID_HANDLE_VALUE)
    and FileExists(FModuleFile);

  if Result then
  begin
    FPE := TCnPE.Create(FModuleHandle);
    try
      FPE.Parse;
      FPE.SortExports;
    except
      Result := False;
    end;
  end;
end;

function TCnModuleDebugInfo.VAFromAddr(Address: Pointer): DWORD;
begin
  Result := DWORD(TCnNativeUInt(Address) - TCnNativeUInt(FModuleHandle)
    - TCnNativeUInt(FPE.FOptionalBaseOfCode));
end;

{ TCnModuleDebugInfoTD32 }

constructor TCnModuleDebugInfoTD32.Create(AModuleHandle: HMODULE);
begin
  inherited;
  FNames := TStringList.Create;
  FSourceModuleNames := TStringList.Create;
  FProcedureNames := TStringList.Create;

  FLineNumbers := TCnIntegerList.Create;
  FOffsets := TCnIntegerList.Create;

  FNames.Add('');  // NameIndex 从 1 开始
end;

destructor TCnModuleDebugInfoTD32.Destroy;
var
  I: Integer;
begin
  FStream.Free;

  FOffsets.Free;
  FLineNumbers.Free;

  for I := 0 to FProcedureNames.Count - 1 do
    FProcedureNames.Objects[I].Free;
  FProcedureNames.Free;

  for I := 0 to FSourceModuleNames.Count - 1 do
    FSourceModuleNames.Objects[I].Free;
  FSourceModuleNames.Free;

  FNames.Free;
  inherited;
end;

function TCnModuleDebugInfoTD32.GetDebugInfoFromAddr(Address: Pointer;
  out OutModuleFile, OutUnitName, OutProcName: string; out OutLineNumber,
  OutOffsetLineNumber, OutOffsetProc: Integer): Boolean;
var
  I: Integer;
  VA: DWORD;
begin
  VA := VAFromAddr(Address);

  // 模块名
  OutModuleFile := ExtractFileName(FModuleFile);

  // 源码文件名
  OutUnitName := '';
  for I := 0 to SourceModuleCount - 1 do
  begin
    if SourceModules[I].IsAddressInSource(VA) then
    begin
      OutUnitName := SourceModules[I].Name;
      Break;
    end;
  end;

  // 方法名
  for I := 0 to ProcedureCount - 1 do
  begin
    if Procedures[I].IsAddressInProcedure(VA) then
    begin
      OutProcName := Procedures[I].Name;
      OutOffsetProc := VA - Procedures[I].Offset;
      Break;
    end;
  end;

  // 行号与偏移
  OutOffsetLineNumber := CN_INVALID_LINENUMBER_OFFSET;
  OutLineNumber := CN_INVALID_LINENUMBER_OFFSET;

  if OutUnitName <> '' then
  begin
    // 有源文件才有行号和行号间偏移
    for I := 0 to FLineNumbers.Count - 1 do
    begin
      if VA = DWORD(FOffsets[I]) then
      begin
        OutLineNumber := FLineNumbers[I];
        OutOffsetLineNumber := 0;
        Break;
      end
      else if (I > 0) and (DWORD(FOffsets[I - 1]) <= VA) and (DWORD(FOffsets[I]) > VA) then
      begin
        OutLineNumber := FLineNumbers[I - 1];
        OutOffsetLineNumber := VA - DWORD(FOffsets[I - 1]);
        Break;
      end;
    end;
  end;

  Result := True;
end;

function TCnModuleDebugInfoTD32.GetLineNumberCount: Integer;
begin
  Result := FLineNumbers.Count;
end;

function TCnModuleDebugInfoTD32.GetLineNumbers(Index: Integer): Integer;
begin
  Result := FLineNumbers[Index];
end;

function TCnModuleDebugInfoTD32.GetOffsetCount: Integer;
begin
  Result := FOffsets.Count;
end;

function TCnModuleDebugInfoTD32.GetOffsets(Index: Integer): Integer;
begin
  Result := FOffsets[Index];
end;

function TCnModuleDebugInfoTD32.GetProcedureCount: Integer;
begin
  Result := FProcedureNames.Count;
end;

function TCnModuleDebugInfoTD32.GetProcedures(Index: Integer): TCnTDProcedureSymbol;
begin
  Result := TCnTDProcedureSymbol(FProcedureNames.Objects[Index]);
end;

function TCnModuleDebugInfoTD32.GetSourceModuleCount: Integer;
begin
  Result := FSourceModuleNames.Count;
end;

function TCnModuleDebugInfoTD32.GetSourceModules(Index: Integer): TCnTDSourceModule;
begin
  Result := TCnTDSourceModule(FSourceModuleNames.Objects[Index]);
end;

function TCnModuleDebugInfoTD32.Init: Boolean;
var
  I: Integer;
  Sig: PTDFileSignature;
  DH: PTDDirectoryHeader;
  Tds: string;
begin
  Result := inherited Init;
  if not Result then
    Exit;

  // 父类创建了 PE 对象，读出其 Debug Information 指针与尺寸
  Result := False;
  FData := FPE.DebugContent;
  FSize := FPE.DebugSizeOfData;

  if (FPE.DebugType <> IMAGE_DEBUG_TYPE_UNKNOWN) or (FData = nil) or (FSize <= SizeOf(TTDFileSignature)) then
  begin
    Tds := ChangeFileExt(FModuleFile, '.tds');  // 无调试信息或不合法则判断 tds 文件
    if FileExists(Tds) then
    begin
      FStream := TMemoryStream.Create;
      FStream.LoadFromFile(Tds);

      FData := FStream.Memory;
      FSize := FStream.Size;
    end
    else
      Exit;  
  end;

  Sig := PTDFileSignature(FData);
  if (Sig^.Signature <> TD_SIGNATURE_DELPHI) and (Sig^.Signature <> TD_SIGNATURE_BCB) then
    Exit;  // 标记值不对则退出

  DH := PTDDirectoryHeader(TCnNativeUInt(Sig) + Sig^.Offset);
  while True do
  begin
    for I := 0 to DH^.DirEntryCount - 1 do
    begin
      // 处理 DH^.DirEntries[I];
      ParseSubSection(@DH^.DirEntries[I]);
    end;

    // 寻找下一个 Header
    if DH^.lfoNextDir = 0 then
      Break;
    DH := PTDDirectoryHeader(TCnNativeUInt(DH) + DH^.lfoNextDir);
  end;
  SyncNames;

  Result := True;
end;

procedure TCnModuleDebugInfoTD32.ParseSubSection(DSE: Pointer);
var
  DE: PTDDirectoryEntry;
  DS: Pointer;
  C, O: DWORD;
  I, J, L: Integer;
  PName: PAnsiChar;
  S: string;
  MI: PTDSourceModuleInfo;
  SE: PTDSourceFileEntry;
  LM: PTDLineMappingEntry;
  SM: TCnTDSourceModule;
  SIS: PTDSymbolInfos;
  SI: PTDSymbolInfo;
  PS: TCnTDProcedureSymbol;
begin
  DE := PTDDirectoryEntry(DSE); // 参数是 Entry
  DS := Pointer(TCnNativeUInt(FData) + DE^.Offset); // 找到真正的 SubSection 内容，其尺寸是 DE^.Size
  case DE^.SubsectionType of
    TD_SUBSECTION_TYPE_NAMES:
      begin
        C := PDWORD(DS)^; // 第一个 DWORD 是数量
        PName := PAnsiChar(DS);
        Inc(PName, SizeOf(DWORD));

        for I := 0 to C - 1 do // 后面是一字节长度加字符串内容加 #0
        begin
          L := Ord(PName^);
          Inc(PName);
          S := StrNew(PName);
          FNames.Add(S);
          Inc(PName, L + 1);
        end;
      end;
    TD_SUBSECTION_TYPE_SOURCE_MODULE:
      begin
        MI := PTDSourceModuleInfo(DS);
        for I := 0 to MI^.FileCount - 1 do
        begin
          SE := PTDSourceFileEntry(TCnNativeUInt(MI) + MI^.BaseSrcFiles[I]);
          if SE^.NameIndex > 0 then
          begin
            // 一个 SourceFileEntry 有文件名索引、段数、每段对应的 LineMappingEntry 结构与段起始结构
            SM := TCnTDSourceModule.Create;
            SM.NameIndex := SE^.NameIndex;
            SM.SegmentCount := SE^.SegmentCount;
            SM.SegmentArray := @SE^.BaseSrcLines[SE^.SegmentCount];

            for J := 0 to SM.SegmentCount - 1 do
            begin
              LM := PTDLineMappingEntry(TCnNativeUInt(MI) + SE^.BaseSrcLines[J]);
              for L := 0 to LM^.PairCount - 1 do
              begin
                FOffsets.Add(Integer(LM^.Offsets[L]));
                FLineNumbers.Add(Integer(PCnWord16Array(@LM^.Offsets[LM^.PairCount])^[L]));
              end;
            end;

            FSourceModuleNames.AddObject('', SM); // 名称事后再补充
          end;
        end;
      end;
    TD_SUBSECTION_TYPE_ALIGN_SYMBOLS:
      begin
        SIS := PTDSymbolInfos(DS);
        O := SizeOf(SIS^.Signature);
        while O < DE^.Size do
        begin
          SI := PTDSymbolInfo(TCnNativeUInt(SIS) + O);
          if (SI^.SymbolType = SYMBOL_TYPE_LPROC32) or (SI^.SymbolType = SYMBOL_TYPE_GPROC32) then
          begin
            PS := TCnTDProcedureSymbol.Create;
            PS.NameIndex := SI^.Proc.NameIndex;
            PS.Offset := SI^.Proc.Offset;
            PS.Size := SI^.Proc.Size;
            FProcedureNames.AddObject('', PS);
          end;
          Inc(O, SI^.Size + SizeOf(SI^.Size));
        end;
      end;
  end;
end;

procedure TCnModuleDebugInfoTD32.SyncNames;
var
  I: Integer;
  SM: TCnTDSourceModule;
  PS: TCnTDProcedureSymbol;
begin
  for I := 0 to FSourceModuleNames.Count - 1 do
  begin
    SM := TCnTDSourceModule(FSourceModuleNames.Objects[I]);
    if (SM <> nil) and (SM.Name = '') then
    begin
      SM.Name := ExtractFileName(FNames[SM.NameIndex]);
      FSourceModuleNames[I] := SM.Name;
    end;
  end;

  for I := 0 to FProcedureNames.Count - 1 do
  begin
    PS := TCnTDProcedureSymbol(FProcedureNames.Objects[I]);
    if (PS <> nil) and (PS.Name = '') then
    begin
      PS.Name := FNames[PS.NameIndex];
      FProcedureNames[I] := PS.Name;
      // TODO: Win32 下只取第一个 @ 后的部分且把后面的 @ 替换成 .
      // Win64 下去掉 __Zn 前缀并把表示长度的数字替换成 .
    end;
  end;
end;

{ TCnTDSourceModule }

function TCnTDSourceModule.GetSegmentEnd(Index: Integer): DWORD;
var
  P: PDWORD;
begin
  P := FSegmentArray;
  Inc(P, 2 * Index + 1);
  Result := P^;
end;

function TCnTDSourceModule.GetSegmentStart(Index: Integer): DWORD;
var
  P: PDWORD;
begin
  P := FSegmentArray;
  Inc(P, 2 * Index);
  Result := P^;
end;

function TCnTDSourceModule.IsAddressInSource(Address: DWORD): Boolean;
var
  I: Integer;
  S, E: DWORD;
begin
  Result := False;
  for I := 0 to FSegmentCount - 1 do
  begin
    S := SegmentStart[I];
    E := SegmentEnd[I];
    if (Address >= S) and (Address <= E) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

{ TCnTDProcSymbol }

function TCnTDProcedureSymbol.IsAddressInProcedure(Address: DWORD): Boolean;
begin
  Result := (Address >= FOffset) and (Address <= FOffset + FSize);
end;

{ TCnMapSourceModule }

procedure TCnMapSourceModule.AddLineOffset(ALineNumber: Integer;
  AnOffset: DWORD);
begin
  FLineNumbers.Add(ALineNumber);
  FOffsets.Add(Integer(AnOffset));
end;

procedure TCnMapSourceModule.AddSegment(ASegStart, ASegEnd: DWORD);
begin
  FSegStarts.Add(Integer(ASegStart));
  FSegEnds.Add(Integer(ASegEnd));
end;

constructor TCnMapSourceModule.Create;
begin
  inherited;
  FSegStarts := TCnIntegerList.Create;
  FSegEnds := TCnIntegerList.Create;
  FOffsets := TCnIntegerList.Create;
  FLineNumbers := TCnIntegerList.Create;
end;

destructor TCnMapSourceModule.Destroy;
begin
  FLineNumbers.Free;
  FOffsets.Free;
  FSegEnds.Free;
  FSegStarts.Free;
  inherited;
end;

function TCnMapSourceModule.GetLineNumberCount: Integer;
begin
  Result := FLineNumbers.Count;
end;

function TCnMapSourceModule.GetLineNumbers(Index: Integer): Integer;
begin
  Result := FLineNumbers[Index];
end;

function TCnMapSourceModule.GetOffsetCount: Integer;
begin
  Result := FOffsets.Count;
end;

function TCnMapSourceModule.GetOffsets(Index: Integer): Integer;
begin
  Result := FOffsets[Index];
end;

function TCnMapSourceModule.GetSegmentCount: Integer;
begin
  Result := FSegStarts.Count;
end;

function TCnMapSourceModule.GetSegmentEnd(Index: Integer): DWORD;
begin
  Result := DWORD(FSegEnds[Index]);
end;

function TCnMapSourceModule.GetSegmentStart(Index: Integer): DWORD;
begin
  Result := DWORD(FSegStarts[Index]);
end;

{ TCnModuleDebugInfoMap }

constructor TCnModuleDebugInfoMap.Create(AModuleHandle: HMODULE);
begin
  inherited;
  FSourceModuleNames := TStringList.Create;
  FProcedureNames := TStringList.Create;
end;

destructor TCnModuleDebugInfoMap.Destroy;
var
  I: Integer;
begin
  for I := 0 to FSourceModuleNames.Count - 1 do
    FSourceModuleNames.Objects[I].Free;

  FProcedureNames.Free;
  inherited;
end;

function TCnModuleDebugInfoMap.GetDebugInfoFromAddr(Address: Pointer;
  out OutModuleFile, OutUnitName, OutProcName: string; out OutLineNumber,
  OutOffsetLineNumber, OutOffsetProc: Integer): Boolean;
begin

end;

function TCnModuleDebugInfoMap.GetProcedureAddress(Index: Integer): DWORD;
begin
  Result := DWORD(FProcedureNames.Objects[Index]);
end;

function TCnModuleDebugInfoMap.GetProcedureCount: Integer;
begin
  Result := FProcedureNames.Count;
end;

function TCnModuleDebugInfoMap.GetProcedures(Index: Integer): string;
begin
  Result := FProcedureNames[Index];
end;

function TCnModuleDebugInfoMap.GetSourceModuleCount: Integer;
begin
  Result := FSourceModuleNames.Count;
end;

function TCnModuleDebugInfoMap.GetSourceModules(
  Index: Integer): TCnMapSourceModule;
begin
  Result := TCnMapSourceModule(FSourceModuleNames.Objects[Index]);
end;

function TCnModuleDebugInfoMap.Init: Boolean;
const
  LINE_NUMBER_PREFIX = 'Line numbers for ';
  ADDRESS_PREFIX = 'Address ';
  PUBLIC_BY_NAME_SUBFIX = 'Publics by Name';
  PUBLIC_BY_VALUE_SUBFIX = 'Publics by Value';
  SEGMENT_PREFIX = 'Detailed map of segments';
var
  I, LB, RB: Integer;
  S, N, F, MF: string;
  SL: TStringList;
  SM: TCnMapSourceModule;

  function ReadNextLine: string;
  begin
    Result := Trim(SL[I]);
    Inc(I);
  end;

  // 解析 S，形式类似于一个 0005:00035388       CnDebug.$pdata$_ZN7Cndebug10CnDebuggerEv
  procedure ParseProcedure(const Proc: string);
  var
    CP, SP: Integer;
    O, P: string;
  begin
    CP := Pos(':', Proc);
    SP := Pos(' ', Proc);
    if (CP > 1) and (SP > CP + 1) then
    begin
      O := Copy(Proc, CP + 1, SP - CP - 1);
      P := Trim(Copy(Proc, SP + 1, MaxInt));

      CP := HexStrToDWord(O);
      FProcedureNames.AddObject(P, TObject(CP));
    end;
  end;

  // 解析 S，形式类似于多个 387 0001:003DD9F5   388 0001:003DDA05 ……
  procedure ParseLineOffset(Module: TCnMapSourceModule; const LineOffset: string);
  var
    LO, SL, SO: string;
    SP, CP, SSP: Integer;
  begin
    LO := LineOffset;
    while LO <> '' do
    begin
      SP := Pos(' ', LO);
      CP := Pos(':', LO);
      if (CP > 1) and (SP > 1) and (CP > SP + 1) then
      begin
        SL := Copy(LO, 1, SP - 1);
        Delete(LO, 1, CP); // 删去分号以及之前的内容

        SSP := Pos(' ', LO);
        if SSP > 1 then // 后面还有
        begin
          SO := Copy(LO, 1, SSP - 1);
          Module.AddLineOffset(StrToInt(SL), HexStrToDWord(SO));
          Delete(LO, 1, SSP);

          LO := Trim(LO); // 可能还有空格分隔
        end
        else // 后面没了
        begin
          Module.AddLineOffset(StrToInt(SL), HexStrToDWord(LO));
          Exit;
        end;
      end
      else
        Exit;
    end;
  end;

  // 解析 S，形式类似于一个 0001:00005994 00000174 C=CODE     S=.text    G=(none)   M=SysInit  ACBP=A9
  procedure ParseSegement(const Seg: string);
  const
    CODE_NAME = ' C=CODE ';
    NAME_PREFIX = ' M=';
  var
    CP, SP, NP: Integer;
    LS, SO, SS, SN: string;
    SM: TCnMapSourceModule;
  begin
    if Pos(CODE_NAME, Seg) <= 0 then // 只处理 Code 段
      Exit;

    LS := Seg;
    CP := Pos(':', LS);
    SP := Pos(' ', LS);
    if (CP > 1) and (SP > CP + 1) then
    begin
      SO := Copy(LS, CP + 1, SP - CP - 1);
      LS := Trim(Copy(LS, SP + 1, MaxInt));

      SP := Pos(' ', LS);
      if SP > 1 then
      begin
        SS := Copy(LS, 1, SP - 1);
        Delete(LS, 1, SP);
        LS := Trim(LS);

        NP := Pos(NAME_PREFIX, LS);
        if NP > 1 then
        begin
          Delete(LS, 1, NP + Length(NAME_PREFIX) - 1);
          SP := Pos(' ', LS);
          if SP > 0 then
          begin
            SN := Copy(LS, 1, SP - 1);
            SM := MakeSureModuleExists(SN);
            SM.AddSegment(HexStrToDWord(SO), HexStrToDWord(SO) + HexStrToDWord(SS));
          end;
        end;
      end;
    end;
  end;

begin
  Result := False;

  MF := ChangeFileExt(FModuleFile, '.map');  // 判断 map 文件
  if not FileExists(MF) then
    Exit;

  // 读入 map 文件挨行解析
  SL := TStringList.Create;
  try
    SL.LoadFromFile(MF);

    I := 0;
    while I < SL.Count do
    begin
      S := ReadNextLine;

      // 根据 S 的不同分块处理
      if Pos(LINE_NUMBER_PREFIX, S) = 1 then
      begin
        Delete(S, 1, Length(LINE_NUMBER_PREFIX));
        S := Trim(S);

        LB := Pos('(', S);
        RB := Pos(')', S);
        if (LB > 1) and (RB > 1) and (RB > LB + 1) then
        begin
          N := Copy(S, 1, LB - 1);
          F := Copy(S, LB + 1, RB - LB - 1);
          SM := MakeSureModuleExists(N, F);

          // 往下越过空行
          repeat
            S := ReadNextLine;
          until S <> '';

          // 读行号偏移量映射
          while S <> '' do
          begin
            ParseLineOffset(SM, S);
            S := ReadNextLine;
          end;
        end;
      end
      else if (Pos(ADDRESS_PREFIX, S) = 1) and (Pos(PUBLIC_BY_VALUE_SUBFIX, S) > 0) then
      begin
        // 往下越过空行
        repeat
          S := ReadNextLine;
        until S <> '';

        // 解析函数与其偏移量
        while S <> '' do
        begin
          ParseProcedure(S);
          S := ReadNextLine;
        end;
      end
      else if Pos(SEGMENT_PREFIX, S) > 0 then // 解析不带源码文件行号信息的单元文件名
      begin
        // 往下越过空行
        repeat
          S := ReadNextLine;
        until S <> '';

        // 解析段
        while S <> '' do
        begin
          ParseSegement(S);
          S := ReadNextLine;
        end;
      end;
    end;
    Result := True;
  finally
    SL.Free;
  end;
end;

function TCnModuleDebugInfoMap.MakeSureModuleExists(const SourceName: string;
  const SourceFile: string): TCnMapSourceModule;
var
  Idx: Integer;
begin
  Idx := FSourceModuleNames.IndexOf(SourceName);
  if Idx >= 0 then
  begin
    Result := TCnMapSourceModule(FSourceModuleNames.Objects[Idx]);
    if SourceFile <> '' then
      Result.FileName := SourceFile;
  end
  else
  begin
    Result := TCnMapSourceModule.Create;
    Result.Name := SourceName;
    Result.FileName := SourceFile;
    FSourceModuleNames.AddObject(SourceName, Result);
  end;
end;

end.
