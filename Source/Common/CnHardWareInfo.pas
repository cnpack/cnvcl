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

unit CnHardWareInfo;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：硬件信息单元
* 单元作者：SkyJacker
*           LiuXiao
*           Yock
*           Bahamut
* 备    注：硬件信息单元，目前只实现获取多核、多CPU系统中指定CPU的序列号与占用率
*           以及部分 BIOS 的 ID，部分硬盘的物理序列号.
* 开发平台：WindowsXP sp2 + Delphi 6.0 up2
* 兼容测试：Win2000/XP + Delphi 5、6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2014.12.04 V1.5
*               加入读取物理磁盘数以及序列号的功能，暂未支持 Win9X
*               加入读取逻辑驱动器数以及卷标的功能
*           2012.05.10 V1.4
*               修正 64 位下的编译问题
*           2008.08.01 V1.3
*               加入 Bahamut 的获取 BIOS ID 的过程，但只支持小部分 BIOS
*           2008.04.12 V1.2
*               LiuXiao 加入对 CPU 生产厂商名的读取与是否支持 cpuid 指令与序列号
*               的属性，感谢 Yock。
*           2008.01.12 V1.1
*               LiuXiao 加入对 CPU 占用率的读取
*           2007.01.23 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils, ExtCtrls, CnNative, CnCommon;

const
  RelationCache = 2;
  RelationNumaNode = 1;
  RelationProcessorCore = 0;
  RelationProcessorPackage = 3;

type
  TCacheType = (CacheUnified, CacheInstruction, CacheData, CacheTrace);

  TCacheDescriptor = packed record
    Level: Byte;
    Associativity: Byte;
    LineSize: Word;
    Size: DWORD;
    CacheType: TCacheType;
  end;

  TSystemLogicalProcessorInformation = packed record
    ProcessorMask: TCnNativeUInt;
    Relationship: DWORD;
    case Integer of
      0: (ProcessorCoreFlag: Byte);
      1: (NumaModeNumber: DWORD);
      2: (Cache: TCacheDescriptor);
      3: (Reserved: array[0..1] of Int64);
  end;
  PSystemLogicalProcessorInformation = ^TSystemLogicalProcessorInformation;

  TGetLogicalProcessorInformation = function(Buffer: PSystemLogicalProcessorInformation;
    out ReturnLength: DWORD): BOOL; stdcall;

  TCnCPUIdFormat = (ifContinuous, ifDashed);
  {* CPU序列号与信息串显示样式
   |<PRE>
     ifContinuous:  -连续型
     ifDashed:      -使用分割符'-'分割
   |</PRE>
  }

  TCnCpuId = class(TPersistent)
  {CPU 信息类}
  private
    FTimer: TTimer;
    FCPUCount: Integer;
    FCPUIds: TStrings;
    FCPUInfos: TStrings;
    FSupportCpuIds: TList;
    FSupportCpuSns: TList;
    FCPUOems: TStrings;
    FCPUIdFormat: TCnCPUIdFormat;
    FCPUUsageRead: Boolean;
    FCPUUsage: array[0..255] of Integer; // 总不会超过 256 个 CPU 吧？
    FCurCnt, FLastCnt: array[0..255] of Integer;
    FAverageCPUUsage: Integer;
    FProcessorCoreCount: Integer;
    FL1CacheCount: Integer;
    FNumaNodeCount: Integer;
    FL3CacheCount: Integer;
    FProcessorPackageCount: Integer;
    FLogicalProcessorCount: Integer;
    FL2CacheCount: Integer;
    function GetFirstCPUId: string;
    function GetCPUId(Index: Integer): string;
    procedure SetCPUIdFormat(ACPUIdFormat: TCnCPUIdFormat);
    function GetAverageCPUUsage: Integer;
    function GetCPUUsage(Index: Integer): Integer;
    function GetFirstCPUUsage: Integer;

    function RefreshCPUUsages: Cardinal; // 只被定时调用
    procedure CpuUsageTimer(Sender: TObject);
    function GetCPUOem(Index: Integer): string;
    function GetFirstCPUOem: string;
    function GetSupportCPUId(Index: Integer): Boolean;
    function GetSupportCPUSn(Index: Integer): Boolean;
    function GetFirstSupportCPUId: Boolean;
    function GetFirstSupportCPUSn: Boolean;
    function GetCPUInfoString(Index: Integer): string;
    function GetFirstCPUInfoString: string;

    function GetCnCPUOem: AnsiString;
    function GetCnCPUInfoString: AnsiString;
    function GetCnCPUID: AnsiString;

    procedure ClearLocalCPUInfo;
  public
    constructor Create;
    {* 构造函数，创建 FCPUIds 并调用 ReadCPUId}
    destructor Destroy; override;

    procedure ReadCPUInfo;
    {* 获得所有 CPU 内核的序列号和其他信息，并存入各个列表}

    procedure ReadLogicalCPUInfo;
    {* 通过 GetLogicalProcessorInformation API 获取更多的逻辑 CPU 信息}

    property CPUIdFormat: TCnCPUIdFormat read FCPUIdFormat write SetCPUIdFormat;
    {* CPU 序列号显示样式}
    property CPUCount: Integer read FCPUCount;
    {* 系统中的逻辑 CPU 总数}
    property FirstCPUId: string read GetFirstCPUId;
    {* 获取首个 CPU 的 ID，用于单 CPU 系统}
    property FirstCPUInfoString: string read GetFirstCPUInfoString;
    {* 获取首个 CPU 的信息字符串，用于单 CPU 系统}
    property FirstCPUOem: string read GetFirstCPUOem;
    {* 获取首个 CPU 的生产厂商，用于单 CPU 系统}
    property FirstSupportCPUId: Boolean read GetFirstSupportCPUId;
    {* 获取首个 CPU 是否支持 CPUID 指令，用于单 CPU 系统}
    property FirstSupportCPUSn: Boolean read GetFirstSupportCPUSn;
    {* 获取首个 CPU 是否支持读取 CPU 序列号，用于单 CPU 系统}
    property SupportCPUId[Index: Integer]: Boolean read GetSupportCPUId;
    {* 获取指定 CPU 是否支持 CPUID 指令}
    property SupportCPUSn[Index: Integer]: Boolean read GetSupportCPUSn;
    {* 获取指定 CPU 是否支持读取 CPU 序列号}
    property CPUId[Index: Integer]: string read GetCPUId;
    {* 获得指定 CPU 的序列号。索引 Index 从 0 开始。
       需要说明的是，序列号很多 CPU 被禁止读取了因此此属性全 0；而网络上流传的
       CPU 序列号读取，很多情况下读的是下面的“信息字符串”的属性}
    property CPUInfoString[Index: Integer]: string read GetCPUInfoString;
    {* 获得指定 CPU 的信息字符串。索引 Index 从 0 开始。
       此信息字符串包括了 CPU 的一些特性说明，非唯一，很多情况下被误当做 CPU ID。}
    property CPUOem[Index: Integer]: string read GetCPUOem;
     {* 获得指定 CPU 的生产厂商。索引 Index 从 0 开始}
    property CPUUsage[Index: Integer]: Integer read GetCPUUsage;
    {* 获得指定 CPU 的占用率，0 到 100
       需要说明的是，本类在 NT 系统上采用定时采样获得 CPU 的忙周期数再计算而来，
       因此在刚实例化、未采样完成时，得到的 CPU 占用率可能有误。以下同。
    }
    property AverageCPUUsage: Integer read GetAverageCPUUsage;
    {* 获得平均 CPU 占用率，0 到 100}
    property FirstCPUUsage: Integer read GetFirstCPUUsage;
    {* 获得首个 CPU 的占用率，0 到 100，用于单 CPU 系统}

    // 以下信息使用 API GetLogicalProcessorInformation 获得
    property ProcessorPackageCount: Integer read FProcessorPackageCount;
    {* 物理处理器封装个数}
    property ProcessorCoreCount: Integer read FProcessorCoreCount;
    {* 处理器核心数}
    property LogicalProcessorCount: Integer read FLogicalProcessorCount;
    {* 逻辑处理器数，等于 CPUCount 值}
    property NumaNodeCount: Integer read FNumaNodeCount;
    {* 非统一内存访问架构的节点个数}
    property L1CacheCount: Integer read FL1CacheCount;
    {* 第一级高速缓存个数}
    property L2CacheCount: Integer read FL2CacheCount;
    {* 第二级高速缓存个数}
    property L3CacheCount: Integer read FL3CacheCount;
    {* 第三级高速缓存个数}
  end;

  TCnHardDiskInfo = class(TPersistent)
  private
    FHardDiskCount: Integer;
    FHardDiskSns: TStrings;
    FVolumnCount: Integer;
    FVolumnLetters: TStrings;
    FVolumnNames: TStrings;

    function GetVolumnLetter(Index: Integer): string;
    function GetVolumnName(Index: Integer): string;
    function GetDiskSerialNo(Index: Integer): string;

    procedure ReadPhysicalDriveInNTWithZeroRights;
    procedure ReadVolumnInfo;
  public
    constructor Create;
    {* 构造函数，创建列表并调用 ReadHardDisk}
    destructor Destroy; override;

    procedure ReadHardDisk;
    {* 读取物理硬盘以及分区的信息}
    
    property HardDiskCount: Integer read FHardDiskCount;
    {* 物理硬盘个数}
    property DiskSerialNo[Index: Integer]: string read GetDiskSerialNo;
    {* 物理硬盘的序列号}

    property VolumnCount: Integer read FVolumnCount;
    {* 逻辑驱动器个数}
    property VolumnLetter[Index: Integer]: string read GetVolumnLetter;
    {* 逻辑驱动器盘符}
    property VolumnName[Index: Integer]: string read GetVolumnName;
    {* 逻辑驱动器卷标}
  end;

function CnGetBiosID: string;
{* 获得 BIOS 的 ID，只支持小部分 BIOS，而且旧式的主板由于不规范，无法获取 ID}

implementation

const
  BiosOffset: array[0..2] of DWORD = ($6577, $7196, $7550);
  SCN_CPUID_BIT = $200000; //CPU ID 位标记

type
  TCnCPUIDResult = array[0..3] of Longint;

  PUNICODE_STRING = ^TUNICODE_STRING;
  _UNICODE_STRING = record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWChar;
  end;
  TUNICODE_STRING = _UNICODE_STRING;

  POBJECT_ATTRIBUTES = ^TOBJECT_ATTRIBUTES;
  _OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: Pointer;
    SecurityQualityOfService: Pointer;
  end;
  TOBJECT_ATTRIBUTES = _OBJECT_ATTRIBUTES;

  PLARGE_INTEGER = ^LARGE_INTEGER;
  PPByte = ^PByte;

  _SYSTEM_BASIC_INFORMATION = record
    Unknown: ULONG;
    MaximumIncrement: ULONG;
    PhysicalPageSize: ULONG;
    NumberOfPhysicalPages: ULONG;
    LowestPhysicalPage: ULONG;
    HighestPhysicalPage: ULONG;
    AllocationGranularity: ULONG;
    LowestUserAddress: ULONG;
    HighestUserAddress: ULONG;
    ActiveProcessors: ULONG;
    NumberProcessors: UCHAR;
  end;
  SYSTEM_BASIC_INFORMATION = _SYSTEM_BASIC_INFORMATION;
  PSYSTEM_BASIC_INFORMATION = ^SYSTEM_BASIC_INFORMATION;
  TSystemBasicInformation = SYSTEM_BASIC_INFORMATION;
  PSystemBasicInformation = ^TSystemBasicInformation;

  SYSTEM_PROCESSOR_TIMES = packed record
    IdleTime: LARGE_INTEGER;
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    DpcTime: LARGE_INTEGER;
    InterruptTime: LARGE_INTEGER;
    InterruptCount: ULONG;
  end;
  TSystemProcessorTimes = SYSTEM_PROCESSOR_TIMES;
  PSystemProcessorTimes = ^TSystemProcessorTimes;

  SYSTEM_INFORMATION_CLASS = (
          SystemBasicInformation,
          SystemProcessorInformation,
          SystemPerformanceInformation,
          SystemTimeOfDayInformation,
          SystemNotImplemented1,
          SystemProcessesAndThreadsInformation,
          SystemCallCounts,
          SystemConfigurationInformation,
          SystemProcessorTimes,
          SystemGlobalFlag,
          SystemNotImplemented2,
          SystemModuleInformation,
          SystemLockInformation,
          SystemNotImplemented3,
          SystemNotImplemented4,
          SystemNotImplemented5,
          SystemHandleInformation,
          SystemObjectInformation,
          SystemPagefileInformation,
          SystemInstructionEmulationCounts,
          SystemInvalidInfoClass1,
          SystemCacheInformation,
          SystemPoolTagInformation,
          SystemProcessorStatistics,
          SystemDpcInformation,
          SystemNotImplemented6,
          SystemLoadImage,
          SystemUnloadImage,
          SystemTimeAdjustment,
          SystemNotImplemented7,
          SystemNotImplemented8,
          SystemNotImplemented9,
          SystemCrashDumpInformation,
          SystemExceptionInformation,
          SystemCrashDumpStateInformation,
          SystemKernelDebuggerInformation,
          SystemContextSwitchInformation,
          SystemRegistryQuotaInformation,
          SystemLoadAndCallImage,
          SystemPrioritySeparation,
          SystemNotImplemented10,
          SystemNotImplemented11,
          SystemInvalidInfoClass2,
          SystemInvalidInfoClass3,
          SystemTimeZoneInformation,
          SystemLookasideInformation,
          SystemSetTimeSlipEvent,
          SystemCreateSession,
          SystemDeleteSession,
          SystemInvalidInfoClass4,
          SystemRangeStartInformation,
          SystemVerifierInformation,
          SystemAddVerifier,
          SystemSessionProcessesInformation);
  TSystemInformationClass = SYSTEM_INFORMATION_CLASS;

  TNativeQuerySystemInformation = function(SystemInformationClass:
    TSystemInformationClass; SystemInformation: Pointer; SystemInformationLength:
    Cardinal; ReturnLength: PDWORD): Cardinal; stdcall;

  TZwOpenSection = function (var hWnd: THandle; dwMask: DWORD; PObject: POBJECT_ATTRIBUTES): DWORD; stdcall;

  TZwMapViewOfSection = function (hWnd: THandle; ViewHandle: THandle; PBaseAddr: Pointer;
    dwLength: ULONG; dwAllocLen: ULONG; PRealAddr: PLARGE_INTEGER; PReadLen: PDWORD;
    dwInherite: DWORD; dwAllocType: ULONG; dwProtectType: ULONG): DWORD; stdcall;

  TZwUnmapViewOfSection = function (hWnd: THandle; PBaseAddr: Pointer): DWORD; stdcall;

  // Hard Disk begin

  TStorageQueryType = ( PropertyStandardQuery, PropertyExistsQuery,
    PropertyMaskQuery, PropertyQueryMaxDefined);

  TStoragePropertyId = (StorageDeviceProperty, StorageAdapterProperty);

  TStoragePropertyQuery = packed record
    PropertyId: TStoragePropertyId;
    QueryType: TStorageQueryType;
    AdditionalParameters: array[0..9] of Char;
  end;

  TStorageBusType = (
    BusTypeUnknown,
    BusTypeScsi,
    BusTypeAtapi,
    BusTypeAta,
    BusType1394,
    BusTypeSsa,
    BusTypeFibre,
    BusTypeUsb,
    BusTypeRAID,
    BusTypeiScsi,
    BusTypeSas,
    BusTypeSata,
    BusTypeSD,
    BusTypeMmc,
    BusTypeVirtual,
    BusTypeFileBackedVirtual,
    BusTypeSpaces,
    BusTypeMax,
    BusTypeMaxReserved);

  TStorageDeviceDescriptor = packed record
    Version: DWORD;
    Size: DWORD;
    DeviceType: Byte;
    DeviceTypeModifier: Byte;
    RemovableMedia: Boolean;
    CommandQueueing: Boolean;
    VendorIdOffset: DWORD;
    ProductIdOffset: DWORD;
    ProductRevisionOffset: DWORD;
    SerialNumberOffset: DWORD;
    BusType: TStorageBusType;
    RawPropertiesLength: DWORD;
    RawDeviceProperties: array [0..0] of AnsiChar;
  end;
  PStorageDeviceDescriptor = ^TStorageDeviceDescriptor;

  TIDERegs = packed record
    bFeaturesReg: BYTE;
    bSectorCountReg: BYTE;
    bSectorNumberReg: BYTE;
    bCylLowReg: BYTE;
    bCylHighReg: BYTE;
    bDriveHeadReg: BYTE;
    bCommandReg: BYTE;
    bReserved: BYTE;
  end;

  TSendCmdInParams = packed record
    cBufferSize: DWORD;
    irDriveRegs: TIDERegs;
    bDriveNumber: BYTE;
    bReserved: array[0..2] of Byte;
    dwReserved: array[0..3] of DWORD;
    bBuffer: array[0..0] of Byte;
  end;

  TIdSector = packed record
    wGenConfig: Word;
    wNumCyls: Word;
    wReserved: Word;
    wNumHeads: Word;
    wBytesPerTrack: Word;
    wBytesPerSector: Word;
    wSectorsPerTrack: Word;
    wVendorUnique: array[0..2] of Word;
    sSerialNumber: array[0..19] of CHAR;
    wBufferType: Word;
    wBufferSize: Word;
    wECCSize: Word;
    sFirmwareRev: array[0..7] of Char;
    sModelNumber: array[0..39] of Char;
    wMoreVendorUnique: Word;
    wDoubleWordIO: Word;
    wCapabilities: Word;
    wReserved1: Word;
    wPIOTiming: Word;
    wDMATiming: Word;
    wBS: Word;
    wNumCurrentCyls: Word;
    wNumCurrentHeads: Word;
    wNumCurrentSectorsPerTrack: Word;
    ulCurrentSectorCapacity: DWORD;
    wMultSectorStuff: Word;
    ulTotalAddressableSectors: DWORD;
    wSingleWordDMA: Word;
    wMultiWordDMA: Word;
    bReserved: array[0..127] of BYTE;
  end;
  PIdSector = ^TIdSector;

  TDriverStatus = packed record
    bDriverError: Byte;
    bIDEStatus: Byte;
    bReserved: array[0..1] of Byte;
    dwReserved: array[0..1] of DWORD;
  end;

  TSendCmdOutParams = packed record
    cBufferSize: DWORD;
    DriverStatus: TDriverStatus;
    bBuffer: array[0..0] of BYTE;
  end;
  // Hard Disk end.

const
  STATUS_SUCCESS = $00000000;
  IOCTL_STORAGE_QUERY_PROPERTY = $002D1400; // ($2D shl 16) or ($0500 shl 2); //

var
  NtDllHandle: THandle = 0;
  NtDllNeedFree: Boolean = False;

  NtQuerySystemInformation: TNativeQuerySystemInformation = nil;

  ZwOpenSection: TZwOpenSection = nil;

  ZwMapViewOfSection: TZwMapViewOfSection = nil;

  ZwUnmapViewOfSection: TZwUnmapViewOfSection = nil;

  GetLogicalProcessorInformation: TGetLogicalProcessorInformation = nil;

function GetNtNativeAPIs: Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    NtDllHandle := GetModuleHandle('NTDLL.DLL');
    if NtDllHandle = 0 then
    begin
      NtDllHandle := LoadLibrary('NTDLL.DLL');
      NtDllNeedFree := NtDllHandle <> 0;
    end;

    if NtDllHandle <> 0 then
    begin
//      @NtQueryInformationToken:=GetProcAddress(NtDllHandle,'NtQueryInformationToken');
//      @NtOpenProcessToken := GetProcAddress(NtDllHandle,'NtOpenProcessToken');
//      @NtOpenSection := GetProcAddress(NtDllHandle,'NtOpenSection');
//      @NtClose := GetProcAddress(NtDllHandle,'NtClose');
//      @NtOpenProcess := GetProcAddress(NtDllHandle,'NtOpenProcess');
      @NtQuerySystemInformation := GetProcAddress(NtDllHandle, 'NtQuerySystemInformation');
//      @NtCreateSection := GetProcAddress(NtDllHandle,'NtCreateSection');
//      @NtCreateToken := GetProcAddress(NtDllHandle,'NtCreateToken');
//      @NtMapViewOfSection := GetProcAddress(NtDllHandle,'NtMapViewOfSection');
//      @NtUnmapViewOfSection := GetProcAddress(NtDllHandle,'NtUnmapViewOfSection');
//      @NtOpenFile := GetProcAddress(NtDllHandle,'NtOpenFile');
//      @NtCreateFile := GetProcAddress(NtDllHandle,'NtCreateFile');
//      @NtQueryObject := GetProcAddress(NtDllHandle,'NtQueryObject');
//      @NtQueryInformationProcess := GetProcAddress(NtDllHandle,'NtQueryInformationProcess');
//      @NtQueryInformationThread := GetProcAddress(NtDllHandle,'NtQueryInformationThread');
//      @NtQueryInformationFile := GetProcAddress(NtDllHandle,'NtQueryInformationFile');
//      @NtDuplicateObject := GetProcAddress(NtDllHandle,'NtDuplicateObject');
//      @NtDeviceIoControlFile := GetProcAddress(NtDllHandle,'NtDeviceIoControlFile');

      @ZwOpenSection := GetProcAddress(NtDllHandle, 'ZwOpenSection');
      @ZwMapViewOfSection := GetProcAddress(NtDllHandle, 'ZwMapViewOfSection');
      @ZwUnmapViewOfSection := GetProcAddress(NtDllHandle, 'ZwUnmapViewOfSection');
    end;
  end;

  @GetLogicalProcessorInformation := GetProcAddress(GetModuleHandle('KERNEL32.DLL'), 'GetLogicalProcessorInformation');
  Result := NtDllHandle <> 0;
end;

procedure FreeNtNativeAPIs;
begin
  if (NtDllHandle <> 0) and NtDllNeedFree then
  begin
    FreeLibrary(NtDllHandle);
    NtDllHandle := 0;
  end;
end;

{$IFDEF WIN64}

function GetCPUInfo64: TCnCPUIDResult; assembler; register;
asm
        PUSH    RBX
        PUSH    RDI
        MOV     RDI, RCX
        MOV     EAX, 1
        CPUID
        MOV     [RDI], EAX;
        MOV     [RDI + 4], EBX;
        MOV     [RDI + 8], ECX;
        MOV     [RDI + 12], EDX;
        POP     RDI
        POP     RBX
end;

function GetCPUID64: TCnCPUIDResult; assembler; register;
asm
        PUSH    RBX
        PUSH    RDI
        MOV     RDI, RCX
        MOV     EAX, 1
        CPUID
        MOV     [RDI], EAX;
        MOV     [RDI + 4], EDX;
        MOV EAX, $3
        CPUID
        MOV     [RDI], EAX;
        MOV     [RDI + 8], ECX;
        MOV     [RDI + 12], EDX;
        POP     RDI
        POP     RBX
end;

function GetCPUOem64: TCnCPUIDResult; assembler; register;
asm
        PUSH    RBX
        PUSH    RDI
        MOV     RDI, RCX
        MOV     EAX, 0
        CPUID
        MOV     [RDI], EAX;
        MOV     [RDI + 4], EBX;
        MOV     [RDI + 8], ECX;
        MOV     [RDI + 12], EDX;
        POP     RDI
        POP     RBX
end;

{$ENDIF}

constructor TCnCpuId.Create;
begin
  FSupportCpuIds := TList.Create;
  FSupportCpuSns := TList.Create;
  FCPUIds := TStringList.Create;
  FCPUInfos := TStringList.Create;
  FCPUOems := TStringList.Create;
  FCPUIdFormat := ifContinuous;

  ReadCPUInfo;
  ReadLogicalCPUInfo;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := CpuUsageTimer;
  FTimer.Enabled := True;
  RefreshCPUUsages;

  if Win32Platform = VER_PLATFORM_WIN32_NT then // NT 下需要采样判断
    FCPUUsageRead := False;
end;

destructor TCnCpuId.Destroy;
begin
  FTimer.Free;
  FCPUOems.Free;
  FCPUInfos.Free;
  FCPUIds.Free;
  FSupportCpuSns.Free;
  FSupportCpuIds.Free;
end;

// 获取 CPU 信息字符串
function TCnCpuId.GetCnCPUInfoString: AnsiString;
const
  cnIFContinuous = '%.8x%.8x%.8x%.8x';
  cnIFDashed = '%.8x-%.8x-%.8x-%.8x';
var
  iEax,iEbx,iEcx,iEdx: Integer;
{$IFDEF WIN64}
  Rec64: TCnCPUIDResult;
{$ENDIF}
begin
{$IFDEF WIN64}
  Rec64 := GetCPUInfo64;
  iEax := Rec64[0];
  iEbx := Rec64[1];
  iEcx := Rec64[2];
  iEdx := Rec64[3];
{$ELSE}
  asm
    PUSH EBX
    PUSH ECX
    PUSH EDX
    MOV  EAX, $1
    DW $A20F      //CPUID
    MOV IEAX, EAX
    MOV IEBX, EBX
    MOV IECX, ECX
    MOV IEDX, EDX
    POP EDX
    POP ECX
    POP EBX
  end;
{$ENDIF}

  if FCPUIdFormat = ifContinuous then
    Result := AnsiString(Format(cnIFContinuous, [iEax, iEbx, iEcx, iEdx]))
  else
    Result := AnsiString(Format(cnIFDashed, [iEax, iEbx, iEcx, iEdx]))
end;

// 获取 CPU 序列号
function TCnCpuId.GetCnCPUID: AnsiString;
const
  SCnIFContinuous = '%.4x%.4x%.4x%.4x%.4x%.4x';
  SCnIFDashed = '%.4x-%.4x-%.4x-%.4x-%.4x-%.4x';
var
  SFmt: string;
  iEax, iEcx, iEdx, iEdx1: Integer;
{$IFDEF WIN64}
  Rec64: TCnCPUIDResult;
{$ENDIF}
begin
{$IFDEF WIN64}
  Rec64 := GetCPUID64;
  iEax := Rec64[0];
  iEdx1 := Rec64[1];
  iEcx := Rec64[2];
  iEdx := Rec64[3];
{$ELSE}
  asm
    PUSH EBX
    PUSH ECX
    PUSH EDX
    MOV  EAX, $1
    DW $A20F      //CPUID
    MOV IEAX, EAX
    MOV IEDX1, EDX
    MOV EAX, $3
    DW $A20F
    MOV IECX, ECX
    MOV IEDX, EDX
    POP EDX
    POP ECX
    POP EBX
  end;
{$ENDIF}

  if FCPUIdFormat = ifContinuous then
    SFmt := SCnIFContinuous
  else
    SFmt := SCnIFDashed;

  if iEdx1 and (1 shr 18) = 0 then // Cpu 序列号不能读，返回全0
  begin
    Result := AnsiString(Format(SFmt, [0, 0, 0, 0, 0, 0]));
    FSupportCpuSns.Add(nil); // 加 False
  end
  else
  begin
    FSupportCpuSns.Add(Pointer(True));
    Result := AnsiString(Format(SFmt,
      [(iEax and $FFFF0000) shr 16, iEax and $FFFF,
       (iEcx and $FFFF0000) shr 16, iEcx and $FFFF,
       (iEdx and $FFFF0000) shr 16, iEdx and $FFFF]));
  end;
end;

// 获得 CPU OEM 厂商名
function TCnCpuId.GetCnCPUOem: AnsiString;
var
  iEax, iEbx, iEcx, iEdx: Integer;
{$IFDEF WIN64}
  Rec64: TCnCPUIDResult;
{$ENDIF}
begin
{$IFDEF WIN64}
  Rec64 := GetCPUOem64;
  iEax := Rec64[0];
  iEbx := Rec64[1];
  iEcx := Rec64[2];
  iEdx := Rec64[3];
{$ELSE}
  asm
    PUSH EBX
    PUSH ECX
    PUSH EDX
    MOV  EAX, $0
    DW $A20F      //CPUID
    MOV IEAX, EAX
    MOV IEBX, EBX
    MOV IECX, ECX
    MOV IEDX, EDX
    POP EDX
    POP ECX
    POP EBX
  end;
{$ENDIF}
  SetLength(Result, 3 * SizeOf(Integer));
  CopyMemory(@Result[1], @iEbx, SizeOf(Integer));
  CopyMemory(@Result[1 + SizeOf(Integer)], @iEdx, SizeOf(Integer));
  CopyMemory(@Result[1 + 2 * SizeOf(Integer)], @iEcx, SizeOf(Integer));
end;

function GetCnCpuIdSupport: Boolean;
asm
{$IFDEF WIN64}
  MOV     AL, 1  // 64 位暂返回支持
{$ELSE}
  PUSHFD   //不允许直接存取，必须通过堆栈
  POP     EAX
  MOV     EDX, EAX
  XOR     EAX, SCN_CPUID_BIT
  PUSH    EAX
  POPFD
  PUSHFD
  POP     EAX
  XOR     EAX,EDX  // 检测 ID 位是否受影响
  JZ      @exit    // CPUID 无效
  MOV     AL, 1  // CPUID 有效
@exit:
{$ENDIF}
end;

// 获取所有 CPU 的序列号
procedure TCnCpuId.ReadCPUInfo;
var
  I: Integer;
  Mask: Integer;
  CurrProc: THandle;
  SysInfo: TSystemInfo;
  ProcessAffinityOld: TCnNativeUInt;
  ProcessAffinity: TCnNativeUInt;
  SystemAffinity: TCnNativeUInt;
begin
  FCPUCount := 0;
  FSupportCpuIds.Clear;
  FSupportCpuSns.Clear;
  FCPUIds.Clear;
  FCPUOems.Clear;
  FCPUInfos.Clear;
  
  // 获取 CPU 个数
  GetSystemInfo(SysInfo);
  FCPUCount := SysInfo.dwNumberOfProcessors;

  // 获取所有 CPU 的序列号
  Mask := $1;
  CurrProc := GetCurrentProcess;
  if not GetProcessAffinityMask(CurrProc, ProcessAffinityOld, SystemAffinity) then
    Exit;

  try
    for I := 0 to FCpuCount - 1 do
    begin
      ProcessAffinity := Mask shl I;
      if not SetProcessAffinityMask(CurrProc, ProcessAffinity) then
        Break;

      FSupportCpuIds.Add(Pointer(GetCnCpuIdSupport));
      if FSupportCpuIds[FSupportCpuIds.Count - 1] <> nil then
      begin
        FCPUIds.Add(string(GetCnCPUID));
        FCPUInfos.Add(string(GetCnCPUInfoString));
        FCPUOems.Add(string(GetCnCPUOem));
      end
      else
      begin
        FCPUIds.Add('');
        FCPUInfos.Add('');
        FCPUOems.Add('');
      end;
    end;
  finally
    //恢复默认
    SetProcessAffinityMask(CurrProc, ProcessAffinityOld);
  end;
end;

procedure TCnCpuId.SetCPUIdFormat(ACPUIdFormat: TCnCPUIdFormat);
begin
  if FCPUIdFormat <> ACPUIdFormat then
  begin
    FCPUIdFormat := ACPUIdFormat;
    ReadCPUInfo;
  end;
end;

// 获得单 CPU 系统 ID
function TCnCpuId.GetFirstCPUId: string;
begin
  if FCPUIds.Count > 0 then
    Result := FCPUIds.Strings[0];
end;

// 得到第几个 CPU 的序列号
function TCnCpuId.GetCPUId(Index: Integer): string;
begin
  Result := '';
  // 保证 FCPUIds 索引的合法性
  if (Index < 0) or (Index > FCPUIds.Count - 1) then
    Exit;

  Result := FCPUIds.Strings[Index];
end;

function TCnCpuId.GetAverageCPUUsage: Integer;
begin
  if not FCPUUsageRead then
    Result := -1
  else
    Result := FAverageCPUUsage;
end;

function TCnCpuId.GetCPUUsage(Index: Integer): Integer;
begin
  if not FCPUUsageRead or (Index > FCPUCount - 1) then
    Result := -1
  else
    Result := FCPUUsage[Index];
end;

function TCnCpuId.GetFirstCPUUsage: Integer;
begin
  if FCPUUsageRead and (FCPUCount > 0) then
    Result := FCPUUsage[0]
  else
    Result := -1;
end;

function TCnCpuId.RefreshCPUUsages: Cardinal;
var
  CpuCnt: Cardinal;
  I: integer;
  Spt: Pointer;
  Sbi: TSystemBasicInformation;

  dwType, cbData: Cardinal;
  hOpen: HKEY;
  Buffer: Cardinal;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    for I := 0 to FCPUCount - 1 do // 保存旧值
      FLastCnt[I] := FCurCnt[I];

    if NtQuerySystemInformation(SystemBasicInformation, @Sbi, SizeOf(Sbi), nil)
      <> STATUS_SUCCESS then
      CpuCnt := 1
    else
      CpuCnt := Sbi.NumberProcessors;

    Spt := AllocMem(CpuCnt * (SizeOf(TSystemProcessorTimes) + 4));
    NtQuerySystemInformation(SystemProcessorTimes, Spt, CpuCnt * (SizeOf(TSystemProcessorTimes) + 4), @Result);

    for I := 0 to CpuCnt - 1  do
    begin
      with PSystemProcessorTimes(PChar(Spt) + I * (sizeof(TSystemProcessorTimes) + 4))^ do
        FCurCnt[I] := IdleTime.QuadPart;
        
      // 采样后计算
      try
        FCPUUsage[I] := Round((10000000 - (FCurCnt[I] - FLastCnt[I]) / (FTimer.Interval / 1000)) / 10000000 * 100);
      except
        FCPUUsage[I] := 0;
      end;
    end;
    FreeMem(spt);
  end
  else
  begin
    if RegOpenKeyEx(HKEY_DYN_DATA,'PerfStats\StatData', 0, KEY_READ, hOpen) = ERROR_SUCCESS then
    begin
      cbData:=sizeof(Cardinal);
      if RegQueryValueEx(hOpen, 'KERNEL\CPUUsage', nil, @dwType, PBYTE(@Buffer),
        @cbData) = ERROR_SUCCESS then
        FCPUUsage[0] := Buffer;
      RegCloseKey(hOpen);
    end
    else
      FCPUUsage[0] := -1;
  end;

  FCPUUsageRead := True;
end;

procedure TCnCpuId.CpuUsageTimer(Sender: TObject);
var
  I: Integer;
begin
  RefreshCPUUsages;
  
  FAverageCPUUsage := 0;
  for I := 0 to FCPUCount - 1 do
  begin
    if FCPUUsage[I] <> -1 then
      FAverageCPUUsage := FAverageCPUUsage + FCPUUsage[I];
  end;

  if FCPUCount > 0 then
    FAverageCPUUsage := Round(FAverageCPUUsage / FCPUCount)
  else
    FAverageCPUUsage := -1;
end;

function TCnCpuId.GetCPUOem(Index: Integer): string;
begin
  Result := '';
  // 保证 FCPUIds 索引的合法性
  if (Index < 0) or (Index > FCPUOems.Count - 1) then
    Exit;

  Result := FCPUOems.Strings[Index];
end;

function TCnCpuId.GetFirstCPUOem: string;
begin
  if FCPUOems.Count > 0 then
    Result := FCPUOems.Strings[0];
end;

function TCnCpuId.GetSupportCPUId(Index: Integer): Boolean;
begin
  Result := False;
  // 保证 FSupportCpuIds 索引的合法性
  if (Index < 0) or (Index > FSupportCpuIds.Count - 1) then
    Exit;

  Result := Boolean(FSupportCpuIds[Index]);
end;

function TCnCpuId.GetSupportCPUSn(Index: Integer): Boolean;
begin
  Result := False;
  // 保证 FSupportCpuIds 索引的合法性
  if (Index < 0) or (Index > FSupportCpuSns.Count - 1) then
    Exit;

  Result := Boolean(FSupportCpuSns[Index]);
end;

function TCnCpuId.GetFirstSupportCPUId: Boolean;
begin
  Result := False;
  if FSupportCpuIds.Count > 0 then
    Result := Boolean(FSupportCpuIds[0]);
end;

function TCnCpuId.GetFirstSupportCPUSn: Boolean;
begin
  Result := False;
  if FSupportCpuSns.Count > 0 then
    Result := Boolean(FSupportCpuSns[0]);
end;

function TCnCpuId.GetCPUInfoString(Index: Integer): string;
begin
  Result := '';
  if (Index < 0) or (Index > FCPUInfos.Count - 1) then
    Exit;

  Result := FCPUInfos.Strings[Index];
end;

function TCnCpuId.GetFirstCPUInfoString: string;
begin
  if FCPUInfos.Count > 0 then
    Result := FCPUInfos[0];
end;

function FindAwardBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  Loop: Byte;
begin
  Result:= 0;
  ABiosAddr:= PByte(DWORD(BiosAddr) + $EC71);

  CopyMemory(@szBiosData[0], ABiosAddr, 127);
  szBiosData[127]:= #0;

  Len:= StrLen(PChar(@szBiosData[0]));
  if (Len <= 0) or (Len >= 128) then
    Exit;

  //AWard:         07/08/2002-i845G-ITE8712-JF69VD0CC-00
  //Phoenix-Award: 03/12/2002-sis645-p4s333
  if (szBiosData[2] <> '/') or (szBiosData[5] <> '/') then
    Exit;

  Loop:= 0;
  while szBiosData[Loop] <> #0 do
  begin
    if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
      Break;
    Inc(Loop);
  end;

  if szBiosData[Loop] = #0 then
  begin
    BiosAddr:= ABiosAddr;
    Result:= Len;
  end;
end;

function FindAmiBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  Loop: Byte;
begin
  Result:= 0;
  ABiosAddr:= PByte(DWORD(BiosAddr) + $F478);

  CopyMemory(@szBiosData[0], ABiosAddr, 127);
  szBiosData[127]:= #0;

  Len:= StrLen(PChar(@szBiosData[0]));
  if (Len <= 0) or (Len >= 128) then
    Exit;

  // Example: "AMI: 51-2300-000000-00101111-030199-"
  if (szBiosData[2] <> '-') or (szBiosData[7] <> '-') then
    Exit;

  Loop:= 0;
  while szBiosData[Loop] <> #0 do
  begin
    if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
      Break;
    Inc(Loop);
  end;

  if szBiosData[Loop] = #0 then
  begin
    BiosAddr:= ABiosAddr;
    Result:= Len;
  end;
end;

function FindPhoenixBios(var BiosAddr: PByte): UINT;
var
  ABiosAddr: PByte;
  szBiosData: array [0..127] of Char;
  Len: Integer;
  I, Loop: Byte;
begin
  for I := 0 to 2 do
  begin
    ABiosAddr:= PByte(DWORD(BiosAddr) + BiosOffset[I]);
    CopyMemory(@szBiosData[0], ABiosAddr, 127);
    szBiosData[127]:= #0;
    Len:= StrLen(PChar(@szBiosData[0]));
    if (Len <= 0) or (Len >= 128) then
      Continue;

    // Example: Phoenix "NITELT0.86B.0044.P11.9910111055"
    if (szBiosData[7] <> '.') or (szBiosData[11] <> '.') then
      Continue;

    Loop:= 0;
    while szBiosData[Loop] <> #0 do
    begin
      if (szBiosData[Loop] < ' ') or (szBiosData[Loop] >= Chr(127)) then
        Break;
      Inc(Loop);
    end;

    if szBiosData[Loop] = #0 then
    begin
      BiosAddr:= ABiosAddr;
      Result:= Len;
      Exit;
    end;
  end;
  Result:= 0;
end;

function CnGetBiosID: string;
var
  Size: DWORD;
  RealAddr: LARGE_INTEGER;
  Path: PWCHAR;
  BaseAddr: DWORD;
  UniString: TUNICODE_STRING;
  Obj: TOBJECT_ATTRIBUTES;
  hSection: THandle;
  PBiosSerial: PByte;
  uBiosSerialLen: UINT;
  szSystemInfo: array [0..4095] of Char;
  ReturnLen: UINT;
begin
  FillChar(szSystemInfo, 4096, 0);
  ReturnLen:= 0;

  RealAddr.LowPart:= $000F0000;
  RealAddr.HighPart:= $00000000;
  Size:= $FFFF;
  Path:= '\device\physicalmemory';
  BaseAddr:= 0;
  UniString.Buffer:= Path;
  UniString.Length:= $2C;
  UniString.MaximumLength:= $2E;

  Obj.Attributes:= 64;
  Obj.Length:= 24;
  Obj.ObjectName:= @UniString;
  Obj.RootDirectory:= 0;
  Obj.SecurityDescriptor:= nil;
  Obj.SecurityQualityOfService:= nil;

  //调用函数，对物理内存进行映射
  if (ZwOpenSection(hSection, 4, @Obj) = 0) and
     (ZwMapViewOfSection(hSection, $FFFFFFFF, @BaseAddr, 0, $FFFF, @RealAddr, @Size, 1, 0, 2) = 0) then
  begin
    //执行后会在当前进程的空间开辟一段64k的空间，并把f000:0000到f000:ffff处的内容映射到这里
    //映射的基址由BaseAddr返回,如果映射不再有用,应该用ZwUnmapViewOfSection断开映射
    PBiosSerial:= PByte(BaseAddr);
    uBiosSerialLen:= FindAwardBios(PBiosSerial);
    if uBiosSerialLen = 0 then
    begin
      uBiosSerialLen:= FindAmiBios(PBiosSerial);
      if uBiosSerialLen = 0 then
        uBiosSerialLen:= FindPhoenixBios(PBiosSerial);
    end;

    if uBiosSerialLen <> 0 then
    begin
      CopyMemory(Pointer(szSystemInfo + ReturnLen), PBiosSerial, uBiosSerialLen);
      Inc(ReturnLen, uBiosSerialLen);
    end;
    ZwUnmapViewOfSection($FFFFFFFF, Pointer(BaseAddr));
  end;

  if ReturnLen <> 0 then
  begin
    SetLength(Result, ReturnLen);
    MoveMemory(@Result[1], @szSystemInfo[0], ReturnLen);
  end;
end;

{ TCnHardDiskInfo }

constructor TCnHardDiskInfo.Create;
begin
  inherited;
  FHardDiskSns := TStringList.Create;
  FVolumnLetters := TStringList.Create;
  FVolumnNames := TStringList.Create;
  ReadHardDisk;
end;

destructor TCnHardDiskInfo.Destroy;
begin
  FVolumnNames.Free;
  FVolumnLetters.Free;
  FHardDiskSns.Free;
  inherited;
end;

function TCnHardDiskInfo.GetDiskSerialNo(Index: Integer): string;
begin
  if (Index >= 0) or (Index < FHardDiskSns.Count) then
    Result := FHardDiskSns[Index]
  else
    Result := '';
end;

function TCnHardDiskInfo.GetVolumnLetter(Index: Integer): string;
begin
  if (Index >= 0) or (Index < FVolumnLetters.Count) then
    Result := FVolumnLetters[Index]
  else
    Result := '';
end;

function TCnHardDiskInfo.GetVolumnName(Index: Integer): string;
begin
  if (Index >= 0) or (Index < FVolumnNames.Count) then
    Result := FVolumnNames[Index]
  else
    Result := '';
end;

procedure TCnHardDiskInfo.ReadHardDisk;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    ReadPhysicalDriveInNTWithZeroRights;
  end
  else
  begin
    // TODO: Win9x
  end;

  ReadVolumnInfo;
end;

function        LoCase( ch : AnsiChar ) : AnsiChar;
begin
//asm
//{ ->    AL      Character       }
//{ <-    AL      Result          }
//
//        CMP     AL,'A'
//        JB      @@exit
//        CMP     AL,'Z'
//        JA      @@exit
//        ADD     AL,'a' - 'A'
//@@exit:
  if Ch in ['A'..'Z'] then
    Result := AnsiChar(Chr(Ord(Ch) + (Ord('a') - Ord('A'))))
  else
    Result := Ch;
end;

procedure TCnHardDiskInfo.ReadPhysicalDriveInNTWithZeroRights;
var
  I: Integer;
  H: THandle;
  Drive: string;
  BytesReturned: Cardinal;
  Query: TStoragePropertyQuery;
  Buffer: array[0..8191] of Byte;
  Descriptor: PStorageDeviceDescriptor;
  Sn: AnsiString;

  function flipAndCodeBytes(Str: PAnsiChar; Offset: Integer): AnsiString;
  var
    I, J, K: Integer;
    P: Integer;
    C, T: AnsiChar;
    Buf: PAnsiChar;
  begin
    if Offset <= 0 then // If offset 0, no serial number.
    begin
      Result := '';
      Exit;
    end;

    SetLength(Result, 1024);
    ZeroMemory(@(Result[1]), Length(Result));

    K := 0;
    Buf := @(Result[1]);

    P := 0;
    J := 1;
    I := Offset;
    while (J > 0) and (Str[I] <> #0) do  // 十六进制解码
    begin
      C := LoCase(Str[I]);
      if C = ' ' then
        C := '0';

      Inc(P);
      Buf[K] := AnsiChar(Chr((Ord(Buf[K]) shl 4)));
      if C in ['0'..'9'] then
        Buf[K] := AnsiChar(Chr(Ord(Buf[K]) or (Ord(C) - Ord('0'))))
      else if C in ['a'..'f'] then
        Buf[K] := AnsiChar(Chr(Ord(Buf[K]) or (Ord(C) - Ord('a') + 10)))
      else
      begin
        J := 0;
        Break;
      end;

      if P = 2 then
      begin
        if (Buf[K] <> #0) and (Ord(Buf[K]) <= $1F) then
        begin
          J := 0;
          Break;
        end;
        Inc(K);
        P := 0;
        Buf[K] := #0;
      end;

      Inc(I);
    end;

    if J = 0 then
    begin
      J := 1;
      K := 0;
      I := Offset;
      while (J <> 0) and (Str[I] <> #0) do
      begin
        if Ord(Str[I]) <= $1F then
        begin
          J := 0;
          Break;
        end;
        Buf[K] := Str[I];
        Inc(K);
        Inc(I);
      end;
    end;

    if J = 0 then
      K := 0;

    // 翻转前后字节
    Buf[K] := #0;
    J := 0;
    while J < K do
    begin
      T := Buf[J];
      Buf[J] := Buf[J + 1];
      Buf[J + 1] := T;
      Inc(J, 2);
    end;
{$IFDEF UNICODE}
    Result := AnsiTrim(Result);
{$ELSE}
    Result := Trim(Result);
{$ENDIF}
  end;

begin
  for I := 0 to 15 do  // Max Disk number is 16
  begin
    Drive := '\\.\PhysicalDrive' + IntToStr(I);
    H := CreateFile(PChar(Drive), 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, 0, 0);
    if H = INVALID_HANDLE_VALUE then
      Exit;

    try
      FillChar(Buffer, SizeOf(Buffer), 0);
      FillChar(Query, SizeOf(Query), 0);
      Query.PropertyId := StorageDeviceProperty;
      Query.QueryType := PropertyStandardQuery;

      BytesReturned := 0;
      if DeviceIoControl(H, IOCTL_STORAGE_QUERY_PROPERTY, @Query, SizeOf(Query),
        @(Buffer[0]), SizeOf(Buffer), BytesReturned, nil) then
      begin
        Descriptor := PStorageDeviceDescriptor(@(Buffer[0]));
        SetLength(Sn, 1024);
        ZeroMemory(@(Sn[1]), Length(Sn));

        StrCopy(PAnsiChar(@(Sn[1])), PAnsiChar(flipAndCodeBytes(@(Buffer[0]), Descriptor^.SerialNumberOffset)));

{$IFDEF UNICODE}
        Sn := AnsiTrim(Sn);
{$ELSE}
        Sn := Trim(Sn);
{$ENDIF}

        FHardDiskSns.Add(Trim(string(Sn)));
        Inc(FHardDiskCount);
      end;
    finally
      CloseHandle(H);
    end
  end;
end;

procedure TCnHardDiskInfo.ReadVolumnInfo;
var
  Letter: Char;
  Drive: string;
  NotUsed: DWORD;
  VolumeFlags: DWORD;
  VSNumber: DWORD;
  PType: array[0..63] of Char;
  VName: array[0..63] of Char;
begin
  Letter := 'C';
  while Ord(Letter) <= Ord('Z') do
  begin
    Drive := Letter + ':\';
    if GetVolumeInformation(PChar(Drive), @VName, SizeOf(VName),
      @VSNumber, NotUsed, VolumeFlags, PType, 64) then
    begin
      FVolumnLetters.Add(Drive);
      FVolumnNames.Add(VName);
      Inc(FVolumnCount);
    end;

    Inc(Letter);
  end;
end;

procedure TCnCpuId.ReadLogicalCPUInfo;
var
  Buf: array of TSystemLogicalProcessorInformation;
  RetLen: DWORD;
  I: Integer;
begin
  ClearLocalCPUInfo;
  if not Assigned(GetLogicalProcessorInformation) then
    Exit;

  RetLen := 0;
  if GetLogicalProcessorInformation(nil, RetLen) then
    Exit;

  if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
    Exit;

  SetLength(Buf, (RetLen div SizeOf(TSystemLogicalProcessorInformation)) + 1);
  if not GetLogicalProcessorInformation(@Buf[0], RetLen) then
    Exit;

  for I := Low(Buf) to High(Buf) do
  begin
    case Buf[I].Relationship of
      RelationCache:
        begin
          case Buf[I].Cache.Level of
            1: Inc(FL1CacheCount);
            2: Inc(FL2CacheCount);
            3: Inc(FL3CacheCount);
          end;
        end;
      RelationNumaNode:
        begin
          Inc(FNumaNodeCount);
        end;
      RelationProcessorCore:
        begin
          Inc(FProcessorCoreCount);
          Inc(FLogicalProcessorCount, CountSetBits(Buf[I].ProcessorMask));
        end;
      RelationProcessorPackage:
        begin
          Inc(FProcessorPackageCount);
        end;
    end;
  end;
end;

procedure TCnCpuId.ClearLocalCPUInfo;
begin
  FProcessorCoreCount := 0;
  FL1CacheCount := 0;
  FNumaNodeCount := 0;
  FL3CacheCount := 0;
  FProcessorPackageCount := 0;
  FLogicalProcessorCount := 0;
  FL2CacheCount := 0;
end;

initialization
  GetNtNativeAPIs;

finalization
  FreeNtNativeAPIs;

end.
