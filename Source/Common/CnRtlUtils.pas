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

unit CnRtlUtils;
{* |<PRE>
================================================================================
* 软件名称：CnDebugger
* 单元名称：CnDebug 相关的运行期工具单元
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：该单元实现了部分 CnDebugger 所需的 Module/Stack 相关内容
*           部分内容引用了 JCL
* 开发平台：PWin7 + Delphi 5
* 兼容测试：Win32/Win64
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2020.05.04
*               实现当前堆栈的两种调用地址追踪实现，同时支持 32/64，其中 StackWalk64 有问题
*           2020.04.26
*               创建单元,实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Contnrs, TLHelp32, Psapi, Imagehlp;

type
  PCnStackFrame = ^TCnStackFrame;
  TCnStackFrame = record
    CallersEBP: Pointer;
    CallerAdr: Pointer;
  end;

  TCnModuleInfo = class(TObject)
  {* 描述一个模块信息，exe 或 dll 或 bpl 等，支持 32/64 位}
  private
    FSize: Cardinal;
    FStartAddr: Pointer;
    FEndAddr: Pointer;
    FBaseName: string;
    FFullName: string;
    FIsDelphi: Boolean;
    FHModule: HMODULE;
  public
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property BaseName: string read FBaseName write FBaseName;
    {* 模块文件名}
    property FullName: string read FFullName write FFullName;
    {* 模块完整路径名}
    property Size: Cardinal read FSize write FSize;
    {* 模块大小}
    property HModule: HMODULE read FHModule write FHModule;
    {* 模块 Handle，也就是 AllocationBase，一般等于 StartAddr}
    property StartAddr: Pointer read FStartAddr write FStartAddr;
    {* 模块在本进程地址空间的起始地址，也即 lpBaseOfDll}
    property EndAddr: Pointer read FEndAddr write FEndAddr;
    {* 模块在本进程地址空间的结束地址}
    property IsDelphi: Boolean read FIsDelphi write FIsDelphi;
    {* 是否是 Delphi 模块，指通过 System.pas 中的 LibModuleList 注册过的}
  end;

  TCnModuleInfoList = class(TObjectList)
  {* 描述本进程内的所有模块信息，exe 或 dll 或 bpl 等，支持 32/64 位}
  private
    FDelphiOnly: Boolean;
    function GetItems(Index: Integer): TCnModuleInfo;
    function GetModuleFromAddress(Addr: Pointer): TCnModuleInfo;
    function AddModule(PH: THandle; MH: HMODULE): TCnModuleInfo;
    procedure CheckDelphiModule(Info: TCnModuleInfo);
  protected
    procedure BuildModulesList;
    function CreateItemForAddress(Addr: Pointer; AIsDelphi: Boolean): TCnModuleInfo;
  public
    constructor Create(ADelphiOnly: Boolean = False); virtual;
    destructor Destroy; override;

    procedure DumpToStrings(List: TStrings);

    function IsValidModuleAddress(Addr: Pointer): Boolean;
    {* 判断某地址是否属于本进程内一个模块内的地址}
    function IsDelphiModuleAddress(Addr: Pointer): Boolean;
    {* 判断某地址是否属于本进程内一个 Delphi 模块内的地址}
    property Items[Index: Integer]: TCnModuleInfo read GetItems;
  end;

  TCnStackInfo = class
  {* 描述一层堆栈调用地址，支持 32/64 位}
  private
    FCallerAddr: Pointer;
  public
    property CallerAddr: Pointer read FCallerAddr write FCallerAddr;
  end;

  TCnStackInfoList = class(TObjectList)
  {* 描述对象实例化时当前调用栈的多层地址，支持 32/64 位}
  private
    FModuleList: TCnModuleInfoList;
    function GetItems(Index: Integer): TCnStackInfo;
    procedure TraceStackFrames;
  public
    constructor Create(OnlyDelphi: Boolean = False);
    destructor Destroy; override;

    procedure DumpToStrings(List: TStrings);

    property Items[Index: Integer]: TCnStackInfo read GetItems; default;
  end;

implementation

const
{$IFDEF WIN64}
  HEX_FMT = '$%16.16x';
{$ELSE}
  HEX_FMT = '$%8.8x';
{$ENDIF}

  MODULE_INFO_FMT = 'HModule: ' + HEX_FMT + ' Base: ' + HEX_FMT + ' End: ' +
    HEX_FMT + ' Size: ' + HEX_FMT + ' IsDelphiModule %d. Name: %s - %s';
  STACK_INFO_FMT = 'Caller: ' + HEX_FMT;

  MAX_STACK_COUNT = 1024;
  ImagehlpLib = 'IMAGEHLP.DLL';

type
{$IFDEF WIN64}
  TCnNativeUInt = NativeUInt;
{$ELSE}
  TCnNativeUInt = Cardinal;
{$ENDIF}

  TRtlCaptureStackBackTrace = function (FramesToSkip: LongWord; FramesToCapture: LongWord;
    var BackTrace: Pointer; BackTraceHash: PLongWord): Word; stdcall;
  TRtlCaptureContext = procedure (ContextRecord: PContext); stdcall;

{$IFDEF WIN64}
  // Types of Address
  LPADDRESS64 = ^ADDRESS64;
  {$EXTERNALSYM PADDRESS64}
  _tagADDRESS64 = record
    Offset: DWORD64;
    Segment: WORD;
    Mode: ADDRESS_MODE;
  end;
  {$EXTERNALSYM _tagADDRESS64}
  ADDRESS64 = _tagADDRESS64;
  {$EXTERNALSYM ADDRESS64}
  TAddress64 = ADDRESS64;
  PAddress64 = LPADDRESS64;

  // Types of KDHelp
  PKDHELP64 = ^KDHELP64;
  {$EXTERNALSYM PKDHELP64}
  _KDHELP64 = record
    Thread: DWORD64;
    ThCallbackStack: DWORD;
    ThCallbackBStore: DWORD;
    NextCallback: DWORD;
    FramePointer: DWORD;
    KiCallUserMode: DWORD64;
    KeUserCallbackDispatcher: DWORD64;
    SystemRangeStart: DWORD64;
    Reserved: array [0..7] of DWORD64;
  end;
  {$EXTERNALSYM _KDHELP64}
  KDHELP64 = _KDHELP64;
  {$EXTERNALSYM KDHELP64}
  TKdHelp64 = KDHELP64;

  // Types of StackFrame64
  LPSTACKFRAME64 = ^STACKFRAME64;
  {$EXTERNALSYM LPSTACKFRAME64}
  _tagSTACKFRAME64 = record
    AddrPC: ADDRESS64; // program counter
    AddrReturn: ADDRESS64; // return address
    AddrFrame: ADDRESS64; // frame pointer
    AddrStack: ADDRESS64; // stack pointer
    AddrBStore: ADDRESS64; // backing store pointer
    FuncTableEntry: PVOID; // pointer to pdata/fpo or NULL
    Params: array [0..3] of DWORD64; // possible arguments to the function
    Far: BOOL; // WOW far call
    Virtual: BOOL; // is this a virtual frame?
    Reserved: array [0..2] of DWORD64;
    KdHelp: KDHELP64;
  end;
  {$EXTERNALSYM _tagSTACKFRAME64}
  STACKFRAME64 = _tagSTACKFRAME64;
  {$EXTERNALSYM STACKFRAME64}
  TStackFrame64 = STACKFRAME64;
  PStackFrame64 = LPSTACKFRAME64;

  // Types of Other Routines
  PREAD_PROCESS_MEMORY_ROUTINE64 = function (hProcess: THandle; qwBaseAddress: DWORD64;
    lpBuffer: PVOID; nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;

  PFUNCTION_TABLE_ACCESS_ROUTINE64 = function (hProcess: THandle;
    AddrBase: DWORD64): PVOID; stdcall;

  PGET_MODULE_BASE_ROUTINE64 = function (hProcess: THandle;
    Address: DWORD64): DWORD64; stdcall;

  PTRANSLATE_ADDRESS_ROUTINE64 = function (hProcess: THandle; hThread: THandle;
    const lpaddr: ADDRESS64): DWORD64; stdcall;

  TStackWalk64 = function(MachineType: DWORD; hProcess, hThread: THandle;
    StackFrame: PStackFrame64; ContextRecord: Pointer;
    ReadMemoryRoutine: PREAD_PROCESS_MEMORY_ROUTINE64;
    FunctionTableAccessRoutine: PFUNCTION_TABLE_ACCESS_ROUTINE64;
    GetModuleBaseRoutine: PGET_MODULE_BASE_ROUTINE64;
    TranslateAddress: PTRANSLATE_ADDRESS_ROUTINE64): BOOL; stdcall;

function SymFunctionTableAccess64(hProcess: THandle; AddrBase: DWORD64): PVOID; stdcall;
  external ImagehlpLib name 'SymFunctionTableAccess64';
function SymGetModuleBase64(hProcess: THandle; Address: DWORD64): DWORD64; stdcall;
  external ImagehlpLib name 'SymGetModuleBase64';

{$ENDIF}

var
  // XP 以前的平台不支持这批 API，需要动态加载
  RtlCaptureStackBackTrace: TRtlCaptureStackBackTrace = nil;
  RtlCaptureContext: TRtlCaptureContext = nil;

{$IFDEF WIN64} // 64 位下必须用未声明的 StackWalk64
  StackWalk64: TStackWalk64 = nil;
{$ENDIF}

// 查询某虚拟地址所属的分配模块 Handle，也就是 AllocationBase
function ModuleFromAddr(const Addr: Pointer): HMODULE;
var
  MI: TMemoryBasicInformation;
begin
  VirtualQuery(Addr, MI, SizeOf(MI));
  if MI.State <> MEM_COMMIT then
    Result := 0
  else
    Result := HMODULE(MI.AllocationBase);
end;

{ TCnStackInfoList }

constructor TCnStackInfoList.Create(OnlyDelphi: Boolean);
begin
  inherited Create(True);
  FModuleList := TCnModuleInfoList.Create(OnlyDelphi);
  TraceStackFrames;
end;

destructor TCnStackInfoList.Destroy;
begin
  FModuleList.Free;
  inherited;
end;

function TCnStackInfoList.GetItems(Index: Integer): TCnStackInfo;
begin
  Result := TCnStackInfo(inherited Items[Index]);
end;

procedure TCnStackInfoList.TraceStackFrames;
var
  Ctx: TContext;     // Ctx 貌似得声明靠上，不能放 Callers 这个大数组后面，否则虚拟机会出莫名其妙的错
  Info: TCnStackInfo;
  C: Word;
  I: Integer;
  Callers: array[0..MAX_STACK_COUNT - 1] of Pointer;
{$IFDEF WIN64}
  STKF64: TStackFrame64;
{$ELSE}
  STKF: TStackFrame;
{$ENDIF}
  P, T: THandle;
  Res: Boolean;
begin
  Capacity := 32;
  if Assigned(RtlCaptureStackBackTrace) then // XP/2003 or above, Support 32/64
  begin
    C := RtlCaptureStackBackTrace(0, MAX_STACK_COUNT, Callers[0], nil);
    for I := 0 to C - 1 do
    begin
      Info := TCnStackInfo.Create;
      Info.CallerAddr := Callers[I];
      Add(Info);
    end;
  end
  else if Assigned(RtlCaptureContext) {$IFDEF WIN64} and Assigned(StackWalk64) {$ENDIF} then
  begin
    // Using StackWalk in ImageHlp and RtlCaptureContext
    FillChar(Ctx, SizeOf(TContext), 0);
    RtlCaptureContext(@Ctx);                   // 64位的情况下，居然虚拟机上可能会出错
{$IFDEF WIN64}
    FillChar(STKF64, SizeOf(TStackFrame64), 0);

    STKF64.AddrPC.Mode         := AddrModeFlat;
    STKF64.AddrStack.Mode      := AddrModeFlat;
    STKF64.AddrFrame.Mode      := AddrModeFlat;
    STKF64.AddrPC.Offset       := Ctx.Rip;
    STKF64.AddrStack.Offset    := Ctx.Rsp;
    STKF64.AddrFrame.Offset    := Ctx.Rbp;
{$ELSE}
    FillChar(STKF, SizeOf(TStackFrame), 0);

    STKF.AddrPC.Mode         := AddrModeFlat;
    STKF.AddrStack.Mode      := AddrModeFlat;
    STKF.AddrFrame.Mode      := AddrModeFlat;
    STKF.AddrPC.Offset       := Ctx.Eip;
    STKF.AddrStack.Offset    := Ctx.Esp;
    STKF.AddrFrame.Offset    := Ctx.Ebp;
{$ENDIF}

    P := GetCurrentProcess;
    T := GetCurrentThread;

    while True do
    begin
{$IFDEF WIN64}
      // FIXME: 64位下 StackWalk64 始终抓不到堆栈，咋办？
      Res := StackWalk64(IMAGE_FILE_MACHINE_AMD64, P, T, @STKF64, @Ctx, nil, @SymFunctionTableAccess64,
        @SymGetModuleBase64, nil);

      if Res and (STKF64.AddrPC.Offset <> 0) then
      begin
        if STKF64.AddrReturn.Offset = 0 then
          Break;

        Info := TCnStackInfo.Create;
        Info.CallerAddr := Pointer(STKF64.AddrPC.Offset);
        Add(Info);
      end
      else
        Break;

      if STKF64.AddrReturn.Offset = 0 then
        Break;
{$ELSE}
      Res := StackWalk(IMAGE_FILE_MACHINE_I386, P, T, @STKF, @Ctx, nil, @SymFunctionTableAccess,
        @SymGetModuleBase, nil);

      if Res and (STKF.AddrPC.Offset <> 0) then
      begin
        if STKF.AddrReturn.Offset = 0 then
          Break;

        Info := TCnStackInfo.Create;
        Info.CallerAddr := Pointer(STKF.AddrPC.Offset);
        Add(Info);
      end
      else
        Break;

      if STKF.AddrReturn.Offset = 0 then
        Break;
{$ENDIF}
    end;
  end;
end;

procedure TCnStackInfoList.DumpToStrings(List: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    List.Add(Format(STACK_INFO_FMT, [TCnNativeUInt(Items[I].CallerAddr)]));
end;

{ TCnModuleInfoList }

function TCnModuleInfoList.AddModule(PH: THandle; MH: HMODULE): TCnModuleInfo;
var
  ModuleInfo: TModuleInfo;
  Info: TCnModuleInfo;
  Res: DWORD;
  AName: array[0..MAX_PATH - 1] of Char;
begin
  Result := nil;

  // 根据每个 Module Handle 拿 Module 基地址等信息
  if GetModuleInformation(PH, MH, @ModuleInfo, SizeOf(TModuleInfo)) then
  begin
    Info := TCnModuleInfo.Create;
    Info.HModule := MH;
    Info.StartAddr := ModuleInfo.lpBaseOfDll;
    Info.Size := ModuleInfo.SizeOfImage;
    Info.EndAddr := Pointer(TCnNativeUInt(ModuleInfo.lpBaseOfDll) + ModuleInfo.SizeOfImage);

    Res := GetModuleBaseName(PH, MH, @AName[0], SizeOf(AName));
    if Res > 0 then
    begin
      SetLength(Info.FBaseName, Res);
      System.Move(AName[0], Info.FBaseName[1], Res * SizeOf(Char));
    end;
    Res := GetModuleFileName(MH, @AName[0], SizeOf(AName));
    if Res > 0 then
    begin
      SetLength(Info.FFullName, Res);
      System.Move(AName[0], Info.FFullName[1], Res * SizeOf(Char));
    end;
    Add(Info);
    Result := Info;
  end;
end;

procedure TCnModuleInfoList.BuildModulesList;
var
  ProcessHandle: THandle;
  Needed: DWORD;
  Modules: array of THandle;
  I, Cnt: Integer;
  Res: Boolean;
  MemInfo: TMemoryBasicInformation;
  Base: PByte;
  LastAllocBase: Pointer;
  QueryRes: DWORD;
  CurModule: PLibModule;
begin
  if FDelphiOnly then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      CreateItemForAddress(Pointer(CurModule.Instance), True);
      CurModule := CurModule.Next;
    end;
  end
  else
  begin
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, GetCurrentProcessId);
    if ProcessHandle <> 0 then
    begin
      try
        Res := EnumProcessModules(ProcessHandle, nil, 0, Needed);
        if Res then
        begin
          Cnt := Needed div SizeOf(HMODULE);
          SetLength(Modules, Cnt);
          if EnumProcessModules(ProcessHandle, @Modules[0], Needed, Needed) then
          begin
            for I := 0 to Cnt - 1 do
              CheckDelphiModule(AddModule(ProcessHandle, Modules[I]));
          end;
        end
        else
        begin
          Base := nil;
          LastAllocBase := nil;
          FillChar(MemInfo, SizeOf(TMemoryBasicInformation), #0);

          QueryRes := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(TMemoryBasicInformation));
          while QueryRes = SizeOf(TMemoryBasicInformation) do
          begin
            if MemInfo.AllocationBase <> LastAllocBase then
            begin
              if MemInfo.Type_9 = MEM_IMAGE then
                CheckDelphiModule(AddModule(ProcessHandle, HMODULE(MemInfo.AllocationBase)));
              LastAllocBase := MemInfo.AllocationBase;
            end;
            Inc(Base, MemInfo.RegionSize);
            QueryRes := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(TMemoryBasicInformation));
          end;
        end;
      finally
        CloseHandle(ProcessHandle);
      end;
    end;
  end;
end;

// 在 System 的系统模块链里查询 Delphi 模块
procedure TCnModuleInfoList.CheckDelphiModule(Info: TCnModuleInfo);
var
  CurModule: PLibModule;
begin
  if (Info <> nil) and (Info.HModule <> 0) then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      if CurModule.Instance = Info.HModule then
      begin
        Info.IsDelphi := True;
        Exit;
      end;
      CurModule := CurModule.Next;
    end;
  end;
end;

constructor TCnModuleInfoList.Create;
begin
  inherited Create(True);
  FDelphiOnly := ADelphiOnly;
  BuildModulesList;
end;

function TCnModuleInfoList.CreateItemForAddress(Addr: Pointer;
  AIsDelphi: Boolean): TCnModuleInfo;
var
  Module: HMODULE;
  ProcessHandle: THandle;
begin
  Result := nil;
  Module := ModuleFromAddr(Addr);
  if Module > 0 then
  begin
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, GetCurrentProcessId);
    if ProcessHandle <> 0 then
    begin
      try
        Result := AddModule(ProcessHandle, Module);
        if Result <> nil then
          Result.IsDelphi := AIsDelphi;
      finally
        CloseHandle(ProcessHandle);
      end;
    end;
  end;
end;

destructor TCnModuleInfoList.Destroy;
begin

  inherited;
end;

procedure TCnModuleInfoList.DumpToStrings(List: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    List.Add(Items[I].ToString);
end;

function TCnModuleInfoList.GetItems(Index: Integer): TCnModuleInfo;
begin
  Result := TCnModuleInfo(inherited Items[Index]);
end;

function TCnModuleInfoList.GetModuleFromAddress(Addr: Pointer): TCnModuleInfo;
var
  I: Integer;
  Item: TCnModuleInfo;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if (TCnNativeUInt(Item.StartAddr) <= TCnNativeUInt(Addr)) and
      (TCnNativeUInt(Item.EndAddr) > TCnNativeUInt(Addr)) then
    begin
      Result := Item;
      Exit;
    end;
  end;
end;

function TCnModuleInfoList.IsDelphiModuleAddress(Addr: Pointer): Boolean;
var
  Info: TCnModuleInfo;
begin
  Info := GetModuleFromAddress(Addr);
  Result := (Info <> nil) and Info.IsDelphi;
end;

function TCnModuleInfoList.IsValidModuleAddress(Addr: Pointer): Boolean;
begin
  Result := GetModuleFromAddress(Addr) <> nil;
end;

{ TCnModuleInfo }

function TCnModuleInfo.ToString: string;
begin
  Result := Format(MODULE_INFO_FMT, [FHModule, TCnNativeUInt(FStartAddr),
    TCnNativeUInt(FEndAddr), FSize, Integer(FIsDelphi), FBaseName, FFullName]);
end;

procedure InitAPIs;
var
  H: HINST;
  P: Pointer;
begin
  H := GetModuleHandle(kernel32);
  if H <> 0 then
  begin
    P := GetProcAddress(H, 'RtlCaptureStackBackTrace');
    if P <> nil then
      RtlCaptureStackBackTrace := TRtlCaptureStackBackTrace(P);
  end;
  H := GetModuleHandle('ntdll.dll');
  if H <> 0 then
  begin
    P := GetProcAddress(H, 'RtlCaptureContext');
    if P <> nil then
      RtlCaptureContext := TRtlCaptureContext(P);
  end;
{$IFDEF WIN64}
  H := GetModuleHandle(ImagehlpLib);
  if H <> 0 then
  begin
    P := GetProcAddress(H, 'StackWalk64');
    if P <> nil then
      StackWalk64 := TStackWalk64(P);
  end;
{$ENDIF}
end;

initialization
  InitAPIs;

end.
